;;; bltzread-range.el --- Pick the range of text you read -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;;; Commentary:

;; Four ways of saying "I read from here to there", all working on the
;; current buffer and sharing one set of buffer-local state:
;;
;;   farthest    start marker to the farthest point reached
;;   points      start and end marks you set
;;   paragraphs  paragraph numbers appear; you type from/to
;;   lines       line numbers appear; you type from/to
;;
;; `bltzread-range-pick' returns (BEG . END).  Nothing here knows about words.

;;; Code:

(require 'subr-x)
(require 'bltzread-custom)

(defconst bltzread-range-methods '(farthest points paragraphs lines))

(defvar-local bltzread-range--start nil "Marker where reading began.")
(defvar-local bltzread-range--end nil "Marker where reading ended, for `points'.")
(defvar-local bltzread-range--farthest 1 "Farthest position reached so far.")
(defvar-local bltzread-range--overlay nil "Overlay showing the picked range.")
(defvar-local bltzread-range--numbers nil "Paragraph number overlays.")
(defvar-local bltzread-range--saved-numbering nil
  "Cons of (LOCALP . VALUE) of `display-line-numbers' before we changed it.")

;;; Text helpers

(defun bltzread-range-paragraphs ()
  "List of (BEG . END) for each paragraph in the accessible buffer."
  (save-excursion
    (let ((pos (point-min)) result)
      (while (< pos (point-max))
        (goto-char pos)
        (skip-chars-forward " \t\n")
        (if (eobp)
            (setq pos (point-max))
          (let ((beg (point)))
            (if (re-search-forward "\n[ \t]*\n" nil t)
                (setq result (cons (cons beg (match-beginning 0)) result)
                      pos (match-end 0))
              (setq result (cons (cons beg (point-max)) result)
                    pos (point-max))))))
      (nreverse result))))

;;; State

(defun bltzread-range-reset (&optional start)
  "Start tracking afresh in this buffer, with reading beginning at START."
  (let ((start (or start (point-min))))
    (setq bltzread-range--start (copy-marker start)
          bltzread-range--end nil
          bltzread-range--farthest start))
  (bltzread-range-clear-highlight))

(defun bltzread-range-start ()
  "Position where reading began, or nil."
  (and bltzread-range--start (marker-position bltzread-range--start)))

(defun bltzread-range-farthest ()
  "Farthest position reached so far."
  (when (functionp bltzread-range-farthest-source) (bltzread-range-track))
  bltzread-range--farthest)

(defun bltzread-range-end ()
  "Position marked as the end, or nil."
  (and bltzread-range--end (marker-position bltzread-range--end)))

(defun bltzread-range-track ()
  "Remember the farthest position reached in this buffer."
  (let ((pos (pcase bltzread-range-farthest-source
               ('point (point))
               ('window-end (window-end nil t))
               ((pred functionp) (funcall bltzread-range-farthest-source)))))
    (when (and (integerp pos) (> pos bltzread-range--farthest))
      (setq bltzread-range--farthest pos))))

;;; Display

(defun bltzread-range-clear-numbers ()
  "Remove paragraph numbers and restore line numbering."
  (mapc #'delete-overlay bltzread-range--numbers)
  (setq bltzread-range--numbers nil)
  (when bltzread-range--saved-numbering
    (if (car bltzread-range--saved-numbering)
        (setq-local display-line-numbers (cdr bltzread-range--saved-numbering))
      (kill-local-variable 'display-line-numbers))
    (setq bltzread-range--saved-numbering nil)))

(defun bltzread-range-show-numbers ()
  "Show the numbers the current method needs."
  (bltzread-range-clear-numbers)
  (pcase bltzread-range-method
    ('lines
     (setq bltzread-range--saved-numbering
           (cons (local-variable-p 'display-line-numbers) display-line-numbers))
     (setq-local display-line-numbers t))
    ('paragraphs
     (let ((n 0))
       (dolist (p (bltzread-range-paragraphs))
         (let ((ov (make-overlay (car p) (car p))))
           (overlay-put ov 'before-string
                        (propertize (format "[%d] " (setq n (1+ n)))
                                    'face 'bltzread-number))
           (push ov bltzread-range--numbers)))))))

(defun bltzread-range-highlight (beg end)
  "Highlight BEG to END as the picked range."
  (if bltzread-range--overlay
      (move-overlay bltzread-range--overlay beg end)
    (setq bltzread-range--overlay (make-overlay beg end))
    (overlay-put bltzread-range--overlay 'face 'bltzread-range)
    (overlay-put bltzread-range--overlay 'priority 100)))

(defun bltzread-range-clear-highlight ()
  "Remove the range highlight."
  (when bltzread-range--overlay
    (delete-overlay bltzread-range--overlay)
    (setq bltzread-range--overlay nil)))

(defun bltzread-range-describe ()
  "One-line description of the current method and its marks."
  (format "range: %s | start %s | %s"
          bltzread-range-method
          (or (bltzread-range-start) "-")
          (pcase bltzread-range-method
            ('points (format "end %s" (or (bltzread-range-end) "-")))
            ('farthest (format "reached %s" bltzread-range--farthest))
            (_ "type numbers when finishing"))))

;;; Commands

(defun bltzread-range-set-start ()
  "Mark the start of what you read at point."
  (interactive)
  (setq bltzread-range--start (copy-marker (point)))
  (message "Start set at %d" (point)))

(defun bltzread-range-set-end ()
  "Mark the end of what you read at point."
  (interactive)
  (setq bltzread-range--end (copy-marker (point)))
  (message "End set at %d" (point)))

(defun bltzread-range-change-method (method)
  "Switch to range-picking METHOD."
  (interactive
   (list (intern (completing-read "Range method: "
                                  (mapcar #'symbol-name bltzread-range-methods)
                                  nil t nil nil
                                  (symbol-name bltzread-range-method)))))
  (setq bltzread-range-method method)
  (bltzread-range-show-numbers)
  (force-mode-line-update)
  (message "Range method: %s" method))

(defun bltzread-range-pick ()
  "Return (BEG . END), the range read, picked per `bltzread-range-method'.
Signals a `user-error' when the method cannot produce a range."
  (let ((range
         (pcase bltzread-range-method
           ('farthest
            (cons (or (and bltzread-range-start-function
                           (funcall bltzread-range-start-function))
                      (bltzread-range-start) (point-min))
                  (max (bltzread-range-farthest)
                       ;; point only counts when it is what is being tracked
                       (if (functionp bltzread-range-farthest-source)
                           (point-min)
                         (point)))))
           ('points
            (let ((s (bltzread-range-start)) (e (bltzread-range-end)))
              (unless (and s e) (user-error "Mark both ends first"))
              (cons (min s e) (max s e))))
           ('paragraphs
            (let* ((paras (bltzread-range-paragraphs))
                   (from (read-number (format "From paragraph (1-%d): " (length paras))))
                   (to (read-number (format "To paragraph (%d-%d): " from (length paras)))))
              (unless (<= 1 from to (length paras))
                (user-error "Paragraph numbers out of range"))
              (cons (car (nth (1- from) paras)) (cdr (nth (1- to) paras)))))
           ('lines
            (let* ((last (line-number-at-pos (point-max)))
                   (from (read-number (format "From line (1-%d): " last)))
                   (to (read-number (format "To line (%d-%d): " from last))))
              (unless (<= 1 from to last)
                (user-error "Line numbers out of range"))
              (save-excursion
                (cons (progn (goto-char (point-min)) (forward-line (1- from)) (point))
                      (progn (goto-char (point-min)) (forward-line (1- to))
                             (line-end-position)))))))))
    (when (>= (car range) (cdr range))
      (user-error "Nothing read yet"))
    range))

(provide 'bltzread-range)
;;; bltzread-range.el ends here
