;;; bltzread-review.el --- Review words before they join a skip list -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;;; Commentary:

;; Words that reach level 5 wait in a queue.  `bltzread-review' lists them.
;; Every word starts as `add'.  Mark a word `d' to leave it in the queue
;; for later, or `r' to reject it (it drops back to level 4).  `x'
;; applies: the `add' words are appended to the skip list.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'bltzread-custom)
(require 'bltzread-state)

(defvar-local bltzread-review--lang nil "Language being reviewed.")
(defvar-local bltzread-review--actions nil "Hash table form -> `add', `later' or `reject'.")

(defvar bltzread-review-after-apply-hook nil
  "Hook run after the review is applied, so open buffers can redraw.")

(defvar bltzread-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "a" (lambda () (interactive) (bltzread-review--mark 'add)))
    (define-key map "d" (lambda () (interactive) (bltzread-review--mark 'later)))
    (define-key map "r" (lambda () (interactive) (bltzread-review--mark 'reject)))
    (define-key map "A" (lambda () (interactive) (bltzread-review--mark-all 'add)))
    (define-key map "x" #'bltzread-review-apply)
    (define-key map (kbd "C-c C-c") #'bltzread-review-apply)
    map))

(define-derived-mode bltzread-review-mode tabulated-list-mode "Bltzread-Review"
  "Decide which known words are written to the bltzr skip list.
\\{bltzread-review-mode-map}"
  (setq tabulated-list-format [("Word" 24 t) ("Basic word" 24 t) ("Action" 10 t)]
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun bltzread-review--refresh ()
  "Redraw the list."
  (let ((state (bltzread-state-get bltzread-review--lang)))
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (list (car e)
                          (vector (car e) (or (cdr e) "")
                                  (symbol-name (gethash (car e) bltzread-review--actions 'add)))))
                  (bltzread-state-queue state)))
    (tabulated-list-print t)))

(defun bltzread-review--mark (action)
  "Give the word on this line ACTION and move on."
  (when-let ((form (tabulated-list-get-id)))
    (puthash form action bltzread-review--actions)
    (bltzread-review--refresh)
    (forward-line 1)))

(defun bltzread-review--mark-all (action)
  "Give every word ACTION."
  (dolist (e (bltzread-state-queue (bltzread-state-get bltzread-review--lang)))
    (puthash (car e) action bltzread-review--actions))
  (bltzread-review--refresh))

(defun bltzread-review-apply ()
  "Write the `add' words, demote the `reject' words, keep the rest queued."
  (interactive)
  (let* ((state (bltzread-state-get bltzread-review--lang))
         (queue (bltzread-state-queue state))
         (adds (cl-remove-if-not (lambda (e) (eq (gethash (car e) bltzread-review--actions 'add) 'add))
                                 queue))
         (rejects (cl-remove-if-not (lambda (e) (eq (gethash (car e) bltzread-review--actions) 'reject))
                                    queue)))
    (when (or adds rejects)
      (when (and adds (y-or-n-p (format "Add %d word%s to the skip list? "
                                        (length adds) (if (cdr adds) "s" ""))))
        (let ((result (bltzread-state-write-known state adds)))
          (bltzread-state-queue-remove state (mapcar #'car adds))
          (message "Added %d to %s" (length (cdr result)) (car result))))
      (dolist (e rejects)
        (bltzread-state-set-level state (bltzread-state-key (car e) (and (cdr e) (list (cdr e)))) 4)
        (bltzread-state-queue-remove state (list (car e))))
      (bltzread-state-save state)
      (run-hooks 'bltzread-review-after-apply-hook))
    (bltzread-review--refresh)))

;;;###autoload
(defun bltzread-review (lang)
  "Review the queued known words of LANG."
  (interactive (list (or (bound-and-true-p bltzread-language)
                         bltzread-default-language
                         (read-string "Language code: "))))
  (let ((buf (get-buffer-create (format "*bltzread-review: %s*" lang))))
    (with-current-buffer buf
      (bltzread-review-mode)
      (setq bltzread-review--lang lang
            bltzread-review--actions (make-hash-table :test 'equal))
      (bltzread-review--refresh)
      (setq header-line-format
            (concat " a add  d later  r reject  A add all  x apply")))
    (pop-to-buffer buf)))

(provide 'bltzread-review)
;;; bltzread-review.el ends here
