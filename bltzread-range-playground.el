;;; bltzread-range-playground.el --- Try ways of saying "I read from here to there" -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;;; Commentary:

;; A throwaway test bench for `bltzread-range'.  It shows some text read-only
;; and lets you finish a "reading session" with any of the range methods
;; (see `bltzread-range-method') so you can feel which one is least annoying.
;;
;; M-x bltzread-range-playground          use the region, else the clipboard
;; C-u M-x bltzread-range-playground      read a file instead
;;
;; It also turns on `bltzread-mode' (asking for the language), so the words are
;; coloured and C-c l f finishes through the real command.
;; Keys: s start, e end, m change method, c or C-c C-c finish, q quit.

;;; Code:

(require 'subr-x)
(require 'bltzread-range)
(require 'bltzread)

(defconst bltzread-playground--sample
  (mapconcat
   (lambda (n)
     (mapconcat (lambda (s) (format "Sentence %d.%d is here to be read." n s))
                '(1 2 3 4) " "))
   (number-sequence 1 12) "\n\n")
  "Text used when there is no region or clipboard.")

(defvar-local bltzread-playground--last nil "Last result as (BEG END METHOD).")

(defun bltzread-playground--word-count (beg end)
  "Number of words between BEG and END."
  (let ((count 0))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\w+" end t)
        (setq count (1+ count))))
    count))

(defun bltzread-playground-finish ()
  "Pick the range you read with the current method and report it."
  (interactive)
  (pcase-let ((`(,beg . ,end) (bltzread-range-pick)))
    (bltzread-range-highlight beg end)
    (when (or (not bltzread-session-confirm)
              (y-or-n-p (format "Read %d chars, %d words (%s)? "
                                (- end beg) (bltzread-playground--word-count beg end)
                                bltzread-range-method)))
      (setq bltzread-playground--last (list beg end bltzread-range-method))
      (message "Counted %d-%d: %d words via %s"
               beg end (bltzread-playground--word-count beg end) bltzread-range-method))))

(defvar bltzread-playground-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'bltzread-range-set-start)
    (define-key map "e" #'bltzread-range-set-end)
    (define-key map "m" #'bltzread-range-change-method)
    (define-key map (kbd "C-c C-c") #'bltzread-playground-finish)
    map))

(define-derived-mode bltzread-playground-mode special-mode "Bltzread-Playground"
  "Read-only text with several ways to pick the range you read."
  (setq-local header-line-format '(:eval (concat " " (bltzread-range-describe))))
  (add-hook 'post-command-hook #'bltzread-range-track nil t))

;;;###autoload
(defun bltzread-range-playground (&optional file)
  "Open a playground on the region, the clipboard, or (with FILE) a file."
  (interactive (list (and current-prefix-arg (read-file-name "File: "))))
  (let ((text (cond (file (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string)))
                    ((use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))
                    ((let ((clip (ignore-errors (current-kill 0 t))))
                       (and clip (not (string-blank-p clip)) clip)))
                    (t bltzread-playground--sample)))
        (buf (get-buffer-create "*bltzread-playground*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (bltzread-playground-mode)
      (bltzread-range-reset)
      (goto-char (point-min))
      (bltzread-range-show-numbers))
    (pop-to-buffer-same-window buf)
    (with-current-buffer buf
      (unless bltzread-mode (bltzread-mode 1)))))

(provide 'bltzread-range-playground)
;;; bltzread-range-playground.el ends here
