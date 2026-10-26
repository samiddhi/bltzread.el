;;; bltzread-avy.el --- Jump to unknown words with avy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;;; Commentary:

;; `bltzread-avy-unknown' tags every unknown word in view with an avy hint.
;; Type a hint to go to the word and open its popup.  Escape or C-g removes
;; the tags and leaves avy open (`avy-goto-char-timer'), so you can carry on
;; typing characters as usual.  Bind it to a key, or add it to a hook such
;; as the one that runs when playback pauses.

;;; Code:

(require 'bltzread)
(require 'avy nil t)

(declare-function avy-process "avy")
(declare-function avy-goto-char-timer "avy")
(declare-function avy-handler-default "avy")
(defvar avy-handler-function)
(defvar avy-escape-chars)

(defun bltzread-avy--candidates (&optional beg end)
  "Unknown words from BEG to END (default: the window), as avy candidates."
  (let ((beg (or beg (window-start))) (end (or end (window-end nil t))) cands)
    (dolist (ov (bltzread--word-overlays beg end))
      (when (bltzread--unknown-p ov)
        (push (cons (cons (overlay-start ov) (overlay-end ov)) (selected-window)) cands)))
    (nreverse cands)))

;;;###autoload
(defun bltzread-avy-unknown ()
  "Tag the unknown words in view with avy hints and go to the one you pick.
Escape or C-g removes the tags and starts `avy-goto-char-timer' instead."
  (interactive)
  (unless (require 'avy nil t) (user-error "avy is not installed"))
  (unless bltzread-mode (user-error "bltzread-mode is not on"))
  (let* ((cands (or (bltzread-avy--candidates) (user-error "No unknown words in view")))
         (fallback nil)
         (avy-handler-function
          (lambda (char)
            (when (memq char avy-escape-chars) (setq fallback t))
            (avy-handler-default char)))
         (pos (avy-process cands)))
    (cond (fallback (avy-goto-char-timer))
          ((numberp pos)
           (goto-char pos)
           (bltzread-popup-at-point)))))

(provide 'bltzread-avy)
;;; bltzread-avy.el ends here
