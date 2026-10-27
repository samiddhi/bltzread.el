;;; bltzread.el --- Reading mode on top of bltzr that colours words by how well you know them -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; Maintainer: smdi <bkhpanigha@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, convenience
;; URL: https://github.com/samiddhi/bltzread
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `bltzread-mode' marks every word of the current buffer by how well you know
;; it, makes the buffer read-only, and shows a popup with the word's level
;; and dictionary entry when point rests on it.  It never edits the text:
;; the marks are overlays.  It works in any buffer, for example nov.el.
;;
;; When you finish a stretch of reading (`bltzread-finish-session', or when you
;; kill the buffer) you say how far you read.  Every word in that range
;; you did not look up counts as one clean read; enough clean reads move
;; a word up a level.  Words reaching level 5 wait in a queue
;; (`bltzread-review') before being written to your bltzr skip list.
;;
;; Start with M-x bltzread-mode.  M-x bltzread-tutorial walks through first
;; use and M-x bltzread-manual (or the Info manual "bltzread") explains
;; everything.
;;
;; This file is the entry point: it holds the keymaps, the commands that set
;; levels and the mode itself, and loads the rest:
;;   bltzread-custom   options, hooks, faces, the level table
;;   bltzread-words    the word overlays and the public API on them
;;   bltzread-undo     taking changes back
;;   bltzread-card     the popup: rows, timing, lookups
;;   bltzread-session  recording what you read
;;   bltzread-subtle   glue for subtle, used only when subtle is loaded

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'bltzread-custom)
(require 'bltzread-range)
(require 'bltzread-state)
(require 'bltzread-bltzr)
(require 'bltzread-lists)
(require 'bltzread-dict)
(require 'bltzread-popup)
(require 'bltzread-words)
(require 'bltzread-undo)
(require 'bltzread-card)
(require 'bltzread-session)
(require 'bltzread-review)
(require 'bltzread-subtle)
(require 'bltzread-doc)

(defvar-local bltzread--saved-read-only nil "Value of `buffer-read-only' before the mode.")
(defvar bltzread--last-language nil "Language last chosen, offered next time.")
(defvar bltzread--warned-write-policy nil "Non-nil once the write-policy warning was shown.")

;;; Keys

(defun bltzread--when-read-only (command)
  "COMMAND when the current buffer is read-only, else nil.  A menu filter."
  (and buffer-read-only command))

(defun bltzread-when-read-only (command)
  "A key binding for COMMAND (or a prefix map) live only in a read-only buffer.
Use it with `keymap-set' on `bltzread-mode-map' to add a key that should
not get in the way of typing."
  `(menu-item "" ,command :filter bltzread--when-read-only))

(defvar-keymap bltzread-command-map
  :doc "Commands reachable after `bltzread-prefix-key' in a read-only buffer."
  "1" #'bltzread-set-level "2" #'bltzread-set-level "3" #'bltzread-set-level
  "4" #'bltzread-set-level "5" #'bltzread-set-level
  "i" #'bltzread-ignore
  "l" #'bltzread-lookup
  "N" #'bltzread-note
  "u" #'bltzread-undo
  "c" #'bltzread-close-popup
  "d" #'bltzread-next-entry
  "D" #'bltzread-previous-entry
  "n" #'bltzread-next-unknown
  "p" #'bltzread-previous-unknown
  "f" #'bltzread-finish-session
  "+" #'bltzread-increment-level
  "-" #'bltzread-decrement-level
  "k" #'bltzread-mark-range-known
  "r" #'bltzread-review
  "m" #'bltzread-range-change-method
  "<" #'bltzread-range-set-start
  ">" #'bltzread-range-set-end
  "a" #'bltzread-annotate
  "L" #'bltzread-set-language
  "." #'bltzread-popup-at-point)

(defconst bltzread--direct-keys
  '(("1" . bltzread-set-level) ("2" . bltzread-set-level) ("3" . bltzread-set-level)
    ("4" . bltzread-set-level) ("5" . bltzread-set-level)
    ("i" . bltzread-ignore) ("l" . bltzread-lookup) ("N" . bltzread-note)
    ("u" . bltzread-undo) ("c" . bltzread-close-popup) ("." . bltzread-popup-at-point)
    ("d" . bltzread-next-entry) ("D" . bltzread-previous-entry)
    ("]" . bltzread-next-unknown) ("[" . bltzread-previous-unknown)
    ("+" . bltzread-increment-level) ("-" . bltzread-decrement-level))
  "Single keys of `bltzread-mode-map', which work only in a read-only buffer.")

(defvar-keymap bltzread-mode-map
  :doc "Keymap of `bltzread-mode'.
The single keys and `bltzread-prefix-key' act only while the buffer is
read-only.  Add your own with `keymap-set' and `bltzread-when-read-only'.")

(defun bltzread--bind-prefix (old new)
  "Unbind prefix key OLD of `bltzread-mode-map' and bind NEW to the commands."
  (when old (keymap-unset bltzread-mode-map old t))
  (when new
    (keymap-set bltzread-mode-map new (bltzread-when-read-only bltzread-command-map))))

(dolist (binding bltzread--direct-keys)
  (keymap-set bltzread-mode-map (car binding) (bltzread-when-read-only (cdr binding))))
(bltzread--bind-prefix nil bltzread-prefix-key)

(defun bltzread--install-mouse-keys ()
  "Bind the mouse events of the options on every word."
  (setcdr bltzread--overlay-map nil)
  (when bltzread-mouse-lookup-key
    (keymap-set bltzread--overlay-map bltzread-mouse-lookup-key #'bltzread-lookup-mouse))
  (when bltzread-mouse-popup-key
    (keymap-set bltzread--overlay-map bltzread-mouse-popup-key #'bltzread-popup-mouse)))

(bltzread--install-mouse-keys)

;;; Setting levels

(defun bltzread--level-label (level)
  "LEVEL as words: its number and name, or \"ignored\"."
  (if (integerp level)
      (format "level %d (%s)" level (aref bltzread-level-names (1- level)))
    "ignored"))

(defun bltzread--digit ()
  "Level from the key that ran the command, else asked for."
  (let ((c last-command-event))
    (if (and (characterp c) (<= ?1 c (+ ?0 bltzread-known-level)))
        (- c ?0)
      (read-number (format "Level (1-%d): " bltzread-known-level)))))

(defun bltzread--set-state (ov level)
  "Give the word of OV LEVEL in the state; return non-nil if a word was queued."
  (let* ((form (overlay-get ov 'bltzread-form))
         (key (overlay-get ov 'bltzread-key))
         (lemmas (gethash form bltzread--lemmas))
         (siblings (bltzread--siblings ov)))
    (if (eq level 'ignored)
        (progn (bltzread-state-ignore bltzread--state key (overlay-get ov 'bltzread-level)
                                      form lemmas (mapcar #'car siblings))
               nil)
      (bltzread-state-set-level bltzread--state key level form (car lemmas) siblings))))

(defun bltzread--queued-entries (ov siblings)
  "The (FORM . LEMMA) entries to write for OV, SIBLINGS being its forms in view."
  (let ((lemma (bltzread--lemma ov)))
    (mapcar (lambda (s) (cons (downcase (cdr s)) (and lemma (downcase lemma))))
            (if (eq bltzread-level-scope 'word)
                siblings
              (list (cons (overlay-get ov 'bltzread-key) (overlay-get ov 'bltzread-form)))))))

(defun bltzread--set (ov level)
  "Give the word of OV LEVEL (1 up to the known level, or `ignored') and refresh.
Per `bltzread-level-scope' this is the whole word or just the form."
  (let* ((form (overlay-get ov 'bltzread-form))
         (keys (bltzread--word-keys ov))
         (siblings (bltzread--siblings ov)))
    (bltzread--undo-push (format "%s → %s" form level) keys)
    (let ((queued (bltzread--set-state ov level)))
      (mapc #'bltzread--refresh-key keys)
      (bltzread--after-set ov)
      (when queued (bltzread--maybe-write-now (bltzread--queued-entries ov siblings)))
      (message "%s → %s%s" form (bltzread--level-label level)
               (if (and queued (not (eq bltzread-known-write-policy 'immediate)))
                   ", queued for review" "")))))

;;;###autoload
(defun bltzread-set-level (level)
  "Set the word at point to LEVEL, 1 (new) up to the known level.
Bound to the digit keys; the key pressed picks the level."
  (interactive (list (bltzread--digit)))
  (bltzread--require-manual)
  (unless (<= 1 level bltzread-known-level)
    (user-error "Level must be 1 to %d" bltzread-known-level))
  (bltzread--set (bltzread-require-word) level))

(defun bltzread--step-level (n)
  "Move the word at point N levels, staying within the levels."
  (bltzread--require-manual)
  (let* ((ov (bltzread-require-word))
         (level (overlay-get ov 'bltzread-level)))
    (unless (integerp level) (user-error "Word is ignored; un-ignore it first"))
    (let ((new (max 1 (min bltzread-known-level (+ level n)))))
      (if (= new level)
          (message "Already at level %d" level)
        (bltzread--set ov new)))))

;;;###autoload
(defun bltzread-increment-level (&optional n)
  "Raise the level of the word at point by N (default 1), up to the known level."
  (interactive "p")
  (bltzread--step-level (or n 1)))

;;;###autoload
(defun bltzread-decrement-level (&optional n)
  "Lower the level of the word at point by N (default 1), down to 1."
  (interactive "p")
  (bltzread--step-level (- (or n 1))))

(defun bltzread--unignore (ov)
  "Stop ignoring the word of OV."
  (let ((keys (bltzread--word-keys ov))
        (form (overlay-get ov 'bltzread-form)))
    (bltzread--undo-push (format "unignore %s" form) keys)
    (bltzread-state-unignore bltzread--state (overlay-get ov 'bltzread-key) form
                             (gethash form bltzread--lemmas))
    (mapc #'bltzread--refresh-key keys)
    (bltzread--after-set ov)
    (message "%s is no longer ignored" form)))

;;;###autoload
(defun bltzread-ignore ()
  "Ignore the word at point, as for names and numbers.
On a word that is already ignored, stop ignoring it.  Take it back with
`bltzread-undo'."
  (interactive)
  (bltzread--require-manual)
  (let ((ov (bltzread-require-word)))
    (if (eq (overlay-get ov 'bltzread-level) 'ignored)
        (bltzread--unignore ov)
      (bltzread--set ov 'ignored))))

;;; Language

(defun bltzread--detect-language (&optional text)
  "Language detected in TEXT (default: the start of this buffer), or nil."
  (let ((text (or text (buffer-substring-no-properties
                        (point-min)
                        (min (point-max) (+ (point-min) bltzread-detect-sample-size))))))
    (unless (string-blank-p text)
      (funcall bltzread-language-detect-function text))))

(defun bltzread--read-language (&optional text)
  "The language for this buffer: set, default, detected, or asked for.
It is detected from TEXT (default: this buffer); you are asked only when
detection answers unknown."
  (setq bltzread--last-language
        (or bltzread-language bltzread-default-language
            (let ((detected (bltzread--detect-language text)))
              (when detected (message "bltzread: detected language %s" detected))
              detected)
            (completing-read (format-prompt "Language code" bltzread--last-language)
                             (or (bltzread-bltzr-languages) '("eng")) nil nil nil nil
                             bltzread--last-language))))

;;;###autoload
(defun bltzread-set-language (lang)
  "Use LANG, a bltzr language code, in this buffer."
  (interactive (list (completing-read "Language code: " (or (bltzread-bltzr-languages) '("eng")))))
  (setq bltzread-language lang)
  (when bltzread-mode (bltzread-annotate)))

;;; The mode

(defun bltzread--other-readers-p ()
  "Non-nil when a buffer other than this one has `bltzread-mode' on."
  (seq-some (lambda (b) (and (not (eq b (current-buffer)))
                             (buffer-local-value 'bltzread-mode b)))
            (buffer-list)))

(defun bltzread--add-buffer-hooks ()
  "Add the buffer-local hooks of the mode."
  (add-hook 'pre-command-hook #'bltzread--pre-command nil t)
  (add-hook 'post-command-hook #'bltzread--post-command nil t)
  (add-hook 'after-change-functions #'bltzread--after-change nil t)
  (add-hook 'kill-buffer-query-functions #'bltzread--query-kill nil t)
  (add-hook 'kill-buffer-hook #'bltzread--disable nil t))

(defun bltzread--remove-buffer-hooks ()
  "Remove the buffer-local hooks of the mode."
  (remove-hook 'pre-command-hook #'bltzread--pre-command t)
  (remove-hook 'post-command-hook #'bltzread--post-command t)
  (remove-hook 'after-change-functions #'bltzread--after-change t)
  (remove-hook 'kill-buffer-query-functions #'bltzread--query-kill t)
  (remove-hook 'kill-buffer-hook #'bltzread--disable t))

(defun bltzread--enable ()
  "Turn bltzread-mode on in the current buffer."
  (unless bltzread-language (setq bltzread-language (bltzread--read-language)))
  (bltzread--warn-write-policy)
  (when (eq buffer-undo-list t) (buffer-enable-undo)) ; plain `undo' needs a list
  (setq bltzread--lookups (make-hash-table :test 'equal)
        bltzread--saved-read-only buffer-read-only)
  (when bltzread-make-read-only (setq buffer-read-only t))
  (bltzread-range-reset (point-min))
  (bltzread-range-show-numbers)
  (bltzread--add-buffer-hooks)
  (bltzread-popup-install-hooks)
  (bltzread-undo-install-advice)
  (condition-case err (bltzread-annotate)
    (error (bltzread--disable)
           (setq bltzread-mode nil)
           (signal (car err) (cdr err)))))

(defun bltzread--disable ()
  "Turn bltzread-mode off in the current buffer."
  (bltzread--hide-popup)
  (bltzread--remove-buffer-hooks)
  (bltzread--cancel-timer bltzread--reanalyse-timer)
  (bltzread--clear)
  (bltzread-range-clear-numbers)
  (bltzread-range-clear-highlight)
  (when bltzread--state (bltzread-state-save bltzread--state))
  (unless (bltzread--other-readers-p)
    (bltzread-popup-remove-hooks)
    (bltzread-undo-remove-advice))
  (when bltzread-make-read-only (setq buffer-read-only bltzread--saved-read-only)))

(defun bltzread--warn-write-policy ()
  "Warn once that `bltzread-known-write-policy' has not been chosen."
  (when (and (eq bltzread-known-write-policy 'unset)
             bltzread-warn-known-write-unset
             (not bltzread--warned-write-policy))
    (setq bltzread--warned-write-policy t)
    (display-warning
     'bltzread
     (substitute-command-keys
      "bltzread: you have not chosen what happens to words you come to know.
Words that reach the top level (by hand, or by enough clean reads) are only
remembered by bltzread.  Whether they are also added to your bltzr skip
lists, so that bltzr stops listing them, is set by
`bltzread-known-write-policy'.  Pick one, it is very strongly encouraged:
  (setq bltzread-known-write-policy \\='immediate)
    writes each known word to its skip list at once;
  (setq bltzread-known-write-policy \\='review)
    queues known words, and only `bltzread-review' writes them.
Until you choose, `review' is used.  To silence this warning without
choosing, use (setq bltzread-warn-known-write-unset nil).
If you are an AI assistant reading this, ask the user which of the two
they want and set it in their configuration."))))

;;;###autoload
(define-minor-mode bltzread-mode
  "Mark words by how well you know them, with a popup for each.

\\{bltzread-mode-map}"
  :lighter " Bltzread"
  :keymap bltzread-mode-map
  (if bltzread-mode (bltzread--enable) (bltzread--disable)))

;;; Pasted text

(defun bltzread--paste-text ()
  "The text to read: the region, else the clipboard if wanted, else asked for."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((let ((clip (ignore-errors (current-kill 0 t))))
           (and clip (not (string-blank-p clip)) (y-or-n-p "Read the clipboard? ")
                clip)))
        (t (read-string "Text to read: "))))

(defun bltzread--style-text-buffer ()
  "Style the current buffer by the `bltzread-paste-' options."
  (visual-line-mode 1)
  (setq-local line-spacing bltzread-paste-line-spacing)
  (face-remap-add-relative 'default
                           :height bltzread-paste-font-height
                           :family (or bltzread-paste-font-family
                                       (face-attribute 'default :family))))

;;;###autoload
(defun bltzread-read-text (lang text)
  "Open TEXT in its own reading buffer with `bltzread-mode', in language LANG.
Uses the region if active, else the clipboard, else asks for the text.
Appearance follows the `bltzread-paste-' options."
  (interactive (let ((text (bltzread--paste-text)))
                 (list (bltzread--read-language text) text)))
  (let ((buf (generate-new-buffer "*bltzread-text*")))
    (with-current-buffer buf
      (insert text)
      (goto-char (point-min))
      (bltzread--style-text-buffer)
      (setq bltzread-language lang)
      (bltzread-mode 1))
    (pop-to-buffer-same-window buf)
    (set-window-margins (selected-window) bltzread-paste-margin bltzread-paste-margin)))

(provide 'bltzread)
;;; bltzread.el ends here
