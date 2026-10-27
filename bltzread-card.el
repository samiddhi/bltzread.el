;;; bltzread-card.el --- The word card: popup contents, timing and lookups -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; What the popup says (`bltzread-popup-row-functions', each a small function
;; from a word to a list of rows), when it appears (a scheduler run after every
;; command), and what looking a word up means (`bltzread--note-lookup').  The
;; box itself is drawn by bltzread-popup.el.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'bltzread-custom)
(require 'bltzread-words)
(require 'bltzread-undo)
(require 'bltzread-dict)
(require 'bltzread-popup)

(defvar bltzread-mode)
(defvar bltzread-mode-map)

(defvar-local bltzread--popup-timer nil "Idle timer that shows or hides the popup.")
(defvar-local bltzread--popup-ov nil "Word overlay the popup is showing.")
(defvar-local bltzread--popup-dismissed nil "Word overlay whose popup was closed by hand.")
(defvar-local bltzread--popup-full nil "Non-nil when the popup shows a long entry.")
(defvar-local bltzread--entry-ov nil "Word overlay `bltzread--entry-index' belongs to.")
(defvar-local bltzread--entry-index 0 "Which dictionary entry the popup shows.")
(defvar-local bltzread--dwell-timer nil "Timer counting a resting popup as a lookup.")
(defvar-local bltzread--pre-command-point nil "Point when the running command started.")

;;; Hints

(defun bltzread--key-of (command)
  "First key sequence that runs COMMAND in this buffer, or nil.
Keys of the mode's map come first, then those of any active map.  A key
that something else shadows here (your own `l' for navigation, say) is
passed over, so the hints never advertise a key that does something else."
  (seq-find (lambda (key) (eq (key-binding key t) command))
            (append (where-is-internal command (list bltzread-mode-map))
                    (where-is-internal command))))

(defun bltzread--key-text (command)
  "The key of COMMAND as text, or nil.  Level keys read as a range."
  (when-let ((key (bltzread--key-of command)))
    (replace-regexp-in-string (format "[1-%d]\\'" bltzread-known-level)
                              (format "1-%d" bltzread-known-level)
                              (key-description key))))

(defun bltzread--hint-text (hint)
  "HINT, a (COMMAND . LABEL), as a key cap and a label, or nil without a key."
  (when-let ((key (bltzread--key-text (car hint))))
    (concat (bltzread-popup-key key) " " (bltzread-popup-dim (cdr hint)))))

(defconst bltzread--level-commands
  '(bltzread-set-level bltzread-ignore bltzread-increment-level bltzread-decrement-level)
  "Commands that set levels by hand, hidden from the hints in no-colors mode.")

(defun bltzread--active-hints ()
  "The hints to show now: `bltzread-popup-hints' less what no-colors mode forbids."
  (if bltzread-no-colors
      (seq-remove (lambda (h) (memq (car h) bltzread--level-commands)) bltzread-popup-hints)
    bltzread-popup-hints))

;;; Rows

(defun bltzread--lookup-words (ov)
  "The words to look up for OV: its spelling, then its basic words."
  (let ((form (overlay-get ov 'bltzread-form)))
    (seq-uniq (cons form (gethash form bltzread--lemmas)))))

(defun bltzread--entries (ov full)
  "Dictionary entries for word OV, or nil; none unless FULL or glosses are on."
  (and (or full bltzread-popup-show-gloss)
       (bltzread-dict-lookup bltzread-language (bltzread--lookup-words ov))))

(defun bltzread--entry-position (ov entries)
  "Index in ENTRIES of the entry shown for OV."
  (if (and entries (eq ov bltzread--entry-ov))
      (mod bltzread--entry-index (length entries))
    0))

(defun bltzread--show-level-p ()
  "Non-nil if the popup is to show level and clean reads."
  (or (not bltzread-no-colors) bltzread-no-colors-show-level))

(defun bltzread-card-title-rows (ov _full)
  "Row with the spelling of word OV and, if different, its basic words."
  (let* ((form (overlay-get ov 'bltzread-form))
         (lemmas (gethash form bltzread--lemmas)))
    (list (concat (bltzread-popup-title form)
                  (when (and lemmas (not (equal (car lemmas) form)))
                    (bltzread-popup-dim (concat "  → " (string-join lemmas ", "))))))))

(defun bltzread--level-text (level)
  "LEVEL as the coloured text of the status row."
  (propertize (if (integerp level)
                  (format " %d %s " level (aref bltzread-level-names (1- level)))
                " ignored ")
              'face (or (bltzread--face level) 'bltzread-popup-dim)))

(defun bltzread--reads-text (ov)
  "The clean-read progress of word OV as dim text, or the empty string."
  (let* ((level (overlay-get ov 'bltzread-level))
         (reads (bltzread-state-reads bltzread--state (overlay-get ov 'bltzread-key)))
         (need (and (bltzread-learning-level-p level)
                    (nth (1- level) bltzread-promotion-thresholds))))
    (if need (bltzread-popup-dim (format "  %d/%d clean reads" reads need)) "")))

(defun bltzread-card-status-rows (ov _full)
  "Return rows with the level, clean reads and lookup mark of word OV."
  (let ((status (concat
                 (if (bltzread--show-level-p)
                     (concat (bltzread--level-text (overlay-get ov 'bltzread-level))
                             (bltzread--reads-text ov))
                   "")
                 (if (gethash (overlay-get ov 'bltzread-key) bltzread--lookups)
                     (bltzread-popup-dim "  looked up")
                   ""))))
    (unless (string-empty-p status) (list status))))

(defun bltzread-card-note-rows (ov _full)
  "Row with your note on word OV, if it has one."
  (when-let ((note (bltzread-state-note bltzread--state (overlay-get ov 'bltzread-key))))
    (list (concat (bltzread-popup-dim "note: ") note))))

(defun bltzread-card-entry-rows (ov full)
  "Return rows naming which entry of several is shown for word OV.
FULL is non-nil for a long entry."
  (let ((entries (bltzread--entries ov full)))
    (when (cdr entries)
      (list (bltzread-popup-dim
             (format "entry %d/%d  (%s next, %s previous)"
                     (1+ (bltzread--entry-position ov entries)) (length entries)
                     (or (bltzread--key-text 'bltzread-next-entry) "-")
                     (or (bltzread--key-text 'bltzread-previous-entry) "-")))))))

(defun bltzread--name-dictionary (gloss)
  "GLOSS with its first line, the dictionary's name, in `bltzread-popup-dict-name'."
  (let ((copy (copy-sequence gloss))
        (newline (string-search "\n" gloss)))
    (when newline
      (add-face-text-property 0 newline 'bltzread-popup-dict-name nil copy))
    copy))

(defun bltzread--gloss-text (entry full)
  "ENTRY as card text; with FULL, the dictionary is named."
  (let ((gloss (bltzread-dict-display entry full)))
    (if (and gloss (eq gloss entry)) (bltzread--name-dictionary gloss) gloss)))

(defun bltzread-card-gloss-rows (ov full)
  "Return rows with the dictionary entry of word OV, wrapped.
FULL is non-nil for a long entry."
  (let* ((entries (bltzread--entries ov full))
         (gloss (bltzread--gloss-text (nth (bltzread--entry-position ov entries) entries)
                                      full)))
    (when gloss
      (cons "" (funcall bltzread-popup-wrap-function gloss (bltzread-popup-text-width)
                        (if full bltzread-popup-lookup-max-lines bltzread-popup-max-lines))))))

(defun bltzread-card-hint-rows (_ov _full)
  "Row listing the keys of `bltzread-popup-hints', or none."
  (when-let ((texts (delq nil (mapcar #'bltzread--hint-text (bltzread--active-hints)))))
    (list (string-join texts bltzread-popup-hint-separator))))

(defun bltzread--popup-rows (ov full)
  "Rows describing word overlay OV, from `bltzread-popup-row-functions'.
A non-nil FULL allows a long dictionary entry."
  (seq-mapcat (lambda (fn) (funcall fn ov full)) bltzread-popup-row-functions))

;;; Showing and hiding

(defun bltzread--cancel-timer (timer)
  "Cancel TIMER if it is one."
  (when (timerp timer) (cancel-timer timer)))

(defun bltzread--cancel-popup-timers ()
  "Cancel the timers that show, hide or count the popup."
  (bltzread--cancel-timer bltzread--popup-timer)
  (bltzread--cancel-timer bltzread--dwell-timer)
  (setq bltzread--popup-timer nil bltzread--dwell-timer nil))

(defun bltzread--hide-popup ()
  "Hide the popup and forget which word it showed."
  (bltzread--cancel-popup-timers)
  (when bltzread--popup-ov
    (setq bltzread--popup-ov nil)
    (bltzread-popup-hide)
    (run-hooks 'bltzread-popup-hide-hook)))

(defun bltzread--hide-popup-later ()
  "Hide the popup after `bltzread-popup-hide-delay' unless a word is reached."
  (bltzread--cancel-timer bltzread--popup-timer)
  (setq bltzread--popup-timer
        (run-with-idle-timer
         bltzread-popup-hide-delay nil
         (lambda (buf)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (unless (bltzread-word-at) (bltzread--hide-popup)))))
         (current-buffer))))

(defun bltzread--sync-entry (ov)
  "Start at the first dictionary entry if the popup moved to word OV."
  (unless (eq ov bltzread--entry-ov)
    (setq bltzread--entry-ov ov bltzread--entry-index 0)))

(defun bltzread--show-popup (buffer &optional full)
  "Show the popup for the word at point in BUFFER; FULL for a long entry."
  (when-let* (((buffer-live-p buffer))
              ((eq buffer (current-buffer)))
              (ov (bltzread-word-at)))
    (setq bltzread--popup-full full)
    (bltzread--sync-entry ov)
    (bltzread-popup-show (bltzread--popup-rows ov full) (overlay-start ov))
    (setq bltzread--popup-ov ov)
    (run-hooks 'bltzread-popup-show-hook)))

(defun bltzread--redraw-popup ()
  "Redraw the popup if one is showing, after a change to its word."
  (when (and bltzread--popup-ov (bltzread-popup-visible-p))
    (bltzread--show-popup (current-buffer) bltzread--popup-full)))

(defun bltzread--dismiss-popup (ov)
  "Hide the popup and keep it away while point stays on OV."
  (setq bltzread--popup-dismissed ov)
  (bltzread--hide-popup))

(defun bltzread--after-set (ov)
  "Deal with the popup after the word of OV changed level."
  (if bltzread-popup-hide-after-set
      (bltzread--dismiss-popup ov)
    (bltzread--redraw-popup)))

(defun bltzread-popup-anchor ()
  "Start of the word whose popup is on screen in this buffer, or nil."
  (and bltzread-mode bltzread--popup-ov (bltzread-popup-visible-p)
       (overlay-buffer bltzread--popup-ov)
       (overlay-start bltzread--popup-ov)))

;;;###autoload
(defun bltzread-close-popup ()
  "Close the popup.  It comes back when you move to another word."
  (interactive)
  (bltzread--dismiss-popup (or bltzread--popup-ov (bltzread-word-at))))

(defun bltzread-dismiss-popup ()
  "Close the popup if one is on screen; return non-nil if there was one."
  (when (bltzread-popup-anchor)
    (bltzread-close-popup)
    t))

;;; Entries

(defun bltzread--cycle-entry (n)
  "Show the dictionary entry N places on, in the popup."
  (let* ((ov (bltzread-require-word))
         (form (overlay-get ov 'bltzread-form))
         (entries (bltzread-dict-lookup bltzread-language (bltzread--lookup-words ov))))
    (cond ((null entries) (message "No dictionary entries for %s" form))
          ((null (cdr entries)) (message "Only one entry for %s" form))
          (t (bltzread--sync-entry ov)
             (setq bltzread--entry-index (mod (+ bltzread--entry-index n) (length entries))
                   bltzread--popup-dismissed nil)
             (bltzread--count-interaction ov)
             (bltzread--show-popup (current-buffer) bltzread--popup-full)))))

;;;###autoload
(defun bltzread-next-entry ()
  "Show the next dictionary entry of the word at point."
  (interactive)
  (bltzread--cycle-entry 1))

;;;###autoload
(defun bltzread-previous-entry ()
  "Show the previous dictionary entry of the word at point."
  (interactive)
  (bltzread--cycle-entry -1))

;;; Lookups

(defun bltzread--note-lookup (ov)
  "Record that the word of OV was looked up this session.
Only the first lookup of a word in a session counts and can be undone."
  (let ((key (overlay-get ov 'bltzread-key))
        (form (overlay-get ov 'bltzread-form))
        (level (overlay-get ov 'bltzread-level)))
    (unless (gethash key bltzread--lookups)
      (bltzread--undo-push (format "lookup %s" form) (list key))
      (puthash key t bltzread--lookups)
      (bltzread-state-record-lookup bltzread--state key level)
      (bltzread--refresh-key key)
      (run-hook-with-args 'bltzread-lookup-functions key form level))))

(defun bltzread--count-interaction (ov)
  "Count asking more of the popup of OV as a lookup, when the options say so."
  (when (memq bltzread-lookup-counts '(interaction popup))
    (bltzread--note-lookup ov)))

(defun bltzread--dwell-fire (buffer ov)
  "Count OV as looked up if point rested on it in BUFFER with its popup showing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and bltzread-mode (eq buffer (window-buffer (selected-window)))
                 (eq (bltzread-word-at) ov) (eq bltzread--popup-ov ov)
                 (bltzread-popup-visible-p))
        (bltzread--note-lookup ov)))))

;;;###autoload
(defun bltzread-lookup ()
  "Look up the word at point, showing its full dictionary entry."
  (interactive)
  (let ((ov (bltzread-require-word)))
    (bltzread--note-lookup ov)
    (bltzread--hide-popup)
    (setq bltzread--popup-dismissed nil)
    (bltzread--show-popup (current-buffer) t)))

;;;###autoload
(defun bltzread-popup-at-point ()
  "Show the popup for the word at point now."
  (interactive)
  (let ((ov (bltzread-require-word)))
    (setq bltzread--popup-dismissed nil)
    (bltzread--count-interaction ov)
    (bltzread--show-popup (current-buffer))))

;;;###autoload
(defun bltzread-note ()
  "Add or edit your own note on the word at point."
  (interactive)
  (let* ((ov (bltzread-require-word))
         (key (overlay-get ov 'bltzread-key))
         (form (overlay-get ov 'bltzread-form))
         (note (read-string (format "Note for %s: " form)
                            (bltzread-state-note bltzread--state key))))
    (bltzread--undo-push (format "note on %s" form) (list key))
    (bltzread-state-set-note bltzread--state key note)
    (bltzread--redraw-popup)
    (message "Note %s" (if (string-empty-p note) "removed" "saved"))))

;;; Mouse

(defun bltzread-popup-mouse (event)
  "Open the popup of the word clicked in EVENT, even if point was on it already."
  (interactive "e")
  (mouse-set-point event)
  (when-let ((ov (bltzread-word-at (posn-point (event-end event)))))
    (setq bltzread--popup-dismissed nil)
    (if bltzread-mouse-click-counts-as-lookup
        (bltzread--note-lookup ov)
      (bltzread--count-interaction ov))
    (bltzread--show-popup (current-buffer))))

(defun bltzread-lookup-mouse (event)
  "Look up the word clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (bltzread-lookup))

;;; Scheduling

(defun bltzread--pre-command ()
  "Save point before each command."
  (setq bltzread--pre-command-point (point)))

(defun bltzread--popup-wanted-p ()
  "Non-nil if the command that just ran asks for a popup at point.
Without `bltzread-popup-only-on-motion' resting on a word is enough."
  (or (not bltzread-popup-only-on-motion)
      (not (eql bltzread--pre-command-point (point)))
      (eq this-command 'bltzread-popup-at-point)))

(defun bltzread--arm-dwell (buffer ov)
  "Count OV as looked up if point stays on it for `bltzread-lookup-dwell' seconds.
Only when `bltzread-lookup-counts' is `popup'; BUFFER is the reading buffer."
  (when (eq bltzread-lookup-counts 'popup)
    (setq bltzread--dwell-timer
          (run-with-idle-timer bltzread-lookup-dwell nil #'bltzread--dwell-fire buffer ov))))

(defun bltzread--show-and-arm (buffer)
  "Show the popup for the word at point in BUFFER and start counting the dwell."
  (bltzread--show-popup buffer)
  (when bltzread--popup-ov
    (bltzread--arm-dwell buffer bltzread--popup-ov)))

(defun bltzread--popup-timer-fire (buffer)
  "Show the popup in BUFFER if it is still the one being read."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and bltzread-mode (eq buffer (window-buffer (selected-window))))
        (bltzread--show-and-arm buffer)))))

(defun bltzread--leave-popup-word (ov)
  "Point left the word of the popup for OV, which may be nil.
A popup window swaps its text from word to word instead of closing and
reopening, which would make the buffer jump."
  (cond ((not (and (bltzread-popup-window-visible-p)
                   (not (eq ov bltzread--popup-dismissed))))
         (bltzread--hide-popup))
        (ov (setq bltzread--popup-dismissed nil)
            (bltzread--show-and-arm (current-buffer)))
        (t (bltzread--hide-popup-later))))

(defun bltzread--popup-due-p (ov)
  "Non-nil when a popup for word OV should be scheduled now."
  (and ov
       (eq bltzread-popup-trigger 'point)
       (bltzread--popup-wanted-p)
       (not (and (eq ov bltzread--popup-ov) (bltzread-popup-visible-p)))
       (not (eq ov bltzread--popup-dismissed))))

(defun bltzread--schedule-popup ()
  "Show or hide the popup to match where point is."
  (let ((ov (bltzread-word-at)))
    (bltzread--cancel-popup-timers)
    (when (and bltzread--popup-ov (not (eq ov bltzread--popup-ov)))
      (bltzread--leave-popup-word ov))
    (unless (eq ov bltzread--popup-dismissed)
      (setq bltzread--popup-dismissed nil))
    (when (bltzread--popup-due-p ov)
      (setq bltzread--popup-timer
            (run-with-idle-timer bltzread-popup-delay nil #'bltzread--popup-timer-fire
                                 (current-buffer))))))

(defun bltzread--post-command ()
  "Per-command bookkeeping: reading progress and the popup."
  (bltzread-range-track)
  (bltzread--schedule-popup))

(provide 'bltzread-card)
;;; bltzread-card.el ends here
