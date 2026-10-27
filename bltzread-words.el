;;; bltzread-words.el --- The words of a bltzread buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; Every word of the buffer is an overlay carrying its spelling (`bltzread-form'),
;; its state key (`bltzread-key') and its level (`bltzread-level').  This file
;; makes and styles the overlays, keeps them current when the text changes, and
;; answers questions about them: the public API other packages build on is
;; `bltzread-words', `bltzread-word-at', `bltzread-word-overlays',
;; `bltzread-unknown-word-p' and `bltzread-first-unknown-between'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'bltzread-custom)
(require 'bltzread-range)
(require 'bltzread-state)
(require 'bltzread-bltzr)

(defvar bltzread-mode)
(declare-function bltzread--redraw-popup "bltzread-card")

(defvar-local bltzread--state nil "The `bltzread-state' of this buffer's language.")
(defvar-local bltzread--lemmas nil "Hash table of spelling -> basic words.")
(defvar-local bltzread--index nil "Hash table of key -> word overlays.")
(defvar-local bltzread--words nil "Vector of all word overlays in text order.")
(defvar-local bltzread--lookups nil "Hash table of keys looked up this session.")
(defvar-local bltzread--stale nil "Non-nil when the text changed and needs re-reading.")
(defvar-local bltzread--reanalyse-timer nil "Idle timer that re-reads a changed buffer.")
(defvar-local bltzread--fingerprint nil
  "Hash of the words, spacing ignored, that the overlays were made from.")

(defvar bltzread--overlay-map (make-sparse-keymap)
  "Keymap on every word overlay (mouse); filled by `bltzread--install-mouse-keys'.")

;;; Styling

(defun bltzread--require-mode ()
  "Signal a `user-error' unless `bltzread-mode' is on."
  (unless bltzread-mode (user-error "Mode `bltzread-mode' is not on")))

(defun bltzread--require-manual ()
  "Signal a `user-error' when levels may not be set by hand (no-colors mode)."
  (when bltzread-no-colors
    (user-error "Manual levels are off in no-colors mode")))

(defun bltzread--face (level)
  "Face for LEVEL (1 up to `bltzread-known-level', or `ignored'), or nil."
  (cond (bltzread-no-colors nil)
        ((eq level 'ignored) 'bltzread-ignored)
        ((and (integerp level) (<= 1 level (length bltzread-level-faces)))
         (aref bltzread-level-faces (1- level)))))

(defun bltzread--level-of (ov)
  "The level the state gives the word of overlay OV."
  (let ((form (overlay-get ov 'bltzread-form)))
    (bltzread-state-level bltzread--state (overlay-get ov 'bltzread-key) form
                          (gethash form bltzread--lemmas))))

(defun bltzread--style (ov)
  "Set the level and face of word overlay OV from the state."
  (let ((level (bltzread--level-of ov)))
    (overlay-put ov 'bltzread-level level)
    (overlay-put ov 'face (bltzread--face level))))

(defun bltzread--notify-level-change (key ov old)
  "Run `bltzread-level-change-functions' if the word OV of KEY left level OLD."
  (let ((new (overlay-get ov 'bltzread-level)))
    (unless (eql old new)
      (run-hook-with-args 'bltzread-level-change-functions
                          key (overlay-get ov 'bltzread-form) old new))))

(defun bltzread--refresh-key (key)
  "Restyle every occurrence of KEY and announce a change of its level."
  (when-let* ((ovs (gethash key bltzread--index))
              (old (overlay-get (car ovs) 'bltzread-level)))
    (mapc #'bltzread--style ovs)
    (bltzread--notify-level-change key (car ovs) old)))

;;;###autoload
(defun bltzread-toggle-colors ()
  "Toggle no-colors mode in this buffer."
  (interactive)
  (bltzread--require-mode)
  (setq-local bltzread-no-colors (not bltzread-no-colors))
  (mapc #'bltzread--style (seq-filter #'overlay-buffer bltzread--words))
  (bltzread--redraw-popup)
  (message "bltzread: colours %s" (if bltzread-no-colors "off" "on")))

;;; Public API

(defun bltzread-words ()
  "List of the live word overlays of this buffer, in text order.
Each has the properties `bltzread-form' (the lowercased spelling),
`bltzread-key' (what its level is tracked under) and `bltzread-level'
\(1 up to `bltzread-known-level', or `ignored')."
  (seq-filter #'overlay-buffer (append bltzread--words nil)))

(defun bltzread-word-at (&optional pos)
  "Word overlay at POS (default point), also when POS is just after the word."
  (let* ((pos (or pos (point)))
         (word-p (lambda (o) (overlay-get o 'bltzread))))
    (or (seq-find word-p (overlays-at pos))
        (and (> pos (point-min))
             (seq-find word-p (overlays-at (1- pos)))))))

(defun bltzread-require-word ()
  "The word overlay at point, or signal a `user-error'."
  (bltzread--require-mode)
  (or (bltzread-word-at) (user-error "No word at point")))

(defun bltzread-word-overlays (beg end)
  "Word overlays lying wholly within BEG..END, in text order."
  (let ((inside (lambda (o) (and (overlay-get o 'bltzread)
                                 (>= (overlay-start o) beg)
                                 (<= (overlay-end o) end)))))
    (sort (seq-filter inside (overlays-in beg end))
          (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun bltzread-unknown-word-p (ov)
  "Non-nil when word overlay OV is at or below `bltzread-unknown-max-level'."
  (let ((level (overlay-get ov 'bltzread-level)))
    (and (integerp level) (<= level bltzread-unknown-max-level))))

(define-obsolete-function-alias 'bltzread--overlay-at #'bltzread-word-at "0.1.0")
(define-obsolete-function-alias 'bltzread--require-word #'bltzread-require-word "0.1.0")
(define-obsolete-function-alias 'bltzread--word-overlays #'bltzread-word-overlays "0.1.0")
(define-obsolete-function-alias 'bltzread--unknown-p #'bltzread-unknown-word-p "0.1.0")
(define-obsolete-function-alias 'bltzread--all-words #'bltzread-words "0.1.0")

(defun bltzread--lemma (ov)
  "First basic word of OV, or nil."
  (car (gethash (overlay-get ov 'bltzread-form) bltzread--lemmas)))

;;; Words sharing a basic word

(defun bltzread--lemma-key-of (form)
  "The whole-word state key of FORM in this buffer."
  (bltzread-state-lemma-key form (gethash form bltzread--lemmas)))

(defun bltzread--siblings (ov)
  "List of (KEY . FORM) for the forms in view that share the word of OV.
Just OV's own form when levels are set per form."
  (let ((own (cons (overlay-get ov 'bltzread-key) (overlay-get ov 'bltzread-form)))
        (lemma-key (bltzread--lemma-key-of (overlay-get ov 'bltzread-form)))
        (acc nil))
    (when (eq bltzread-level-scope 'word)
      (maphash (lambda (key ovs)
                 (let ((form (overlay-get (car ovs) 'bltzread-form)))
                   (when (and (not (equal key (car own)))
                              (equal (bltzread--lemma-key-of form) lemma-key))
                     (push (cons key form) acc))))
               bltzread--index))
    (cons own acc)))

(defun bltzread--word-keys (ov)
  "All state keys a by-hand change to the word of OV can touch."
  (let ((lemma-key (bltzread--lemma-key-of (overlay-get ov 'bltzread-form))))
    (delete-dups (append (mapcar #'car (bltzread--siblings ov))
                         (list lemma-key (overlay-get ov 'bltzread-key))
                         (bltzread-state-siblings bltzread--state lemma-key)))))

(defun bltzread--maybe-write-now (entries)
  "Under the `immediate' policy write ENTRIES, a list of (FORM . LEMMA), now."
  (when (and entries (eq bltzread-known-write-policy 'immediate))
    (let ((result (bltzread-state-write-known bltzread--state entries)))
      (bltzread-state-queue-remove bltzread--state (mapcar #'car entries))
      (message "Added %d word%s to %s" (length (cdr result))
               (if (cdr (cdr result)) "s" "") (car result)))))

;;; Annotation

(defun bltzread--clear ()
  "Remove all word overlays."
  (remove-overlays (point-min) (point-max) 'bltzread t)
  (setq bltzread--index nil bltzread--words nil))

(defun bltzread--make-word (token)
  "Make, style and index the overlay for TOKEN, a list (BEG END FORM).
Return the overlay."
  (pcase-let* ((`(,beg ,end ,form) token)
               (key (bltzread-state-key form (gethash form bltzread--lemmas)))
               (ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'bltzread t)
    (overlay-put ov 'bltzread-form form)
    (overlay-put ov 'bltzread-key key)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'keymap bltzread--overlay-map)
    (bltzread--style ov)
    (push ov (gethash key bltzread--index))
    ov))

(defun bltzread--text-fingerprint ()
  "Hash of the buffer's words, ignoring how they are spaced."
  (secure-hash 'md5 (mapconcat #'identity
                               (split-string (buffer-substring-no-properties
                                              (point-min) (point-max)))
                               " ")))

;;;###autoload
(defun bltzread-annotate ()
  "Read the buffer's words again and restyle them."
  (interactive)
  (bltzread--require-mode)
  (bltzread--clear)
  (setq bltzread--stale nil
        bltzread--fingerprint (bltzread--text-fingerprint))
  (let* ((lang bltzread-language)
         (tokens (funcall bltzread-tokenizer-function (point-min) (point-max) lang)))
    (setq bltzread--state (bltzread-state-get lang)
          bltzread--index (make-hash-table :test 'equal))
    (when tokens (message "bltzread: asking bltzr about %d words…" (length tokens)))
    (setq bltzread--lemmas (funcall bltzread-lemmatizer-function lang (mapcar #'caddr tokens))
          bltzread--words (vconcat (mapcar #'bltzread--make-word tokens)))
    (message "bltzread: %d words" (length tokens))
    (run-hooks 'bltzread-after-annotate-hook)))

;;; Keeping up with edits

(defun bltzread--words-intact-p ()
  "Non-nil if the text still has the same words and all their overlays."
  (and bltzread--words
       (equal bltzread--fingerprint (bltzread--text-fingerprint))
       (seq-every-p #'overlay-buffer bltzread--words)))

(defun bltzread--reannotate-delay ()
  "Idle seconds to wait before re-reading, or nil for never."
  (pcase bltzread-reannotate-on-change
    ('nil nil)
    ('t 0.3)
    ((pred numberp) bltzread-reannotate-on-change)))

(defun bltzread--after-change (&rest _)
  "Note that the text changed and re-read it once things settle."
  (setq bltzread--stale t)
  (when (timerp bltzread--reanalyse-timer) (cancel-timer bltzread--reanalyse-timer))
  (when-let ((delay (bltzread--reannotate-delay)))
    (setq bltzread--reanalyse-timer
          (run-with-idle-timer delay nil #'bltzread--reanalyse (current-buffer)))))

(defun bltzread--reread ()
  "Re-read the words of the buffer after a change, forgetting the range."
  (bltzread-range-reset (point-min))
  (clrhash bltzread--lookups)
  (condition-case err (bltzread-annotate)
    (error (message "bltzread: %s" (error-message-string err)))))

(defun bltzread--reanalyse (buffer)
  "Re-read BUFFER if its text changed.
When only the spacing changed (a layout switch) the lookups and the range stay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and bltzread-mode bltzread--stale)
        (if (bltzread--words-intact-p)
            (setq bltzread--stale nil)
          (bltzread--reread))))))

;;; Moving between unknown words

(defun bltzread--word-index (pos)
  "Index in `bltzread--words' of the first word starting at or after POS."
  (let ((lo 0) (hi (length bltzread--words)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (< (or (overlay-start (aref bltzread--words mid)) 0) pos)
            (setq lo (1+ mid))
          (setq hi mid))))
    lo))

(defun bltzread--live-unknown-p (ov)
  "Non-nil when OV is a live word overlay that is unknown."
  (and (overlay-buffer ov) (bltzread-unknown-word-p ov)))

(defun bltzread--scan-unknown (from step want)
  "Walk the words from index FROM by STEP; return (MOVED . LAST-HIT).
Stops after WANT unknown words."
  (let ((i from) (moved 0) (hit nil))
    (while (and (< moved want) (>= i 0) (< i (length bltzread--words)))
      (let ((ov (aref bltzread--words i)))
        (when (bltzread--live-unknown-p ov)
          (setq moved (1+ moved) hit ov)))
      (setq i (+ i step)))
    (cons moved hit)))

(defun bltzread-first-unknown-between (start end)
  "Start of the first unknown word between START and END, or nil.
For a package that moves point along with playback, such as subtle."
  (when (and bltzread-mode bltzread--words)
    (let ((hit (cdr (bltzread--scan-unknown (bltzread--word-index start) 1 1))))
      (and hit (< (overlay-start hit) end) (overlay-start hit)))))

(defun bltzread--scan-start (forward)
  "Index of the first word to look at when moving FORWARD or back from point."
  (let ((cur (bltzread-word-at)))
    (if forward
        (bltzread--word-index (if cur (overlay-end cur) (point)))
      (1- (bltzread--word-index (if cur (overlay-start cur) (point)))))))

(defun bltzread--report-move (forward moved want)
  "Tell how a move of WANT unknown words went, MOVED being how many it made.
FORWARD is non-nil when moving forward."
  (cond ((zerop moved)
         (message (if forward "No more unknown words" "No earlier unknown words")))
        ((< moved want)
         (message "Moved %d of %d (no %s unknown words)" moved want
                  (if forward "more" "earlier")))))

(defun bltzread--move-unknown (n)
  "Move N unknown words forward (backward when N is negative), in one pass."
  (bltzread--require-mode)
  (let* ((forward (> n 0))
         (result (bltzread--scan-unknown (bltzread--scan-start forward)
                                         (if forward 1 -1) (abs n))))
    (when (cdr result) (goto-char (overlay-start (cdr result))))
    (unless (zerop n) (bltzread--report-move forward (car result) (abs n)))))

;;;###autoload
(defun bltzread-next-unknown (&optional n)
  "Move to the next word that is still unknown.
With prefix argument N, move N unknown words on; a negative N goes back."
  (interactive "p")
  (bltzread--move-unknown (or n 1)))

;;;###autoload
(defun bltzread-previous-unknown (&optional n)
  "Move to the previous word that is still unknown.
With prefix argument N, move N unknown words back; a negative N goes on."
  (interactive "p")
  (bltzread--move-unknown (- (or n 1))))

(provide 'bltzread-words)
;;; bltzread-words.el ends here
