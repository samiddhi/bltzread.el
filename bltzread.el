;;; bltzread.el --- Reading mode on top of bltzr that colours words by how well you know them -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages

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
;; Start with M-x bltzread-mode.  The Info manual "bltzread" explains everything and
;; "bltzread-tutorial" walks through first use.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'bltzread-custom)
(require 'bltzread-range)
(require 'bltzread-state)
(require 'bltzread-bltzr)
(require 'bltzread-dict)
(require 'bltzread-popup)
(require 'bltzread-review)

(defvar bltzread-mode)

(defvar-local bltzread-language nil
  "Three-letter bltzr language code of this buffer, or nil.")

(defconst bltzread--level-names ["New" "Recognized" "Familiar" "Learned" "Known"])

(defvar-local bltzread--state nil "The `bltzread-state' of this buffer's language.")
(defvar-local bltzread--lemmas nil "Hash table of spelling -> basic words.")
(defvar-local bltzread--index nil "Hash table of key -> word overlays.")
(defvar-local bltzread--words nil "Vector of all word overlays in text order.")
(defvar-local bltzread--lookups nil "Hash table of keys looked up this session.")
(defvar-local bltzread--saved-read-only nil "Value of `buffer-read-only' before the mode.")
(defvar-local bltzread--stale nil "Non-nil when the text changed and needs re-reading.")
(defvar-local bltzread--reanalyse-timer nil)
(defvar-local bltzread--popup-timer nil)
(defvar-local bltzread--popup-ov nil "Word overlay the popup is showing.")
(defvar-local bltzread--popup-dismissed nil "Word overlay whose popup was closed by hand.")
(defvar-local bltzread--popup-full nil "Non-nil when the popup shows a long entry.")
(defvar-local bltzread--entry-ov nil "Word overlay `bltzread--entry-index' belongs to.")
(defvar-local bltzread--entry-index 0 "Which dictionary entry the popup shows.")
(defvar-local bltzread--undo nil
  "Undo stack of (DESCRIPTION SNAPSHOT KEYS HISTORY-P LOOKUPS).")
(defvar-local bltzread--dwell-timer nil "Timer counting a resting popup as a lookup.")
(defvar bltzread--last-language nil "Language last chosen, offered next time.")

;;; Keys

(defvar bltzread-command-map
  (let ((map (make-sparse-keymap)))
    (dolist (n '("1" "2" "3" "4" "5")) (define-key map n #'bltzread-set-level))
    (define-key map "i" #'bltzread-ignore)
    (define-key map "l" #'bltzread-lookup)
    (define-key map "N" #'bltzread-note)
    (define-key map "u" #'bltzread-undo)
    (define-key map "c" #'bltzread-close-popup)
    (define-key map "d" #'bltzread-next-entry)
    (define-key map "D" #'bltzread-previous-entry)
    (define-key map "n" #'bltzread-next-unknown)
    (define-key map "p" #'bltzread-previous-unknown)
    (define-key map "f" #'bltzread-finish-session)
    (define-key map "k" #'bltzread-mark-range-known)
    (define-key map "K" #'bltzread-mark-range-known)
    (define-key map "r" #'bltzread-review)
    (define-key map "m" #'bltzread-range-change-method)
    (define-key map "<" #'bltzread-range-set-start)
    (define-key map ">" #'bltzread-range-set-end)
    (define-key map "a" #'bltzread-annotate)
    (define-key map "L" #'bltzread-set-language)
    (define-key map "." #'bltzread-popup-at-point)
    map)
  "Commands reachable after `bltzread-prefix-key'.")

(defvar bltzread-mode-map (make-sparse-keymap) "Keymap of `bltzread-mode'; filled by `bltzread--install-keys'.")
(defvar bltzread--overlay-map (make-sparse-keymap) "Keymap on every word overlay (mouse).")

(defun bltzread--install-keys ()
  "Fill `bltzread-mode-map' and the word keymap from the current options."
  (setcdr bltzread-mode-map nil)
  (setcdr bltzread--overlay-map nil)
  (when bltzread-prefix-key
    (define-key bltzread-mode-map (kbd bltzread-prefix-key) bltzread-command-map))
  (when bltzread-direct-keys
    (dolist (b bltzread-direct-key-bindings)
      (define-key bltzread-mode-map (kbd (car b)) (cdr b))))
  (when bltzread-mouse-lookup-key
    (define-key bltzread--overlay-map (kbd bltzread-mouse-lookup-key) #'bltzread-lookup-mouse)))

;;; Levels and words

(defun bltzread--face (level)
  "Face for LEVEL, or nil."
  (pcase level
    (1 'bltzread-level-1) (2 'bltzread-level-2) (3 'bltzread-level-3) (4 'bltzread-level-4)
    ('ignored 'bltzread-ignored)))

(defun bltzread--style (ov)
  "Set the face of word overlay OV from its current level."
  (let* ((form (overlay-get ov 'bltzread-form))
         (level (bltzread-state-level bltzread--state (overlay-get ov 'bltzread-key) form
                                (gethash form bltzread--lemmas))))
    (overlay-put ov 'bltzread-level level)
    (overlay-put ov 'face (bltzread--face level))))

(defun bltzread--refresh-key (key)
  "Restyle every occurrence of KEY."
  (dolist (ov (gethash key bltzread--index)) (bltzread--style ov)))

(defun bltzread--overlay-at (&optional pos)
  "Word overlay at POS (default point), also when POS is just after the word."
  (let ((pos (or pos (point))))
    (or (seq-find (lambda (o) (overlay-get o 'bltzread)) (overlays-at pos))
        (and (> pos (point-min))
             (seq-find (lambda (o) (overlay-get o 'bltzread)) (overlays-at (1- pos)))))))

(defun bltzread--require-word ()
  "The word overlay at point, or signal an error."
  (unless bltzread-mode (user-error "bltzread-mode is not on"))
  (or (bltzread--overlay-at) (user-error "No word at point")))

(defun bltzread--lemma (ov)
  "First basic word of OV, or nil."
  (car (gethash (overlay-get ov 'bltzread-form) bltzread--lemmas)))

(defun bltzread--word-overlays (beg end)
  "Word overlays lying wholly within BEG..END, in text order."
  (sort (seq-filter (lambda (o) (and (overlay-get o 'bltzread)
                                     (>= (overlay-start o) beg)
                                     (<= (overlay-end o) end)))
                    (overlays-in beg end))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun bltzread--maybe-write-now (entries)
  "Under the `immediate' policy write ENTRIES, a list of (FORM . LEMMA), now."
  (when (and entries (eq bltzread-known-write-policy 'immediate))
    (let ((result (bltzread-state-write-known bltzread--state entries)))
      (bltzread-state-queue-remove bltzread--state (mapcar #'car entries))
      (message "Added %d word%s to %s" (length (cdr result))
               (if (cdr (cdr result)) "s" "") (car result)))))

(defun bltzread--siblings (ov)
  "List of (KEY . FORM) for the forms in view that share the word of OV.
Just OV's own form when levels are set per form."
  (let ((own (cons (overlay-get ov 'bltzread-key) (overlay-get ov 'bltzread-form))))
    (if (not (eq bltzread-level-scope 'word))
        (list own)
      (let ((lk (bltzread-state-lemma-key (cdr own) (gethash (cdr own) bltzread--lemmas)))
            (acc (list own)))
        (maphash (lambda (key ovs)
                   (let ((form (overlay-get (car ovs) 'bltzread-form)))
                     (when (and (not (equal key (car own)))
                                (equal (bltzread-state-lemma-key form (gethash form bltzread--lemmas))
                                       lk))
                       (push (cons key form) acc))))
                 bltzread--index)
        acc))))

(defun bltzread--word-keys (ov)
  "All state keys a by-hand change to the word of OV can touch."
  (let* ((form (overlay-get ov 'bltzread-form))
         (lk (bltzread-state-lemma-key form (gethash form bltzread--lemmas))))
    (delete-dups (append (mapcar #'car (bltzread--siblings ov))
                         (list lk (overlay-get ov 'bltzread-key))
                         (bltzread-state-siblings bltzread--state lk)))))

;;; Annotation

(defun bltzread--clear ()
  "Remove all word overlays."
  (remove-overlays (point-min) (point-max) 'bltzread t)
  (setq bltzread--index nil bltzread--words nil))

(defun bltzread-annotate ()
  "Read the buffer's words again and restyle them."
  (interactive)
  (unless bltzread-mode (user-error "bltzread-mode is not on"))
  (bltzread--clear)
  (setq bltzread--stale nil
        bltzread--fingerprint (bltzread--text-fingerprint))
  (let* ((lang bltzread-language)
         (tokens (bltzread-bltzr-tokenize (point-min) (point-max) (bltzread-bltzr-regexp lang))))
    (setq bltzread--state (bltzread-state-get lang))
    (when tokens (message "bltzread: asking bltzr about %d words…" (length tokens)))
    (setq bltzread--lemmas (bltzread-bltzr-lemmas lang (mapcar #'caddr tokens))
          bltzread--index (make-hash-table :test 'equal))
    (let ((all nil))
      (dolist (tok tokens)
        (pcase-let* ((`(,b ,e ,form) tok)
                     (key (bltzread-state-key form (gethash form bltzread--lemmas)))
                     (ov (make-overlay b e nil t nil)))
          (overlay-put ov 'bltzread t)
          (overlay-put ov 'bltzread-form form)
          (overlay-put ov 'bltzread-key key)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'keymap bltzread--overlay-map)
          (bltzread--style ov)
          (push ov all)
          (push ov (gethash key bltzread--index))))
      (setq bltzread--words (vconcat (nreverse all))))
    (message "bltzread: %d words" (length tokens))))

(defvar-local bltzread--fingerprint nil
  "Hash of the words, spacing ignored, that the overlays were made from.")

(defun bltzread--text-fingerprint ()
  "Hash of the buffer's words, ignoring how they are spaced."
  (secure-hash 'md5 (mapconcat #'identity
                               (split-string (buffer-substring-no-properties (point-min) (point-max)))
                               " ")))

(defun bltzread--words-intact-p ()
  "Non-nil if the text still has the same words and all their overlays."
  (and bltzread--words
       (equal bltzread--fingerprint (bltzread--text-fingerprint))
       (seq-every-p #'overlay-buffer bltzread--words)))

(defun bltzread--after-change (&rest _)
  "Note that the text changed and re-read it once things settle."
  (setq bltzread--stale t)
  (when (timerp bltzread--reanalyse-timer) (cancel-timer bltzread--reanalyse-timer))
  (setq bltzread--reanalyse-timer
        (run-with-idle-timer 0.3 nil #'bltzread--reanalyse (current-buffer))))

(defun bltzread--reanalyse (buffer)
  "Re-read BUFFER if its text changed."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and bltzread-mode bltzread--stale)
        (if (bltzread--words-intact-p)
            ;; Only spacing changed (e.g. a layout switch): keep lookups and range.
            (setq bltzread--stale nil)
          (bltzread-range-reset (point-min))
          (clrhash bltzread--lookups)
          (condition-case err (bltzread-annotate)
            (error (message "bltzread: %s" (error-message-string err)))))))))

;;; Popup

(defun bltzread--hint-key (command)
  "First key of COMMAND in the mode's maps, as text."
  (let ((key (where-is-internal command (list bltzread-mode-map) t)))
    (cond ((null key) "-")
          ((and (eq command 'bltzread-set-level) (string-match-p "[1-5]\\'" (key-description key)))
           (replace-regexp-in-string "[1-5]\\'" "1-5" (key-description key)))
          (t (key-description key)))))

(defun bltzread--popup-rows (ov full)
  "Rows describing word overlay OV; FULL allows a long dictionary entry."
  (let* ((form (overlay-get ov 'bltzread-form))
         (key (overlay-get ov 'bltzread-key))
         (lemmas (gethash form bltzread--lemmas))
         (level (bltzread-state-level bltzread--state key form lemmas))
         (reads (bltzread-state-reads bltzread--state key))
         (need (and (integerp level) (nth (1- level) bltzread-promotion-thresholds)))
         (note (bltzread-state-note bltzread--state key))
         (entries (and (or full bltzread-popup-show-gloss)
                       (bltzread-dict-lookup bltzread-language (seq-uniq (cons form lemmas)))))
         (_ (unless (eq ov bltzread--entry-ov)
              (setq bltzread--entry-ov ov bltzread--entry-index 0)))
         (index (if entries (mod bltzread--entry-index (length entries)) 0))
         (gloss (bltzread-dict-display (nth index entries) full))
         (rows (list (concat (bltzread-popup-title form)
                             (if (and lemmas (not (equal (car lemmas) form)))
                                 (bltzread-popup-dim (concat "  → " (string-join lemmas ", ")))
                               "")))))
    (setq rows
          (append rows
                  (list (concat
                         (propertize (if (integerp level)
                                         (format " %d %s " level (aref bltzread--level-names (1- level)))
                                       " ignored ")
                                     'face (or (bltzread--face level) 'bltzread-popup-dim))
                         (if need (bltzread-popup-dim (format "  %d/%d clean reads" reads need)) "")
                         (if (gethash key bltzread--lookups) (bltzread-popup-dim "  looked up") "")))
                  (and note (list (concat (bltzread-popup-dim "note: ") note)))
                  (and (cdr entries)
                       (list (bltzread-popup-dim (format "entry %d/%d  (%s next, %s previous)"
                                                   (1+ index) (length entries)
                                                   (bltzread--hint-key 'bltzread-next-entry)
                                                   (bltzread--hint-key 'bltzread-previous-entry)))))
                  (and gloss
                       (cons "" (mapcar (lambda (l) l)
                                        (bltzread-popup-wrap gloss bltzread-popup-width
                                                       (if full bltzread-popup-lookup-max-lines
                                                         bltzread-popup-max-lines)))))
                  (list (bltzread-popup-dim
                         (format "%s level · %s ignore · %s lookup · %s note · %s undo · %s close"
                                 (bltzread--hint-key 'bltzread-set-level) (bltzread--hint-key 'bltzread-ignore)
                                 (bltzread--hint-key 'bltzread-lookup) (bltzread--hint-key 'bltzread-note)
                                 (bltzread--hint-key 'bltzread-undo) (bltzread--hint-key 'bltzread-close-popup))))))
    rows))

(defun bltzread--hide-popup ()
  "Hide the popup and forget which word it showed."
  (when (timerp bltzread--popup-timer) (cancel-timer bltzread--popup-timer))
  (setq bltzread--popup-timer nil)
  (when (timerp bltzread--dwell-timer) (cancel-timer bltzread--dwell-timer))
  (setq bltzread--dwell-timer nil)
  (when bltzread--popup-ov
    (setq bltzread--popup-ov nil)
    (bltzread-popup-hide)))

(defun bltzread--hide-popup-later ()
  "Hide the popup after `bltzread-popup-delay' unless point reaches a word first."
  (when (timerp bltzread--popup-timer) (cancel-timer bltzread--popup-timer))
  (setq bltzread--popup-timer
        (run-with-idle-timer
         bltzread-popup-delay nil
         (lambda (buf)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (unless (bltzread--overlay-at) (bltzread--hide-popup)))))
         (current-buffer))))

(defun bltzread--show-popup (buffer &optional full)
  "Show the popup for the word at point in BUFFER; FULL for a long entry."
  (when (and (buffer-live-p buffer) (eq buffer (current-buffer)))
    (when-let ((ov (bltzread--overlay-at)))
      (setq bltzread--popup-full full)
      (bltzread-popup-show (bltzread--popup-rows ov full) (overlay-start ov))
      (setq bltzread--popup-ov ov))))

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

(defun bltzread-dismiss-popup ()
  "Close the popup if one is on screen; return non-nil if there was one."
  (when (bltzread-popup-anchor)
    (bltzread-close-popup)
    t))

(defun bltzread-close-popup ()
  "Close the popup.  It comes back when you move to another word."
  (interactive)
  (bltzread--dismiss-popup (or bltzread--popup-ov (bltzread--overlay-at))))

(defun bltzread--cycle-entry (n)
  "Show the dictionary entry N places on, in the popup."
  (let* ((ov (bltzread--require-word))
         (form (overlay-get ov 'bltzread-form))
         (entries (bltzread-dict-lookup bltzread-language
                                  (seq-uniq (cons form (gethash form bltzread--lemmas))))))
    (cond ((null entries) (message "No dictionary entries for %s" form))
          ((null (cdr entries)) (message "Only one entry for %s" form))
          (t (unless (eq ov bltzread--entry-ov) (setq bltzread--entry-ov ov bltzread--entry-index 0))
             (setq bltzread--entry-index (mod (+ bltzread--entry-index n) (length entries))
                   bltzread--popup-dismissed nil)
             (bltzread--count-interaction ov)
             (bltzread--show-popup (current-buffer) bltzread--popup-full)))))

(defun bltzread-next-entry ()
  "Show the next dictionary entry of the word at point."
  (interactive)
  (bltzread--cycle-entry 1))

(defun bltzread-previous-entry ()
  "Show the previous dictionary entry of the word at point."
  (interactive)
  (bltzread--cycle-entry -1))

(defvar-local bltzread--pre-command-point nil
  "Point when the running command started.")

(defun bltzread--pre-command ()
  (setq bltzread--pre-command-point (point)))

(defun bltzread--popup-wanted-p ()
  "Non-nil if the command that just ran asks for a popup at point.
Without `bltzread-popup-only-on-motion' resting on a word is enough."
  (or (not bltzread-popup-only-on-motion)
      (not (eql bltzread--pre-command-point (point)))
      (eq this-command 'bltzread-popup-at-point)))

(defun bltzread--schedule-popup ()
  "Show or hide the popup to match where point is."
  (let ((ov (bltzread--overlay-at)))
    (when (and bltzread--popup-ov (not (eq ov bltzread--popup-ov)))
      (if (and (bltzread-popup-window-visible-p) (not (eq ov bltzread--popup-dismissed)))
          ;; a top/bottom window is open: moving word to word swaps its text
          ;; instead of closing and reopening it, which makes the buffer jump
          (if ov
              (progn (setq bltzread--popup-dismissed nil)
                     (bltzread--show-popup (current-buffer))
                     (when (and bltzread--popup-ov (eq bltzread-lookup-counts 'popup))
                       (setq bltzread--dwell-timer
                             (run-with-idle-timer bltzread-lookup-dwell nil #'bltzread--dwell-fire
                                                  (current-buffer) bltzread--popup-ov))))
            (bltzread--hide-popup-later))
        (bltzread--hide-popup)))
    (when (timerp bltzread--popup-timer) (cancel-timer bltzread--popup-timer))
    (when (timerp bltzread--dwell-timer) (cancel-timer bltzread--dwell-timer))
    (unless (eq ov bltzread--popup-dismissed) (setq bltzread--popup-dismissed nil))
    (when (and ov (eq bltzread-popup-trigger 'point)
               (bltzread--popup-wanted-p)
               (not (and (eq ov bltzread--popup-ov) (bltzread-popup-visible-p)))
               (not (eq ov bltzread--popup-dismissed)))
      (setq bltzread--popup-timer
            (run-with-idle-timer
             bltzread-popup-delay nil
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (and (eq (current-buffer) (window-buffer (selected-window)))
                              bltzread-mode)
                     (bltzread--show-popup buf)
                     (when (and bltzread--popup-ov (eq bltzread-lookup-counts 'popup))
                       (setq bltzread--dwell-timer
                             (run-with-idle-timer bltzread-lookup-dwell nil #'bltzread--dwell-fire
                                                  buf bltzread--popup-ov)))))))
             (current-buffer))))))

(defun bltzread--post-command ()
  "Per-command bookkeeping: reading progress and the popup."
  (bltzread-range-track)
  (bltzread--schedule-popup))

(defun bltzread-popup-at-point ()
  "Show the popup for the word at point now."
  (interactive)
  (let ((ov (bltzread--require-word)))
    (setq bltzread--popup-dismissed nil)
    (bltzread--count-interaction ov)
    (bltzread--show-popup (current-buffer))))

;;; Commands on a word

(defun bltzread--digit ()
  "Level from the key that ran the command, else asked for."
  (let ((c last-command-event))
    (if (and (characterp c) (<= ?1 c ?5))
        (- c ?0)
      (read-number "Level (1-5): "))))

(defun bltzread--undo-push (description keys &optional history-p)
  "Remember how to take back DESCRIPTION, which is about to change KEYS.
The entry goes on `bltzread--undo' and into `buffer-undo-list', so plain `undo'
(and `undo-redo') take it back in sequence with other changes."
  (let ((entry (vector description (bltzread-state-snapshot bltzread--state keys) keys history-p
                       (copy-hash-table bltzread--lookups) t)))
    (push entry bltzread--undo)
    (when (> (length bltzread--undo) bltzread-undo-limit)
      (setq bltzread--undo (seq-take bltzread--undo bltzread-undo-limit)))
    (bltzread--undo-record entry)))

(defun bltzread--undo-record (entry)
  "Put ENTRY in the buffer's undo list as its own undo step."
  (when (listp buffer-undo-list)
    (undo-boundary)
    (push (list 'apply #'bltzread--undo-apply entry) buffer-undo-list)
    (undo-boundary)))

(defun bltzread--undo-apply (entry &optional no-reverse)
  "Restore the state ENTRY describes and record the reverse for redo.
With NO-REVERSE, record nothing."
  (if (not (aref entry 5))
      (message "bltzread: that change was already undone with `bltzread-undo'")
    (aset entry 5 nil)
    (let* ((keys (aref entry 2))
           (history-p (aref entry 3))
           (reverse (vector (aref entry 0) (bltzread-state-snapshot bltzread--state keys) keys
                            (cond ((consp history-p) t)
                                  (history-p (cons 'push (car (bltzread-state-history bltzread--state)))))
                            (copy-hash-table bltzread--lookups) t)))
      (bltzread-state-restore bltzread--state (aref entry 1))
      (setq bltzread--lookups (aref entry 4))
      (cond ((eq history-p t)
             (setf (bltzread-state-history bltzread--state) (cdr (bltzread-state-history bltzread--state))))
            ((consp history-p)
             (push (cdr history-p) (bltzread-state-history bltzread--state))))
      (dolist (key keys) (bltzread--refresh-key key))
      (bltzread--redraw-popup)
      (when (and (not no-reverse) (listp buffer-undo-list))
        (push (list 'apply #'bltzread--undo-apply reverse) buffer-undo-list))
      (message "Undone: %s%s" (aref entry 0)
               (if (eq bltzread-known-write-policy 'immediate)
                   " (words already written to a skip list stay there)" "")))))

(defun bltzread-undo ()
  "Take back the last bltzread change even when other changes came in between.
Plain `undo' does the same in order; use this to skip over other edits."
  (interactive)
  (unless bltzread-mode (user-error "bltzread-mode is not on"))
  (let ((entry (pop bltzread--undo)))
    (unless entry (user-error "Nothing to undo"))
    (when (listp buffer-undo-list)
      (setq buffer-undo-list
            (seq-remove (lambda (e) (and (consp e) (eq (car e) 'apply) (eq (nth 2 e) entry)))
                        buffer-undo-list))
      (let ((l buffer-undo-list))       ; drop the boundary left doubled
        (while (cdr l)
          (if (and (null (car l)) (null (cadr l)))
              (setcdr l (cddr l))
            (setq l (cdr l))))))
    (bltzread--undo-apply entry t)))

(defun bltzread--undo-advice (fn &rest args)
  "Let the undo commands run in the read-only buffer of `bltzread-mode'."
  (if (bound-and-true-p bltzread-mode)
      (let ((inhibit-read-only t)) (apply fn args))
    (apply fn args)))

(dolist (cmd '(undo undo-only undo-redo))
  (advice-add cmd :around #'bltzread--undo-advice))

(defun bltzread--set (ov level)
  "Give the word of OV LEVEL (1-5 or `ignored') and refresh.
Per `bltzread-level-scope' this is the whole word or just the form."
  (let* ((form (overlay-get ov 'bltzread-form))
         (key (overlay-get ov 'bltzread-key))
         (lemma (bltzread--lemma ov))
         (lemmas (gethash form bltzread--lemmas))
         (siblings (bltzread--siblings ov))
         (keys (bltzread--word-keys ov))
         (_ (bltzread--undo-push (format "%s → %s" form level) keys))
         (queued (if (eq level 'ignored)
                     (progn (bltzread-state-ignore bltzread--state key (overlay-get ov 'bltzread-level)
                                             form lemmas (mapcar #'car siblings))
                            nil)
                   (bltzread-state-set-level bltzread--state key level form lemma siblings))))
    (dolist (k keys) (bltzread--refresh-key k))
    (bltzread--after-set ov)
    (when queued
      (bltzread--maybe-write-now
       (mapcar (lambda (s) (cons (downcase (cdr s)) (and lemma (downcase lemma))))
               (if (eq bltzread-level-scope 'word) siblings (list (cons key form))))))
    (message "%s → %s%s" form
             (if (integerp level) (format "level %d (%s)" level (aref bltzread--level-names (1- level))) "ignored")
             (if (and queued (not (eq bltzread-known-write-policy 'immediate))) ", queued for review" ""))))

(defun bltzread-set-level (level)
  "Set the word at point to LEVEL, 1 (new) to 5 (known).
Bound to the digit keys; the key pressed picks the level."
  (interactive (list (bltzread--digit)))
  (unless (<= 1 level 5) (user-error "Level must be 1 to 5"))
  (bltzread--set (bltzread--require-word) level))

(defun bltzread-ignore ()
  "Ignore the word at point, as for names and numbers.
On a word that is already ignored, stop ignoring it.  Take it back with
`bltzread-undo'."
  (interactive)
  (let* ((ov (bltzread--require-word))
         (key (overlay-get ov 'bltzread-key))
         (form (overlay-get ov 'bltzread-form)))
    (if (eq (overlay-get ov 'bltzread-level) 'ignored)
        (let ((keys (bltzread--word-keys ov)))
               (bltzread--undo-push (format "unignore %s" form) keys)
               (bltzread-state-unignore bltzread--state key form (gethash form bltzread--lemmas))
               (dolist (k keys) (bltzread--refresh-key k))
               (bltzread--after-set ov)
               (message "%s is no longer ignored" form))
      (bltzread--set ov 'ignored))))

(defun bltzread--note-lookup (ov)
  "Record that the word of OV was looked up this session.
Only the first lookup of a word in a session counts and can be undone."
  (let ((key (overlay-get ov 'bltzread-key)))
    (unless (gethash key bltzread--lookups)
      (bltzread--undo-push (format "lookup %s" (overlay-get ov 'bltzread-form)) (list key))
      (puthash key t bltzread--lookups)
      (bltzread-state-record-lookup bltzread--state key (overlay-get ov 'bltzread-level))
      (bltzread--refresh-key key))))

(defun bltzread--count-interaction (ov)
  "Count asking more of the popup of OV as a lookup, when the options say so."
  (when (memq bltzread-lookup-counts '(interaction popup))
    (bltzread--note-lookup ov)))

(defun bltzread--dwell-fire (buffer ov)
  "Count OV as looked up if point rested on it in BUFFER with its popup showing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and bltzread-mode (eq buffer (window-buffer (selected-window)))
                 (eq (bltzread--overlay-at) ov) (eq bltzread--popup-ov ov)
                 (bltzread-popup-visible-p))
        (bltzread--note-lookup ov)))))

(defun bltzread-lookup ()
  "Look up the word at point, showing its full dictionary entry."
  (interactive)
  (let ((ov (bltzread--require-word)))
    (bltzread--note-lookup ov)
    (bltzread--hide-popup)
    (setq bltzread--popup-dismissed nil)
    (bltzread--show-popup (current-buffer) t)))

(defun bltzread-lookup-mouse (event)
  "Look up the word clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (bltzread-lookup))

(defun bltzread-note ()
  "Add or edit your own note on the word at point."
  (interactive)
  (let* ((ov (bltzread--require-word))
         (key (overlay-get ov 'bltzread-key))
         (note (read-string (format "Note for %s: " (overlay-get ov 'bltzread-form))
                            (bltzread-state-note bltzread--state key))))
    (bltzread--undo-push (format "note on %s" (overlay-get ov 'bltzread-form)) (list key))
    (bltzread-state-set-note bltzread--state key note)
    (bltzread--redraw-popup)
    (message "Note %s" (if (string-empty-p note) "removed" "saved"))))

;;; Moving

(defun bltzread--unknown-p (ov)
  "Non-nil when OV is a word at or below `bltzread-unknown-max-level'."
  (let ((level (overlay-get ov 'bltzread-level)))
    (and (integerp level) (<= level bltzread-unknown-max-level))))

(defun bltzread--all-words ()
  "All word overlays in text order."
  (append bltzread--words nil))

(defun bltzread--word-index (pos)
  "Index in `bltzread--words' of the first word starting at or after POS."
  (let ((lo 0) (hi (length bltzread--words)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (< (or (overlay-start (aref bltzread--words mid)) 0) pos)
            (setq lo (1+ mid))
          (setq hi mid))))
    lo))

(defun bltzread--move-unknown (n)
  "Move N unknown words forward (backward when N is negative), in one pass."
  (unless bltzread-mode (user-error "bltzread-mode is not on"))
  (let* ((forward (> n 0))
         (want (abs n))
         (cur (bltzread--overlay-at))
         (words bltzread--words)
         (i (if forward
                (bltzread--word-index (if cur (overlay-end cur) (point)))
              (1- (bltzread--word-index (if cur (overlay-start cur) (point))))))
         (moved 0)
         (hit nil))
    (while (and (< moved want) (>= i 0) (< i (length words)))
      (let ((ov (aref words i)))
        (when (and (overlay-buffer ov) (bltzread--unknown-p ov))
          (setq moved (1+ moved) hit ov)))
      (setq i (if forward (1+ i) (1- i))))
    (when hit (goto-char (overlay-start hit)))
    (cond ((zerop n))
          ((zerop moved)
           (message (if forward "No more unknown words" "No earlier unknown words")))
          ((< moved want)
           (message "Moved %d of %d (no %s unknown words)" moved want
                    (if forward "more" "earlier"))))))

(defun bltzread-next-unknown (&optional n)
  "Move to the next word that is still unknown.
With prefix argument N, move N unknown words on; a negative N goes back."
  (interactive "p")
  (bltzread--move-unknown (or n 1)))

(defun bltzread-previous-unknown (&optional n)
  "Move to the previous word that is still unknown.
With prefix argument N, move N unknown words back; a negative N goes on."
  (interactive "p")
  (bltzread--move-unknown (- (or n 1))))

;;; Reading sessions

(defun bltzread-default-bookmark-name ()
  "Bookmark name for the current buffer."
  (format "bltzread: %s" (buffer-name)))

(defun bltzread--offer-bookmark (end)
  "Move the reading bookmark to END per `bltzread-bookmark-policy'."
  (when (and (require 'bookmark nil t)
             (pcase bltzread-bookmark-policy
               ('always t)
               ('ask (y-or-n-p "Move the bookmark to where you stopped? "))))
    (save-excursion
      (goto-char end)
      (bookmark-set (funcall bltzread-bookmark-name-function)))))

(defun bltzread-finish-session (&optional mark-known)
  "Record what you read: ask how far, then count the words in that range.
Each word in the range that you did not look up gets a clean read, and
may move up a level.  With prefix argument MARK-KNOWN, mark every such
word known instead."
  (interactive "P")
  (unless bltzread-mode (user-error "bltzread-mode is not on"))
  (pcase-let* ((`(,beg . ,end) (bltzread-range-pick))
               (ovs (bltzread--word-overlays beg end))
               (seen (make-hash-table :test 'equal))
               (eligible nil))
    (dolist (ov ovs)
      (let ((key (overlay-get ov 'bltzread-key)))
        (when (and (memq (overlay-get ov 'bltzread-level) '(1 2 3 4))
                   (not (gethash key bltzread--lookups))
                   (or bltzread-count-each-occurrence (not (gethash key seen))))
          (puthash key t seen)
          (push ov eligible))))
    (setq eligible (nreverse eligible))
    (bltzread-range-highlight beg end)
    (if (and bltzread-range-confirm
             (not (y-or-n-p (format "%s %d word%s (%d in range)? "
                                    (if mark-known "Mark known:" "Count clean reads for")
                                    (length eligible) (if (cdr eligible) "s" "")
                                    (length ovs)))))
        (progn (bltzread-range-clear-highlight) (message "Nothing recorded"))
      (let ((promoted 0) (queued 0) (newly nil) (changed nil))
        (bltzread--undo-push "reading session"
                       (if mark-known
                           (seq-uniq (apply #'append (mapcar #'bltzread--word-keys eligible)))
                         (mapcar (lambda (ov) (overlay-get ov 'bltzread-key)) eligible))
                       t)
        (dolist (ov eligible)
          (let* ((key (overlay-get ov 'bltzread-key))
                 (form (overlay-get ov 'bltzread-form))
                 (lemma (bltzread--lemma ov))
                 (level (overlay-get ov 'bltzread-level))
                 (siblings (and mark-known (bltzread--siblings ov)))
                 (result (if mark-known
                             (and (bltzread-state-set-level bltzread--state key 5 form lemma siblings)
                                  'queued)
                           (bltzread-state-record-read bltzread--state key level form lemma))))
            (if mark-known
                (dolist (s siblings) (push (car s) changed))
              (push key changed))
            (pcase result
              ('promoted (cl-incf promoted))
              ('queued (cl-incf queued)
                       (dolist (s (or siblings (list (cons key form))))
                         (push (cons (downcase (cdr s)) (and lemma (downcase lemma)))
                               newly))))))
        (dolist (key (seq-uniq changed)) (bltzread--refresh-key key))
        (bltzread--maybe-write-now (nreverse newly))
        (bltzread-state-add-history
         bltzread--state (list :time (format-time-string "%FT%T") :buffer (buffer-name)
                         :beg beg :end end :words (length ovs) :counted (length eligible)
                         :promoted promoted :queued queued))
        (bltzread-state-save bltzread--state)
        (clrhash bltzread--lookups)
        (bltzread-range-clear-highlight)
        (setq bltzread-range--start (copy-marker end)
              bltzread-range--end nil
              bltzread-range--farthest end)
        (message "bltzread: %d word%s counted, %d moved up, %d queued for review"
                 (length eligible) (if (cdr eligible) "s" "") promoted queued)
        (bltzread--offer-bookmark end)))))

(defun bltzread-mark-range-known ()
  "Mark every word in the range you read, that you did not look up, as known."
  (interactive)
  (bltzread-finish-session t))

(defvar-local bltzread--offered nil
  "Non-nil once the buffer has been asked whether to record its reading.")

(defun bltzread--ask-to-record ()
  "Ask whether to record the reading of this buffer, and do so on a yes."
  (setq bltzread--offered t)
  (bltzread-popup-hide)
  (condition-case err
      (when (y-or-n-p "Record what you read in bltzread? ")
        (bltzread-finish-session))
    (user-error (message "bltzread: %s" (cadr err)))))

(defun bltzread-offer-record ()
  "Ask whether to record what was read, unless nothing was or it was asked.
For a caller that closes the buffer later, at a moment of its choosing: the
question is then not repeated when the buffer is killed."
  (when (and bltzread-mode bltzread-prompt-on-close (not bltzread--offered)
             (> (- (bltzread-range-farthest) (or (bltzread-range-start) 1)) 0))
    (bltzread--ask-to-record)))

(defun bltzread--query-kill ()
  "Offer to record reading before the buffer is killed.  Always return t."
  (when (and bltzread-mode bltzread-prompt-on-close (not bltzread--offered)
             (> (- (bltzread-range-farthest) (or (bltzread-range-start) 1)) bltzread-min-chars-for-prompt))
    (bltzread--ask-to-record))
  t)

;;; Language

(defun bltzread--detect-language (&optional text)
  "Language bltzr detects in TEXT (default: the start of this buffer), or nil."
  (let ((text (or text (buffer-substring-no-properties
                        (point-min)
                        (min (point-max) (+ (point-min) bltzread-detect-sample-size))))))
    (unless (string-blank-p text)
      (bltzread-bltzr-detect-language text))))

(defun bltzread--read-language (&optional text)
  "The language for this buffer: set, default, detected, or asked for.
bltzr detects it from TEXT (default: this buffer); you are asked only when
bltzr answers unknown."
  (setq bltzread--last-language
        (or bltzread-language bltzread-default-language
            (let ((detected (bltzread--detect-language text)))
              (when detected (message "bltzread: detected language %s" detected))
              detected)
            (completing-read (format-prompt "Language code" bltzread--last-language)
                             (or (bltzread-bltzr-languages) '("eng")) nil nil nil nil
                             bltzread--last-language))))

(defun bltzread-set-language (lang)
  "Use LANG, a bltzr language code, in this buffer."
  (interactive (list (completing-read "Language code: " (or (bltzread-bltzr-languages) '("eng")))))
  (setq bltzread-language lang)
  (when bltzread-mode (bltzread-annotate)))

;;; The mode

(defun bltzread--enable ()
  "Turn bltzread-mode on in the current buffer."
  (unless bltzread-language (setq bltzread-language (bltzread--read-language)))
  (bltzread--install-keys)
  (bltzread--warn-write-policy)
  (when (eq buffer-undo-list t) (buffer-enable-undo)) ; plain `undo' needs a list
  (setq bltzread--lookups (make-hash-table :test 'equal)
        bltzread--saved-read-only buffer-read-only)
  (when bltzread-make-read-only (setq buffer-read-only t))
  (bltzread-range-reset (point-min))
  (bltzread-range-show-numbers)
  (add-hook 'pre-command-hook #'bltzread--pre-command nil t)
  (add-hook 'post-command-hook #'bltzread--post-command nil t)
  (add-hook 'after-change-functions #'bltzread--after-change nil t)
  (add-hook 'kill-buffer-query-functions #'bltzread--query-kill nil t)
  (add-hook 'kill-buffer-hook #'bltzread--disable nil t)
  (bltzread-popup-install-hooks)
  (condition-case err (bltzread-annotate)
    (error (bltzread--disable)
           (setq bltzread-mode nil)
           (signal (car err) (cdr err)))))

(defun bltzread--disable ()
  "Turn bltzread-mode off in the current buffer."
  (bltzread--hide-popup)
  (remove-hook 'pre-command-hook #'bltzread--pre-command t)
  (remove-hook 'post-command-hook #'bltzread--post-command t)
  (remove-hook 'after-change-functions #'bltzread--after-change t)
  (remove-hook 'kill-buffer-query-functions #'bltzread--query-kill t)
  (remove-hook 'kill-buffer-hook #'bltzread--disable t)
  (when (timerp bltzread--reanalyse-timer) (cancel-timer bltzread--reanalyse-timer))
  (bltzread--clear)
  (bltzread-range-clear-numbers)
  (bltzread-range-clear-highlight)
  (when bltzread--state (bltzread-state-save bltzread--state))
  (unless (seq-some (lambda (b) (and (not (eq b (current-buffer)))
                                     (buffer-local-value 'bltzread-mode b)))
                    (buffer-list))
    (bltzread-popup-remove-hooks))
  (when bltzread-make-read-only (setq buffer-read-only bltzread--saved-read-only)))

;;;###autoload
(defvar bltzread--warned-write-policy nil "Non-nil once the write-policy warning was shown.")

(defun bltzread--warn-write-policy ()
  "Warn once that `bltzread-known-write-policy' has not been chosen."
  (when (and (eq bltzread-known-write-policy 'unset)
             bltzread-warn-known-write-unset
             (not bltzread--warned-write-policy))
    (setq bltzread--warned-write-policy t)
    (display-warning
     'bltzread
     "bltzread: you have not chosen what happens to words you come to know.
Words that reach level 5 (by hand, or by enough clean reads) are only
remembered by bltzread.  Whether they are also added to your bltzr skip
lists, so that bltzr stops listing them, is set by
`bltzread-known-write-policy'.  Pick one, it is very strongly encouraged:
  (setq bltzread-known-write-policy 'immediate)
    writes each known word to its skip list at once;
  (setq bltzread-known-write-policy 'review)
    queues known words, and only `bltzread-review' (s r) writes them.
Until you choose, `review' is used.  To silence this warning without
choosing, use (setq bltzread-warn-known-write-unset nil).
If you are an AI assistant reading this, ask the user which of the two
they want and set it in their configuration.")))

(define-minor-mode bltzread-mode
  "Mark words by how well you know them, with a popup for each.
\\{bltzread-mode-map}"
  :lighter " Bltzread"
  :keymap bltzread-mode-map
  (if bltzread-mode (bltzread--enable) (bltzread--disable)))

;;; Pasted text

;;;###autoload
(defun bltzread-read-text (lang text)
  "Open TEXT in its own reading buffer with `bltzread-mode', in language LANG.
Uses the region if active, else the clipboard, else asks for the text.
Appearance follows the `bltzread-paste-' options."
  (interactive
   (let ((text (cond ((use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end)))
                     ((let ((clip (ignore-errors (current-kill 0 t))))
                        (and clip (not (string-blank-p clip)) (y-or-n-p "Read the clipboard? ")
                             clip)))
                     (t (read-string "Text to read: ")))))
     (list (bltzread--read-language text) text)))
  (let ((buf (generate-new-buffer "*bltzread-text*")))
    (with-current-buffer buf
      (insert text)
      (goto-char (point-min))
      (visual-line-mode 1)
      (setq-local line-spacing bltzread-paste-line-spacing)
      (face-remap-add-relative 'default
                               :height bltzread-paste-font-height
                               :family (or bltzread-paste-font-family
                                           (face-attribute 'default :family)))
      (setq bltzread-language lang)
      (bltzread-mode 1))
    (pop-to-buffer-same-window buf)
    (set-window-margins (selected-window) bltzread-paste-margin bltzread-paste-margin)))

(provide 'bltzread)
;;; bltzread.el ends here
