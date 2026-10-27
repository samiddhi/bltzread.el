;;; bltzread-state.el --- Per-language word levels, notes and history -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; One state file per language, an s-expression written with `prin1'.
;; Levels run 1 (new) to `bltzread-known-level'; `ignored' marks names and the like.
;; A word without a record is level 1, unless a bltzr skip list names it,
;; in which case it is level 5 (see `bltzread-skip-lists-count-as-known').
;;
;; Words are keyed by spelling (lowercased) or by basic word, per
;; `bltzread-track-by'.  A level set by hand applies to the whole word (the
;; basic word, kept under the key "lemma:BASIC") unless `bltzread-level-scope' is
;; `form'.  Clean reads and the levels they earn belong to the form.
;; Pure data and functions; no buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bltzread-custom)
(require 'bltzread-lists)

(cl-defstruct (bltzread-state (:constructor bltzread-state--create))
  lang
  (words (make-hash-table :test 'equal)) ; key -> plist (:level :reads :note)
  queue                                   ; list of (FORM . LEMMA) awaiting review
  exposed                                 ; queued forms that got there by reading exposure
  history                                 ; newest first, plists
  known-forms known-lemmas                ; skip-list tables, not saved
  dirty timer)

(defvar bltzread-state--table (make-hash-table :test 'equal)
  "Loaded states keyed by language code.")

(defconst bltzread-state--version 1)

;;; Files

(defun bltzread-state-file (lang)
  "State file for LANG."
  (expand-file-name (concat lang ".eld") bltzread-state-directory))

(defun bltzread-state--read-file (file)
  "The data saved in FILE, or nil.
A file that cannot be read is copied to FILE.corrupt and reported, so the
next save does not destroy it."
  (when (file-readable-p file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))
      (error
       (copy-file file (concat file ".corrupt") t)
       (display-warning 'bltzread (format "Cannot read %s (%s); kept a copy as %s.corrupt"
                                          file (error-message-string err) file))
       nil))))

(defun bltzread-state--check-version (data file)
  "Warn when DATA, read from FILE, was written by a newer bltzread."
  (let ((version (or (plist-get data :version) 1)))
    (when (> version bltzread-state--version)
      (display-warning 'bltzread (format "%s has version %d; this bltzread knows %d"
                                         file version bltzread-state--version)))))

(defun bltzread-state--fill (state data)
  "Put the saved DATA into STATE."
  (dolist (entry (plist-get data :words))
    (puthash (car entry) (cdr entry) (bltzread-state-words state)))
  (setf (bltzread-state-queue state) (plist-get data :queue)
        (bltzread-state-exposed state) (plist-get data :exposed)
        (bltzread-state-history state) (plist-get data :history)))

(defun bltzread-state--load (lang)
  "Read the state of LANG from disk, or make an empty one."
  (let* ((state (bltzread-state--create :lang lang))
         (file (bltzread-state-file lang))
         (data (bltzread-state--read-file file)))
    (bltzread-state--check-version data file)
    (bltzread-state--fill state data)
    (bltzread-state-reload-lists state)
    state))

(defun bltzread-state-reload-lists (state)
  "Re-read the skip lists of STATE."
  (let ((lang (bltzread-state-lang state)))
    (setf (bltzread-state-known-forms state) (bltzread-lists-read (bltzread-lists-file lang 'exact-words))
          (bltzread-state-known-lemmas state) (bltzread-lists-read (bltzread-lists-file lang 'word-families)))))

(defun bltzread-state-get (lang)
  "The state of LANG, loading it on first use."
  (or (gethash lang bltzread-state--table)
      (puthash lang (bltzread-state--load lang) bltzread-state--table)))

(defun bltzread-state-save (state)
  "Write STATE to disk now."
  (when-let ((timer (bltzread-state-timer state)))
    (cancel-timer timer)
    (setf (bltzread-state-timer state) nil))
  (let ((file (bltzread-state-file (bltzread-state-lang state)))
        (words nil))
    (maphash (lambda (k v) (push (cons k v) words)) (bltzread-state-words state))
    (make-directory (file-name-directory file) t)
    (let ((tmp (concat file ".tmp")))
      (with-temp-file tmp
        (let ((print-length nil) (print-level nil))
          (prin1 (list :version bltzread-state--version
                       :words (sort words (lambda (a b) (string< (car a) (car b))))
                       :queue (bltzread-state-queue state)
                       :exposed (bltzread-state-exposed state)
                       :history (bltzread-state-history state))
                 (current-buffer))))
      (rename-file tmp file t))
    (setf (bltzread-state-dirty state) nil)))

(defun bltzread-state-touch (state)
  "Mark STATE changed and schedule a save after `bltzread-save-delay' idle seconds."
  (setf (bltzread-state-dirty state) t)
  (unless (bltzread-state-timer state)
    (setf (bltzread-state-timer state)
          (run-with-idle-timer bltzread-save-delay nil #'bltzread-state-save state))))

(defun bltzread-state-save-all ()
  "Write every changed state to disk."
  (maphash (lambda (_ s) (when (bltzread-state-dirty s) (bltzread-state-save s)))
           bltzread-state--table))

(defun bltzread-state-flush ()
  "Write every changed state and forget all loaded ones.
Needed before `bltzread-state-directory' changes."
  (bltzread-state-save-all)
  (clrhash bltzread-state--table))

(add-hook 'kill-emacs-hook #'bltzread-state-save-all)

;;; Keys and levels

(defun bltzread-state-key (form lemmas)
  "Key under which FORM, with basic words LEMMAS, is tracked."
  (if (and (eq bltzread-track-by 'lemma) lemmas)
      (downcase (car lemmas))
    (downcase form)))

(defun bltzread-state-record (state key)
  "Plist of KEY in STATE, or nil."
  (gethash key (bltzread-state-words state)))

(defun bltzread-state--put (state key prop value)
  "Set PROP of KEY in STATE to VALUE."
  (let ((rec (copy-sequence (bltzread-state-record state key))))
    (puthash key (plist-put rec prop value) (bltzread-state-words state))
    (bltzread-state-touch state)))

(defun bltzread-state-lemma-key (form lemmas)
  "Return the key of the whole-word level record of FORM, basic words LEMMAS."
  (concat "lemma:" (downcase (or (car lemmas) form))))

(defun bltzread-state-level (state key &optional form lemmas)
  "Level of KEY in STATE: 1 to 5, or `ignored'.
FORM and LEMMAS give the whole-word level and let the bltzr skip lists
make an unrecorded word known."
  (let ((rec (bltzread-state-record state key))
        (lrec (and form (eq bltzread-level-scope 'word)
                   (bltzread-state-record state (bltzread-state-lemma-key form lemmas)))))
    (cond ((plist-member rec :level) (plist-get rec :level))
          ((plist-member lrec :level) (plist-get lrec :level))
          ((and bltzread-skip-lists-count-as-known
                (or (and form (gethash (downcase form) (bltzread-state-known-forms state)))
                    (cl-some (lambda (l) (gethash (downcase l) (bltzread-state-known-lemmas state)))
                             lemmas)))
           bltzread-known-level)
          (t 1))))

(defun bltzread-state-reads (state key)
  "Return the clean-read count of KEY since it last changed level.
STATE is the language state."
  (or (plist-get (bltzread-state-record state key) :reads) 0))

(defun bltzread-state-note (state key)
  "Return the personal note of KEY.
STATE is the language state."
  (plist-get (bltzread-state-record state key) :note))

(defun bltzread-state-set-note (state key note)
  "Set the personal NOTE of KEY (an empty string clears it).
STATE is the language state."
  (bltzread-state--put state key :note (and (not (string-empty-p note)) note)))

;;; Undo snapshots

(defun bltzread-state-snapshot (state keys)
  "Capture the records of KEYS and the review queue of STATE."
  (list (mapcar (lambda (k) (cons k (copy-sequence (bltzread-state-record state k))))
                (delete-dups (copy-sequence keys)))
        (copy-sequence (bltzread-state-queue state))
        (copy-sequence (bltzread-state-exposed state))))

(defun bltzread-state-restore (state snapshot)
  "Put STATE back as captured in SNAPSHOT."
  (dolist (e (car snapshot))
    (if (cdr e)
        (puthash (car e) (cdr e) (bltzread-state-words state))
      (remhash (car e) (bltzread-state-words state))))
  (setf (bltzread-state-queue state) (cadr snapshot)
        (bltzread-state-exposed state) (nth 2 snapshot))
  (bltzread-state-touch state))

(defun bltzread-state-siblings (state lemma-key)
  "Keys of the form records in STATE that belong to the word LEMMA-KEY."
  (let ((basic (substring lemma-key (length "lemma:"))) keys)
    (maphash (lambda (k rec) (when (equal (plist-get rec :lemma) basic) (push k keys)))
             (bltzread-state-words state))
    keys))

(defun bltzread-state--drop-props (state key props)
  "Remove PROPS from the record of KEY in STATE."
  (when-let ((rec (bltzread-state-record state key)))
    (let (new)
      (cl-loop for (k v) on rec by #'cddr
               unless (memq k props) do (setq new (plist-put new k v)))
      (if new
          (puthash key new (bltzread-state-words state))
        (remhash key (bltzread-state-words state)))
      (bltzread-state-touch state))))

(defun bltzread-state--clear-forms (state keys lemma-key)
  "Forget the levels and read counts of the form KEYS and of siblings.
LEMMA-KEY identifies the siblings.
STATE is the language state."
  (dolist (k (delete-dups (append keys (bltzread-state-siblings state lemma-key))))
    (bltzread-state--drop-props state k '(:level :reads :before-ignore))))

(defun bltzread-state--unignore-key (state key)
  "Take KEY out of the ignored state, back to the level it had before.
STATE is the language state."
  (let* ((rec (bltzread-state-record state key))
         (before (plist-get rec :before-ignore))
         (new nil))
    (when (eq (plist-get rec :level) 'ignored)
      (cl-loop for (k v) on rec by #'cddr
               unless (memq k '(:level :before-ignore)) do (setq new (plist-put new k v)))
      (when before (setq new (plist-put new :level before)))
      (if new
          (puthash key new (bltzread-state-words state))
        (remhash key (bltzread-state-words state)))
      (bltzread-state-touch state))))

(defun bltzread-state-unignore (state key &optional form lemmas)
  "Stop ignoring KEY (and, when FORM is given, its whole word).
STATE is the language state."
  (bltzread-state--unignore-key state key)
  (when (and form (eq bltzread-level-scope 'word))
    (bltzread-state--unignore-key state (bltzread-state-lemma-key form lemmas))))

(defun bltzread-state-ignore (state key level &optional form lemmas siblings)
  "Ignore KEY, remembering LEVEL (an explicit level, or nil) to return to.
STATE is the language state.
With FORM and scope `word' the whole word is ignored; SIBLINGS is a list
of the keys of the forms to clear."
  (if (and form (eq bltzread-level-scope 'word))
      (let* ((lk (bltzread-state-lemma-key form lemmas))
             (before (and (integerp level) (plist-member (bltzread-state-record state lk) :level)
                          level)))
        (bltzread-state--clear-forms state (cons key siblings) lk)
        (bltzread-state--put state lk :before-ignore before)
        (bltzread-state--put state lk :level 'ignored))
    (let ((before (and (integerp level) (plist-member (bltzread-state-record state key) :level)
                       level)))
      (bltzread-state--put state key :before-ignore before)
      (bltzread-state--put state key :level 'ignored))))

;;; Changing levels

(defun bltzread-state--unexpose (state forms)
  "Forget that the queued FORMS arrived by exposure.
STATE is the language state."
  (setf (bltzread-state-exposed state)
        (cl-remove-if (lambda (f) (member f forms)) (bltzread-state-exposed state))))

(defun bltzread-state--enqueue (state form lemma &optional origin)
  "Queue FORM (basic word LEMMA) for review.
STATE is the language state.
ORIGIN `exposure' marks it as reached by reading; otherwise it was by hand,
which also overrides an earlier exposure mark."
  (if (eq origin 'exposure)
      (cl-pushnew form (bltzread-state-exposed state) :test #'string=)
    (bltzread-state--unexpose state (list form)))
  (unless (cl-find form (bltzread-state-queue state) :key #'car :test #'string=)
    (setf (bltzread-state-queue state)
          (append (bltzread-state-queue state) (list (cons form lemma))))))

(defun bltzread-state--set-form-level (state key level &optional form lemma origin)
  "Set the form KEY to LEVEL, resetting its read count.  Queue FORM at 5.
STATE is the language state."
  (bltzread-state--put state key :reads 0)
  (bltzread-state--put state key :level level)
  (when lemma (bltzread-state--put state key :lemma (downcase lemma)))
  (cond ((and (eql level bltzread-known-level) form)
         (bltzread-state--enqueue state (downcase form) (and lemma (downcase lemma)) origin)
         t)
        (t
         (when form
           (bltzread-state--unexpose state (list (downcase form)))
           (setf (bltzread-state-queue state)
                 (cl-remove (downcase form) (bltzread-state-queue state)
                            :key #'car :test #'string=)))
         nil)))

(defun bltzread-state-set-level (state key level &optional form lemma siblings)
  "Set KEY to LEVEL (1 to 5) by hand, resetting its read count.
STATE is the language state.
With scope `word' the level belongs to the whole word of FORM and LEMMA,
and SIBLINGS, a list of (KEY . FORM) for its forms in view, lose their own
levels.  Reaching 5 queues FORM, and the siblings, for review.  Return
non-nil when a word was queued."
  (if (and form (eq bltzread-level-scope 'word))
      (let ((lk (bltzread-state-lemma-key form (and lemma (list lemma))))
            (siblings (or siblings (list (cons key form)))))
        (bltzread-state--clear-forms state (mapcar #'car siblings) lk)
        (bltzread-state--put state lk :level level)
        (dolist (s siblings)
          (bltzread-state--put state (car s) :lemma (downcase (or lemma form))))
        (cond ((eql level bltzread-known-level)
               (dolist (s siblings)
                 (bltzread-state--enqueue state (downcase (cdr s)) (and lemma (downcase lemma))))
               t)
              (t (bltzread-state--unexpose state (mapcar (lambda (s) (downcase (cdr s))) siblings))
                 (setf (bltzread-state-queue state)
                       (cl-remove-if (lambda (e) (assoc (car e) siblings
                                                        (lambda (a b) (string= a (downcase b)))))
                                     (bltzread-state-queue state)))
                 nil)))
    (bltzread-state--set-form-level state key level form lemma)))

(defun bltzread-state-record-read (state key level form lemma)
  "Count one clean read of KEY, currently at LEVEL.
STATE is the language state.
Return `promoted', `queued' (promoted to 5 and queued) or nil."
  (when (bltzread-learning-level-p level)
    (let* ((reads (1+ (bltzread-state-reads state key)))
           (need (or (nth (1- level) bltzread-promotion-thresholds) most-positive-fixnum)))
      (if (>= reads need)
          (if (bltzread-state--set-form-level state key (1+ level) form lemma 'exposure) 'queued 'promoted)
        (bltzread-state--put state key :reads reads)
        (when lemma (bltzread-state--put state key :lemma (downcase lemma)))
        nil))))

(defun bltzread-state-record-lookup (state key level)
  "Apply `bltzread-lookup-effect' to KEY, currently at LEVEL.
STATE is the language state."
  (pcase bltzread-lookup-effect
    ('reset-reads (bltzread-state--put state key :reads 0))
    ('demote (bltzread-state--put state key :reads 0)
             (when (and (integerp level) (> level 1))
               (bltzread-state--put state key :level (1- level))))))

;;; History

(defun bltzread-state-add-history (state entry)
  "Remember the reading session ENTRY (a plist), newest first.
STATE is the language state."
  (setf (bltzread-state-history state)
        (seq-take (cons entry (bltzread-state-history state)) bltzread-history-limit))
  (bltzread-state-touch state))

;;; Review queue

(defun bltzread-state-queue-remove (state forms)
  "Drop the queued entries whose form is in FORMS.
STATE is the language state."
  (bltzread-state--unexpose state forms)
  (setf (bltzread-state-queue state)
        (cl-remove-if (lambda (e) (member (car e) forms)) (bltzread-state-queue state)))
  (bltzread-state-touch state))

(defun bltzread-state-write-known (state entries &optional target)
  "Append ENTRIES, a list of (FORM . LEMMA), to a skip list of STATE's language.
TARGET defaults per `bltzread-list-target'.  The work is done by
`bltzread-known-writer-function'.  Return (FILE . ADDED)."
  (let ((result (funcall bltzread-known-writer-function
                         (bltzread-state-lang state) entries target)))
    (bltzread-state-reload-lists state)
    result))

(provide 'bltzread-state)
;;; bltzread-state.el ends here
