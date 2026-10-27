;;; bltzread-bltzr.el --- Words and basic words, via bltzr -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; bltzr reports no buffer positions, so Emacs finds the words itself with
;; `bltzread-word-regexp'.  bltzr is then asked, once per set of new spellings,
;; which basic words (lemmas) they belong to.  Each spelling is sent on a
;; line of its own; the example sentence bltzr returns for a lemma then
;; names exactly the spelling that led to it.
;;
;; bltzr keeps at most 20 examples per entry, so spellings of a lemma with
;; more than 20 forms need further passes; we repeat on whatever was not
;; reported until a pass reports nothing new.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'bltzread-custom)

(defconst bltzread-bltzr--max-passes 12)

(defvar bltzread-bltzr--cache (make-hash-table :test 'equal)
  "Language code -> hash table of spelling -> list of basic words.")

(defun bltzread-bltzr-regexp (lang)
  "Word regexp for LANG."
  (or (cdr (assoc lang bltzread-word-regexp-alist)) bltzread-word-regexp))

(defun bltzread-bltzr-tokenize (beg end lang)
  "Return a list of (BEG END FORM) for each word in BEG..END in language LANG.
FORM is lowercased.  Words are found with the regexp of
`bltzread-bltzr-regexp'.  With `bltzread-skip-numerals', matches containing
no letter (2, 5.26, 1990-1995) are not words.  This is the default
`bltzread-tokenizer-function'."
  (let ((regexp (bltzread-bltzr-regexp lang))
        tokens)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp end t)
        (unless (and bltzread-skip-numerals
                     (not (string-match-p "[[:alpha:]]" (match-string-no-properties 0))))
          (push (list (match-beginning 0) (match-end 0)
                      (downcase (match-string-no-properties 0)))
                tokens))))
    (nreverse tokens)))

;;; Running bltzr

(defun bltzread-bltzr--require-executable ()
  "Signal a `user-error' unless the bltzr executable can be found."
  (unless (executable-find bltzread-bltzr-executable)
    (user-error "Cannot find `%s'; install bltzr or set `bltzread-bltzr-executable'"
                bltzread-bltzr-executable)))

(defun bltzread-bltzr--run (lang input)
  "Run `bltzr blitz' for LANG on INPUT and return its parsed JSON, or signal."
  (bltzread-bltzr--require-executable)
  (with-temp-buffer
    (let* ((out-buffer (current-buffer))
           (err-file (make-temp-file "bltzread-bltzr"))
           (status (with-temp-buffer
                     (insert input)
                     (apply #'call-process-region (point-min) (point-max)
                            bltzread-bltzr-executable nil (list out-buffer err-file) nil
                            (append (list "blitz" "-l" lang "-L" "-N" "-c" "-m" "20"
                                          "-b" "off" "-o" "json")
                                    bltzread-bltzr-extra-args))))
           (out (buffer-string)))
      (prog1
          (if (eql status 0)
              (json-parse-string out :object-type 'alist :array-type 'list)
            (user-error "Bltzr failed (%s): %s" status
                        (string-trim (with-temp-buffer
                                       (insert-file-contents err-file)
                                       (buffer-string)))))
        (delete-file err-file)))))

(defun bltzread-bltzr--pass (lang forms table)
  "Ask bltzr in LANG about FORMS and record what it reports in TABLE.
Return the count."
  (let ((data (bltzread-bltzr--run lang (mapconcat (lambda (f) (concat f ".")) forms "\n")))
        (found 0))
    (dolist (entry data)
      (let ((lemma (alist-get 'term entry)))
        (dolist (ctx (alist-get 'contexts entry))
          (let* ((text (alist-get 'text ctx))
                 (form (substring text (alist-get 'highlight_start ctx)
                                  (alist-get 'highlight_end ctx))))
            (unless (member lemma (gethash form table))
              (puthash form (append (gethash form table) (list lemma)) table)
              (setq found (1+ found)))))))
    found))

(defun bltzread-bltzr-lemmas (lang forms)
  "Return a hash table of spelling -> basic words for FORMS in LANG.
FORMS is a list of lowercase strings.
Spellings bltzr does not report map to themselves."
  (let* ((cache (or (gethash lang bltzread-bltzr--cache)
                    (puthash lang (make-hash-table :test 'equal) bltzread-bltzr--cache)))
         (todo (seq-remove (lambda (f) (gethash f cache)) (seq-uniq forms))))
    (when todo
      (let ((fresh (make-hash-table :test 'equal))
            (pending todo)
            (passes 0))
        (while (and pending (< passes bltzread-bltzr--max-passes))
          (setq passes (1+ passes))
          (let ((before (hash-table-count fresh)))
            (bltzread-bltzr--pass lang pending fresh)
            (setq pending (seq-remove (lambda (f) (gethash f fresh)) pending))
            (when (= before (hash-table-count fresh))
              (setq pending nil))))
        (dolist (f todo)
          (puthash f (or (gethash f fresh) (list f)) cache))))
    cache))

(defun bltzread-bltzr-languages ()
  "Language codes known to bltzr, or nil when bltzr is missing."
  (with-temp-buffer
    (when (and (executable-find bltzread-bltzr-executable)
               (eql 0 (apply #'call-process bltzread-bltzr-executable nil t nil
                        (append '("list-languages") bltzread-bltzr-extra-args))))
      (goto-char (point-min))
      (let (codes)
        (while (re-search-forward "(\\([a-z]\\{3,4\\}\\))" nil t)
          (push (match-string 1) codes))
        (nreverse codes)))))

(defun bltzread-bltzr-detect-language (text)
  "Return the language code for TEXT as guessed by bltzr.
Return nil when it says unknown.
Also nil when bltzr cannot be run or does not know `detect-language'."
  (with-temp-buffer
    (let ((out-buffer (current-buffer)))
      (with-temp-buffer
        (insert text)
        (when (and (executable-find bltzread-bltzr-executable)
                   (eql 0 (apply #'call-process-region (point-min) (point-max)
                            bltzread-bltzr-executable nil (list out-buffer nil) nil
                            (append '("detect-language" "--format" "json")
                                    bltzread-bltzr-extra-args))))
          (let ((lang (ignore-errors
                        (alist-get 'language
                                   (progn (goto-char (point-min))
                                          (with-current-buffer out-buffer
                                            (goto-char (point-min))
                                            (json-parse-buffer :object-type 'alist
                                                               :array-type 'list)))))))
            (and (stringp lang) (not (equal lang "unknown")) lang)))))))

(provide 'bltzread-bltzr)
;;; bltzread-bltzr.el ends here
