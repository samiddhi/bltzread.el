;;; bltzread-lists.el --- Read and extend bltzr skip lists -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; bltzr skip lists are UTF-8 files with one word per line; blank lines
;; and whole-line # comments are allowed.  We only ever read them and
;; append to them, backing the old file up as PATH.bak first.

;;; Code:

(require 'subr-x)
(require 'bltzread-custom)

;;; Finding the files

(defun bltzread-lists--config-file ()
  "The bltzr configuration file to consult, or nil."
  (let ((file (or bltzread-bltzr-config-file
                  (getenv "BLITZER_CONFIG")
                  (expand-file-name "~/.config/bltzr/bltzr.toml"))))
    (and file (file-readable-p file) file)))

(defun bltzread-lists--toml-value (lang key)
  "Value of string KEY in the [languages.LANG] table of the bltzr config."
  (when-let ((file (bltzread-lists--config-file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward
             (format "^\\[languages\\.%s\\][ \t]*$" (regexp-quote lang)) nil t)
        (let ((end (save-excursion
                     (if (re-search-forward "^\\[" nil t) (match-beginning 0) (point-max)))))
          (when (re-search-forward
                 (format "^[ \t]*%s[ \t]*=[ \t]*\"\\([^\"]*\\)\"" (regexp-quote key))
                 end t)
            (expand-file-name (match-string 1) (file-name-directory file))))))))

(defun bltzread-lists-file (lang kind)
  "Skip-list file for LANG; KIND is `exact-words' or `word-families'."
  (pcase kind
    ('exact-words
     (or (cdr (assoc lang bltzread-skip-exact-words-files))
         (bltzread-lists--toml-value lang "skip_exact_words_file")))
    ('word-families
     (or (cdr (assoc lang bltzread-skip-word-families-files))
         (bltzread-lists--toml-value lang "skip_word_families_file")))))

(defun bltzread-lists-target (lang)
  "Which kind of list receives known words for LANG."
  (or (cdr (assoc lang bltzread-list-target-alist)) bltzread-list-target))

;;; Reading

(defun bltzread-lists-read (file)
  "Hash table (test `equal') of the normalized words in FILE."
  (let ((table (make-hash-table :test 'equal)))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (line (split-string (buffer-string) "\n"))
          (let ((word (string-trim line)))
            (unless (or (string-empty-p word) (string-prefix-p "#" word))
              (puthash (downcase word) t table))))))
    table))

;;; Writing

(defun bltzread-lists-add (file words)
  "Append WORDS not already in FILE.  Return the words actually added.
An existing file is first copied to FILE.bak."
  (let* ((existing (bltzread-lists-read file))
         (new (seq-uniq (seq-remove (lambda (w) (gethash w existing))
                                    (mapcar #'downcase words)))))
    (when new
      (make-directory (file-name-directory file) t)
      (when (file-exists-p file)
        (copy-file file (concat file ".bak") t))
      (with-temp-buffer
        (when (file-exists-p file)
          (insert-file-contents file)
          (goto-char (point-max))
          (unless (or (bobp) (eq (char-before) ?\n)) (insert "\n")))
        (dolist (w new) (insert w "\n"))
        (write-region (point-min) (point-max) file nil 'silent)))
    new))

(defun bltzread-lists--ask-target ()
  "Ask which kind of skip list to add to; return its symbol."
  (intern (completing-read "Add to which list? "
                           '("exact-words" "word-families") nil t)))

(defun bltzread-lists--entry-word (entry target)
  "The word of ENTRY, a (FORM . LEMMA), to write for TARGET."
  (if (eq target 'word-families)
      (or (cdr entry) (car entry))
    (car entry)))

;;;###autoload
(defun bltzread-lists-write-known (lang entries &optional target)
  "Append ENTRIES, a list of (FORM . LEMMA), to a skip list of LANG.
TARGET is `exact-words', `word-families' or `ask'; nil means the configured
one (`bltzread-list-target').  Return (FILE . ADDED).  This is the default
`bltzread-known-writer-function'."
  (let* ((target (or target (bltzread-lists-target lang)))
         (target (if (eq target 'ask) (bltzread-lists--ask-target) target))
         (file (or (bltzread-lists-file lang target)
                   (read-file-name (format "File for the %s list: " target))))
         (words (delq nil (mapcar (lambda (e) (bltzread-lists--entry-word e target))
                                  entries))))
    (cons file (bltzread-lists-add file words))))

(provide 'bltzread-lists)
;;; bltzread-lists.el ends here
