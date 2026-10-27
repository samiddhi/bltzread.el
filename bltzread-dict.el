;;; bltzread-dict.el --- Dictionary glosses for bltzread -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; `bltzread-dictionary-function' is the one seam: give it a language and words,
;; get text back.  An entry is, by convention, the dictionary's name, a
;; newline and then the text (see `bltzread-dictionary-function').  The default shells out to sdcv, which reads StarDict
;; dictionaries such as the DSSJ export.  StarDict entries are often HTML;
;; they are turned into plain text here.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'shr)
(require 'subr-x)
(require 'bltzread-custom)
(require 'bltzread-dict-filters)

(defvar bltzread-dict--cache (make-hash-table :test 'equal)
  "(LANG . WORDS) -> dictionary text, or `none'.")

(defun bltzread-dict-html-to-text (html)
  "HTML as readable plain text."
  (if (and (fboundp 'libxml-available-p) (libxml-available-p))
      (with-temp-buffer
        (let ((dom (with-temp-buffer
                     (insert html)
                     (libxml-parse-html-region (point-min) (point-max))))
              (shr-use-fonts nil) (shr-use-colors nil) (shr-inhibit-images t)
              (shr-width 10000))
          (shr-insert-document dom))
        (string-trim (buffer-substring-no-properties (point-min) (point-max))))
    (string-trim (replace-regexp-in-string "<[^>]*>" " " html))))

(defun bltzread-dict--clean (text)
  "TEXT as plain text with tidy blank lines."
  (let ((text (if (string-match-p "<[a-zA-Z/][^>]*>" text)
                  (bltzread-dict-html-to-text text)
                text)))
    (string-trim (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text))))

(defvar bltzread-dict--book-cache (make-hash-table :test 'equal)
  "Data dir -> list of (BOOKNAME . FOLDER-NAME) that sdcv knows.")

(defvar bltzread-dict--warned nil "Names already complained about this session.")

(defmacro bltzread-sdcv--in-language (lang &rest body)
  "Run BODY with `bltzread-sdcv-data-dir' as configured for LANG."
  (declare (indent 1))
  `(let ((bltzread-sdcv-data-dir (or (cdr (assoc ,lang bltzread-sdcv-data-dir-alist))
                               bltzread-sdcv-data-dir)))
     ,@body))

(defun bltzread-sdcv--data-args ()
  "Return the sdcv arguments choosing the data directory."
  (and bltzread-sdcv-data-dir (list "-x" "-2" (expand-file-name bltzread-sdcv-data-dir))))

(defun bltzread-sdcv--ifo-folders ()
  "Alist of (BOOKNAME . FOLDER-NAME) read from the .ifo files in the data dir."
  (when (and bltzread-sdcv-data-dir (file-directory-p bltzread-sdcv-data-dir))
    (delq nil
          (mapcar
           (lambda (ifo)
             (with-temp-buffer
               (insert-file-contents ifo nil 0 2000)
               (when (re-search-forward "^bookname=\\(.*\\)$" nil t)
                 (cons (string-trim (match-string 1))
                       (file-name-nondirectory
                        (directory-file-name (file-name-directory ifo)))))))
           (directory-files-recursively (expand-file-name bltzread-sdcv-data-dir)
                                        "\\.ifo\\'" nil)))))

(defun bltzread-sdcv-books ()
  "List of (BOOKNAME . FOLDER-NAME) for the dictionaries sdcv can use."
  (let ((dir (or bltzread-sdcv-data-dir "")))
    (or (gethash dir bltzread-dict--book-cache)
        (let* ((folders (bltzread-sdcv--ifo-folders))
               (names (with-temp-buffer
                        (when (and (executable-find bltzread-sdcv-executable)
                                   (eql 0 (apply #'call-process bltzread-sdcv-executable nil
                                                 '(t nil) nil
                                                 (append '("-l" "-n") bltzread-sdcv-extra-args
                                                         (bltzread-sdcv--data-args)))))
                          (goto-char (point-min))
                          (forward-line 1)
                          (let (acc)
                            (while (re-search-forward "^\\(.*[^ ]\\) +[0-9]+$" nil t)
                              (push (match-string 1) acc))
                            (nreverse acc)))))
               (books (mapcar (lambda (n) (cons n (cdr (assoc n folders)))) names)))
          (when books (puthash dir books bltzread-dict--book-cache))
          books))))

(defun bltzread-sdcv-resolve-book (name)
  "The sdcv bookname that NAME stands for, or nil.
NAME may be a bookname, a folder name, or an unambiguous part of a
bookname, ignoring case."
  (let ((books (bltzread-sdcv-books))
        (down (downcase name)))
    (or (car (assoc name books))
        (car (seq-find (lambda (b) (equal (downcase (car b)) down)) books))
        (car (seq-find (lambda (b) (and (cdr b) (equal (downcase (cdr b)) down))) books))
        (let ((hits (seq-filter (lambda (b) (string-match-p (regexp-quote down)
                                                           (downcase (car b))))
                                books)))
          (and hits (null (cdr hits)) (car (car hits)))))))

(defun bltzread-sdcv--books-for (lang)
  "Booknames to search for LANG: a list (possibly empty), or `all'."
  (let ((entry (assoc lang bltzread-sdcv-dictionaries)))
    (cond
     (entry
      (delq nil
            (mapcar (lambda (n)
                      (or (bltzread-sdcv-resolve-book n)
                          (unless (member (cons lang n) bltzread-dict--warned)
                            (push (cons lang n) bltzread-dict--warned)
                            (message "bltzread: no dictionary matches \"%s\" (language %s); see M-x bltzread-dict-diagnose"
                                     n lang)
                            nil)))
                    (cdr entry))))
     ((eq bltzread-sdcv-unlisted-languages 'all) 'all)
     (t nil))))

(defun bltzread-sdcv--lookup-one (lang word &optional books depth)
  "Return a list of dictionary entries (strings) for exactly WORD, or nil.
LANG is the language.
BOOKS, from `bltzread-sdcv--books-for', is looked up when not given.
DEPTH counts the links followed to get here (see `bltzread-dict-follow-depth')."
  (let* ((books (or books (bltzread-sdcv--books-for lang)))
         (args (append '("-n" "-j" "-e" "-0" "-1")
                       bltzread-sdcv-extra-args
                       (bltzread-sdcv--data-args)
                       (and (listp books)
                            (mapcan (lambda (b) (list "-u" b)) books))
                       (list word))))
    (when (and books (executable-find bltzread-sdcv-executable))
      (with-temp-buffer
        (when (eql 0 (apply #'call-process bltzread-sdcv-executable nil '(t nil) nil args))
          (goto-char (point-min))
          (when (re-search-forward "^\\[" nil t)
            (goto-char (match-beginning 0))
            (let ((entries (ignore-errors
                             (json-parse-string
                              (buffer-substring (point) (point-max))
                              :object-type 'alist :array-type 'list))))
              (when entries
                (mapcar
                 (lambda (e)
                   (let ((raw (or (alist-get 'definition e) ""))
                         (dict (alist-get 'dict e)))
                     (bltzread-dict-shorten
                      dict lang raw
                      (format "%s\n%s" dict (bltzread-dict--clean raw))
                      (and (< (or depth 0) bltzread-dict-follow-depth)
                           (lambda (target)
                             (bltzread-sdcv--follow lang target books dict
                                                    (1+ (or depth 0))))))))
                 entries)))))))))

(defun bltzread-sdcv--follow (lang word books dict depth)
  "Entries of DICT alone for WORD, reached by following a link to DEPTH."
  (let ((prefix (concat dict "\n")))
    (seq-filter (lambda (e) (string-prefix-p prefix e))
                (bltzread-sdcv--lookup-one lang word books depth))))

(defun bltzread-sdcv-lookup (lang words)
  "Entries (a list of strings) for all of WORDS in sdcv for LANG."
  (bltzread-sdcv--in-language lang
    (let ((books (bltzread-sdcv--books-for lang)))
      (delete-dups (apply #'append
                          (mapcar (lambda (w) (bltzread-sdcv--lookup-one lang w books))
                                  words))))))

(defun bltzread-dict--failed-p (entry)
  "Non-nil when ENTRY has a filter that could not parse it."
  (and (stringp entry) (> (length entry) 0)
       (get-text-property 0 'bltzread-parse-failed entry)))

(defun bltzread-dict--order (entries)
  "ENTRIES with those a filter could not parse last, if the options say so."
  (if (and bltzread-dict-failed-entries-last (cdr entries))
      (append (seq-remove #'bltzread-dict--failed-p entries)
              (seq-filter #'bltzread-dict--failed-p entries))
    entries))

(defun bltzread-dict--fetch (lang words)
  "Entries for WORDS in LANG from `bltzread-dictionary-function', as a list.
An error is reported with `message' and gives `error', which is not cached."
  (condition-case err
      (let ((res (funcall bltzread-dictionary-function lang words)))
        (bltzread-dict--order (if (stringp res) (list res) res)))
    (error (message "bltzread: dictionary failed: %s" (error-message-string err))
           'error)))

(defun bltzread-dict-lookup (lang words)
  "List of entries for WORDS in LANG, via `bltzread-dictionary-function'.
The function may return one string or a list of strings.  Results are
cached, except after an error.  Return nil when nothing is found or no
dictionary is available."
  (let* ((key (cons lang words))
         (hit (gethash key bltzread-dict--cache)))
    (if hit
        (and (listp hit) hit)
      (let ((text (bltzread-dict--fetch lang words)))
        (unless (eq text 'error)
          (puthash key (or text 'none) bltzread-dict--cache))
        (and (listp text) text)))))

(defun bltzread-dict-cached-p (lang words)
  "Non-nil when the entries for WORDS in LANG are already cached."
  (and (gethash (cons lang words) bltzread-dict--cache) t))

;;;###autoload
(defun bltzread-dict-clear-cache ()
  "Forget cached dictionary results."
  (interactive)
  (clrhash bltzread-dict--cache)
  (clrhash bltzread-dict--book-cache)
  (setq bltzread-dict--warned nil))

;;;###autoload
(defun bltzread-dict-diagnose (&optional word)
  "Show how dictionaries are set up, and try looking up WORD.
Use this when a language shows no entries or entries from the wrong book."
  (interactive (list (read-string "Test word (empty to skip): ")))
  (bltzread-dict-clear-cache)
  (with-current-buffer (get-buffer-create "*bltzread-dict-diagnose*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "sdcv: %s\ndata dir: %s\nunlisted languages: %s\n\n"
                      (or (executable-find bltzread-sdcv-executable) "NOT FOUND")
                      (or bltzread-sdcv-data-dir "(sdcv default)") bltzread-sdcv-unlisted-languages))
      (let ((books (bltzread-sdcv-books)))
        (insert (format "Dictionaries sdcv can use (%d):\n" (length books)))
        (dolist (b books)
          (insert (format "  %s%s\n" (car b) (if (cdr b) (format "   [folder %s]" (cdr b)) ""))))
        (dolist (d bltzread-sdcv-data-dir-alist)
          (insert (format "  (language %s uses its own data dir %s)\n" (car d) (cdr d))))
        (let (bad)
          (dolist (dir (delete-dups
                        (mapcar #'expand-file-name
                                (delq nil (cons bltzread-sdcv-data-dir
                                                (mapcar #'cdr bltzread-sdcv-data-dir-alist))))))
            (dolist (ifo (and (file-directory-p dir)
                              (directory-files-recursively dir "\\.ifo\\'")))
              (let ((base (file-name-sans-extension ifo)))
                (unless (or (file-exists-p (concat base ".dict.dz"))
                            (file-exists-p (concat base ".dict")))
                  (push base bad)))))
          (when bad
            (insert "\nBROKEN dictionaries (the .dict or .dict.dz data file is missing or a\nbroken symlink, so every lookup returns nothing):\n")
            (dolist (b bad) (insert (format "  %s\n" b)))))
        (insert "\nPer language:\n")
        (if (null bltzread-sdcv-dictionaries)
            (insert "  bltzread-sdcv-dictionaries is EMPTY: check init.el (use-package :custom needs\n  (bltzread-sdcv-dictionaries '(...)), not a setq).\n")
          (dolist (entry bltzread-sdcv-dictionaries)
            (insert (format "  %s\n" (car entry)))
            (dolist (n (cdr entry))
              (let ((r (bltzread-sdcv--in-language (car entry) (bltzread-sdcv-resolve-book n))))
                (insert (format "    %-32s -> %s\n" n (or r "NO MATCH")))))))
        (when (and word (not (string-empty-p word)))
          (insert (format "\nTest lookup of \"%s\":\n" word))
          (dolist (entry bltzread-sdcv-dictionaries)
            (let ((res (bltzread-sdcv-lookup (car entry) (list word))))
              (insert (format "  %s: %d entr%s%s\n" (car entry) (length res)
                              (if (= (length res) 1) "y" "ies")
                              (if res (format " (%s)"
                                              (mapconcat (lambda (e) (car (split-string e "\n")))
                                                         res ", "))
                                ""))))))))
    (special-mode)
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(provide 'bltzread-dict)
;;; bltzread-dict.el ends here
