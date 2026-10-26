;;; bltzread-dict.el --- Dictionary glosses for bltzread -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;;; Commentary:

;; `bltzread-dictionary-function' is the one seam: give it a language and words,
;; get text back.  The default shells out to sdcv, which reads StarDict
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
  "sdcv arguments choosing the data directory."
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
                                                 (append '("-l" "-n") (bltzread-sdcv--data-args)))))
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

(defun bltzread-sdcv--lookup-one (lang word &optional books)
  "List of dictionary entries (strings) for exactly WORD, or nil.
BOOKS, from `bltzread-sdcv--books-for', is looked up when not given."
  (let* ((books (or books (bltzread-sdcv--books-for lang)))
         (args (append '("-n" "-j" "-e" "-0" "-1")
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
                   (let ((raw (or (alist-get 'definition e) "")))
                     (bltzread-dict-shorten
                      (alist-get 'dict e) lang raw
                      (format "%s\n%s" (alist-get 'dict e) (bltzread-dict--clean raw)))))
                 entries)))))))))

(defun bltzread-sdcv-lookup (lang words)
  "Entries (a list of strings) for all of WORDS in sdcv for LANG."
  (bltzread-sdcv--in-language lang
    (let ((books (bltzread-sdcv--books-for lang)))
      (delete-dups (apply #'append
                          (mapcar (lambda (w) (bltzread-sdcv--lookup-one lang w books))
                                  words))))))

(defun bltzread-dict-lookup (lang words)
  "List of entries for WORDS in LANG, via `bltzread-dictionary-function'.
The function may return one string or a list of strings.  Results are
cached.  Return nil when nothing is found or no dictionary is available."
  (let* ((key (cons lang words))
         (hit (gethash key bltzread-dict--cache)))
    (cond ((eq hit 'none) nil)
          (hit hit)
          (t (let* ((res (condition-case nil
                             (funcall bltzread-dictionary-function lang words)
                           (error nil)))
                    (text (if (stringp res) (list res) res)))
               (puthash key (or text 'none) bltzread-dict--cache)
               text)))))

(defun bltzread-dict-cached-p (lang words)
  "Non-nil when the entries for WORDS in LANG are already cached."
  (and (gethash (cons lang words) bltzread-dict--cache) t))

(defun bltzread-dict-clear-cache ()
  "Forget cached dictionary results."
  (interactive)
  (clrhash bltzread-dict--cache)
  (clrhash bltzread-dict--book-cache)
  (setq bltzread-dict--warned nil))

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
