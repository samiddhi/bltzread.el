;;; bltzread-dict-filters.el --- Short dictionary entries for bltzread -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;;; Commentary:

;; The popup shown on hover wants only the relevant part of an entry (say
;; the English translations); the full entry (the lookup key) shows
;; everything.  `bltzread-dict-filters' says, per dictionary, how to pick that
;; part out of the entry's raw HTML.  Nothing here knows any dictionary: the
;; module ships no filters, and the picking is plain regexp search on the HTML
;; string, which costs microseconds (no DOM is built).

;;; Code:

(require 'subr-x)
(require 'xml)
(require 'bltzread-custom)

(defcustom bltzread-dict-filters
  nil
  "Short forms of dictionary entries, shown in the hover popup.
An alist of (KEY . SPEC).  KEY is a dictionary's bookname or a language
code such as \"pli\" (case is ignored; the bookname wins).  The lookup key
always shows the whole entry; so does any dictionary without a SPEC, and any
SPEC that finds nothing.

SPEC is one of:
 - a regexp string.  Every match in the entry's raw HTML contributes the
   text of its first group that matched (or of the whole match when it has
   no group).
 - a plist with any of
     :regexp RE   as above.
     :class C     a class name, or a list of them: contributes the text
                  right after each element having that class, up to the next
                  tag.  Combined with :regexp, both are searched.
     :split RE    cut each piece at RE (say \" · \" or \"; \") into
                  separate items.
     :join SEP    string put between the items (default a newline, so each
                  shows on its own line).
     :max N       keep at most the first N items.
 - a function taking the raw HTML and returning a string (or nil), for
   anything the above cannot say.

Pieces are stripped of tags, have entities decoded and whitespace
collapsed; empty and repeated ones are dropped.  Pick only what you want
shown: the headword is already in the popup title.  Example:
  (setq bltzread-dict-filters
        \='((\"slv\" :class \"gloss\" :split \" · \")
          (\"pli\" . \"<div class=dpd><p>[^<]*<b>\\\\([^<]*\\\\)\")))"
  :type '(alist :key-type string
                :value-type (choice regexp function (plist :value-type sexp)))
  :group 'bltzread)

(defun bltzread-dict-filter-for (dict lang)
  "The filter spec for dictionary DICT (a bookname) or language LANG, or nil."
  (cdr (or (assoc-string dict bltzread-dict-filters t)
           (assoc-string lang bltzread-dict-filters t))))

(defun bltzread-dict--class-regexp (classes)
  "Regexp whose group 1 is the text after an element having one of CLASSES."
  (concat "class=[\"']?\\(?:[^\"'>]* \\)?\\(?:"
          (mapconcat #'regexp-quote (ensure-list classes) "\\|")
          "\\)\\(?:[\"' ][^>]*\\)?>\\([^<]*\\)"))

(defun bltzread-dict--tidy (s)
  "S without tags and entities, whitespace collapsed."
  (string-trim
   (replace-regexp-in-string
    "[ \t\n\r\u00a0]+" " "
    (xml-substitute-special (replace-regexp-in-string "<[^>]*>" "" s)))))

(defun bltzread-dict--pieces (html regexps max split)
  "Tidy texts matched by REGEXPS in HTML, in document order, at most MAX."
  (let (hits)
    (dolist (re regexps)
      (let ((pos 0))
        (while (and (string-match re html pos) (/= (match-end 0) pos))
          (let ((n 1) g)
            (while (and (not g) (<= n 9))
              (setq g (match-string n html) n (1+ n)))
            (push (cons (match-beginning 0) (or g (match-string 0 html))) hits))
          (setq pos (match-end 0)))))
    (let (out)
      (dolist (h (sort hits (lambda (a b) (< (car a) (car b)))))
        (dolist (s (let ((s (bltzread-dict--tidy (cdr h))))
                     (if split (mapcar #'string-trim (split-string s split t)) (list s))))
          (unless (or (string-empty-p s) (member s out)) (push s out))))
      (setq out (nreverse out))
      (if (and max (> (length out) max)) (seq-take out max) out))))

(defun bltzread-dict-apply-filter (spec html)
  "Short text from HTML according to SPEC (see `bltzread-dict-filters'), or nil."
  (cond
   ((functionp spec) (funcall spec html))
   ((stringp spec) (bltzread-dict-apply-filter (list :regexp spec) html))
   ((consp spec)
    (let* ((re (plist-get spec :regexp))
           (cl (plist-get spec :class))
           (pieces (bltzread-dict--pieces
                    html
                    (append (and re (list re))
                            (and cl (list (bltzread-dict--class-regexp cl))))
                    (plist-get spec :max) (plist-get spec :split))))
      (and pieces (string-join pieces (or (plist-get spec :join) "\n")))))))

(defun bltzread-dict-shorten (dict lang html full)
  "Entry string FULL for DICT with a short form attached, if a filter makes one.
The short form (only the filtered text, without the dictionary name) is the
text property `bltzread-short' on FULL; HTML is the raw entry."
  (let* ((spec (bltzread-dict-filter-for dict lang))
         (short (and spec (condition-case nil (bltzread-dict-apply-filter spec html)
                            (error nil)))))
    (if (and short (not (string-empty-p (string-trim short))))
        (propertize full 'bltzread-short (string-trim short))
      full)))

(defun bltzread-dict-display (entry full)
  "ENTRY as shown in the popup: whole when FULL, else its short form if any."
  (or (and (not full) entry (get-text-property 0 'bltzread-short entry))
      entry))

(provide 'bltzread-dict-filters)
;;; bltzread-dict-filters.el ends here
