;;; bltzread-dict-filters.el --- Short dictionary entries for bltzread -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

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
(require 'url-util)
(require 'bltzread-custom)

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

(defconst bltzread-dict-follow-default-regexp
  "href=[\"']bword://\\([^\"']+\\)[\"']"
  "Regexp for the links of StarDict entries; group 1 is the word linked to.")

(defun bltzread-dict--link-word (raw)
  "The word of a link target RAW: entities and %-escapes decoded."
  (string-trim
   (decode-coding-string (url-unhex-string (xml-substitute-special raw)) 'utf-8)))

(defun bltzread-dict-follow-targets (spec html)
  "Words that the entry HTML refers to, per the :follow of SPEC; a list.
:follow is t (the \"bword://\" links of StarDict), a regexp whose group 1 is
the word, or a function taking the HTML and returning a list of words."
  (when (and (consp spec) (not (functionp spec)))
    (let ((follow (plist-get spec :follow)))
      (delete-dups
       (cond
        ((null follow) nil)
        ((functionp follow) (funcall follow html))
        (t (let ((re (if (stringp follow) follow bltzread-dict-follow-default-regexp))
                 (pos 0) words)
             (while (and (string-match re html pos) (/= (match-end 0) pos))
               (setq pos (match-end 0))
               (when-let ((w (match-string 1 html)))
                 (push (bltzread-dict--link-word w) words)))
             (seq-remove #'string-empty-p (nreverse words)))))))))

(defun bltzread-dict--entry-body (entry)
  "ENTRY, a dictionary name, a newline and the text, without the name."
  (if-let ((newline (string-search "\n" entry)))
      (substring entry (1+ newline))
    entry))

(defun bltzread-dict--with-followed (full followed)
  "FULL, an entry that only refers to other words, completed with FOLLOWED.
FOLLOWED is an alist of (WORD . ENTRY).  The whole text gets each entry
below it; the short form is their short forms, each under a line naming the
word."
  (let (long short)
    (dolist (f followed)
      (let ((body (bltzread-dict--entry-body (cdr f))))
        (push (format "→ %s\n%s" (car f) body) long)
        (push (format "→ %s\n%s" (car f)
                      (or (get-text-property 0 'bltzread-short (cdr f)) body))
              short)))
    (propertize (concat full "\n\n" (string-join (nreverse long) "\n\n"))
                'bltzread-short (string-join (nreverse short) "\n"))))

(defun bltzread-dict-shorten (dict lang html full &optional follower)
  "Return entry string FULL for DICT with a short form attached, if possible.
LANG is the language; a filter may make the short form.
The short form (only the filtered text, without the dictionary name) is the
text property `bltzread-short' on FULL; HTML is the raw entry.
When the filter finds nothing and its :follow names words the entry refers
to, FOLLOWER, a function from a word to its entries in DICT, is asked for
them, and they are added to the entry."
  (let* ((spec (bltzread-dict-filter-for dict lang))
         (short (and spec (condition-case nil (bltzread-dict-apply-filter spec html)
                            (error nil))))
         (usable (and short (not (string-empty-p (string-trim short))))))
    (cond (usable (propertize full 'bltzread-short (string-trim short)))
          ((and spec follower
                (let* ((targets (ignore-errors (bltzread-dict-follow-targets spec html)))
                       (followed (mapcan (lambda (w)
                                           (mapcar (lambda (e) (cons w e))
                                                   (funcall follower w)))
                                         targets)))
                  (and followed (bltzread-dict--with-followed full followed)))))
          (spec (propertize full 'bltzread-parse-failed t))
          (t full))))

(defun bltzread-dict-display (entry full)
  "ENTRY as shown in the popup: whole when FULL, else its short form if any."
  (or (and (not full) entry (get-text-property 0 'bltzread-short entry))
      entry))

(provide 'bltzread-dict-filters)
;;; bltzread-dict-filters.el ends here
