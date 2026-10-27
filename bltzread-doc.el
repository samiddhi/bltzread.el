;;; bltzread-doc.el --- Show the bltzread documentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; `bltzread-manual' and `bltzread-tutorial' show the org documents next to
;; this file in a help buffer, with each command placeholder replaced by the key that
;; runs it in your configuration.  The Info manual holds both: the tutorial is
;; its last chapter (`bltzread-doc-render-info-file').

;;; Code:

(require 'subr-x)

(defvar bltzread-mode)
(declare-function org-mode "org")

(defconst bltzread-doc--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory of the bltzread documents.")

(defun bltzread-doc--read (name)
  "Text of the document file NAME next to this file."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name bltzread-doc--directory))
    (buffer-string)))

(defun bltzread-doc--keys (text)
  "TEXT with each command placeholder replaced by its key in `bltzread-mode-map'.
The keys are those of a read-only buffer, where the single keys work, and
quotes stay as typed, so code samples survive."
  (let ((buffer-read-only t)
        (text-quoting-style 'grave))
    (substitute-command-keys (concat "\\<bltzread-mode-map>" text))))

(defun bltzread-doc--text (name reader)
  "Document NAME with its keys filled in, as seen from the buffer READER.
READER is the reading buffer if `bltzread-mode' is on in it, else nil, and
the default keys are then shown."
  (let ((text (bltzread-doc--read name)))
    (if (buffer-live-p reader)
        (with-current-buffer reader (bltzread-doc--keys text))
      (bltzread-doc--keys text))))

(defun bltzread-doc-render-file (name out)
  "Write the document NAME, with the default keys, to the file OUT.
Used to make the Info manual: the keymaps are the ones of a fresh Emacs."
  (with-temp-file out
    (insert (bltzread-doc--text name nil))))

(defun bltzread-doc--demote (text)
  "TEXT, an Org document, as a chapter: no title lines, headings one level down."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (looking-at "#\\+\\|[ \t]*$") (forward-line 1))
    (delete-region (point-min) (point))
    (while (re-search-forward "^\\*+ " nil t)
      (goto-char (match-beginning 0))
      (insert "*")
      (forward-line 1))
    (buffer-string)))

(defun bltzread-doc-render-info-file (out)
  "Write the text of the Info manual, with the default keys, to the file OUT.
It is the manual followed by the tutorial as its last chapter."
  (with-temp-file out
    (insert (string-trim-right (bltzread-doc--text "bltzread-manual.org" nil))
            "\n\n* Tutorial\n\n"
            (bltzread-doc--demote (bltzread-doc--text "bltzread-tutorial.org" nil)))))

(defun bltzread-doc--show (file title)
  "Show the document FILE in a help buffer called TITLE."
  (let* ((reader (and (bound-and-true-p bltzread-mode) (current-buffer)))
         (text (bltzread-doc--text file reader)))
    (with-current-buffer (get-buffer-create title)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min)))
      (delay-mode-hooks (org-mode))
      (view-mode 1)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun bltzread-manual ()
  "Show the bltzread manual, with the keys of your configuration."
  (interactive)
  (bltzread-doc--show "bltzread-manual.org" "*bltzread manual*"))

;;;###autoload
(defun bltzread-tutorial ()
  "Show the bltzread tutorial, with the keys of your configuration."
  (interactive)
  (bltzread-doc--show "bltzread-tutorial.org" "*bltzread tutorial*"))

(provide 'bltzread-doc)
;;; bltzread-doc.el ends here
