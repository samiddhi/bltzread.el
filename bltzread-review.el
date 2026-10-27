;;; bltzread-review.el --- Review words before they join a skip list -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; Words that reach level 5 wait in a queue.  `bltzread-review' lists them.
;; Every word starts as `add'.  Mark a word `later' to leave it in the
;; queue, or `reject' to send it back to `bltzread-review-reject-level'.
;; `bltzread-review-apply' appends the `add' words to the skip list.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tabulated-list)
(require 'bltzread-custom)
(require 'bltzread-state)

(defvar-local bltzread-review--lang nil "Language being reviewed.")
(defvar-local bltzread-review--actions nil "Hash table form -> `add', `later' or `reject'.")

(defun bltzread-review-add ()
  "Mark the word on this line to be added to the skip list, and move on."
  (interactive)
  (bltzread-review--mark 'add))

(defun bltzread-review-later ()
  "Leave the word on this line in the queue for later, and move on."
  (interactive)
  (bltzread-review--mark 'later))

(defun bltzread-review-reject ()
  "Send the word on this line back to a learning level, and move on."
  (interactive)
  (bltzread-review--mark 'reject))

(defun bltzread-review-add-all ()
  "Mark every queued word to be added to the skip list."
  (interactive)
  (bltzread-review--mark-all 'add))

(defvar-keymap bltzread-review-mode-map
  :doc "Keymap of `bltzread-review-mode'."
  :parent tabulated-list-mode-map
  "a" #'bltzread-review-add
  "d" #'bltzread-review-later
  "r" #'bltzread-review-reject
  "A" #'bltzread-review-add-all
  "x" #'bltzread-review-apply
  "C-c C-c" #'bltzread-review-apply)

(defun bltzread-review--header ()
  "The header line: the keys of the review and what the colour means."
  (substitute-command-keys
   (concat "\\<bltzread-review-mode-map>"
           " \\[bltzread-review-add] add  \\[bltzread-review-later] later"
           "  \\[bltzread-review-reject] reject  \\[bltzread-review-add-all] add all"
           "  \\[bltzread-review-apply] apply  "
           (propertize " highlighted = reached the top level by reading exposure "
                       'face 'bltzread-review-exposure))))

(define-derived-mode bltzread-review-mode tabulated-list-mode "Bltzread-Review"
  "Decide which known words are written to the bltzr skip list.
\\{bltzread-review-mode-map}"
  (setq tabulated-list-format [("Word" 24 t) ("Basic word" 24 t) ("Action" 10 t) ("Source" 10 t)]
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun bltzread-review--refresh ()
  "Redraw the list."
  (let* ((state (bltzread-state-get bltzread-review--lang))
         (exposed (bltzread-state-exposed state))
         (queue (bltzread-state-queue state))
         (queue (append (seq-filter (lambda (e) (member (car e) exposed)) queue)
                        (seq-remove (lambda (e) (member (car e) exposed)) queue))))
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (let* ((expo (member (car e) exposed))
                           (cols (list (car e) (or (cdr e) "")
                                       (symbol-name (gethash (car e) bltzread-review--actions 'add))
                                       (if expo "exposure" "manual"))))
                      (list (car e)
                            (vconcat (if expo
                                         (mapcar (lambda (c) (propertize c 'face 'bltzread-review-exposure))
                                                 cols)
                                       cols)))))
                  queue))
    (tabulated-list-print t)))

(defun bltzread-review--mark (action)
  "Give the word on this line ACTION and move on."
  (when-let ((form (tabulated-list-get-id)))
    (puthash form action bltzread-review--actions)
    (bltzread-review--refresh)
    (forward-line 1)))

(defun bltzread-review--mark-all (action)
  "Give every word ACTION."
  (dolist (e (bltzread-state-queue (bltzread-state-get bltzread-review--lang)))
    (puthash (car e) action bltzread-review--actions))
  (bltzread-review--refresh))

(defun bltzread-review--confirmed-p (adds)
  "Non-nil when ADDS may be written; asks if `bltzread-review-confirm'."
  (or (not bltzread-review-confirm)
      (y-or-n-p (format "Add %d word%s to the skip list? "
                        (length adds) (if (cdr adds) "s" "")))))

(defun bltzread-review--reject (state entry)
  "Send the queued ENTRY, (FORM . LEMMA), of STATE back to a learning level."
  (let ((key (bltzread-state-key (car entry) (and (cdr entry) (list (cdr entry))))))
    (bltzread-state-set-level state key bltzread-review-reject-level)
    (bltzread-state-queue-remove state (list (car entry)))))

;;;###autoload
(defun bltzread-review-apply ()
  "Write the `add' words, demote the `reject' words, keep the rest queued."
  (interactive)
  (let* ((state (bltzread-state-get bltzread-review--lang))
         (queue (bltzread-state-queue state))
         (adds (cl-remove-if-not (lambda (e) (eq (gethash (car e) bltzread-review--actions 'add) 'add))
                                 queue))
         (rejects (cl-remove-if-not (lambda (e) (eq (gethash (car e) bltzread-review--actions) 'reject))
                                    queue)))
    (when (or adds rejects)
      (when (and adds (bltzread-review--confirmed-p adds))
        (let ((result (bltzread-state-write-known state adds)))
          (bltzread-state-queue-remove state (mapcar #'car adds))
          (message "Added %d to %s" (length (cdr result)) (car result))))
      (dolist (e rejects) (bltzread-review--reject state e))
      (bltzread-state-save state)
      (run-hooks 'bltzread-review-after-apply-hook))
    (bltzread-review--refresh)))

;;;###autoload
(defun bltzread-review (lang)
  "Review the queued known words of LANG."
  (interactive (list (or (bound-and-true-p bltzread-language)
                         bltzread-default-language
                         (read-string "Language code: "))))
  (let ((buf (get-buffer-create (format "*bltzread-review: %s*" lang))))
    (with-current-buffer buf
      (bltzread-review-mode)
      (setq bltzread-review--lang lang
            bltzread-review--actions (make-hash-table :test 'equal))
      (bltzread-review--refresh)
      (setq header-line-format (bltzread-review--header)))
    (pop-to-buffer buf)))

(provide 'bltzread-review)
;;; bltzread-review.el ends here
