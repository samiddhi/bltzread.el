;;; bltzread-undo.el --- Taking back bltzread changes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; Every change to levels, notes or lookups first pushes a snapshot of the
;; keys it will touch.  The snapshot goes on `bltzread--undo' for
;; `bltzread-undo' and into `buffer-undo-list', so plain `undo' and
;; `undo-redo' take it back in sequence with other changes.

;;; Code:

(require 'seq)
(require 'bltzread-custom)
(require 'bltzread-words)

(defvar-local bltzread--undo nil
  "Undo stack of vectors [DESCRIPTION SNAPSHOT KEYS HISTORY-P LOOKUPS LIVE].")

(declare-function bltzread--redraw-popup "bltzread-card")

(defun bltzread--undo-entry (description keys history-p)
  "A new undo entry for DESCRIPTION, about to alter KEYS.
HISTORY-P non-nil also restores the history."
  (vector description (bltzread-state-snapshot bltzread--state keys) keys history-p
          (copy-hash-table bltzread--lookups) t))

(defun bltzread--undo-record (entry)
  "Put ENTRY in the buffer's undo list as its own undo step."
  (when (listp buffer-undo-list)
    (undo-boundary)
    (push (list 'apply #'bltzread--undo-apply entry) buffer-undo-list)
    (undo-boundary)))

(defun bltzread--undo-push (description keys &optional history-p)
  "Remember how to take back DESCRIPTION, about to alter KEYS.
A non-nil HISTORY-P also restores the history."
  (let ((entry (bltzread--undo-entry description keys history-p)))
    (push entry bltzread--undo)
    (setq bltzread--undo (seq-take bltzread--undo bltzread-undo-limit))
    (bltzread--undo-record entry)))

(defun bltzread--undo-reverse-history (history-p)
  "What redoing needs to know about HISTORY-P, the history part of an entry."
  (cond ((consp history-p) t)
        (history-p (cons 'push (car (bltzread-state-history bltzread--state))))))

(defun bltzread--undo-restore-history (history-p)
  "Undo the history change described by HISTORY-P."
  (cond ((eq history-p t)
         (setf (bltzread-state-history bltzread--state)
               (cdr (bltzread-state-history bltzread--state))))
        ((consp history-p)
         (push (cdr history-p) (bltzread-state-history bltzread--state)))))

(defun bltzread--undo-apply (entry &optional no-reverse)
  "Restore the state ENTRY describes and record the reverse for redo.
With NO-REVERSE, record nothing."
  (if (not (aref entry 5))
      (message "bltzread: that change was already undone with `bltzread-undo'")
    (aset entry 5 nil)
    (let* ((keys (aref entry 2))
           (history-p (aref entry 3))
           (reverse (vector (aref entry 0) (bltzread-state-snapshot bltzread--state keys) keys
                            (bltzread--undo-reverse-history history-p)
                            (copy-hash-table bltzread--lookups) t)))
      (bltzread-state-restore bltzread--state (aref entry 1))
      (setq bltzread--lookups (aref entry 4))
      (bltzread--undo-restore-history history-p)
      (mapc #'bltzread--refresh-key keys)
      (bltzread--redraw-popup)
      (when (and (not no-reverse) (listp buffer-undo-list))
        (push (list 'apply #'bltzread--undo-apply reverse) buffer-undo-list))
      (message "Undone: %s%s" (aref entry 0)
               (if (eq bltzread-known-write-policy 'immediate)
                   " (words already written to a skip list stay there)" "")))))

(defun bltzread--undo-forget (entry)
  "Remove ENTRY's step from `buffer-undo-list', with the boundary it left doubled."
  (setq buffer-undo-list
        (seq-remove (lambda (e) (and (consp e) (eq (car e) 'apply) (eq (nth 2 e) entry)))
                    buffer-undo-list))
  (let ((l buffer-undo-list))
    (while (cdr l)
      (if (and (null (car l)) (null (cadr l)))
          (setcdr l (cddr l))
        (setq l (cdr l))))))

;;;###autoload
(defun bltzread-undo ()
  "Take back the last bltzread change, even after other edits in between.
Plain `undo' does the same in order; use this to skip over other edits."
  (interactive)
  (bltzread--require-mode)
  (let ((entry (or (pop bltzread--undo) (user-error "Nothing to undo"))))
    (when (listp buffer-undo-list) (bltzread--undo-forget entry))
    (bltzread--undo-apply entry t)))

(defun bltzread--undo-advice (fn &rest args)
  "Let the undo commands run in the read-only buffer of `bltzread-mode'.
FN is the advised function and ARGS its arguments."
  (let ((inhibit-read-only (or inhibit-read-only (bound-and-true-p bltzread-mode))))
    (apply fn args)))

(defconst bltzread--undo-commands '(undo undo-only undo-redo)
  "The commands advised so they work in a read-only reading buffer.")

(defun bltzread-undo-install-advice ()
  "Advise the undo commands for read-only reading buffers."
  (dolist (cmd bltzread--undo-commands)
    (advice-add cmd :around #'bltzread--undo-advice)))

(defun bltzread-undo-remove-advice ()
  "Undo `bltzread-undo-install-advice'."
  (dolist (cmd bltzread--undo-commands)
    (advice-remove cmd #'bltzread--undo-advice)))

(provide 'bltzread-undo)
;;; bltzread-undo.el ends here
