;;; bltzread-session.el --- Recording what you read -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; A reading session ends with `bltzread-finish-session': you say how far you
;; read, every word in that range you did not look up earns a clean read, and
;; enough clean reads lift a word one level.  The work is split in three:
;; `bltzread--session-eligible' (which words count; pure),
;; `bltzread--session-apply' (change the state) and `bltzread--session-record'
;; (history, saving, the hook).

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'bltzread-custom)
(require 'bltzread-words)
(require 'bltzread-undo)
(require 'bltzread-range)

(defvar bltzread-mode)
(declare-function bltzread-popup-hide "bltzread-popup")

(defvar-local bltzread--offered nil
  "Non-nil once the buffer has been asked whether to record its reading.")

;;; Which words count

(defun bltzread--session-eligible (ovs lookups)
  "The overlays among OVS that earn a clean read, in order.
A word counts when it is at a learning level and its key is not in the hash
table LOOKUPS; it counts once unless `bltzread-count-each-occurrence'."
  (let ((seen (make-hash-table :test 'equal)))
    (seq-filter
     (lambda (ov)
       (let ((key (overlay-get ov 'bltzread-key)))
         (and (bltzread-learning-level-p (overlay-get ov 'bltzread-level))
              (not (gethash key lookups))
              (or bltzread-count-each-occurrence (not (gethash key seen)))
              (puthash key t seen))))
     ovs)))

;;; Changing the state

(defun bltzread--apply-one (ov mark-known)
  "Count a clean read for word OV, or with MARK-KNOWN make it known.
Return a list (RESULT KEYS NEWLY): RESULT is `promoted', `queued' or nil,
KEYS the keys changed and NEWLY the (FORM . LEMMA) entries queued."
  (let* ((key (overlay-get ov 'bltzread-key))
         (form (overlay-get ov 'bltzread-form))
         (lemma (bltzread--lemma ov))
         (siblings (and mark-known (bltzread--siblings ov)))
         (result (if mark-known
                     (and (bltzread-state-set-level bltzread--state key bltzread-known-level
                                                    form lemma siblings)
                          'queued)
                   (bltzread-state-record-read bltzread--state key
                                               (overlay-get ov 'bltzread-level) form lemma))))
    (list result
          (if mark-known (mapcar #'car siblings) (list key))
          (and (eq result 'queued)
               (mapcar (lambda (s) (cons (downcase (cdr s)) (and lemma (downcase lemma))))
                       (or siblings (list (cons key form))))))))

(defun bltzread--session-apply (eligible mark-known)
  "Apply the session to the ELIGIBLE words; MARK-KNOWN makes them known instead.
Return a plist with :promoted and :queued counts, the :keys changed and the
:newly queued (FORM . LEMMA) entries."
  (let ((results (mapcar (lambda (ov) (bltzread--apply-one ov mark-known)) eligible)))
    (list :promoted (seq-count (lambda (r) (eq (car r) 'promoted)) results)
          :queued (seq-count (lambda (r) (eq (car r) 'queued)) results)
          :keys (seq-uniq (seq-mapcat #'cadr results))
          :newly (seq-mapcat #'caddr results))))

(defun bltzread--session-record (beg end ovs eligible applied)
  "Add the session to the history and save; return the history entry.
BEG..END is the range, OVS its words, ELIGIBLE those counted and APPLIED
what `bltzread--session-apply' returned.  Runs
`bltzread-session-finished-functions'."
  (let ((entry (list :time (format-time-string "%FT%T") :buffer (buffer-name)
                     :beg beg :end end :words (length ovs) :counted (length eligible)
                     :promoted (plist-get applied :promoted)
                     :queued (plist-get applied :queued))))
    (bltzread-state-add-history bltzread--state entry)
    (bltzread-state-save bltzread--state)
    (run-hook-with-args 'bltzread-session-finished-functions entry)
    entry))

;;; Finishing

(defun bltzread--session-confirmed-p (mark-known eligible ovs)
  "Non-nil when the session may go ahead; asks if `bltzread-session-confirm'.
MARK-KNOWN, ELIGIBLE and OVS are as for the session itself."
  (or (not bltzread-session-confirm)
      (y-or-n-p (format "%s %d word%s (%d in range)? "
                        (if mark-known "Mark known:" "Count clean reads for")
                        (length eligible) (if (cdr eligible) "s" "") (length ovs)))))

(defun bltzread--offer-bookmark (end)
  "Move the reading bookmark to END per `bltzread-bookmark-policy'."
  (when (and (require 'bookmark nil t)
             (pcase bltzread-bookmark-policy
               ('always t)
               ('ask (y-or-n-p "Move the bookmark to where you stopped? "))))
    (save-excursion
      (goto-char end)
      (bookmark-set (funcall bltzread-bookmark-name-function)))))

(defun bltzread-default-bookmark-name ()
  "Bookmark name for the current buffer."
  (format "bltzread: %s" (buffer-name)))

(defun bltzread--session-commit (beg end ovs eligible mark-known)
  "Record the session over BEG..END: words OVS, of which ELIGIBLE count.
MARK-KNOWN makes them known instead of counting a read."
  (bltzread--undo-push "reading session"
                       (if mark-known
                           (seq-uniq (seq-mapcat #'bltzread--word-keys eligible))
                         (mapcar (lambda (ov) (overlay-get ov 'bltzread-key)) eligible))
                       t)
  (let ((applied (bltzread--session-apply eligible mark-known)))
    (mapc #'bltzread--refresh-key (plist-get applied :keys))
    (bltzread--maybe-write-now (plist-get applied :newly))
    (bltzread--session-record beg end ovs eligible applied)
    (clrhash bltzread--lookups)
    (bltzread-range-clear-highlight)
    (bltzread-range-advance-to end)
    (message "bltzread: %d word%s counted, %d moved up, %d queued for review"
             (length eligible) (if (cdr eligible) "s" "")
             (plist-get applied :promoted) (plist-get applied :queued))
    (bltzread--offer-bookmark end)))

;;;###autoload
(defun bltzread-finish-session (&optional mark-known)
  "Record what you read: ask how far, then count the words in that range.
Each word in the range that you did not look up gets a clean read, and
may move up a level.  With prefix argument MARK-KNOWN, mark every such
word known instead."
  (interactive "P")
  (bltzread--require-mode)
  (when mark-known (bltzread--require-manual))
  (pcase-let* ((`(,beg . ,end) (bltzread-range-pick))
               (ovs (bltzread-word-overlays beg end))
               (eligible (bltzread--session-eligible ovs bltzread--lookups)))
    (bltzread-range-highlight beg end)
    (cond ((bltzread--session-confirmed-p mark-known eligible ovs)
           (bltzread--session-commit beg end ovs eligible mark-known))
          (t (bltzread-range-clear-highlight)
             (message "Nothing recorded")))))

;;;###autoload
(defun bltzread-mark-range-known ()
  "Mark every word in the range you read, that you did not look up, as known."
  (interactive)
  (bltzread-finish-session t))

;;; Offering to record

(defun bltzread--worth-recording-p (min-chars)
  "Non-nil when you moved through more than MIN-CHARS characters."
  (> (- (bltzread-range-farthest) (or (bltzread-range-start) 1)) min-chars))

(defun bltzread--ask-to-record ()
  "Ask whether to record the reading of this buffer, and do so on a yes."
  (setq bltzread--offered t)
  (bltzread-popup-hide)
  (condition-case err
      (when (y-or-n-p "Record what you read in bltzread? ")
        (bltzread-finish-session))
    (user-error (message "bltzread: %s" (cadr err)))))

(defun bltzread--offer-p (min-chars)
  "Non-nil when this buffer should be asked about recording after MIN-CHARS."
  (and bltzread-mode bltzread-prompt-on-close (not bltzread--offered)
       (bltzread--worth-recording-p min-chars)))

;;;###autoload
(defun bltzread-offer-record ()
  "Ask whether to record what was read, unless nothing was or it was asked.
For a caller that closes the buffer later, at a moment of its choosing: the
question is then not repeated when the buffer is killed."
  (when (bltzread--offer-p 0)
    (bltzread--ask-to-record)))

(defun bltzread--query-kill ()
  "Offer to record reading before the buffer is killed.  Always return t."
  (when (bltzread--offer-p bltzread-min-chars-for-prompt)
    (bltzread--ask-to-record))
  t)

(provide 'bltzread-session)
;;; bltzread-session.el ends here
