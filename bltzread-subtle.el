;;; bltzread-subtle.el --- Use bltzread inside subtle's subtitle buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; The one place where bltzread knows about subtle (the subtitle player).
;; Nothing runs unless subtle is loaded, and bltzread works without it.  Once
;; subtle is there:
;;
;; - point tracking follows the first unknown word of each cue
;;   (`bltzread-first-unknown-between' on `subtle-cue-point-functions'), and
;;   the card of that word opens by itself, unless the cue has no unknown
;;   word and point only went to its start
;;   (`bltzread-subtle-show-popup' on `subtle-point-track-moved-functions');
;; - scrolling made by the popup is not taken for the user's own scrolling
;;   (`subtle-ui-note-own-scroll' on `bltzread-popup-scroll-hook');
;; - an open card holds autoscroll back, keeps its word on screen, and is the
;;   first thing a quit or page turn dismisses
;;   (`subtle-hold-functions', `subtle-anchor-functions',
;;   `subtle-dismiss-functions');
;; - closing a session offers to record the reading and hides the card
;;   (`subtle-before-close-hook', `subtle-close-hook');
;; - the popup's hint row shows the key of `subtle-point-track-toggle' while a
;;   subtle session is active in the buffer.

;;; Code:

(require 'bltzread-custom)

(defvar subtle-cue-point-functions)
(defvar subtle-hold-functions)
(defvar subtle-anchor-functions)
(defvar subtle-dismiss-functions)
(defvar subtle-before-close-hook)
(defvar subtle-close-hook)
(defvar subtle-buffer-setup-hook)
(declare-function subtle-active-p "subtle")
(declare-function subtle-ui-note-own-scroll "subtle-ui")
(declare-function bltzread-first-unknown-between "bltzread-words")
(declare-function bltzread-word-at "bltzread-words")
(declare-function bltzread-unknown-word-p "bltzread-words")
(declare-function bltzread--show-popup "bltzread-card")
(defvar bltzread--popup-dismissed)

(defconst bltzread-subtle-hint '(subtle-point-track-toggle . "track point")
  "The popup hint for point tracking, shown only in a subtle session.")

(defun bltzread-subtle--add-hint ()
  "Add the point tracking hint to the popup of this buffer when subtle plays it.
Meant for `subtle-buffer-setup-hook'."
  (when (and (fboundp 'subtle-active-p) (subtle-active-p)
             (not (member bltzread-subtle-hint bltzread-popup-hints)))
    (setq-local bltzread-popup-hints
                (append bltzread-popup-hints (list bltzread-subtle-hint)))))

(defun bltzread-subtle-show-popup (buffer pos)
  "Show the card of the unknown word at POS in BUFFER, if BUFFER is selected.
A cue without an unknown word leaves point at its start and gets no card;
clicking or moving to a word still opens it.
Meant for `subtle-point-track-moved-functions'."
  (when (eq buffer (window-buffer (selected-window)))
    (with-current-buffer buffer
      (when-let ((ov (bltzread-word-at pos)))
        (when (bltzread-unknown-word-p ov)
          (setq bltzread--popup-dismissed nil)
          (bltzread--show-popup buffer))))))

(defconst bltzread-subtle-hooks
  '((subtle-cue-point-functions . bltzread-first-unknown-between)
    (subtle-point-track-moved-functions . bltzread-subtle-show-popup)
    (subtle-hold-functions . bltzread-popup-visible-p)
    (subtle-anchor-functions . bltzread-popup-anchor)
    (subtle-dismiss-functions . bltzread-dismiss-popup)
    (subtle-before-close-hook . bltzread-offer-record)
    (subtle-close-hook . bltzread-popup-hide)
    (subtle-buffer-setup-hook . bltzread-subtle--add-hint))
  "Alist of (SUBTLE-HOOK . BLTZREAD-FUNCTION) that `bltzread-subtle-connect' adds.")

(defun bltzread-subtle-connect ()
  "Connect bltzread to the subtle that is now loaded."
  (pcase-dolist (`(,hook . ,fn) bltzread-subtle-hooks)
    (add-hook hook fn))
  (add-hook 'bltzread-popup-scroll-hook #'subtle-ui-note-own-scroll))


(with-eval-after-load 'subtle
  (bltzread-subtle-connect))

(provide 'bltzread-subtle)
;;; bltzread-subtle.el ends here
