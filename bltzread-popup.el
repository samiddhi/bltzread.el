;;; bltzread-popup.el --- Bordered word popup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;;; Commentary:

;; A bordered, coloured box built as a pure string from rows.  In a
;; graphical Emacs it floats in a child frame above the word; in a
;; terminal it fills a bottom side window.  The design follows
;; subtle-popup.el.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'bltzread-custom)

(defconst bltzread-popup--buffer " *bltzread-popup*")
(defvar bltzread-popup--frame nil "Child frame currently showing the box, or nil.")
(defvar bltzread-popup--owner nil "Buffer the popup is showing for, or nil.")

;;; Pure box building

(defun bltzread-popup--pad (text width)
  "TEXT followed by spaces up to display WIDTH."
  (concat text (make-string (max 0 (- width (string-width text))) ?\s)))

(defun bltzread-popup--body (text)
  "Copy of TEXT with the popup body face added behind its own faces."
  (let ((copy (copy-sequence text)))
    (add-face-text-property 0 (length copy) 'bltzread-popup t copy)
    copy))

(defun bltzread-popup--row (text width)
  "One boxed row holding TEXT padded to WIDTH."
  (concat (propertize "│" 'face 'bltzread-popup-border)
          (bltzread-popup--body (concat " " (bltzread-popup--pad text width) " "))
          (propertize "│" 'face 'bltzread-popup-border)))

(defun bltzread-popup--edge (left right width)
  "Top or bottom edge made of LEFT, a rule of WIDTH, and RIGHT."
  (propertize (concat left (make-string (+ width 2) ?─) right)
              'face 'bltzread-popup-border))

(defun bltzread-popup-box (rows)
  "ROWS (strings) drawn inside a box, as one multi-line string."
  (let ((width (apply #'max 1 (mapcar #'string-width rows))))
    (string-join
     (append (list (bltzread-popup--edge "╭" "╮" width))
             (mapcar (lambda (row) (bltzread-popup--row row width)) rows)
             (list (bltzread-popup--edge "╰" "╯" width)))
     "\n")))

(defun bltzread-popup-title (text)
  "TEXT styled as the box title."
  (propertize text 'face 'bltzread-popup-title))

(defun bltzread-popup-key (key)
  "KEY styled as a key cap."
  (propertize (concat " " key " ") 'face 'bltzread-popup-key))

(defun bltzread-popup-dim (text)
  "TEXT styled as secondary text."
  (propertize text 'face 'bltzread-popup-dim))

(defun bltzread-popup-wrap (text width max-lines)
  "TEXT wrapped to WIDTH columns, at most MAX-LINES lines, as a list."
  (let ((lines (with-temp-buffer
                 (insert text)
                 (let ((fill-column width) (adaptive-fill-mode nil))
                   (fill-region (point-min) (point-max)))
                 (split-string (buffer-string) "\n"))))
    (if (> (length lines) max-lines)
        (append (seq-take lines (1- max-lines)) (list "…"))
      lines)))

;;; Showing

(defun bltzread-popup--fill-buffer (box)
  "Put BOX in the popup buffer and return the buffer."
  (with-current-buffer (get-buffer-create bltzread-popup--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert box))
    (setq-local mode-line-format nil header-line-format nil cursor-type nil
                truncate-lines t)
    (current-buffer)))

(defun bltzread-popup--frame-parameters (parent box)
  "Parameters of a borderless, unfocusable child frame of PARENT fitting BOX."
  `((parent-frame . ,parent)
    (width . ,(+ 2 (apply #'max (mapcar #'string-width (split-string box "\n")))))
    (height . ,(length (split-string box "\n"))) (name . "bltzread") (minibuffer . nil)
    (undecorated . t) (no-accept-focus . t) (no-focus-on-map . t)
    (skip-taskbar . t) (unsplittable . t) (visibility . nil)
    (internal-border-width . 0) (child-frame-border-width . 0)
    (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
    (left-fringe . 0) (right-fringe . 0) (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil) (cursor-type . nil)))

(defun bltzread-popup--gap ()
  "Gap in pixels between the word and the box above it."
  (round (* (default-line-height) (or bltzread-popup-gap 0.75))))

(defvar bltzread-popup--pos nil "Buffer position of the word the floating popup is about.")
(defvar bltzread-popup--pad nil "Overlay of blank lines made when the text could not scroll further.")
(defvar bltzread-popup--refresh-timer nil)

(defvar bltzread-popup-scroll-hook nil
  "Normal hook run, with the window selected, after the popup scrolled it.
A package that follows the window's start position can use it to learn that
this scroll was not the user's.")

(defun bltzread-popup-scroll-lines (free need line headroom)
  "Lines to scroll so NEED pixels are free above a word that has FREE.
LINE is a line's height in pixels and HEADROOM how many pixels the word can
still move down before it would leave the window."
  (if (>= free need)
      0
    (min (ceiling (- need free) line) (max 0 (floor headroom line)))))

(defun bltzread-popup--word-xy (win pos)
  "Pixel position of POS in the frame of WIN as (X . TOP), or nil if not shown."
  (when-let* ((posn (with-selected-window win (posn-at-point pos)))
              (xy (posn-x-y posn)))
    (let ((edges (window-pixel-edges win)))
      (cons (+ (nth 0 edges) (car xy))
            (+ (nth 1 edges) (window-header-line-height win) (cdr xy))))))

(defun bltzread-popup--remove-pad ()
  (when (overlayp bltzread-popup--pad)
    (delete-overlay bltzread-popup--pad))
  (setq bltzread-popup--pad nil))

(defun bltzread-popup--make-room (win pos need)
  "Scroll WIN, if it must, so NEED pixels are free above the word at POS.
The text moves down, as when scrolling back; at the top of the buffer
blank lines are added instead, never more than the word needs.  Any blank
lines of an earlier popup are dropped first, so they never accumulate.  The
popup is never put below the word."
  (when (overlayp bltzread-popup--pad)
    (bltzread-popup--remove-pad)
    (with-selected-window win (redisplay t)))
  (let ((tries 0))
    (while-let ((xy (and (< tries 3) (bltzread-popup--word-xy win pos)))
                (line (default-line-height))
                (word-y (- (cdr xy) (nth 1 (window-pixel-edges win))
                           (window-header-line-height win)))
                (lines (bltzread-popup-scroll-lines
                        word-y need line
                        (- (window-body-height win t) word-y line)))
                ((> lines 0)))
      (setq tries (1+ tries))
      (with-selected-window win
        (let ((moved 0))
          (save-excursion
            (goto-char (window-start win))
            (setq moved (- (vertical-motion (- lines))))
            (set-window-start win (point)))
          (when (and (< moved lines) (= (window-start win) (point-min)))
            (bltzread-popup--remove-pad)
            (setq bltzread-popup--pad (make-overlay (point-min) (point-min)))
            (overlay-put bltzread-popup--pad 'before-string
                         (make-string (- lines moved) ?\n)))
          (redisplay t)
          (run-hooks 'bltzread-popup-scroll-hook))))))

(defun bltzread-popup--place (frame parent pos &optional scroll)
  "Put FRAME above the word at POS, kept inside PARENT.
With SCROLL, first scroll the text if there is no room above the word.
Return nil if the word is not on screen or the box would cover it."
  (let* ((win (selected-window))
         (fw (frame-pixel-width frame)) (fh (frame-pixel-height frame))
         (gap (bltzread-popup--gap)))
    (when scroll (bltzread-popup--make-room win pos (+ fh gap)))
    (when-let* ((xy (bltzread-popup--word-xy win pos))
                ((>= (cdr xy) fh)))
      (set-frame-position frame
                          (max 0 (min (car xy) (- (frame-pixel-width parent) fw)))
                          (max 0 (min (- (cdr xy) fh gap) (- (cdr xy) fh))))
      t)))

(defun bltzread-popup--cancel-refresh ()
  (when (timerp bltzread-popup--refresh-timer)
    (cancel-timer bltzread-popup--refresh-timer))
  (setq bltzread-popup--refresh-timer nil))

(defun bltzread-popup--refresh ()
  "Keep the floating popup with its word after the text moved."
  (setq bltzread-popup--refresh-timer nil)
  (when (and (frame-live-p bltzread-popup--frame) bltzread-popup--pos
             (eq bltzread-popup--owner (window-buffer (selected-window))))
    (unless (and (pos-visible-in-window-p bltzread-popup--pos)
                 (bltzread-popup--place bltzread-popup--frame (selected-frame)
                                        bltzread-popup--pos))
      (bltzread-popup-hide))))

(defun bltzread-popup--on-scroll (window _start)
  "Schedule `bltzread-popup--refresh' when WINDOW, showing the owner, scrolled."
  (when (and (frame-live-p bltzread-popup--frame)
             (eq (window-buffer window) bltzread-popup--owner)
             (not (timerp bltzread-popup--refresh-timer)))
    (setq bltzread-popup--refresh-timer
          (run-at-time 0 nil #'bltzread-popup--refresh))))

(defun bltzread-popup--show-frame (buffer box pos)
  "Float BUFFER, showing BOX, in a child frame above the word at POS."
  (let* ((parent (selected-frame))
         (frame (make-frame (bltzread-popup--frame-parameters parent box))))
    (set-window-buffer (frame-root-window frame) buffer)
    (set-window-dedicated-p (frame-root-window frame) t)
    (if (bltzread-popup--place frame parent pos t)
        (progn (make-frame-visible frame)
               (setq bltzread-popup--frame frame
                     bltzread-popup--pos pos))
      (delete-frame frame)
      ;; No room above the word: never cover it, use a side window instead.
      (when (bltzread-popup--word-xy (selected-window) pos)
        (bltzread-popup--show-side-window buffer)))))

(defvar bltzread-popup--used-window nil
  "The side window the latest `bltzread-popup-show' drew in, or nil.")

(defun bltzread-popup--show-side-window (buffer &optional side)
  "Show BUFFER in a side window on SIDE (default bottom).
An existing window showing BUFFER on that side is reused and refitted, so
moving between words does not close and reopen it."
  (let* ((side (or side 'bottom))
         (old (seq-find (lambda (w) (eq (window-parameter w 'window-side) side))
                        (get-buffer-window-list buffer nil t)))
         (win (or old
                  (display-buffer-in-side-window
                   buffer `((side . ,side) (slot . 0)
                            (window-height . fit-window-to-buffer)
                            (window-parameters . ((no-other-window . t))))))))
    (when (and old (window-live-p old))
      (let ((window-size-fixed nil))
        (fit-window-to-buffer old)))
    (setq bltzread-popup--used-window win)))

(defun bltzread-popup--style ()
  "The style to draw with now."
  (if (eq bltzread-popup-style 'auto)
      (if (display-graphic-p) 'box 'side-window)
    bltzread-popup-style))

(defun bltzread-popup-show (rows &optional pos)
  "Show ROWS, a list of strings, as a popup per `bltzread-popup-style'.
POS is the buffer position of the word the popup is about (default point)."
  ;; Drop the floating frame at once, but keep a side window until we know
  ;; the new popup does not reuse it (no jumping while moving word to word).
  (bltzread-popup--cancel-refresh)
  (when (frame-live-p bltzread-popup--frame) (delete-frame bltzread-popup--frame))
  (setq bltzread-popup--frame nil bltzread-popup--used-window nil
        bltzread-popup--owner (current-buffer))
  (pcase (bltzread-popup--style)
    ('box (let* ((box (bltzread-popup-box rows))
                 (buffer (bltzread-popup--fill-buffer box)))
            (if (display-graphic-p)
                (bltzread-popup--show-frame buffer box (or pos (point)))
              (bltzread-popup--show-side-window buffer))))
    ('side-window (bltzread-popup--show-side-window
                   (bltzread-popup--fill-buffer (bltzread-popup-box rows))))
    ('echo (let ((message-log-max nil))
             (message "%s" (string-join rows "  |  ")))))
  (when-let ((buf (get-buffer bltzread-popup--buffer)))
    (dolist (win (get-buffer-window-list buf nil t))
      (unless (eq win bltzread-popup--used-window)
        (ignore-errors (delete-window win))))))

(defun bltzread-popup-hide ()
  "Remove the popup, if any."
  (bltzread-popup--cancel-refresh)
  (when (frame-live-p bltzread-popup--frame)
    (delete-frame bltzread-popup--frame))
  (bltzread-popup--remove-pad)
  (setq bltzread-popup--frame nil bltzread-popup--owner nil bltzread-popup--pos nil)
  (when-let ((buf (get-buffer bltzread-popup--buffer)))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (window-live-p win) (ignore-errors (delete-window win))))))

(defun bltzread-popup-window-visible-p ()
  "Non-nil when the popup is showing in a window, not a floating frame."
  (and (get-buffer bltzread-popup--buffer)
       (get-buffer-window-list bltzread-popup--buffer nil t)
       t))

(defun bltzread-popup-visible-p ()
  "Non-nil when a popup is on screen."
  (or (frame-live-p bltzread-popup--frame)
      (and (get-buffer bltzread-popup--buffer)
           (get-buffer-window-list bltzread-popup--buffer nil t))))

;;; Following the selected window

(defun bltzread-popup--sync (&rest _)
  "Hide the popup unless its buffer is the one in the selected window."
  (when (and bltzread-popup--owner
             (not (eq bltzread-popup--owner (window-buffer (selected-window)))))
    (bltzread-popup-hide)))

(defun bltzread-popup--hide-hook (&rest _)
  "Hide the popup, for hooks that announce a focus or minibuffer change."
  (bltzread-popup-hide))

(defun bltzread-popup--focus-change ()
  "Hide the popup when Emacs loses focus."
  (unless (frame-focus-state) (bltzread-popup-hide)))

(defun bltzread-popup-install-hooks ()
  "Make the popup vanish when the buffer, window, focus or minibuffer changes."
  (add-hook 'window-selection-change-functions #'bltzread-popup--sync)
  (add-hook 'window-buffer-change-functions #'bltzread-popup--sync)
  (add-hook 'window-scroll-functions #'bltzread-popup--on-scroll)
  (add-hook 'minibuffer-setup-hook #'bltzread-popup--hide-hook)
  (add-function :after after-focus-change-function #'bltzread-popup--focus-change))

(defun bltzread-popup-remove-hooks ()
  "Undo `bltzread-popup-install-hooks'."
  (remove-hook 'window-selection-change-functions #'bltzread-popup--sync)
  (remove-hook 'window-buffer-change-functions #'bltzread-popup--sync)
  (remove-hook 'window-scroll-functions #'bltzread-popup--on-scroll)
  (remove-hook 'minibuffer-setup-hook #'bltzread-popup--hide-hook)
  (remove-function after-focus-change-function #'bltzread-popup--focus-change))

(provide 'bltzread-popup)
;;; bltzread-popup.el ends here
