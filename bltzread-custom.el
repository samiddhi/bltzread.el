;;; bltzread-custom.el --- Options and faces for bltzread -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <bkhpanigha@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is part of bltzread; see bltzread.el for the license notice.

;;; Commentary:

;; Every user-facing knob of bltzread lives here so the other files can depend
;; on it without depending on each other.  Every default is a default,
;; not a rule: change anything with M-x customize-group RET bltzread.
;;
;; The level table (`bltzread-known-level', `bltzread-level-names',
;; `bltzread-level-faces') is the one place that says what the levels are.

;;; Code:

(declare-function bltzread-annotate "bltzread-words")
(declare-function bltzread-popup-wrap "bltzread-popup")
(declare-function bltzread-lists-write-known "bltzread-lists")
(declare-function bltzread-bltzr-detect-language "bltzread-bltzr")
(declare-function bltzread-bltzr-lemmas "bltzread-bltzr")
(declare-function bltzread-bltzr-tokenize "bltzread-bltzr")
(declare-function bltzread-state-flush "bltzread-state")
(declare-function bltzread-sdcv-lookup "bltzread-dict")
(declare-function bltzread-default-bookmark-name "bltzread-session")
(declare-function bltzread--bind-prefix "bltzread")
(declare-function bltzread--install-mouse-keys "bltzread")

(defgroup bltzread nil
  "Reading mode on top of bltzr that colours words by how well you know them."
  :group 'applications
  :prefix "bltzread-"
  :link '(info-link "(bltzread)Top")
  :link '(url-link "https://github.com/samiddhi/bltzread"))

(defgroup bltzread-bltzr nil
  "How bltzread finds words and talks to bltzr."
  :group 'bltzread)

(defgroup bltzread-levels nil
  "Levels, promotion and what a lookup does."
  :group 'bltzread)

(defgroup bltzread-lists nil
  "Skip lists and the stored progress."
  :group 'bltzread)

(defgroup bltzread-popup nil
  "The word card."
  :group 'bltzread)

(defgroup bltzread-dictionary nil
  "Dictionary entries."
  :group 'bltzread)

(defgroup bltzread-keys nil
  "Keys and mouse events."
  :group 'bltzread)

(defgroup bltzread-session nil
  "Reading sessions, bookmarks and the range you read."
  :group 'bltzread)

(defgroup bltzread-paste nil
  "The page `bltzread-read-text' opens."
  :group 'bltzread)

(defgroup bltzread-faces nil
  "Faces of bltzread."
  :group 'bltzread
  :group 'faces)

;;; The level table

(defconst bltzread-known-level 5
  "The highest level: a known word, ready for a skip list.")

(defconst bltzread-max-learning-level 4
  "The highest level a clean read can lift a word from.")

(defun bltzread-learning-level-p (level)
  "Non-nil when LEVEL is a number below `bltzread-known-level'."
  (and (integerp level) (<= 1 level bltzread-max-learning-level)))

;;; Re-annotating setters

(defun bltzread--reannotate-buffers ()
  "Read the words again in every buffer where `bltzread-mode' is on."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p bltzread-mode)
        (with-demoted-errors "bltzread: %S" (bltzread-annotate))))))

(defun bltzread--set-reannotate (symbol value)
  "Set SYMBOL to VALUE and re-annotate open reading buffers.
For options that change which words there are or what they are worth."
  (set-default-toplevel-value symbol value)
  (bltzread--reannotate-buffers))

(defun bltzread--set-state-directory (symbol value)
  "Set SYMBOL to VALUE after saving the loaded state, then re-annotate."
  (when (fboundp 'bltzread-state-flush) (bltzread-state-flush))
  (set-default-toplevel-value symbol value)
  (bltzread--reannotate-buffers))

(defun bltzread--set-prefix-key (symbol value)
  "Set SYMBOL to VALUE and move the prefix binding of `bltzread-mode-map'."
  (when (fboundp 'bltzread--bind-prefix)
    (bltzread--bind-prefix (and (boundp symbol) (default-value symbol)) value))
  (set-default-toplevel-value symbol value))

(defun bltzread--set-mouse-key (symbol value)
  "Set SYMBOL to VALUE and rebuild the mouse bindings of the words."
  (set-default-toplevel-value symbol value)
  (when (fboundp 'bltzread--install-mouse-keys) (bltzread--install-mouse-keys)))

(defun bltzread--thresholds-p (value)
  "Non-nil when VALUE is a list of one count per learning level."
  (and (proper-list-p value)
       (= (length value) bltzread-max-learning-level)
       (seq-every-p #'natnump value)))

(defun bltzread--language-code-p (value)
  "Non-nil when VALUE is shaped like a bltzr language code."
  (and (stringp value) (string-match-p "\\`[a-z]\\{3,4\\}\\'" value)))

;;; bltzr

(defcustom bltzread-language nil
  "Language code of this buffer, or nil to choose when the mode starts.
A bltzr code of three letters, such as \"eng\" or \"slv\" (a few have
four).  Set it per buffer, for example as a file-local variable."
  :type '(choice (const :tag "Choose" nil) string)
  :safe #'bltzread--language-code-p
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))
(make-variable-buffer-local 'bltzread-language)

(defcustom bltzread-no-colors nil
  "Non-nil shows no level colours and disallows setting levels by hand.
Only the popup remains.  Reading sessions still record which words you did
not look up, and words progress as usual.  Set it per buffer with `setq-local'
or toggle it with `bltzread-toggle-colors'."
  :type 'boolean
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))
(make-variable-buffer-local 'bltzread-no-colors)

(defcustom bltzread-no-colors-show-level nil
  "Non-nil still shows a word's level and clean reads in the popup.
This applies to no-colors mode."
  :type 'boolean
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-bltzr-executable "bltzr"
  "Name or path of the bltzr executable."
  :type 'string
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-bltzr-extra-args nil
  "Extra command-line arguments passed to every bltzr call."
  :type '(repeat string)
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-default-language nil
  "Language code used when a buffer has none, or nil to ask.
See `bltzread-language' for the form of a code."
  :type '(choice (const :tag "Ask" nil) string)
  :safe #'bltzread--language-code-p
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-detect-sample-size 20000
  "Characters from the start of a buffer given to bltzr to detect its language."
  :type 'natnum
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-word-regexp "\\w+\\(?:[-'’]\\w+\\)*"
  "Regexp matching one word in the text.
Languages that write without spaces need their own entry in
`bltzread-word-regexp-alist'."
  :type 'regexp
  :set #'bltzread--set-reannotate
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-word-regexp-alist nil
  "Alist of (LANGUAGE-CODE . REGEXP) overriding `bltzread-word-regexp'."
  :type '(alist :key-type string :value-type regexp)
  :set #'bltzread--set-reannotate
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-skip-numerals t
  "Non-nil never treats a number (2, 24, 5.26, 2,81, 1990-1995) as a word.
A word is skipped when it contains no letter."
  :type 'boolean
  :set #'bltzread--set-reannotate
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-track-by 'form
  "What a level belongs to.
`form' tracks each spelling separately (matches bltzr's exact-word
list).  `lemma' tracks the dictionary basic word, so all its forms share
one level."
  :type '(choice (const form) (const lemma))
  :safe #'symbolp
  :set #'bltzread--set-reannotate
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-make-read-only t
  "Non-nil makes the buffer read-only while `bltzread-mode' is on.
The single keys of `bltzread-mode-map' work only in a read-only buffer."
  :type 'boolean
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-reannotate-on-change t
  "Whether and when the words are read again after the text changes.
Nil never does so by itself (run `bltzread-annotate').  A number is the
idle seconds to wait; t waits 0.3."
  :type '(choice (const :tag "Never" nil) (const :tag "After 0.3 seconds" t)
                 (number :tag "Idle seconds"))
  :safe (lambda (v) (or (booleanp v) (numberp v)))
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-tokenizer-function #'bltzread-bltzr-tokenize
  "Function that finds the words of a stretch of text.
Called with BEG, END and the language code; returns a list of
\(BEG END FORM) in text order, FORM being the lowercased spelling."
  :type '(choice (function-item bltzread-bltzr-tokenize) function)
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-lemmatizer-function #'bltzread-bltzr-lemmas
  "Function that finds the basic words of spellings.
Called with the language code and a list of spellings; returns a hash
table (test `equal') of spelling -> list of basic words."
  :type '(choice (function-item bltzread-bltzr-lemmas) function)
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-language-detect-function #'bltzread-bltzr-detect-language
  "Function that guesses the language of a text.
Called with the text; returns a language code or nil."
  :type '(choice (function-item bltzread-bltzr-detect-language) function)
  :group 'bltzread-bltzr :package-version '(bltzread . "0.1.0"))

;;; Levels

(defcustom bltzread-level-names ["New" "Recognized" "Familiar" "Learned" "Known"]
  "Names of levels 1 to 5, as shown in the popup."
  :type '(vector string string string string string)
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-level-faces
  [bltzread-level-1 bltzread-level-2 bltzread-level-3 bltzread-level-4 bltzread-level-5]
  "Faces of the words of levels 1 to 5.
Level 5 has an empty face by default, so known words look plain."
  :type '(vector face face face face face)
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-level-scope 'word
  "What a level you set by hand (1-5, ignore) applies to.
`word' sets the whole word: every form sharing its basic word changes
together.  `form' sets only the spelling you are on.  Clean reads from
reading sessions always count for the single form."
  :type '(choice (const word) (const form))
  :safe #'symbolp
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-promotion-thresholds '(3 3 4 5)
  "Clean reads needed to climb from level 1, 2, 3 and 4 to the next.
A clean read is a reading session in which you did not look the word up."
  :type '(list natnum natnum natnum natnum)
  :safe #'bltzread--thresholds-p
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-count-each-occurrence nil
  "Non-nil counts every occurrence of a word in the range you read.
Nil counts each word at most once per reading session."
  :type 'boolean
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-lookup-effect 'reset-reads
  "What looking a word up does to it.
`reset-reads' restarts its clean-read count; `demote' also drops it one
level (never below 1); `nothing' only records the lookup."
  :type '(choice (const reset-reads) (const demote) (const nothing))
  :safe #'symbolp
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-lookup-counts 'command
  "What counts as looking a word up.
`command' means only `bltzread-lookup'.  `interaction' also counts asking for
more from the popup (`bltzread-next-entry', `bltzread-previous-entry',
`bltzread-popup-at-point').  `popup' also counts the automatic popup, but only
once point has stayed on the word for `bltzread-lookup-dwell' seconds, so
scrolling or jumping past a word does not count."
  :type '(choice (const command) (const interaction) (const popup))
  :safe #'symbolp
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-lookup-dwell 1.5
  "Seconds point must stay on a word before its popup counts as a lookup.
Only used when `bltzread-lookup-counts' is `popup'."
  :type 'number
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-unknown-max-level 1
  "Words at or below this level are found by `bltzread-next-unknown'."
  :type 'natnum
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-review-reject-level 4
  "Level a word falls back to when you reject it in `bltzread-review'."
  :type 'natnum
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-undo-limit 200
  "Number of changes `bltzread-undo' can take back in one buffer."
  :type 'natnum
  :group 'bltzread-levels :package-version '(bltzread . "0.1.0"))

;;; Skip lists and storage

(defcustom bltzread-known-write-policy 'unset
  "Whether words that become known are written to your bltzr skip lists.
`immediate' writes each word to the list the moment it reaches level 5, so
bltzr stops showing it.  `review' only queues it; nothing reaches bltzr
until you run `bltzread-review' and apply.  `unset' behaves like `review'
and warns once per session that you have not chosen (see
`bltzread-warn-known-write-unset')."
  :type '(choice (const unset) (const review) (const immediate))
  :safe #'symbolp
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-warn-known-write-unset t
  "Non-nil warns when `bltzread-known-write-policy' is still `unset'.
Choose a policy instead of turning this off."
  :type 'boolean
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-list-target 'exact-words
  "Which bltzr skip list receives words that become known.
`exact-words' adds the spelling you read; `word-families' adds its
dictionary basic word; `ask' asks each time."
  :type '(choice (const exact-words) (const word-families) (const ask))
  :safe #'symbolp
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-list-target-alist nil
  "Alist of (LANGUAGE-CODE . TARGET) overriding `bltzread-list-target'."
  :type '(alist :key-type string
                :value-type (choice (const exact-words) (const word-families) (const ask)))
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-known-writer-function #'bltzread-lists-write-known
  "Function that puts known words on a skip list.
Called with the language code, a list of (FORM . LEMMA) and the target
\(`exact-words', `word-families', `ask' or nil for the configured one).
Returns (FILE . ADDED), ADDED being the list of words really added."
  :type '(choice (function-item bltzread-lists-write-known) function)
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-skip-exact-words-files nil
  "Alist of (LANGUAGE-CODE . FILE) for exact-word skip lists.
Languages missing here use `skip_exact_words_file' from the bltzr
configuration file when it can be found."
  :type '(alist :key-type string :value-type file)
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-skip-word-families-files nil
  "Alist of (LANGUAGE-CODE . FILE) for word-family skip lists.
Languages missing here use `skip_word_families_file' from the bltzr
configuration file when it can be found."
  :type '(alist :key-type string :value-type file)
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-bltzr-config-file nil
  "Bltzr configuration file to read skip-list paths from, or nil.
Nil tries $BLITZER_CONFIG, then ~/.config/bltzr/bltzr.toml."
  :type '(choice (const nil) file)
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-skip-lists-count-as-known t
  "Non-nil treats words on the bltzr skip lists as level 5."
  :type 'boolean
  :set #'bltzread--set-reannotate
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defun bltzread--default-state-directory ()
  "The state directory: `bltzread/', or the older `lq/' if only that exists."
  (let ((new (locate-user-emacs-file "bltzread/"))
        (old (locate-user-emacs-file "lq/")))
    (if (and (file-directory-p old) (not (file-directory-p new))) old new)))

(defcustom bltzread-state-directory (bltzread--default-state-directory)
  "Directory holding one state file per language.
Progress saved by the earlier name of this mode, in `lq/', is still used
when `bltzread/' does not exist."
  :type 'directory
  :set #'bltzread--set-state-directory
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-save-delay 2
  "Seconds of idle time before changed state is written to disk."
  :type 'number
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-history-limit 200
  "Number of finished reading sessions remembered per language."
  :type 'natnum
  :group 'bltzread-lists :package-version '(bltzread . "0.1.0"))

;;; Extension hooks

(defcustom bltzread-level-change-functions nil
  "Abnormal hook run when the level of a word changes.
Called with KEY (the state key), FORM (a spelling of it), OLD and NEW
level, in the buffer.  Not run when the words are read in."
  :type 'hook
  :group 'bltzread :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-lookup-functions nil
  "Abnormal hook run when a word counts as looked up.
Called with KEY, FORM and the word's LEVEL, in the buffer.  Run once per
word and session."
  :type 'hook
  :group 'bltzread :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-session-finished-functions nil
  "Abnormal hook run after a reading session was recorded.
Called with one argument, the history entry: a plist with :time :buffer
:beg :end :words :counted :promoted :queued."
  :type 'hook
  :group 'bltzread :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-after-annotate-hook nil
  "Normal hook run in the buffer after its words were (re)read."
  :type 'hook
  :group 'bltzread :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-show-hook nil
  "Normal hook run in the buffer after the popup was shown."
  :type 'hook
  :group 'bltzread :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-hide-hook nil
  "Normal hook run in the buffer after the popup was hidden."
  :type 'hook
  :group 'bltzread :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-scroll-hook nil
  "Normal hook run, with the window selected, after the popup scrolled it.
A package that follows the window's start position can use it to learn that
this scroll was not the user's."
  :type 'hook
  :group 'bltzread :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-review-after-apply-hook nil
  "Normal hook run after the review is applied, so open buffers can redraw."
  :type 'hook
  :group 'bltzread :package-version '(bltzread . "0.1.0"))

;;; Popup

(defcustom bltzread-popup-trigger 'point
  "When the word popup appears.
`point' shows it when point rests on a word; `manual' only on
`bltzread-popup-at-point'."
  :type '(choice (const point) (const manual))
  :safe #'symbolp
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-only-on-motion t
  "Non-nil means a popup appears only after a command moved point onto a word.
Point that was moved by a timer or process, such as another package
scrolling a buffer for you, then never opens a popup or counts a lookup."
  :type 'boolean
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-delay 0.1
  "Seconds point must rest on a word before its popup appears."
  :type 'number
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-hide-delay 0.1
  "Seconds point must rest off the words before an open popup window closes."
  :type 'number
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-gap nil
  "Space between the word and the popup, in lines, or nil for the default.
The default is three quarters of a line."
  :type '(choice (const :tag "Default" nil) number)
  :safe (lambda (v) (or (null v) (numberp v)))
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-make-room 'pad
  "How the floating popup finds room above a word near the top of the window.
`pad' adds blank lines above the first visible line while the popup is up, so
the text is never scrolled and the window's start stays where it was.
`scroll' scrolls the text down instead."
  :type '(choice (const pad) (const scroll))
  :safe #'symbolp
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-on-scroll 'follow
  "What the floating popup does when the text moves under it.
`follow' keeps it above its word wherever the word goes, making room if the
word reaches the top.  `stay' leaves it where it was on the screen, even if
the word moves under or above it; it is only closed when the word leaves
the window."
  :type '(choice (const follow) (const stay))
  :safe #'symbolp
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-hide-after-set t
  "Non-nil hides the popup as soon as you set a word's level or ignore it.
It returns when point leaves the word and comes back, or on
`bltzread-popup-at-point'."
  :type 'boolean
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-style 'auto
  "How popups are drawn.
`auto' uses a floating box in graphical Emacs and a bottom window in a
terminal.  `box', `side-window' and `echo' force one way."
  :type '(choice (const auto) (const box) (const side-window) (const echo))
  :safe #'symbolp
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-show-gloss t
  "Non-nil includes the dictionary entry in the automatic popup."
  :type 'boolean
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-max-lines 8
  "Most dictionary lines shown in the automatic popup."
  :type 'natnum
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-lookup-max-lines 24
  "Most dictionary lines shown by `bltzread-lookup'."
  :type 'natnum
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-width 60
  "Widest popup text, in columns.  Never wider than the frame allows."
  :type 'natnum
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-wrap-function #'bltzread-popup-wrap
  "Function that wraps dictionary text for the popup.
Called with the text, the width and the most lines; returns a list of lines."
  :type '(choice (function-item bltzread-popup-wrap) function)
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-frame-parameters nil
  "Alist of frame parameters added to the floating popup's child frame."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-display-action nil
  "Action for `display-buffer' that places a popup shown as a window.
Nil means a bottom side window that fits its text."
  :type '(choice (const :tag "Bottom side window" nil) sexp)
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-hints
  '((bltzread-set-level . "level") (bltzread-ignore . "ignore")
    (bltzread-lookup . "lookup") (bltzread-note . "note")
    (bltzread-close-popup . "close"))
  "Commands listed in the last row of the popup, as (COMMAND . LABEL).
The key shown is the one that runs the command in the current buffer; a
command with no key is left out.  Can be made buffer-local."
  :type '(alist :key-type function :value-type string)
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-hint-separator "  "
  "Text between the entries of the hint row of the popup."
  :type 'string
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-popup-row-functions
  '(bltzread-card-title-rows bltzread-card-status-rows bltzread-card-note-rows
    bltzread-card-entry-rows bltzread-card-gloss-rows bltzread-card-hint-rows)
  "Functions that build the rows of the popup, top to bottom.
Each is called with a word overlay and FULL (non-nil for a long entry)
and returns a list of strings, possibly none."
  :type '(repeat function)
  :group 'bltzread-popup :package-version '(bltzread . "0.1.0"))

;;; Dictionary

(defcustom bltzread-dictionary-function #'bltzread-sdcv-lookup
  "Function returning dictionary text for a word.
Called with the language code and a list of words to try, most specific
first (the spelling, then its basic words).  It returns a plain string,
a list of strings (several entries, cycled with `bltzread-next-entry'), or nil
when nothing is found.  By convention an entry is the dictionary's name, a
newline and then the text, which may be HTML.  An error is reported with
`message' and not remembered."
  :type '(choice (function-item bltzread-sdcv-lookup) function)
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

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
     :follow F    for entries that only refer to another word, such as
                  \"see X\": when the filter finds nothing in an entry, the
                  entries of the words it links to are looked up and shown
                  in its place (the whole entry shows them below it).  F is
                  t for the \"bword://\" links that StarDict dictionaries
                  use, a regexp whose group 1 is the linked word, or a
                  function taking the raw HTML and returning a list of
                  words.  See `bltzread-dict-follow-depth'.
 - a function taking the raw HTML and returning a string (or nil), for
   anything the above cannot say.

Pieces are stripped of tags, have entities decoded and whitespace
collapsed; empty and repeated ones are dropped.  Pick only what you want
shown: the headword is already in the popup title.  Example:
  (setq bltzread-dict-filters
        \='((\"slv\" :class \"gloss\" :split \" · \")
          (\"pli\" . \"<div class=dpd><p>[^<]*<b>\\\\([^<]*\\\\)\")
          (\"pol\" :regexp \"class=\\\"trans\\\">\\\\([^<]*\\\\)\" :follow t)))"
  :type '(alist :key-type string
                :value-type (choice regexp function (plist :value-type sexp)))
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-dict-follow-depth 3
  "How many links in a row a dictionary entry is followed, at most.
Applies to the filters of `bltzread-dict-filters' that have :follow, as
when an entry says only \"see X\" and X says \"see Y\".  0 follows none."
  :type 'natnum
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-dict-failed-entries-last t
  "Non-nil shows dictionary entries your filter could not parse after the others."
  :type 'boolean
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-sdcv-executable "sdcv"
  "Name or path of the sdcv executable."
  :type 'string
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-sdcv-extra-args nil
  "Extra command-line arguments passed to every sdcv call."
  :type '(repeat string)
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-sdcv-dictionaries nil
  "Alist of (LANGUAGE-CODE . BOOKNAMES) limiting which dictionaries sdcv uses.
BOOKNAMES is a list of dictionary names as shown by `sdcv -l'.  A name
may also be the dictionary's folder name or part of its name, as long as
it is unambiguous.  What a missing language searches is set by
`bltzread-sdcv-unlisted-languages'; see also \\[bltzread-dict-diagnose]."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-sdcv-data-dir-alist nil
  "Alist of (LANGUAGE-CODE . DIRECTORY) overriding `bltzread-sdcv-data-dir'.
Useful when one language's dictionary lives apart, for example a
re-indexed copy made by tools/sdcv-fix-index.py."
  :type '(alist :key-type string :value-type directory)
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-sdcv-unlisted-languages 'none
  "What a language missing from `bltzread-sdcv-dictionaries' searches.
`none' looks nothing up (safe: no entries from another language);
`all' searches every dictionary."
  :type '(choice (const none) (const all))
  :safe #'symbolp
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-sdcv-data-dir nil
  "Directory of StarDict dictionaries passed to sdcv, or nil for its default."
  :type '(choice (const nil) directory)
  :group 'bltzread-dictionary :package-version '(bltzread . "0.1.0"))

;;; Keys

(defcustom bltzread-prefix-key "s"
  "Prefix key of the commands in `bltzread-command-map', or nil for none.
Like the single keys of `bltzread-mode-map' it works only in a read-only
buffer.  The default is the one-letter prefix subtle's rarer commands share,
so both packages read alike in a subtitle buffer."
  :type '(choice (const nil) key-sequence)
  :set #'bltzread--set-prefix-key
  :group 'bltzread-keys :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-mouse-lookup-key "<double-mouse-1>"
  "Mouse event that looks a word up, or nil for none."
  :type '(choice (const nil) key-sequence)
  :set #'bltzread--set-mouse-key
  :group 'bltzread-keys :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-mouse-popup-key "<mouse-1>"
  "Mouse event that opens a word's popup, or nil for none.
It opens the popup even when point was already on the word."
  :type '(choice (const nil) key-sequence)
  :set #'bltzread--set-mouse-key
  :group 'bltzread-keys :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-mouse-click-counts-as-lookup t
  "Non-nil counts a click that opens a popup as a lookup of the word."
  :type 'boolean
  :group 'bltzread-keys :package-version '(bltzread . "0.1.0"))

;;; Reading sessions

(defcustom bltzread-prompt-on-close t
  "Non-nil asks to record your reading when the buffer is killed."
  :type 'boolean
  :group 'bltzread-session :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-min-chars-for-prompt 300
  "Characters you must have moved through before the close prompt appears."
  :type 'natnum
  :group 'bltzread-session :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-bookmark-policy 'ask
  "What to do with the bookmark after recording a reading session.
`ask' asks, `always' moves it, `never' leaves it."
  :type '(choice (const ask) (const always) (const never))
  :safe #'symbolp
  :group 'bltzread-session :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-bookmark-name-function #'bltzread-default-bookmark-name
  "Function returning the bookmark name for the current buffer."
  :type '(choice (function-item bltzread-default-bookmark-name) function)
  :group 'bltzread-session :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-range-method 'farthest
  "How you tell bltzread which part of the text you read.
`farthest' proposes the start marker to the farthest point you
reached; `points' uses start and end marks you set; `paragraphs' and
`lines' number the text and ask for from/to.  \\[bltzread-range-change-method]
changes it for one buffer."
  :type '(choice (const farthest) (const points)
                 (const paragraphs) (const lines))
  :safe #'symbolp
  :group 'bltzread-session :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-range-farthest-source 'window-end
  "What counts as having reached a place in the `farthest' method.
`window-end' is the bottom of the text on screen; `point' is the cursor.
A function, called with no argument in the buffer, returns the position
reached, or nil; the cursor then plays no part.  Something that plays the
text out loud can use this to count only what was actually heard."
  :type '(choice (const window-end) (const point) function)
  :group 'bltzread-session :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-range-start-function nil
  "Function returning where the `farthest' range begins, or nil for the usual.
Used with a function as `bltzread-range-farthest-source', when what was read
is a run of text that need not begin at the top."
  :type '(choice (const nil) function)
  :group 'bltzread-session :package-version '(bltzread . "0.1.0"))

(define-obsolete-variable-alias 'bltzread-range-confirm 'bltzread-session-confirm "0.1.0")
(defcustom bltzread-session-confirm t
  "Non-nil asks for confirmation after showing the chosen range."
  :type 'boolean
  :group 'bltzread-session :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-review-confirm t
  "Non-nil asks before `bltzread-review-apply' writes words to a skip list."
  :type 'boolean
  :group 'bltzread-session :package-version '(bltzread . "0.1.0"))

;;; Pasted text

(defcustom bltzread-paste-font-family nil
  "Font family of text opened by `bltzread-read-text', or nil for the default."
  :type '(choice (const nil) string)
  :group 'bltzread-paste :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-paste-font-height 1.3
  "Size of text opened by `bltzread-read-text', relative to the default."
  :type 'number
  :group 'bltzread-paste :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-paste-line-spacing 0.4
  "Extra space between lines in `bltzread-read-text', as a fraction of line height."
  :type 'number
  :group 'bltzread-paste :package-version '(bltzread . "0.1.0"))

(defcustom bltzread-paste-margin 8
  "Blank columns on each side of text opened by `bltzread-read-text'."
  :type 'natnum
  :group 'bltzread-paste :package-version '(bltzread . "0.1.0"))

;;; Faces

(defface bltzread-level-1
  '((((background light)) :background "#ffd54a")
    (((background dark)) :background "#6b5200"))
  "Level 1: a new word."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-level-2
  '((((background light)) :background "#ffe38a")
    (((background dark)) :background "#4f3d00"))
  "Level 2: a recognized word."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-level-3
  '((((background light)) :background "#fff0b8")
    (((background dark)) :background "#3a2e00"))
  "Level 3: a familiar word."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-level-4
  '((t :underline (:color "#b0a060" :style line)))
  "Level 4: a learned word."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-level-5
  '((t))
  "Level 5: a known word.  Empty, so known words look plain."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-ignored
  '((t :inherit shadow))
  "A word you chose to ignore, such as a name."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-review-exposure
  '((((background light)) :background "#bfe3b4" :weight bold)
    (((background dark)) :background "#2f5a2a" :weight bold))
  "Review-list rows of words that reached level 5 by reading exposure."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-range
  '((t :inherit highlight))
  "The range picked as read."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-number
  '((t :inherit shadow))
  "Paragraph numbers shown while picking a range."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-popup
  '((((background light)) :background "#f4efe0" :foreground "#2b2b2b")
    (((background dark)) :background "#2a2a2e" :foreground "#e6e6e6"))
  "Body of the word popup."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-popup-border
  '((t :inherit bltzread-popup :foreground "#8a8a8a"))
  "Border of the word popup."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-popup-title
  '((t :inherit bltzread-popup :weight bold))
  "Title row of the word popup."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-popup-key
  '((t :inherit bltzread-popup :box (:line-width -1) :weight bold))
  "Key caps in the hint row of the word popup."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-popup-dim
  '((t :inherit bltzread-popup :foreground "#8a8a8a"))
  "Secondary text in the word popup."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(defface bltzread-popup-dict-name
  '((t :inherit bltzread-popup-dim :slant italic))
  "Name of the dictionary at the head of an entry in the word popup."
  :group 'bltzread-faces :package-version '(bltzread . "0.1.0"))

(provide 'bltzread-custom)
;;; bltzread-custom.el ends here
