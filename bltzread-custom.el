;;; bltzread-custom.el --- Options and faces for bltzread -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;;; Commentary:

;; Every user-facing knob of bltzread lives here so the other files can depend
;; on it without depending on each other.  Every default is a default,
;; not a rule: change anything with M-x customize-group RET bltzread.

;;; Code:

(declare-function bltzread-sdcv-lookup "bltzread-dict")
(declare-function bltzread-default-bookmark-name "bltzread")

(defgroup bltzread nil
  "Reading mode on top of bltzr that colours words by how well you know them."
  :group 'applications
  :prefix "bltzread-")

;;; bltzr and language

(defcustom bltzread-bltzr-executable "bltzr"
  "Name or path of the bltzr executable."
  :type 'string)

(defcustom bltzread-bltzr-extra-args nil
  "Extra command-line arguments passed to every bltzr call."
  :type '(repeat string))

(defcustom bltzread-default-language nil
  "Three-letter language code used when a buffer has none, or nil to ask.
A buffer can also set `bltzread-language' itself, for example as a file-local
variable."
  :type '(choice (const :tag "Ask" nil) string))

(defcustom bltzread-detect-sample-size 20000
  "Characters from the start of a buffer given to bltzr to detect its language."
  :type 'integer)

(defcustom bltzread-word-regexp "\\w+\\(?:[-'’]\\w+\\)*"
  "Regexp matching one word in the text.
Languages that write without spaces need their own entry in
`bltzread-word-regexp-alist'."
  :type 'regexp)

(defcustom bltzread-word-regexp-alist nil
  "Alist of (LANGUAGE-CODE . REGEXP) overriding `bltzread-word-regexp'."
  :type '(alist :key-type string :value-type regexp))

(defcustom bltzread-level-scope 'word
  "What a level you set by hand (1-5, ignore) applies to.
`word' sets the whole word: every form sharing its basic word changes
together.  `form' sets only the spelling you are on.  Clean reads from
reading sessions always count for the single form."
  :type '(choice (const word) (const form)))

(defcustom bltzread-skip-numerals t
  "Non-nil never treats a number (2, 24, 5.26, 2,81, 1990-1995) as a word.
A word is skipped when it contains no letter."
  :type 'boolean)

(defcustom bltzread-track-by 'form
  "What a level belongs to.
`form' tracks each spelling separately (matches bltzr's exact-word
list).  `lemma' tracks the dictionary basic word, so all its forms share
one level."
  :type '(choice (const form) (const lemma)))

(defcustom bltzread-make-read-only t
  "Non-nil makes the buffer read-only while `bltzread-mode' is on."
  :type 'boolean)

;;; Levels and promotion

(defcustom bltzread-promotion-thresholds '(3 3 4 5)
  "Clean reads needed to climb from level 1, 2, 3 and 4 to the next.
A clean read is a reading session in which you did not look the word up."
  :type '(repeat integer))

(defcustom bltzread-count-each-occurrence nil
  "Non-nil counts every occurrence of a word in the range you read.
Nil counts each word at most once per reading session."
  :type 'boolean)

(defcustom bltzread-lookup-effect 'reset-reads
  "What looking a word up does to it.
`reset-reads' restarts its clean-read count; `demote' also drops it one
level (never below 1); `nothing' only records the lookup."
  :type '(choice (const reset-reads) (const demote) (const nothing)))

(defcustom bltzread-lookup-counts 'command
  "What counts as looking a word up.
`command' means only `bltzread-lookup'.  `interaction' also counts asking for
more from the popup (`bltzread-next-entry', `bltzread-previous-entry',
`bltzread-popup-at-point').  `popup' also counts the automatic popup, but only
once point has stayed on the word for `bltzread-lookup-dwell' seconds, so
scrolling or jumping past a word does not count."
  :type '(choice (const command) (const interaction) (const popup)))

(defcustom bltzread-lookup-dwell 1.5
  "Seconds point must stay on a word before its popup counts as a lookup.
Only used when `bltzread-lookup-counts' is `popup'."
  :type 'number)

(defcustom bltzread-unknown-max-level 1
  "Words at or below this level are found by `bltzread-next-unknown'."
  :type 'integer)

;;; Undo

(defcustom bltzread-undo-limit 200
  "Number of changes `bltzread-undo' can take back in one buffer."
  :type 'integer)

;;; Skip lists

(defcustom bltzread-known-write-policy 'unset
  "Whether words that become known are written to your bltzr skip lists.
`immediate' writes each word to the list the moment it reaches level 5, so
bltzr stops showing it.  `review' only queues it; nothing reaches bltzr
until you run `bltzread-review' and apply.  `unset' behaves like `review'
and warns once per session that you have not chosen (see
`bltzread-warn-known-write-unset')."
  :type '(choice (const unset) (const review) (const immediate)))

(defcustom bltzread-warn-known-write-unset t
  "Non-nil warns when `bltzread-known-write-policy' is still `unset'.
Choose a policy instead of turning this off."
  :type 'boolean)

(defcustom bltzread-list-target 'exact-words
  "Which bltzr skip list receives words that become known.
`exact-words' adds the spelling you read; `word-families' adds its
dictionary basic word; `ask' asks each time."
  :type '(choice (const exact-words) (const word-families) (const ask)))

(defcustom bltzread-list-target-alist nil
  "Alist of (LANGUAGE-CODE . TARGET) overriding `bltzread-list-target'."
  :type '(alist :key-type string
                :value-type (choice (const exact-words) (const word-families) (const ask))))

(defcustom bltzread-skip-exact-words-files nil
  "Alist of (LANGUAGE-CODE . FILE) for exact-word skip lists.
Languages missing here use `skip_exact_words_file' from the bltzr
configuration file when it can be found."
  :type '(alist :key-type string :value-type file))

(defcustom bltzread-skip-word-families-files nil
  "Alist of (LANGUAGE-CODE . FILE) for word-family skip lists.
Languages missing here use `skip_word_families_file' from the bltzr
configuration file when it can be found."
  :type '(alist :key-type string :value-type file))

(defcustom bltzread-bltzr-config-file nil
  "bltzr configuration file to read skip-list paths from, or nil.
Nil tries $BLITZER_CONFIG, then ~/.config/bltzr/bltzr.toml."
  :type '(choice (const nil) file))

(defcustom bltzread-skip-lists-count-as-known t
  "Non-nil treats words on the bltzr skip lists as level 5."
  :type 'boolean)

;;; Storage

(defun bltzread--default-state-directory ()
  "The state directory: `bltzread/', or the older `lq/' if only that exists."
  (let ((new (locate-user-emacs-file "bltzread/"))
        (old (locate-user-emacs-file "lq/")))
    (if (and (file-directory-p old) (not (file-directory-p new))) old new)))

(defcustom bltzread-state-directory (bltzread--default-state-directory)
  "Directory holding one state file per language.
Progress saved by the earlier name of this mode, in `lq/', is still used
when `bltzread/' does not exist."
  :type 'directory)

(defcustom bltzread-save-delay 2
  "Seconds of idle time before changed state is written to disk."
  :type 'number)

(defcustom bltzread-history-limit 200
  "Number of finished reading sessions remembered per language."
  :type 'integer)

;;; Popup and dictionary

(defcustom bltzread-popup-trigger 'point
  "When the word popup appears.
`point' shows it when point rests on a word; `manual' only on
`bltzread-popup-at-point'."
  :type '(choice (const point) (const manual)))

(defcustom bltzread-popup-only-on-motion t
  "Non-nil means a popup appears only after a command moved point onto a word.
Point that was moved by a timer or process, such as another package
scrolling a buffer for you, then never opens a popup or counts a lookup."
  :type 'boolean
  :group 'bltzread)

(defcustom bltzread-popup-delay 0.1
  "Seconds point must rest on a word before its popup appears."
  :type 'number)

(defcustom bltzread-popup-gap nil
  "Space between the word and the popup, in lines, or nil for the default.
The default is three quarters of a line."
  :type '(choice (const :tag "Default" nil) number))

(defcustom bltzread-popup-hide-after-set t
  "Non-nil hides the popup as soon as you set a word's level or ignore it.
It returns when point leaves the word and comes back, or on
`bltzread-popup-at-point'."
  :type 'boolean)

(defcustom bltzread-popup-style 'auto
  "How popups are drawn.
`auto' uses a floating box in graphical Emacs and a bottom window in a
terminal.  `box', `side-window' and `echo' force one way."
  :type '(choice (const auto) (const box) (const side-window) (const echo)))

(defcustom bltzread-popup-show-gloss t
  "Non-nil includes the dictionary entry in the automatic popup."
  :type 'boolean)

(defcustom bltzread-popup-max-lines 8
  "Most dictionary lines shown in the automatic popup."
  :type 'integer)

(defcustom bltzread-popup-lookup-max-lines 24
  "Most dictionary lines shown by `bltzread-lookup'."
  :type 'integer)

(defcustom bltzread-popup-width 60
  "Widest popup text, in columns."
  :type 'integer)

(defcustom bltzread-dictionary-function #'bltzread-sdcv-lookup
  "Function returning dictionary text for a word.
Called with the language code and a list of words to try, most specific
first (the spelling, then its basic words).  It returns a plain string,
a list of strings (several entries, cycled with `bltzread-next-entry'), or nil
when nothing is found."
  :type 'function)

(defcustom bltzread-sdcv-executable "sdcv"
  "Name or path of the sdcv executable."
  :type 'string)

(defcustom bltzread-sdcv-dictionaries nil
  "Alist of (LANGUAGE-CODE . BOOKNAMES) limiting which dictionaries sdcv uses.
BOOKNAMES is a list of dictionary names as shown by `sdcv -l'.  A name
may also be the dictionary's folder name or part of its name, as long as
it is unambiguous.  What a missing language searches is set by
`bltzread-sdcv-unlisted-languages'; see also \\[bltzread-dict-diagnose]."
  :type '(alist :key-type string :value-type (repeat string)))

(defcustom bltzread-sdcv-data-dir-alist nil
  "Alist of (LANGUAGE-CODE . DIRECTORY) overriding `bltzread-sdcv-data-dir'.
Useful when one language's dictionary lives apart, for example a
re-indexed copy made by tools/sdcv-fix-index.py."
  :type '(alist :key-type string :value-type directory))

(defcustom bltzread-sdcv-unlisted-languages 'none
  "What a language missing from `bltzread-sdcv-dictionaries' searches.
`none' looks nothing up (safe: no entries from another language);
`all' searches every dictionary."
  :type '(choice (const none) (const all)))

(defcustom bltzread-sdcv-data-dir nil
  "Directory of StarDict dictionaries passed to sdcv, or nil for its default."
  :type '(choice (const nil) directory))

;;; Keys

(defcustom bltzread-prefix-key "s"
  "Prefix key of the bltzread commands, in `kbd' syntax, or nil for none.
The default is the one-letter prefix subtle's rarer commands share, so both
packages read alike in a subtitle buffer."
  :type '(choice (const nil) string))

(defcustom bltzread-direct-keys t
  "Non-nil binds the keys in `bltzread-direct-key-bindings' without a prefix."
  :type 'boolean)

(defcustom bltzread-direct-key-bindings
  '(("1" . bltzread-set-level) ("2" . bltzread-set-level) ("3" . bltzread-set-level)
    ("4" . bltzread-set-level) ("5" . bltzread-set-level)
    ("i" . bltzread-ignore) ("l" . bltzread-lookup) ("N" . bltzread-note)
    ("u" . bltzread-undo) ("c" . bltzread-close-popup)
    ("d" . bltzread-next-entry) ("D" . bltzread-previous-entry)
    ("}" . bltzread-next-unknown) ("{" . bltzread-previous-unknown))
  "Alist of (KEY . COMMAND) active without the prefix while `bltzread-mode' is on."
  :type '(alist :key-type string :value-type function))

(defcustom bltzread-mouse-lookup-key "<double-mouse-1>"
  "Mouse event, in `kbd' syntax, that looks a word up, or nil for none."
  :type '(choice (const nil) string))

;;; Reading sessions

(defcustom bltzread-prompt-on-close t
  "Non-nil asks to record your reading when the buffer is killed."
  :type 'boolean)

(defcustom bltzread-min-chars-for-prompt 300
  "Characters you must have moved through before the close prompt appears."
  :type 'integer)

(defcustom bltzread-bookmark-policy 'ask
  "What to do with the bookmark after recording a reading session.
`ask' asks, `always' moves it, `never' leaves it."
  :type '(choice (const ask) (const always) (const never)))

(defcustom bltzread-bookmark-name-function #'bltzread-default-bookmark-name
  "Function returning the bookmark name for the current buffer."
  :type 'function)

(defcustom bltzread-range-method 'farthest
  "How you tell bltzread which part of the text you read.
`farthest' proposes the start marker to the farthest point you
reached; `points' uses start and end marks you set; `paragraphs' and
`lines' number the text and ask for from/to."
  :type '(choice (const farthest) (const points)
                 (const paragraphs) (const lines)))

(defcustom bltzread-range-farthest-source 'window-end
  "What counts as having reached a place in the `farthest' method.
`window-end' is the bottom of the text on screen; `point' is the cursor.
A function, called with no argument in the buffer, returns the position
reached, or nil; the cursor then plays no part.  Something that plays the
text out loud can use this to count only what was actually heard."
  :type '(choice (const window-end) (const point) function))

(defcustom bltzread-range-start-function nil
  "Function returning where the `farthest' range begins, or nil for the usual.
Used with a function as `bltzread-range-farthest-source', when what was read
is a run of text that need not begin at the top."
  :type '(choice (const nil) function))

(defcustom bltzread-range-confirm t
  "Non-nil asks for confirmation after showing the chosen range."
  :type 'boolean)

;;; Pasted text

(defcustom bltzread-paste-font-family nil
  "Font family of text opened by `bltzread-read-text', or nil for the default."
  :type '(choice (const nil) string))

(defcustom bltzread-paste-font-height 1.3
  "Size of text opened by `bltzread-read-text', relative to the default."
  :type 'number)

(defcustom bltzread-paste-line-spacing 0.4
  "Extra space between lines in `bltzread-read-text', as a fraction of line height."
  :type 'number)

(defcustom bltzread-paste-margin 8
  "Blank columns on each side of text opened by `bltzread-read-text'."
  :type 'integer)

;;; Faces

(defface bltzread-level-1
  '((((background light)) :background "#ffd54a")
    (((background dark)) :background "#6b5200"))
  "Level 1: a new word.")

(defface bltzread-level-2
  '((((background light)) :background "#ffe38a")
    (((background dark)) :background "#4f3d00"))
  "Level 2: a recognized word.")

(defface bltzread-level-3
  '((((background light)) :background "#fff0b8")
    (((background dark)) :background "#3a2e00"))
  "Level 3: a familiar word.")

(defface bltzread-level-4
  '((t :underline (:color "#b0a060" :style line)))
  "Level 4: a learned word.")

(defface bltzread-ignored
  '((t :inherit shadow))
  "A word you chose to ignore, such as a name.")

(defface bltzread-range
  '((t :inherit highlight))
  "The range picked as read.")

(defface bltzread-number
  '((t :inherit shadow))
  "Paragraph numbers shown while picking a range.")

(defface bltzread-popup
  '((((background light)) :background "#f4efe0" :foreground "#2b2b2b")
    (((background dark)) :background "#2a2a2e" :foreground "#e6e6e6"))
  "Body of the word popup.")

(defface bltzread-popup-border
  '((t :inherit bltzread-popup :foreground "#8a8a8a"))
  "Border of the word popup.")

(defface bltzread-popup-title
  '((t :inherit bltzread-popup :weight bold))
  "Title row of the word popup.")

(defface bltzread-popup-key
  '((t :inherit bltzread-popup :box (:line-width -1) :weight bold))
  "Key caps in the word popup.")

(defface bltzread-popup-dim
  '((t :inherit bltzread-popup :foreground "#8a8a8a"))
  "Secondary text in the word popup.")

(provide 'bltzread-custom)
;;; bltzread-custom.el ends here
