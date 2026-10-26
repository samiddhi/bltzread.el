;;; bltzread-tests.el --- Batch tests for bltzread -*- lexical-binding: t; -*-

;; Run: emacs --batch -L . -l bltzread-tests.el -f ert-run-tests-batch-and-exit
;; bltzr and sdcv are stubbed, and nothing touches your real state or skip lists.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bltzread)

(defvar bltzread-test--lemmas '(("hiša" . "hiša") ("hiše" . "hiša") ("hišo" . "hiša")
                          ("hišami" . "hiša"))
  "Spelling -> basic word for the stub of bltzr.")

(defmacro bltzread-test-with-buffer (text &rest body)
  "Run BODY in a buffer holding TEXT, `bltzread-mode' on, bltzr and sdcv stubbed."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "bltzread-test" t))
          (bltzread-state-directory dir)
          (bltzread-state--table (make-hash-table :test 'equal))
          (bltzread-skip-exact-words-files `(("tst" . ,(expand-file-name "exact.txt" dir))))
          (bltzread-skip-word-families-files `(("tst" . ,(expand-file-name "fam.txt" dir))))
          (bltzread-bltzr-config-file (expand-file-name "none.toml" dir))
          (bltzread-default-language "tst")
          (bltzread-popup-style 'echo)
          (bltzread-prompt-on-close nil)
          (bltzread-save-delay 1000)
          (bltzread-dictionary-function (lambda (_l _w) nil))
          (bltzread-dict--cache (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'bltzread-bltzr-lemmas)
                (lambda (_lang forms)
                  (let ((h (make-hash-table :test 'equal)))
                    (dolist (f forms)
                      (puthash f (list (or (cdr (assoc f bltzread-test--lemmas)) f)) h))
                    h))))
       (with-temp-buffer
         (insert ,text)
         (goto-char (point-min))
         (bltzread-mode 1)
         (unwind-protect (progn ,@body)
           (bltzread-mode -1)
           (delete-directory dir t))))))

(defun bltzread-test-ov (form &optional n)
  "The Nth (default first) word overlay of FORM."
  (nth (or n 0) (seq-filter (lambda (o) (equal (overlay-get o 'bltzread-form) form))
                            (bltzread--all-words))))

(defun bltzread-test-go (form &optional n)
  "Put point on the Nth FORM."
  (goto-char (overlay-start (bltzread-test-ov form n))))

(defun bltzread-test-level (form)
  (overlay-get (bltzread-test-ov form) 'bltzread-level))

;;; Words

(ert-deftest bltzread-numerals-are-not-words ()
  (with-temp-buffer
    (insert "2 24 5.26 2,81 1990-1995 COVID-19 3rd Hiša")
    (should (equal (mapcar #'caddr (bltzread-bltzr-tokenize (point-min) (point-max) bltzread-word-regexp))
                   '("covid-19" "3rd" "hiša")))
    (let ((bltzread-skip-numerals nil))
      (should (member "24" (mapcar #'caddr
                                   (bltzread-bltzr-tokenize (point-min) (point-max) bltzread-word-regexp)))))))

(ert-deftest bltzread-numerals-get-no-overlay ()
  (bltzread-test-with-buffer "Hiša 24 hiše 5.26 2,81."
    (should (equal (mapcar (lambda (o) (overlay-get o 'bltzread-form)) (bltzread--all-words))
                   '("hiša" "hiše")))))

;;; Levels

(ert-deftest bltzread-level-applies-to-whole-word ()
  (bltzread-test-with-buffer "hiša hiše hišo kruh"
    (bltzread-test-go "hiše")
    (bltzread-set-level 3)
    (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše" "hišo")) '(3 3 3)))
    (should (eq (bltzread-test-level "kruh") 1))))

(ert-deftest bltzread-level-scope-form ()
  (let ((bltzread-level-scope 'form))
    (bltzread-test-with-buffer "hiša hiše hišo"
      (bltzread-test-go "hiše")
      (bltzread-set-level 3)
      (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše" "hišo")) '(1 3 1))))))

(ert-deftest bltzread-reads-count-for-the-form-only ()
  (let ((bltzread-promotion-thresholds '(1 1 1 1))
        (bltzread-range-method 'points)
        (bltzread-range-confirm nil)
        (bltzread-bookmark-policy 'never))
    (bltzread-test-with-buffer "hiša hiše hišo"
      (bltzread-test-go "hiše")
      (bltzread-set-level 2)
      (bltzread-test-go "hiša")
      (bltzread-range-set-start)
      (goto-char (point-max))
      (bltzread-range-set-end)
      (bltzread-finish-session)
      ;; every form climbed on its own: 2 -> 3
      (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše" "hišo")) '(3 3 3)))
      (bltzread-test-go "hiša")
      (bltzread-set-level 1)
      (bltzread-test-go "hiše")
      (bltzread-range-set-start)
      (goto-char (overlay-end (bltzread-test-ov "hiše")))
      (bltzread-range-set-end)
      (bltzread-finish-session)
      ;; only "hiše" was in the range, so only it moved up
      (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše" "hišo")) '(1 2 1))))))

(ert-deftest bltzread-ignore-toggles-whole-word ()
  (bltzread-test-with-buffer "hiša hiše"
    (bltzread-test-go "hiša")
    (bltzread-set-level 3)
    (bltzread-ignore)
    (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše")) '(ignored ignored)))
    (bltzread-test-go "hiše")
    (bltzread-ignore)
    (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše")) '(3 3)))))

(ert-deftest bltzread-ignore-persists ()
  (bltzread-test-with-buffer "hiša kruh"
    (bltzread-test-go "kruh")
    (bltzread-ignore)
    (bltzread-state-save bltzread--state)
    (clrhash bltzread-state--table)
    (let ((state (bltzread-state-get "tst")))
      (should (eq (bltzread-state-level state "kruh" "kruh" '("kruh")) 'ignored)))))

;;; Undo

(ert-deftest bltzread-undo-roundtrips-levels-and-ignore ()
  (bltzread-test-with-buffer "hiša hiše hišo"
    (bltzread-test-go "hiše")
    (bltzread-set-level 4)
    (bltzread-set-level 2)
    (bltzread-undo)
    (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše" "hišo")) '(4 4 4)))
    (bltzread-undo)
    (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše" "hišo")) '(1 1 1)))
    (bltzread-ignore)
    (bltzread-undo)
    (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše" "hišo")) '(1 1 1)))
    (should-error (bltzread-undo) :type 'user-error)))

(ert-deftest bltzread-undo-takes-back-review-queue ()
  (bltzread-test-with-buffer "hiša hiše"
    (bltzread-test-go "hiša")
    (bltzread-set-level 5)
    (should (equal (sort (mapcar #'car (bltzread-state-queue bltzread--state)) #'string<)
                   '("hiša" "hiše")))
    (bltzread-undo)
    (should (null (bltzread-state-queue bltzread--state)))))

(ert-deftest bltzread-undo-note ()
  (bltzread-test-with-buffer "hiša"
    (bltzread-test-go "hiša")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "house")))
      (bltzread-note))
    (should (equal (bltzread-state-note bltzread--state "hiša") "house"))
    (bltzread-undo)
    (should (null (bltzread-state-note bltzread--state "hiša")))))

;;; Lookups

(ert-deftest bltzread-lookup-counts-once-per-session ()
  (let ((bltzread-lookup-effect 'demote))
    (bltzread-test-with-buffer "hiša"
      (bltzread-test-go "hiša")
      (bltzread-set-level 3)
      (let ((depth (length bltzread--undo)))
        (bltzread-lookup)
        (bltzread-lookup)
        (bltzread-lookup)
        (should (= (length bltzread--undo) (1+ depth))))
      (should (eq (bltzread-test-level "hiša") 2))
      (bltzread-undo)
      (should (eq (hash-table-count bltzread--lookups) 0))
      (should (eq (bltzread-test-level "hiša") 3)))))

(ert-deftest bltzread-interaction-counting ()
  (let ((bltzread-lookup-counts 'interaction)
        (bltzread-popup-trigger 'manual))
    (bltzread-test-with-buffer "hiša kruh"
      (bltzread-test-go "hiša")
      (bltzread-popup-at-point)
      (should (gethash "hiša" bltzread--lookups))
      (let ((bltzread-lookup-counts 'command))
        (bltzread-test-go "kruh")
        (bltzread-popup-at-point)
        (should-not (gethash "kruh" bltzread--lookups))))))

(ert-deftest bltzread-lookup-undo-lets-word-count-again ()
  (let ((bltzread-promotion-thresholds '(1 1 1 1))
        (bltzread-range-method 'points) (bltzread-range-confirm nil) (bltzread-bookmark-policy 'never))
    (bltzread-test-with-buffer "hiša kruh"
      (bltzread-test-go "hiša")
      (bltzread-lookup)
      (bltzread-undo)
      (bltzread-range-set-start)
      (goto-char (point-max))
      (bltzread-range-set-end)
      (bltzread-finish-session)
      (should (eq (bltzread-test-level "hiša") 2)))))

;;; Reading sessions

(ert-deftest bltzread-finish-session-and-undo ()
  (let ((bltzread-promotion-thresholds '(1 1 1 1))
        (bltzread-range-method 'points) (bltzread-range-confirm nil) (bltzread-bookmark-policy 'never))
    (bltzread-test-with-buffer "alfa beta gama"
      (bltzread-test-go "beta")
      (bltzread-lookup)
      (bltzread-test-go "alfa")
      (bltzread-range-set-start)
      (goto-char (point-max))
      (bltzread-range-set-end)
      (bltzread-finish-session)
      (should (equal (mapcar #'bltzread-test-level '("alfa" "beta" "gama")) '(2 1 2)))
      (should (= (length (bltzread-state-history bltzread--state)) 1))
      (bltzread-undo)
      (should (equal (mapcar #'bltzread-test-level '("alfa" "beta" "gama")) '(1 1 1)))
      (should (null (bltzread-state-history bltzread--state)))
      (should (gethash "beta" bltzread--lookups)))))

(ert-deftest bltzread-mark-known-whole-word-queues-forms ()
  (let ((bltzread-range-method 'points) (bltzread-range-confirm nil) (bltzread-bookmark-policy 'never))
    (bltzread-test-with-buffer "hiša hiše kruh"
      (bltzread-test-go "hiša")
      (bltzread-range-set-start)
      (goto-char (point-max))
      (bltzread-range-set-end)
      (bltzread-mark-range-known)
      (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše" "kruh")) '(5 5 5)))
      (bltzread-undo)
      (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše" "kruh")) '(1 1 1)))
      (should (null (bltzread-state-queue bltzread--state))))))

;;; State files

(ert-deftest bltzread-state-roundtrip-keeps-whole-word-level ()
  (bltzread-test-with-buffer "hiša hiše"
    (bltzread-test-go "hiše")
    (bltzread-set-level 3)
    (bltzread-state-save bltzread--state)
    (clrhash bltzread-state--table)
    (let ((state (bltzread-state-get "tst")))
      (should (eq (bltzread-state-level state "hiša" "hiša" '("hiša")) 3))
      (should (eq (bltzread-state-level state "hišo" "hišo" '("hiša")) 3)))))

(ert-deftest bltzread-state-loads-version-1-file ()
  (let* ((dir (make-temp-file "bltzread-test" t))
         (bltzread-state-directory dir)
         (bltzread-state--table (make-hash-table :test 'equal))
         (bltzread-bltzr-config-file (expand-file-name "none.toml" dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "old.eld" dir)
            (prin1 '(:version 1 :words (("hiša" :level 4 :reads 0) ("kruh" :level ignored))
                              :queue nil :history nil)
                   (current-buffer)))
          (let ((state (bltzread-state-get "old")))
            (should (eq (bltzread-state-level state "hiša" "hiša" '("hiša")) 4))
            (should (eq (bltzread-state-level state "kruh" "kruh" '("kruh")) 'ignored))
            (should (eq (bltzread-state-level state "mleko" "mleko" '("mleko")) 1))))
      (delete-directory dir t))))

;;; Popup logic

(ert-deftest bltzread-set-level-dismisses-popup-until-point-returns ()
  (bltzread-test-with-buffer "hiša kruh"
    (bltzread-test-go "hiša")
    (let ((ov (bltzread-test-ov "hiša")))
      (bltzread-set-level 3)
      (should (eq bltzread--popup-dismissed ov))
      (bltzread--schedule-popup)
      (should (eq bltzread--popup-dismissed ov))
      (should-not (timerp bltzread--popup-timer))
      (bltzread-test-go "kruh")
      (bltzread--schedule-popup)
      (should (null bltzread--popup-dismissed))
      (bltzread-test-go "hiša")
      (bltzread--schedule-popup)
      (should (timerp bltzread--popup-timer))
      (bltzread-set-level 4)
      (bltzread-popup-at-point)
      (should (null bltzread--popup-dismissed)))))

(ert-deftest bltzread-hide-after-set-can-be-turned-off ()
  (let ((bltzread-popup-hide-after-set nil))
    (bltzread-test-with-buffer "hiša"
      (bltzread-test-go "hiša")
      (bltzread-set-level 3)
      (should (null bltzread--popup-dismissed)))))

(ert-deftest bltzread-close-popup-then-next-word ()
  (bltzread-test-with-buffer "hiša kruh"
    (bltzread-test-go "hiša")
    (bltzread-close-popup)
    (should (eq bltzread--popup-dismissed (bltzread-test-ov "hiša")))
    (bltzread-test-go "kruh")
    (bltzread--schedule-popup)
    (should (null bltzread--popup-dismissed))))

(ert-deftest bltzread-popup-hides-when-buffer-changes ()
  (bltzread-test-with-buffer "hiša"
    (let ((bltzread-popup--owner (current-buffer)))
      (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (get-buffer-create "*other*")))
                ((symbol-function 'bltzread-popup-hide) (lambda () (setq bltzread-popup--owner nil))))
        (bltzread-popup--sync)
        (should (null bltzread-popup--owner))))))

;;; Moving

(ert-deftest bltzread-jump-over-unknown-words-with-counts ()
  (bltzread-test-with-buffer "aa bb cc dd ee ff"
    (bltzread-test-go "cc")
    (bltzread-set-level 5)
    (bltzread-test-go "aa")
    (bltzread-next-unknown 1)
    (should (= (point) (overlay-start (bltzread-test-ov "bb"))))
    (bltzread-next-unknown 2)                 ; skips known "cc": dd, ee
    (should (= (point) (overlay-start (bltzread-test-ov "ee"))))
    (bltzread-previous-unknown 3)             ; ee -> dd -> bb -> aa
    (should (= (point) (overlay-start (bltzread-test-ov "aa"))))
    (bltzread-next-unknown 50)
    (should (= (point) (overlay-start (bltzread-test-ov "ff"))))
    (bltzread-next-unknown -2)
    (should (= (point) (overlay-start (bltzread-test-ov "dd"))))))

(ert-deftest bltzread-jump-takes-prefix-argument ()
  (bltzread-test-with-buffer "aa bb cc dd ee"
    (let ((current-prefix-arg 3))
      (call-interactively #'bltzread-next-unknown))
    (should (= (point) (overlay-start (bltzread-test-ov "dd"))))
    (let ((current-prefix-arg 2))
      (call-interactively #'bltzread-previous-unknown))
    (should (= (point) (overlay-start (bltzread-test-ov "bb"))))))

;;; Dictionary

(ert-deftest bltzread-entries-cycle-and-reset ()
  (let ((bltzread-popup-trigger 'manual))
    (bltzread-test-with-buffer "hiša kruh"
      (setq bltzread-dictionary-function
            (lambda (_l words) (list (format "A %s" (car words)) (format "B %s" (car words)))))
      (bltzread-test-go "hiša")
      (bltzread-popup-at-point)
      (should (= bltzread--entry-index 0))
      (bltzread-next-entry)
      (should (= bltzread--entry-index 1))
      (bltzread-next-entry)
      (should (= bltzread--entry-index 0))
      (bltzread-previous-entry)
      (should (= bltzread--entry-index 1))
      (bltzread-test-go "kruh")
      (bltzread-popup-at-point)
      (should (= bltzread--entry-index 0)))))

(ert-deftest bltzread-dict-lookup-accepts-string-or-list ()
  (let ((bltzread-dictionary-function (lambda (_l _w) "one"))
        (bltzread-dict--cache (make-hash-table :test 'equal)))
    (should (equal (bltzread-dict-lookup "x" '("a")) '("one"))))
  (let ((bltzread-dictionary-function (lambda (_l _w) '("one" "two")))
        (bltzread-dict--cache (make-hash-table :test 'equal)))
    (should (equal (bltzread-dict-lookup "x" '("a")) '("one" "two")))))

(ert-deftest bltzread-sdcv-resolves-names ()
  (let ((bltzread-dict--book-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'bltzread-sdcv-books)
               (lambda () '(("Digital Pāḷi Dictionary (pi-en)" . "dpd")
                            ("DPD Grammar (pi-en)" . "dpd-grammar")
                            ("DSSJ-E-sl-en" . "DSSJ-E-sl-en")
                            ("DSSJ-S-sl" . "DSSJ-S-sl")))))
      (should (equal (bltzread-sdcv-resolve-book "dpd") "Digital Pāḷi Dictionary (pi-en)"))
      (should (equal (bltzread-sdcv-resolve-book "DSSJ-E-sl-en") "DSSJ-E-sl-en"))
      (should (equal (bltzread-sdcv-resolve-book "dssj-s") "DSSJ-S-sl"))
      (should (null (bltzread-sdcv-resolve-book "dssj")))   ; ambiguous
      (should (null (bltzread-sdcv-resolve-book "nonexistent"))))))

(ert-deftest bltzread-sdcv-unlisted-language-searches-nothing ()
  (let ((bltzread-sdcv-dictionaries '(("slv" "DSSJ-E-sl-en"))))
    (let ((bltzread-sdcv-unlisted-languages 'none))
      (should (null (bltzread-sdcv--books-for "pli"))))
    (let ((bltzread-sdcv-unlisted-languages 'all))
      (should (eq (bltzread-sdcv--books-for "pli") 'all)))))

(ert-deftest bltzread-bltzr-smoke ()
  (skip-unless (executable-find "bltzr"))
  (let ((bltzread-bltzr--cache (make-hash-table :test 'equal)))
    (let ((table (bltzread-bltzr-lemmas "slv" '("hiše"))))
      (should (gethash "hiše" table)))))

(ert-deftest bltzread-respacing-keeps-lookups-and-range ()
  (bltzread-test-with-buffer "aa bb cc"
    (puthash "aa" t bltzread--lookups)
    (let ((inhibit-read-only t))
      (save-excursion (goto-char 3) (insert "\n") (delete-char 1)))
    (should bltzread--stale)
    (bltzread--reanalyse (current-buffer))
    (should (gethash "aa" bltzread--lookups))
    (should-not bltzread--stale)
    (let ((inhibit-read-only t))
      (save-excursion (goto-char 1) (insert "x")))
    (bltzread--reanalyse (current-buffer))
    (should-not (gethash "aa" bltzread--lookups))))

(ert-deftest bltzread-popup-needs-a-command-that-moved-point ()
  (bltzread-test-with-buffer "aa bb"
    (let ((bltzread-popup-only-on-motion t) (this-command 'ignore))
      (goto-char 1)
      (setq bltzread--pre-command-point 1)
      (should-not (bltzread--popup-wanted-p))
      (setq bltzread--pre-command-point 4)
      (should (bltzread--popup-wanted-p))
      (setq bltzread--pre-command-point 1 this-command 'bltzread-popup-at-point)
      (should (bltzread--popup-wanted-p))
      (setq this-command 'ignore)
      (let ((bltzread-popup-only-on-motion nil))
        (should (bltzread--popup-wanted-p))))))

(ert-deftest bltzread-popup-scroll-lines-only-as-needed ()
  (should (= 0 (bltzread-popup-scroll-lines 200 100 20 500)))
  (should (= 3 (bltzread-popup-scroll-lines 10 70 20 500)))
  (should (= 2 (bltzread-popup-scroll-lines 10 700 20 45)))
  (should (= 0 (bltzread-popup-scroll-lines 10 700 20 5))))

(ert-deftest bltzread-range-farthest-from-a-function ()
  (bltzread-test-with-buffer "aa bb cc dd"
    (let ((bltzread-range-farthest-source (lambda () 6))
          (bltzread-range-start-function (lambda () 4))
          (bltzread-range-method 'farthest))
      (goto-char (point-max))
      (should (= 6 (bltzread-range-farthest)))
      (should (equal '(4 . 6) (bltzread-range-pick))))))

(ert-deftest bltzread-offer-record-asks-once ()
  (bltzread-test-with-buffer "aa bb cc dd"
    (let ((asked 0) (bltzread-prompt-on-close t)
          (bltzread-range-farthest-source (lambda () 6)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (cl-incf asked) nil)))
        (bltzread-range-track)
        (bltzread-offer-record)
        (bltzread-offer-record)
        (bltzread--query-kill))
      (should (= 1 asked)))))

(ert-deftest bltzread-plain-undo-and-redo-work-in-read-only-buffer ()
  (bltzread-test-with-buffer "hiša hiše"
    (should buffer-read-only)
    (bltzread-test-go "hiša")
    (bltzread-set-level 3)
    (bltzread-set-level 5)
    (setq last-command nil)
    (undo)
    (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše")) '(3 3)))
    (undo-boundary)
    (setq last-command 'undo)
    (undo)
    (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše")) '(1 1)))
    (undo-boundary)
    (setq last-command nil)
    (undo-redo)
    (should (equal (mapcar #'bltzread-test-level '("hiša" "hiše")) '(3 3)))))

(ert-deftest bltzread-plain-undo-takes-back-session-and-redo-restores ()
  (let ((bltzread-promotion-thresholds '(1 1 1 1))
        (bltzread-range-method 'points) (bltzread-range-confirm nil) (bltzread-bookmark-policy 'never))
    (bltzread-test-with-buffer "alfa beta"
      (bltzread-test-go "alfa")
      (bltzread-range-set-start)
      (goto-char (point-max))
      (bltzread-range-set-end)
      (bltzread-finish-session)
      (should (= (length (bltzread-state-history bltzread--state)) 1))
      (setq last-command nil)
      (undo)
      (should (null (bltzread-state-history bltzread--state)))
      (should (equal (mapcar #'bltzread-test-level '("alfa" "beta")) '(1 1)))
      (setq last-command nil)
      (undo-redo)
      (should (= (length (bltzread-state-history bltzread--state)) 1))
      (should (equal (mapcar #'bltzread-test-level '("alfa" "beta")) '(2 2))))))

(ert-deftest bltzread-undo-command-and-plain-undo-do-not-double-apply ()
  (bltzread-test-with-buffer "hiša"
    (bltzread-test-go "hiša")
    (bltzread-set-level 3)
    (bltzread-set-level 4)
    (bltzread-undo)
    (should (eq (bltzread-test-level "hiša") 3))
    (setq last-command nil)
    (undo)                              ; takes back the 3, not the 4 again
    (should (eq (bltzread-test-level "hiša") 1))))

(ert-deftest bltzread-popup-window-is-reused-between-words ()
  (progn
    (bltzread-test-with-buffer "hiša kruh"
      (setq bltzread-popup-style 'side-window)
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (bltzread-test-go "hiša")
        (bltzread--show-popup (current-buffer))
        (let ((win (car (get-buffer-window-list bltzread-popup--buffer nil t))))
          (should (window-live-p win))
          (bltzread-test-go "kruh")
          (bltzread--schedule-popup)          ; word to word: swap text, keep window
          (should (window-live-p win))
          (should (eq win (car (get-buffer-window-list bltzread-popup--buffer nil t))))
          (should (eq bltzread--popup-ov (bltzread-test-ov "kruh")))
          (should (string-match-p "kruh" (with-current-buffer bltzread-popup--buffer
                                           (buffer-string))))
          (goto-char (point-max))
          (bltzread--schedule-popup)          ; off words: closes after a delay
          (should (window-live-p win))
          (bltzread--hide-popup)
          (should-not (bltzread-popup-window-visible-p)))))))

(ert-deftest bltzread-avy-candidates-are-unknown-words ()
  (require 'bltzread-avy)
  (bltzread-test-with-buffer "aa bb cc"
    (bltzread-test-go "bb")
    (bltzread-set-level 5)
    (let ((cands (bltzread-avy--candidates (point-min) (point-max))))
      (should (equal (mapcar #'car cands) '((1 . 3) (7 . 9)))))))

(ert-deftest bltzread-warns-once-when-write-policy-is-unset ()
  (let ((shown 0) (bltzread--warned-write-policy nil)
        (bltzread-known-write-policy 'unset) (bltzread-warn-known-write-unset t))
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) (cl-incf shown))))
      (bltzread--warn-write-policy)
      (bltzread--warn-write-policy)
      (should (= shown 1))
      (setq bltzread--warned-write-policy nil bltzread-known-write-policy 'review)
      (bltzread--warn-write-policy)
      (should (= shown 1)))))

(provide 'bltzread-tests)
;;; bltzread-tests.el ends here
