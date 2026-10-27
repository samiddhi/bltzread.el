;;; bltzread-dict-filters-tests.el --- Tests for bltzread-dict-filters -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'bltzread-dict-filters)
(require 'bltzread-dict)

(defconst bltzread-dict-filters-tests--html
  (concat "<div class=\"e\"><span class=\"hw\">dom</span> <i>m</i>"
          "<p class=\"gloss x\">house &amp; home · family</p>"
          "<span class=\"trans\">building <b>x</b></span>"
          "<span class=\"trans ex\">example</span>"
          "<span class=\"trans\">house</span><span class=\"trans\">house</span></div>"))

(defun bltzread-dict-filters-tests--run (spec)
  (bltzread-dict-apply-filter spec bltzread-dict-filters-tests--html))

(ert-deftest bltzread-dict-filters-lookup-by-dict-or-language ()
  (let ((bltzread-dict-filters '(("Foo" . "a") ("pli" . "b"))))
    (should (equal (bltzread-dict-filter-for "foo" "xx") "a"))
    (should (equal (bltzread-dict-filter-for "Bar" "pli") "b"))
    (should-not (bltzread-dict-filter-for "Bar" "xx"))))

(ert-deftest bltzread-dict-filters-regexp-string ()
  (should (equal (bltzread-dict-filters-tests--run "class=\"trans\">\\([^<]*\\)")
                 "building\nhouse"))
  ;; without a group the whole match counts
  (should (equal (bltzread-dict-filters-tests--run "<i>[^<]*") "m")))

(ert-deftest bltzread-dict-filters-class ()
  (should (equal (bltzread-dict-filters-tests--run '(:class "hw")) "dom"))
  (should (equal (bltzread-dict-filters-tests--run '(:class ("hw" "gloss")))
                 "dom\nhouse & home · family")))

(ert-deftest bltzread-dict-filters-split-join-max ()
  (should (equal (bltzread-dict-filters-tests--run '(:class "gloss" :split " · "))
                 "house & home\nfamily"))
  (should (equal (bltzread-dict-filters-tests--run '(:class "gloss" :split " · " :join ", "))
                 "house & home, family"))
  (should (equal (bltzread-dict-filters-tests--run '(:class "gloss" :split " · " :max 1))
                 "house & home")))

(ert-deftest bltzread-dict-filters-function-spec ()
  (should (equal (bltzread-dict-filters-tests--run #'upcase-initials) 
                 (upcase-initials bltzread-dict-filters-tests--html))))

(ert-deftest bltzread-dict-filters-short-only-when-not-full ()
  (let* ((bltzread-dict-filters '(("D" :class "hw")))
         (entry (bltzread-dict-shorten "D" "xx" bltzread-dict-filters-tests--html "D\nwhole")))
    ;; the short form carries no dictionary name
    (should (equal (bltzread-dict-display entry nil) "dom"))
    (should (equal (bltzread-dict-display entry t) "D\nwhole"))))

(ert-deftest bltzread-dict-filters-fallback ()
  (let ((bltzread-dict-filters '(("A" . "nomatch\\(x\\)") ("B" :class "none")
                                 ("C" . (lambda (_) (error "boom"))) ("D" . ignore)
                                 ("E" . "[unbalanced"))))
    (dolist (d '("A" "B" "C" "D" "E" "Unlisted"))
      (let ((entry (bltzread-dict-shorten d "xx" "raw" "whole")))
        (should (equal (bltzread-dict-display entry nil) "whole"))))))

(ert-deftest bltzread-dict-failed-parse-marked-and-sorted-last ()
  (let ((bltzread-dict-filters '(("A" :class "hw")))
        (bltzread-dict-failed-entries-last t))
    (should (get-text-property 0 'bltzread-parse-failed
                               (bltzread-dict-shorten "A" "xx" "raw" "whole")))
    (should-not (get-text-property 0 'bltzread-parse-failed
                                   (bltzread-dict-shorten "Unlisted" "xx" "raw" "whole")))
    (let* ((bad (bltzread-dict-shorten "A" "xx" "raw" "bad"))
           (ok1 (bltzread-dict-shorten "A" "xx" bltzread-dict-filters-tests--html "ok1"))
           (ok2 "ok2")
           (bltzread-dictionary-function (lambda (_l _w) (list bad ok1 ok2)))
           (bltzread-dict--cache (make-hash-table :test 'equal)))
      (should (equal (bltzread-dict-lookup "xx" '("w")) (list ok1 ok2 bad)))
      (let ((bltzread-dict-failed-entries-last nil)
            (bltzread-dict--cache (make-hash-table :test 'equal)))
        (should (equal (bltzread-dict-lookup "xx" '("w")) (list bad ok1 ok2)))))))

(defconst bltzread-dict-filters-tests--ref
  "<span class=\"xr\"><a href=\"bword://spotka%C4%87\" title=\"x\">spotkać </a></span>")

(ert-deftest bltzread-dict-follow-targets ()
  (should (equal (bltzread-dict-follow-targets '(:class "x" :follow t)
                                               bltzread-dict-filters-tests--ref)
                 '("spotkać")))
  (should (equal (bltzread-dict-follow-targets '(:follow "see \\([a-z]+\\)") "see foo, see bar")
                 '("foo" "bar")))
  (should (equal (bltzread-dict-follow-targets '(:follow (lambda (_) '("a"))) "") '("a")))
  (should-not (bltzread-dict-follow-targets '(:class "x") bltzread-dict-filters-tests--ref))
  (should-not (bltzread-dict-follow-targets "x" bltzread-dict-filters-tests--ref)))

(ert-deftest bltzread-dict-follow-fills-in-a-bare-reference ()
  (let* ((bltzread-dict-filters '(("D" :class "trans" :follow t)))
         (asked nil)
         (target (propertize "D\nwhole target" 'bltzread-short "to meet"))
         (entry (bltzread-dict-shorten
                 "D" "xx" bltzread-dict-filters-tests--ref "D\nsee"
                 (lambda (w) (push w asked) (list target)))))
    (should (equal asked '("spotkać")))
    (should (equal (bltzread-dict-display entry nil) "→ spotkać\nto meet"))
    (should (equal (bltzread-dict-display entry t) "D\nsee\n\n→ spotkać\nwhole target"))
    (should-not (get-text-property 0 'bltzread-parse-failed entry))))

(ert-deftest bltzread-dict-follow-leaves-entries-with-content-alone ()
  (let ((bltzread-dict-filters '(("D" :class "hw" :follow t))))
    (should (equal (bltzread-dict-display
                    (bltzread-dict-shorten "D" "xx" bltzread-dict-filters-tests--html "D\nw"
                                           (lambda (_) (error "must not follow")))
                    nil)
                   "dom"))))

(ert-deftest bltzread-dict-follow-nothing-found-is-a-failed-parse ()
  (let ((bltzread-dict-filters '(("D" :class "trans" :follow t))))
    (should (get-text-property 0 'bltzread-parse-failed
                               (bltzread-dict-shorten "D" "xx" bltzread-dict-filters-tests--ref
                                                      "D\nsee" (lambda (_) nil))))))

(provide 'bltzread-dict-filters-tests)
;;; bltzread-dict-filters-tests.el ends here
