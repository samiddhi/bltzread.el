;;; bltzread-dict-filters-tests.el --- Tests for bltzread-dict-filters -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'bltzread-dict-filters)

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

(provide 'bltzread-dict-filters-tests)
;;; bltzread-dict-filters-tests.el ends here
