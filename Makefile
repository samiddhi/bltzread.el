EMACS ?= emacs
LIBS = $(filter-out %-tests.el %-playground.el,$(wildcard bltzread*.el))
RENDERED = bltzread-manual-rendered

.PHONY: all compile checkdoc test info clean

all: compile test

compile:
	$(EMACS) --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(LIBS)

checkdoc:
	@for f in $(LIBS); do \
	  $(EMACS) --batch -L . --eval '(require (quote checkdoc))' \
	    --eval "(checkdoc-file \"$$f\")" 2>&1 | grep '^bltzread.*\.el:[0-9]'; \
	done; true

test:
	$(EMACS) --batch -L . -l bltzread-tests.el -f ert-run-tests-batch-and-exit
	$(EMACS) --batch -L . -l bltzread-dict-filters-tests.el \
	  -f ert-run-tests-batch-and-exit

# Rebuild the Info manual (the manual, then the tutorial as its last chapter):
# fill in the key placeholders of the Org sources with the default keys, then
# export.
info:
	$(EMACS) --batch -L . --eval "(require 'ox-texinfo)" \
	  --eval "(require 'bltzread)" \
	  --eval "(bltzread-doc-render-info-file \"$(RENDERED).org\")" \
	  --eval "(with-current-buffer (find-file-noselect \"$(RENDERED).org\") \
	    (org-texinfo-export-to-texinfo))" \
	  && makeinfo --no-split -o bltzread.info $(RENDERED).texi; \
	status=$$?; rm -f $(RENDERED).org $(RENDERED).texi; exit $$status

clean:
	rm -f *.elc $(RENDERED).org $(RENDERED).texi
