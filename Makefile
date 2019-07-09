.PHONY : test

EMACS ?= /usr/bin/emacs
LOADPATH = -L .

test:
	$(EMACS) -Q -batch $(LOADPATH) \
-l test/org-notion-test.el -f ert-run-tests-batch-and-exit
