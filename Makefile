.PHONY : test

EMACS ?= emacs

test:
	$(EMACS) -Q -batch -L . -l test/test-git-gutter2.el -f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	-rm -f *.elc *~
