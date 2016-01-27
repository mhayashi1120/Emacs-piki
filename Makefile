EMACS = emacs

check: compile
	$(EMACS) -q -batch -L . -l piki-mode.el  \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -L . -l piki-mode.elc \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) --version
	$(EMACS) -q -batch -f batch-byte-compile piki-mode.el
