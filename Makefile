
compile:
	@emacs -batch -f batch-byte-compile init.el custom.el config/*.el

clean:
	@rm -f init.elc custom.elc config/*.elc

