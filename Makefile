LISPFILES = $(wildcard *.lisp)

villa-lobos: $(LISPFILES)
	sbcl --no-userinit --script compile.lisp

clean:
	rm -f villa-lobos
	rm -f *.fasl

