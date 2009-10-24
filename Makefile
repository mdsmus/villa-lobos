LISPFILES = $(wildcard *.lisp) $(wildcard *.asd)

SBCL = sbcl --no-userinit --script compile-sbcl.lisp
CCL = ccl --batch --load compile-ccl.lisp

LISPCOMPILER = $(SBCL)
#LISPCOMPILER = $(CCL)

villa-lobos: $(LISPFILES)
	$(LISPCOMPILER)

dist: villa-lobos
	tar czf villa-lobos.tar.gz villa-lobos /usr/lib/libtk8.5.so.0 /usr/lib/libtcl8.5.so.0 /usr/bin/wish8.5 /usr/bin/tclsh8.5 

clean:
	rm -f villa-lobos
	rm -f *.fasl
	rm -f *.lx32fsl
