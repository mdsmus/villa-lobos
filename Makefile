LISPFILES = $(wildcard *.lisp) $(wildcard *.asd)

ARISTOXENUS = /home/kroger/src/aristoxenus/
ALIEN = /home/kroger/src/cl-distribution/src/
#CCL_BINARY = /usr/local/ccl/lx86cl
CCL_BINARY = /usr/local/ccl/lx86cl64

SBCL = sbcl --no-userinit 
CCL = ccl --batch
ECL = ecl -norc

COMPILER = sbcl
#COMPILER = ccl

ifeq ($(COMPILER),sbcl)
villa-lobos: $(LISPFILES)
	$(SBCL) \
	--eval "(require :asdf)" \
	--eval "(push \"${ARISTOXENUS}\" asdf:*central-registry*)" \
	--eval "(push \"${ALIEN}\" asdf:*central-registry*)" \
	--eval "(require :villa-lobos)" \
	--eval "(sb-ext:save-lisp-and-die \"$@\" :executable t :toplevel #'villa-lobos:run)"
else ifeq ($(COMPILER),ecl)
villa-lobos: $(LISPFILES)
	$(ECL) \
	-eval "(require :asdf)" \
	-eval "(push \"${ARISTOXENUS}\" asdf:*central-registry*)" \
	-eval "(push \"${ALIEN}\" asdf:*central-registry*)" \
	-eval "(require :villa-lobos)" \
	-eval "(asdf:make-build :$@ :type :program :monolithic t :epilogue-code '(progn (villa:run) (ext:quit)))" \
	-eval "(ext:quit)"
	mv villa-lobos-mono villa-lobos
else ifeq ($(COMPILER),ccl)
villa-lobos: $(LISPFILES)
	$(CCL) \
	--eval "(require :asdf)" \
	--eval "(push \"${ARISTOXENUS}\" asdf:*central-registry*)" \
	--eval "(push \"${ALIEN}\" asdf:*central-registry*)" \
	--eval "(asdf:oos 'asdf:load-op :villa-lobos)" \
	--eval "(save-application \"$@\" :toplevel-function #'villa-lobos:run :prepend-kernel \"${CCL_BINARY}\")"
endif

dist: villa-lobos
	tar czf villa-lobos.tar.gz villa-lobos /usr/lib/libtk8.5.so.0 /usr/lib/libtcl8.5.so.0 /usr/bin/wish8.5 /usr/bin/tclsh8.5 

clean:
	rm -f villa-lobos
	rm -f *.fasl
	rm -f *.lx32fsl
	rm -f *.o
	rm -f *.a
	rm -f *.fas

backup:
	rsync -av  --progress --stats --delete . /media/kroger/villa-lobos/
