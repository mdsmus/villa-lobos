LISPFILES = $(wildcard *.lisp) $(wildcard *.asd)

DEPS=deps
ARISTOXENUS = $(DEPS)/aristoxenus/
ALIEN = $(DEPS)/alien/src/
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

clean:
	rm -f villa-lobos
	find -name "*.fasl" | xargs rm -f
	find -name "*.lx64fsl" | xargs rm -f
	find -name "*.lx32fsl" | xargs rm -f
	rm -f *.o
	rm -f *.a
	rm -f *.fas

villa:
	mkdir villa

dist: villa-lobos villa
	cp villa-lobos villa/
	cp -Rv deps/bin villa/deps/
	cp -Rv deps/lib villa/deps/
	cp -Rv 371chorales villa/
	tar czf villa.tar.gz villa

deps/aristoxenus:
	mkdir -p deps/aristoxenus

deps/alien:
	mkdir -p deps/alien

get-deps: deps/aristoxenus deps/alien
	wget -c -P deps http://kroger.genos.mus.br/villa/villa-deps.tar.gz
	wget -c http://kroger.genos.mus.br/villa/371chorales.tar.gz
	wget -c -O deps/aristoxenus.tar.gz http://git.genos.mus.br/cgit.cgi?url=aristoxenus/snapshot/aristoxenus-master.tar.gz
	wget -c -O deps/alien.tar.gz http://git.genos.mus.br/cgit.cgi?url=alien/snapshot/alien-master.tar.gz
	tar xzf 371chorales.tar.gz
	cd deps && tar xzf villa-deps.tar.gz
	cd deps && tar xzf aristoxenus.tar.gz -C aristoxenus
	cd deps && tar xzf alien.tar.gz -C alien

backup:
	rsync -av  --progress --stats --delete . /media/kroger/villa-lobos/
