
PROGS := $(patsubst %.hs,%, $(wildcard [a-z]*.hs))
DEPS := $(patsubst %,%.d, $(PROGS))

BIN = $(HOME)/.cabal/bin/
DOC = $(wildcard [A-Z]*/[A-Z]*.hs [A-Z]*/[A-Z]*/[A-Z]*.hs)

all: $(PROGS)
.PHONY: all

-include .depend

.depend: $(DEPS)
	@rm -f $@~
	for prog in $(PROGS); do \
		sed -ne 's/.*\(:.*\.hs\)$$/'$$prog'\1/p' test.d >> $@~; \
	done
	@mv -f $@~ $@

$(PROGS): %: %.hs
	ghc --make $<
	ghc -M -dep-makefile $@.d~ $<
	@if cmp -s $@.d~ $@.d; then		\
		rm $@.d~;			\
	else					\
		mv -f $@.d~ $@.d;		\
	fi

.PHONY: doc
doc: $(DOC)
	rm -rf $@
	mkdir -p $@/src
	touch doc
	$(BIN)HsColour -print-css -o$@/src/hscolour.css
	for file in $(DOC); do \
	    $(BIN)HsColour -css -anchor \
		-o$@/src/`echo $$file|sed -e 's|/|.|g'`.html $$file; \
	done
	haddock -h -o$@ --source-base=src/ \
	    --source-module=src/%M.hs.html \
	    --source-entity=src/%M.hs.html#%N $(DOC)

ignore:
	rm -f .gitignore~
	(echo '*.hi'; echo '*.o'; echo '*~'; echo '/*.d'; \
			echo '/.depend'; echo '/doc') \
		> .gitignore~
	for prog in $(PROGS); do		\
		echo $$prog >> .gitignore~;	\
	done
	mv -f .gitignore~ .gitignore

clean:
	rm -rf doc
	rm -f .depend $(PROGS)
	@find . \( -name '*~' -o -name '*.o' -o -name '*.hi' -o -name '*.d' \) \
		-print0 > .clean~
	@xargs -0 echo rm -f -- < .clean~
	@xargs -0 rm -f -- < .clean~
.PHONY: clean
