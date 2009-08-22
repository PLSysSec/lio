
PROGS := $(patsubst %.hs,%, $(wildcard [a-z]*.hs))
DEPS := $(patsubst %,%.d, $(PROGS))

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

ignore:
	rm -f .gitignore~
	(echo '*.hi'; echo '*.o'; echo '*~'; echo '/*.d'; echo '/.depend') \
		> .gitignore~
	for prog in $(PROGS); do		\
		echo $$prog >> .gitignore~;	\
	done
	mv -f .gitignore~ .gitignore


clean:
	rm -f .depend $(PROGS)
	find . \( -name '*~' -o -name '*.o' -o -name '*.hi' -o -name '*.d' \) \
		-print0 | xargs -0 rm -f --
.PHONY: clean
