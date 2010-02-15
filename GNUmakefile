
PKG = $(basename $(wildcard *.cabal))
TARGETS = $(basename $(wildcard Examples/*.hs))

all: build $(TARGETS)

.PHONY: all always clean build doc browse install

GHC = ghc -XForeignFunctionInterface -XFlexibleInstances $(WALL)
WALL = -Wall -Werror

always:
	@:

Examples/reliable/%: always
	$(GHC) --make -iExamples/reliable -Wall -Werror $@.hs

Examples/%: always
	$(GHC) --make $@.hs

Setup: Setup.hs
	$(GHC) --make Setup.hs

dist/setup-config: Setup
	./Setup configure --user

build: dist/setup-config
	./Setup build

doc: dist/setup-config
	./Setup haddock --hyperlink-source

install: build doc
	./Setup install

browse: doc
	firefox dist/doc/html/$(PKG)/index.html

clean:
	rm -rf $(TARGETS) Setup dist
	find . \( -name '*~' -o -name '*.hi' -o -name '*.o' \) -print0 \
		| xargs -0 rm -f --
