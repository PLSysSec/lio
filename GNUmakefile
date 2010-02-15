
PKG = $(basename $(wildcard *.cabal))
TARGETS = $(basename $(shell find Examples -name '[a-z]*.hs' -print))

all: $(TARGETS)

.PHONY: all always clean build dist doc browse install

GHC = ghc -XForeignFunctionInterface -XFlexibleInstances $(WALL)
WALL = -Wall -Werror

always:
	@:

Examples/reliable/%: always
	$(GHC) --make -iExamples/reliable -Wall -Werror $@.hs

Examples/%: always build
	$(GHC) --make $@.hs

Setup: Setup.hs
	$(GHC) --make Setup.hs

dist/setup-config: Setup $(PKG).cabal
	./Setup configure --user

build: dist/setup-config
	./Setup build

doc: dist/setup-config
	./Setup haddock --hyperlink-source

dist: dist/setup-config
	./Setup sdist

install: build doc
	./Setup install

uninstall: dist/setup-config
	./Setup unregister --user
	rm -rf $(HOME)/.cabal/lib/$(PKG)-[0-9]*
	rm -rf $(HOME)/.cabal/share/doc/$(PKG)-[0-9]*

browse: doc
	firefox dist/doc/html/$(PKG)/index.html

clean:
	rm -rf $(TARGETS) Setup dist
	find . \( -name '*~' -o -name '*.hi' -o -name '*.o' \) -print0 \
		| xargs -0 rm -f --
