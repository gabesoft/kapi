
repl:
	stack ghci

clean:
	stack clean

setup:
	stack setup

build: setup
	stack build

install: clean
	stack build --copy-bins

serve: build
	stack exec kapi-exe

serve-watch:
	find $(CURDIR)/src $(CURDIR)/app -name "*.hs" | entr -d -r sh -c 'make serve'

.PHONY: install serve test