
repl:
	stack ghci

setup:
	stack setup

build: setup
	stack build

install:
	stack build --copy-bins

serve: build
	stack exec kapi-exe

.PHONY: install serve test