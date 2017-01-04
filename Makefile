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

serve-watch: export KAPI_ENV=development
serve-watch:
	ag -l -Ghs --ignore test | entr -r sh -c 'make serve'

test:
	stack build --test

test-watch:
	stack build --test --file-watch

.PHONY: install serve test
