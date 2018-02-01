PORT = 8001

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

kill:
	-kill -9 $(shell lsof -i:$(PORT) | awk 'FNR == 2 {print $$2}')

serve-watch: export KAPI_ENV=development
serve-watch: kill
	ag -l -Ghs --ignore test | entr -r sh -c 'make serve'

test:
	stack build --test

test-watch:
	stack build --test --file-watch

nix-package: export NIX_PATH=$(HOME)/.nix-deferexpr/channels
nix-package:
	cabal2nix . > kapi.nix

nix-build: export NIX_PATH=$(HOME)/.nix-deferexpr/channels
nix-build:
	nix-build default.nix
	ls -lA ./result/bin

docs:
	curl -XGET http://localhost:$(PORT)/documentation.md | markdown | lynx -stdin

.PHONY: install serve test
