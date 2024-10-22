.PHONY: test

test:
	@LANG=C.UTF-8 stack --nix test
build:
	@LANG=C.UTF-8 stack --nix build
cabal-release:
	@cabal sdist -o .
