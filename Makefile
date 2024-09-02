fmt:
	find . -name '*.hs' -type f -exec ormolu --mode inplace {} \;
	find . -name '*.nix' -exec nixfmt {} \;
	-statix check
	-deadnix -f
unit-test:
	nix run .#test
build: fmt
	nix build
cabal-release:
	cabal sdist -o .
