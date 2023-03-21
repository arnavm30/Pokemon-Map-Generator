.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

graph:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	rm -f generator.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh