build:
	jbuilder build @install

deps:
	opam install ./luml.opam --deps-only --yes

run:
	jbuilder exec luml

test: build _dummy
	jbuilder runtest --no-buffer -j 1

format:
	ocamlformat -i lib/*.ml
	ocamlformat -i bin/*.ml
	ocamlformat -i test/*.ml

utop:
	jbuilder utop lib

release: _dummy build
	rm -r release
	mkdir -p release/bin
	mkdir -p release/lib/luml/stdlib
	_build/default/bin/mkstdlib.exe
	cp _build/default/bin/luml.exe release/bin/luml
	cp -r stdlib/* release/lib/luml/stdlib
	
install: release
	sudo cp release/bin/* /usr/local/bin
	sudo mkdir -p /usr/local/lib/luml/stdlib
	sudo cp -r release/lib/luml/stdlib/* /usr/local/lib/luml/stdlib


_dummy:
