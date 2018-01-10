main:
	ocamlbuild -use-menhir -menhir 'menhir -v' main.native
	mv main.native prustc

clean:
	rm _build/ -rf
	rm -f ./prustc
