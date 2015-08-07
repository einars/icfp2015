all:
	ocamlbuild -tag thread -use-ocamlfind -quiet -pkg core -pkg yojson src/main.native
run: all
	./main.native

.PHONY:all run
