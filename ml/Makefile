THREADS = ye
FILES  = src/gametypes.ml src/movement.ml src/game.ml src/tests.ml src/main.ml
PACKS = core yojson

SOURCES = ${FILES}
RESULT  = play_icfp2015


TARGET  = native-code
#TARGET  = debug-code

all:$(TARGET)
	$(POST)

run:$(TARGET)
	OCAMLRUNPARAM=b ./play_icfp2015

all-clean: clean all

.PHONY: all-clean all run win release publish

-include OCamlMakefile
