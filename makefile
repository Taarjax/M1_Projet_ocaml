# Variables
OCAMLC=ocamlfind ocamlc
PACKAGES=-package csv,unix
EXEC=projet

# Cibles
all: $(EXEC)

$(EXEC): eternity.ml
	$(OCAMLC) -o $(EXEC) $(PACKAGES) -linkpkg eternity.ml

clean:
	rm -f *.cmx *.cmi *.cmo $(EXEC)
	rm -f Puzzle_p1/* Puzzle_p2/* Puzzle_p3/* 

.PHONY: all clean
