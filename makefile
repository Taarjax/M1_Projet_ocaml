# Makefile for OCaml program

# Compiler flags
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = -o

# Source files
SRC = eternity.ml

# Object files
OBJ = $(SRC:.ml=.cmo) $(SRC:.ml=.cmi) $(SRC:.ml=.o)

# Executable name
EXEC = projet

all: $(EXEC)

$(EXEC): $(OBJ)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

%.cmo: %.ml
	$(OCAMLC) -c $(OCAMLFLAGS) $<

%.cmi: %.mli
	$(OCAMLC) -c $(OCAMLFLAGS) $<

clean:
	rm -f $(EXEC) $(OBJ) *.svg
