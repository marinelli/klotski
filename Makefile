
OCAML = ocamlopt
MODS = klotski.ml

PROG = klotski


.PHONY: all
all: $(PROG)


$(PROG): $(MODS)
	$(OCAML) -w +a -o $(PROG) $(MODS)


.PHONY: clean
clean:
	rm *.o *.cmx *.cmi $(PROG)

