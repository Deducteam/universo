Q = @

universo: $(wildcard src/*.ml)
	$(Q)ocamlbuild -quiet -package dedukti -package z3 -use-ocamlfind src/universo.native

all: universo

.PHONY: universo

clean:
	$(Q)ocamlbuild -quiet -clean
