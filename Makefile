Q = @

universo: $(wildcard src/*.ml)
	$(Q)ocamlbuild -quiet -package dedukti.kernel -package Z3 -use-ocamlfind src/universo.native

all: universo

.PHONY: universo

clean:
	$(Q)ocamlbuild -quiet -clean
