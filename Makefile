Q = @

.PHONY: universo
universo: $(wildcard src/*.ml)
	$(Q)ocamlbuild -I src/ -I src/solving -quiet -package dedukti -package dkmeta -package z3 -use-ocamlfind src/universo.native

all: universo

MATITA_PATH=experiments/matita

.PHONY:

test: universo
	$(Q)./universo.native -I $(MATITA_PATH)/theory --in $(MATITA_PATH)/compatibility/in.dk --out $(MATITA_PATH)/compatibility/out.dk $(MATITA_PATH)/input/test.dk -o $(MATITA_PATH)/output \
	--theory $(MATITA_PATH)/compatibility/theory.dk > $(MATITA_PATH)/output/test.dk

logic: universo
	$(Q)./universo.native -I $(MATITA_PATH)/theory --in $(MATITA_PATH)/compatibility/in.dk --out $(MATITA_PATH)/compatibility/out.dk $(MATITA_PATH)/input/matita_basics_logic.dk \
-o $(MATITA_PATH)/output -theory $(MATITA_PATH)/compatibility/theory.dk > $(MATITA_PATH)/output/matita_basics_logic.dk

.PHONY: check_input
check_input:
	$(Q)cd $(MATITA_PATH)/input && make

check_output:
	$(Q)cd $(MATITA_PATH)/output && make

.PHONY: clean
clean:
	$(Q)ocamlbuild -clean
	$(Q)cd $(MATITA_PATH)/input && make clean
	$(Q)cd $(MATITA_PATH)/output && make clean
