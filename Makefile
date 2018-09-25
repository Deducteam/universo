Q = @

.PHONY: universo
universo: $(wildcard src/*.ml)
	$(Q)ocamlbuild -quiet -package dedukti -package dkmeta -package z3 -use-ocamlfind src/universo.native

all: universo

MATITA_PATH=experiments/matita

TEST=matita_basics_logic.dk
.PHONY:
test: universo
	$(Q)./universo.native -I $(MATITA_PATH)/theory --in $(MATITA_PATH)/compatibility/in.dk --out $(MATITA_PATH)/compatibility/out.dk $(MATITA_PATH)/input/$(TEST) -o $(MATITA_PATH)/output \
	--theory $(MATITA_PATH)/compatibility/theory.dk > $(MATITA_PATH)/output/$(TEST)

.PHONY: check_input
check_input:
	$(Q)cd $(MATITA_PATH)/input && make

check_output:
	$(Q)cd $(MATITA_PATH)/output && make

.PHONY: clean
clean:
	$(Q)ocamlbuild -quiet -clean
	$(Q)cd $(MATITA_PATH)/input && make clean
	$(Q)cd $(MATITA_PATH)/output && make clean
