Q = @

.PHONY: universo
universo: $(wildcard src/*.ml)
	$(Q)ocamlbuild -quiet -package dedukti -package dkmeta -package z3 -use-ocamlfind src/universo.native

all: universo

MATITA_PATH=experiments/matita

TEST=matita_basics_logic.dk
.PHONY:
test:
	$(Q)./universo.native --in $(MATITA_PATH)/compatibility/in.dk $(MATITA_PATH)/input/$(TEST) \
	> $(MATITA_PATH)/output/$(TEST)

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
