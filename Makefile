.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

DKCHECK  = dkcheck
UNIVERSO = $(shell readlink -f _build/install/default/bin/universo)

MATITA_PATH=experiments/matita

.PHONY: theory
theory:
	$(DKCHECK) -e $(MATITA_PATH)/theory/cic.dk

.PHONY: univ
univ:
	$(DKCHECK) -e encodings/universo.dk

.PHONY: test
test: bin theory univ
	$(UNIVERSO) -I $(MATITA_PATH)/theory \
	--in $(MATITA_PATH)/compatibility/in.dk \
	--out $(MATITA_PATH)/compatibility/out.dk \
	--theory $(MATITA_PATH)/compatibility/theory.dk	\
	-theory $(MATITA_PATH)/theory/cic.dk \
	$(MATITA_PATH)/input/test.dk -o $(MATITA_PATH)/output > \
	$(MATITA_PATH)/output/test.dk

.PHONY: logic
logic: bin theory univ
	$(UNIVERSO) -I $(MATITA_PATH)/theory \
	--in $(MATITA_PATH)/compatibility/in.dk \
	--out $(MATITA_PATH)/compatibility/out.dk \
	--theory $(MATITA_PATH)/compatibility/theory.dk	\
	-theory $(MATITA_PATH)/theory/cic.dk \
	$(MATITA_PATH)/input/matita_basics_logic.dk -o $(MATITA_PATH)/output > \
	$(MATITA_PATH)/output/matita_basics_logic.dk

.PHONY: check_input
check_input:
	cd $(MATITA_PATH)/input && make

.PHONY: check_output
check_output:
	cd $(MATITA_PATH)/output && make

.PHONY: clean
clean:
	@dune clean
	cd $(MATITA_PATH)/input && make clean
	cd $(MATITA_PATH)/output && make clean
