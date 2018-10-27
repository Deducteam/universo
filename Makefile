Q = @

.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

MATITA_PATH=experiments/matita

.PHONY: test
test: universo
	$(Q)./universo.native -I $(MATITA_PATH)/theory --in $(MATITA_PATH)/compatibility/in.dk --out $(MATITA_PATH)/compatibility/out.dk $(MATITA_PATH)/input/test.dk -o $(MATITA_PATH)/output \
	--theory $(MATITA_PATH)/compatibility/theory.dk > $(MATITA_PATH)/output/test.dk

.PHONY: logic
logic: universo
	$(Q)./universo.native -I $(MATITA_PATH)/theory --in $(MATITA_PATH)/compatibility/in.dk --out $(MATITA_PATH)/compatibility/out.dk $(MATITA_PATH)/input/matita_basics_logic.dk \
-o $(MATITA_PATH)/output -theory $(MATITA_PATH)/compatibility/theory.dk > $(MATITA_PATH)/output/matita_basics_logic.dk

.PHONY: check_input
check_input:
	$(Q)cd $(MATITA_PATH)/input && make

.PHONY: check_output
check_output:
	$(Q)cd $(MATITA_PATH)/output && make

.PHONY: clean
clean:
	@dune clean
	$(Q)cd $(MATITA_PATH)/input && make clean
	$(Q)cd $(MATITA_PATH)/output && make clean
