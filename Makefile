.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

.PHONY: doc
doc:
	@dune build @doc

universo:
	@ln -s _build/install/default/bin/universo universo || true

DKCHECK  = dkcheck
UNIVERSO = $(shell readlink -f _build/install/default/bin/universo)

LIBRARY_PATH=experiments

MATITA_PATH=$(LIBRARY_PATH)/matita3

MATITA_IN=$(MATITA_PATH)/input
MATITA_OUT=$(MATITA_PATH)/output
MATITA_IN_FILES=$(wildcard $(MATITA_IN)/*.dk)
MATITA_LIB=$(notdir $(basename $(MATITA_IN_FILES)))

DKDEP    = dkdep -s --ignore -I $(MATITA_IN)

ifneq ($(MAKECMDGOALS), clean)
	-include .library_depend_dko
endif

MATITA_UNIV_FILES=$(MATITA_LIB:%=$(MATITA_OUT)/%_univ.dk)
MATITA_CSTR_FILES=$(MATITA_LIB:%=$(MATITA_OUT)/%_cstr.dk)
MATITA_SOL_FILES=$(MATITA_LIB:%=$(MATITA_OUT)/%_sol.dk)
MATITA_OUT_FILES=$(MATITA_LIB:%=$(MATITA_OUT)/%.dk)

OPTIONS=-d s \
	-I $(MATITA_PATH)/theory \
	-I encodings \
	--to-elaboration $(MATITA_PATH)/compatibility/in.dk \
	--of-universo    $(MATITA_PATH)/compatibility/out.dk \
	--constraints    $(MATITA_PATH)/compatibility/constraints.dk \
	--theory	 $(MATITA_PATH)/compatibility/cic.dk \
	--target	 $(MATITA_PATH)/compatibility/target.dk


$(MATITA_UNIV_FILES): $(MATITA_OUT)/%_univ.dk: $(MATITA_OUT)/%.dk
$(MATITA_CSTR_FILES): $(MATITA_OUT)/%_cstr.dk: $(MATITA_OUT)/%.dk bin
	$(UNIVERSO) $(OPTIONS) --check-only -o $(dir $@) $<

$(MATITA_SOL_FILES): $(MATITA_OUT)/%_sol.dk: $(MATITA_OUT)/%_cstr.dk $(MATITA_OUT)/%.dk
	$(UNIVERSO) $(OPTIONS) --solve-only -o $(dir $@) $(shell $(DKDEP) $(MATITA_IN)/$*.dk)

$(MATITA_OUT_FILES): $(MATITA_OUT)/%.dk: $(MATITA_IN)/%.dk bin
	$(UNIVERSO) $(OPTIONS) --elab-only -o $(dir $@) $<

.PHONY: debug
debug:
	$(DKDEP) $(MATITA_IN)/test.dk

.PHONY: clean
clean:
	@dune clean

.PHONY: install
install:
	@dune install
