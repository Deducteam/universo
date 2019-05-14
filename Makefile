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
DKDEP    = dkdep --ignore

UNIVERSO = $(shell readlink -f _build/install/default/bin/universo)

LIBRARY_PATH=experiments

MATITA_PATH=$(LIBRARY_PATH)/matita3

MATITA_IN=$(MATITA_PATH)/input
MATITA_OUT=$(MATITA_PATH)/output
MATITA_IN_FILES=$(wildcard $(MATITA_IN)/*.dk)
MATITA_LIB=$(notdir $(basename $(MATITA_IN_FILES)))

MATITA_UNIV_FILES=$(MATITA_LIB:%=$(MATITA_OUT)/%_univ.dk)
MATITA_UNIV_OFILES=$(MATITA_LIB:%=$(MATITA_OUT)/%_univ.dko)

MATITA_CSTR_FILES=$(MATITA_LIB:%=$(MATITA_OUT)/%_cstr.dk)
MATITA_SOL_FILES=$(MATITA_LIB:%=$(MATITA_OUT)/%_sol.dk)

MATITA_OUT_FILES=$(MATITA_LIB:%=$(MATITA_OUT)/%.dk)
MATITA_OUT_OFILES=$(MATITA_LIB:%=$(MATITA_OUT)/%.dko)

MATITA_IN_DEP_FILES=$(MATITA_LIB:%=$(MATITA_IN)/%.dep)
MATITA_OUT_DEP_FILES=$(MATITA_LIB:%=$(MATITA_OUT)/%.dep)

OPTIONS=\
	-l \
	--theory	 $(MATITA_PATH)/compatibility/cic.dk \
	--config	 $(MATITA_PATH)/compatibility/config.dk

$(MATITA_UNIV_FILES): $(MATITA_OUT)/%_univ.dk: $(MATITA_OUT)/%.dk

$(MATITA_UNIV_OFILES): $(MATITA_OUT)/%_univ.dko: $(MATITA_OUT)/%_univ.dk
	$(DKCHECK) -e -I $(MATITA_PATH)/compatibility $<

$(MATITA_CSTR_FILES): $(MATITA_OUT)/%_cstr.dk: $(MATITA_OUT)/%.dko $(UNIVERSO)


$(MATITA_OUT_OFILES): $(MATITA_OUT)/%.dko: $(MATITA_OUT)/%.dk $(MATITA_OUT)/%.dep $(MATITA_OUT)/%_univ.dk
	$(UNIVERSO) $(OPTIONS) --check-only -o $(dir $@) $<

$(MATITA_SOL_FILES): $(MATITA_OUT)/%_sol.dk: $(MATITA_OUT)/%_cstr.dk $(MATITA_OUT)/%.dk $(UNIVERSO)
	$(UNIVERSO) $(OPTIONS) --solve-only -o $(dir $@) \
	$(shell $(DKDEP) -I $(MATITA_IN) -s $(MATITA_IN)/$*.dk)

$(MATITA_OUT_FILES): $(MATITA_OUT)/%.dk: $(MATITA_IN)/%.dk $(UNIVERSO)
	$(UNIVERSO) $(OPTIONS) --elab-only -o $(dir $@) $<

$(MATITA_IN_DEP_FILES): $(MATITA_IN)/%.dep: $(MATITA_IN)/%.dk
	$(DKDEP) -I $(MATITA_IN) $< > $@

$(MATITA_OUT_DEP_FILES): $(MATITA_OUT)/%.dep: $(MATITA_IN)/%.dep
	cat $< | sed 's:$(MATITA_IN):$(MATITA_OUT):g' > $@

include $(MATITA_OUT_DEP_FILES)

.PHONY: debug
debug:

	echo $(DEP)

.PHONY: clean
clean:
	@dune clean

.PHONY: install
install:
	@dune install
