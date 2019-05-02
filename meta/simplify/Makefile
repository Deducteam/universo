DKMETA=dkmeta

MATITA_PATH=../../experiments/matita3
MATITA_C_OUT=$(MATITA_PATH)/compatibility/out.dk
MATITA_IN=$(MATITA_PATH)/input
MATITA_OUT=$(MATITA_PATH)/output

MATITA_IN_FILES=$(wildcard $(MATITA_IN)/*.dk)
MATITA_LIB=$(notdir $(basename $(MATITA_IN_FILES)))

NF_DIR=.
MATITA_NF_FILES=$(MATITA_LIB:%=$(NF_DIR)/%.dk)

.PHONY: all
all: $(MATITA_NF_FILES)


$(NF_DIR)/%.dk: $(MATITA_OUT)/%_sol.dk $(MATITA_OUT)/%.dk
	$(DKMETA)  -m $< -m $(MATITA_C_OUT) $(MATITA_OUT)/$*.dk > $@

.PHONY: debug
debug:
	@echo $(MATITA_NF_FILES)
