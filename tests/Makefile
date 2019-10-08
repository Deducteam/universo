Q=@

SANDBOX=sandbox
INPUT=$(SANDBOX)/in
OUTPUT=$(SANDBOX)/out

TEST_DIR=

CONFIGURATION_FILE=$(SANDBOX)/universo_cfg.dk
THEORY_FILE=$(SANDBOX)/theory.dk

.PHONY: before
before:
	$(Q)rm -rf $(INPUT)
	$(Q)rm -rf $(OUTPUT)
	$(Q)mv $(TEST_DIR)/in/*.dk $(INPUT)
	$(Q)mv $(TEST_DIR)/theory/*.dk $(SANDBOX)
	$(Q)mv $(TEST_DIR)/universo_cfg.dk $(SANDBOX)

$(INPUT):
	$(Q)mkdir -p $(INPUT)

$(OUTPUT):
	$(Q)mkdir -p $(OUTPUT)
