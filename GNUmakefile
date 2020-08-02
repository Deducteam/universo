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

.PHONY: test
test: test/test.sh
	@./tests/test.sh

.PHONY: debug
debug:

	echo $(DEP)

.PHONY: clean
clean:
	@dune clean

.PHONY: install
install: all
	@dune install
