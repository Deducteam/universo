#put the path of your dkcheck binary
DKCHECK ?= dkcheck
#put the path of your dkdep binary
DKDEP ?= dkdep
DKOPTIONS = $(DKFLAGS) -nl -errors-in-snf -e

DKS = $(wildcard *.dk)
DKOS = $(DKS:.dk=.dko)


ifdef DKTIME
DKCHECK_=time --format "TIME: %U" --output /dev/stdout $(DKCHECK) $(DKOPTIONS)
else
DKCHECK_=$(DKCHECK) $(DKOPTIONS)
endif


.PHONY:	all clean universo list logic

all: $(DKOS)

cic.dko:
	@$(DKCHECK) -e ../theory/cic.dk

%.dko: %.dk cic.dko
	@echo "FILE: $<"
	@$(DKCHECK) -e -I ../theory $<


# standard .depend file generation
.depend: $(DKS)
	@echo "Producing .depend"
	@$(DKDEP) -I ../theory $^ -o $@

clean:
	@rm -f matita_*.dko tmp.dk

ifneq ($(MAKECMDGOALS), clean)
-include .depend
endif
