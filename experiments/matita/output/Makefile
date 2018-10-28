# DKOS need not have the same name that the corresponding Dedukti
# files (modulogic.dk becomes zen.dko) but for each file f.dk, dkdep
# f.dk outputs the name of the produced dko file before the ':'
# character.
DKS = $(wildcard *.dk)
DKOS=$(DKS:.dk=.dko)

DKDEP=dkdep -I ../theory
DKCHECK=dkcheck -I ../theory -q
DKINDENT=dkindent
all: $(DKOS)

%.dko:
#	$(DKCHECK) -e -nl $<
	$(DKCHECK) -e -errors-in-snf -nl $<
#	$(DKCHECK) -e -nl $<
#	$(DKINDENT) $< > $(DIR)/$<

clean:
	rm -f *.dko .depend
	rm $(subst cic.dk,, $(DKS)) 2>/dev/null || true

dkclean:
	@ls *.dk | grep -v "cic.dk" | xargs rm

import:
	$(KRAJONO) -extract_dedukti $(LIB)

depend: .depend
.depend:
	$(DKDEP) *.dk > ./.depend

ifneq ($(MAKECMDGOALS), clean)
-include .depend
endif
