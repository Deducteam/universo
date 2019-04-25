# DKOS need not have the same name that the corresponding Dedukti
# files (modulogic.dk becomes zen.dko) but for each file f.dk, dkdep
# f.dk outputs the name of the produced dko file before the ':'
# character.
DKS = $(wildcard *.dk)
DKOS=$(DKS:.dk=.dko)

DKDEP=dkdep -I ../theory
DKCHECK=~/Git/Dedukti/dkcheck.native -I ../theory -q
DKINDENT=dkindent
all: $(DKOS)
E=-errors-in-snf
%.dko:
#	$(DKCHECK) -e -nl $<
	$(DKCHECK) -e $(E) -nl $<
#	$(DKCHECK) -e -nl $<
#	$(DKINDENT) $< > $(DIR)/$<

fullclean:
	rm -f *.dko .depend
	rm $(subst cic.dk,, $(DKS)) 2>/dev/null || true

clean:
	rm -f *.dko .depend

depend: .depend
.depend:
	$(DKDEP) *.dk > ./.depend

ifneq ($(MAKECMDGOALS), clean)
-include .depend
endif
