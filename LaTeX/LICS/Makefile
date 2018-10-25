.PHONY: clean all cleanall

# Make sure this has all of the right dependencies
DEPENDS=$(wildcard *.tex *.sty *.bib)

TARGET=universo

all: $(TARGET).pdf

ifdef NL
%.pdf: %.tex $(DEPENDS)
	rubber --shell-escape -f --pdf -s -c "setlist arguments \newif\iflocal \localtrue" $<
	rubber-info --check $<
else
%.pdf: %.tex $(DEPENDS)
	rubber --shell-escape -f --pdf -s -c "setlist arguments \newif\iflocal \localfalse" $<
	rubber-info --check $<
endif

clean:
	rubber --clean $(TARGET)

cleanall:
	rubber --clean --pdf $(TARGET)
