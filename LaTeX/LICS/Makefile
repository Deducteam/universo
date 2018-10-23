.PHONY: clean all cleanall

# Make sure this has all of the right dependencies
DEPENDS=$(wildcard *.tex *.sty *.bib)

TARGET=universo

all: $(TARGET).pdf

%.pdf: %.tex $(DEPENDS)
	rubber --shell-escape -f --pdf -s $<
	rubber-info --check $<

clean:
	rubber --clean $(TARGET)

cleanall:
	rubber --clean --pdf $(TARGET)
