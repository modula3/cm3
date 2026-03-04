# ESC/Modula-3 documentation

TEX = pdflatex
TEXFLAGS = -interaction=nonstopmode

DOCS = escm3-manual.pdf

.PHONY: all clean

all: $(DOCS)

escm3-manual.pdf: escm3-manual.tex
	$(TEX) $(TEXFLAGS) $<
	$(TEX) $(TEXFLAGS) $<
	$(TEX) $(TEXFLAGS) $<

clean:
	rm -f *.aux *.log *.out *.toc *.pdf
