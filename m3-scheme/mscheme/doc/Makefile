REPORTS = mscheme-interop-report burroughs-tagged-architecture

SRC  = $(addsuffix .md,$(REPORTS))
TEX  = $(addsuffix .tex,$(REPORTS))
PDF  = $(addsuffix .pdf,$(REPORTS))

PANDOC   = pandoc
PDFLATEX = pdflatex

PANDOC_FLAGS = --standalone \
	       -V geometry:margin=1in \
	       -V fontsize=11pt \
	       -V colorlinks=true

all: $(PDF)

%.tex: %.md
	$(PANDOC) $(PANDOC_FLAGS) -o $@ $<

%.pdf: %.tex
	$(PDFLATEX) -halt-on-error $<
	$(PDFLATEX) $<

clean:
	rm -f $(TEX) $(PDF) *.aux *.log *.out *.toc

.PHONY: all clean
