SRC  = mscheme-interop-report.md
TEX  = $(SRC:.md=.tex)
PDF  = $(SRC:.md=.pdf)

PANDOC   = pandoc
PDFLATEX = pdflatex

PANDOC_FLAGS = --standalone \
	       -V geometry:margin=1in \
	       -V fontsize=11pt \
	       -V colorlinks=true

all: $(PDF)

$(TEX): $(SRC)
	$(PANDOC) $(PANDOC_FLAGS) -o $@ $<

$(PDF): $(TEX)
	$(PDFLATEX) -halt-on-error $<
	$(PDFLATEX) $<

clean:
	rm -f $(TEX) $(PDF) *.aux *.log *.out *.toc

.PHONY: all clean
