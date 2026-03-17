#
# $Id$
#

# this thing only works with BSD make.

.SUFFIXES: .pdf .ps .dvi .tex .fig .eps .png .eps_bz2

MAIN=mscheme
TARGETS=$(MAIN).pdf
SETINPUTS= TEXINPUTS=`pwd`/../common:${TEXINPUTS}; export TEXINPUTS
LATEX= $(SETINPUTS); latex
DVIPS= $(SETINPUTS); dvips
X_ARGS= ./x_args.awk
OTHERSRCS=*.tex

# please change as needed:
DERIVED=AMD64_LINUX

GENFIGURES=

PRIMS=prims.tex

.ps.pdf:
	ps2pdf $*.ps

.dvi.ps: 
	$(DVIPS) -tletter $*.dvi 

all: pdfs mscheme-user-manual.pdf mscheme-internals.pdf

pdfs: run.done $(TARGETS)

nodist: $(TARGETS)

force: 
	rm -f $(MAIN).dvi
	make nodist

$(MAIN).dvi: $(MAIN).tex $(GENFIGURES) $(OTHERSRCS)
	$(LATEX) $(MAIN).tex
	$(LATEX) $(MAIN).tex
	$(LATEX) $(MAIN).tex

.fig.eps:
	fig2dev -L eps $*.fig $*.eps

mscheme-user-manual.pdf: mscheme-user-manual.tex
	pdflatex -interaction=nonstopmode mscheme-user-manual.tex
	pdflatex -interaction=nonstopmode mscheme-user-manual.tex

mscheme-internals.pdf: mscheme-internals.tex
	pdflatex -interaction=nonstopmode mscheme-internals.tex
	pdflatex -interaction=nonstopmode mscheme-internals.tex

clean::
	rm -f $(MAIN).ps $(TARGETS) *.log *.aux *~ *.dvi *.toc $(GENFIGURES) *.lot *.lof
	rm -f mscheme-user-manual.pdf mscheme-user-manual.out
	rm -f mscheme-internals.pdf mscheme-internals.out
	rm -rf $(DERIVED)
	rm -f src/prims.tex
	rm -f *.done

copy: $(MAIN).pdf 	
	scp $(MAIN).pdf cam:public_html

build.done: src/Main.m3
	cm3 && touch build.done

run.done: build.done
	cd src ; echo "(load \"rundoc.scm\")" | ../$(DERIVED)/mscheme-doc doc.scm && touch ../run.done

.eps_bz2.eps:
	bzcat < $*.eps_bz2 > $*.eps

.png.eps:
	pngtopnm $*.png | pnmtops -rle -equalpixels -noturn > $*.eps
