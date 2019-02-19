#
# $Id$
#

# this thing only works with BSD make.

.SUFFIXES: .pdf .ps .dvi .tex .fig .eps .png .eps_bz2

MAIN=mscheme
TARGETS=$(MAIN).pdf
SETINPUTS= TEXINPUTS=`pwd`/../../calarm/doc/common:${TEXINPUTS}; export TEXINPUTS
LATEX= $(SETINPUTS); latex
DVIPS= $(SETINPUTS); dvips
X_ARGS= ./x_args.awk
OTHERSRCS=*.tex

GENFIGURES=

PRIMS=prims.tex

.ps.pdf:
	ps2pdf $*.ps

.dvi.ps: 
	$(DVIPS) -tletter -f -Pcmz < $*.dvi > $*.ps

all: pdfs

pdfs: $(TARGETS)
	scp $(TARGETS) cam:public_html/${MAIN}

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

clean::
	rm -f $(MAIN).ps $(TARGETS) *.log *.aux *~ *.dvi *.toc $(GENFIGURES) *.lot *.lof 
	rm -f ArbStrategy.m3 
	rm -f tradeclient.src

copy: $(MAIN).pdf 	
	scp $(MAIN).pdf cam:public_html

.eps_bz2.eps:
	bzcat < $*.eps_bz2 > $*.eps

.png.eps:
	pngtopnm $*.png | pnmtops -rle -equalpixels -noturn > $*.eps
