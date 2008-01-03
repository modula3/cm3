
SECTIONS   = integer real32 real64 complex polynomial vector matrix root interpolation sle
SECTIONTEX = $(patsubst %, section_%.tex, $(SECTIONS))
SECTIONPDF = $(patsubst %, section_%.pdf, $(SECTIONS))

DEPEND     = introduction arch concept utils preamble
DEPENDTEX  = $(patsubst %, %.tex, $(DEPEND))

.PHONY:	all clean

all:	pdf

clean:
	-rm 2>/dev/null $(SECTIONTEX) *.aux *.log

sections:	$(SECTIONPDF)

pdf:	m3na.pdf

%.pdf:	%.tex
	pdflatex $<

m3na.pdf:	m3na.tex $(patsubst %, %.tex, $(SECTIONS)) $(DEPENDTEX)

# was the function of 'mkview' before
section_%.tex:	%.tex
	echo  >$@ "\\documentclass[fleqn]{article}"
	echo >>$@ "\\include{preamble}"
	echo >>$@ "\\begin{document}"
	echo >>$@ "\\include{$*}"
	echo >>$@ "\\end{document}"
