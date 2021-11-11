

.PHONY: all split-html clean

all: split-html m3-defn-complete.html m3-defn-complete.ps m3-defn-complete.pdf

clean:
	rm -f html/*.html
	rm -f html/*.png
	rm -f html/*.tex
	rm -f html/*.aux
	rm -f html/*.log
	rm -f html/*.pl
	rm -f html/*.css
	rm -f html/WARNINGS WARNINGS
	rm -f m3-defn-complete.html m3-defn-complete.ps m3-defn-complete.pdf
	rm -f m3-defn-complete.aux
	rm -f m3-defn-complete.css
	rm -f m3-defn-complete.dvi
	rm -f m3-defn-complete.log
	rm -f m3-defn-completeim*
	rm -f m3-defn-completelabels.pl
	rm -f nx_grp_g.png prev_g.png up_g.png

split-html: html/Contents.html

html/Contents.html: m3-defn-complete.tex
	latex2html -address m3-support@elego.de -link 1 -long_titles 3 -show_section_numbers -local_icons -dir html m3-defn-complete.tex
	perl -p -i -e 's;m3-defn-complete.css;../../../../normal.css;' html/*.html

m3-defn-complete.html: m3-defn-complete.tex
	latex2html -address m3-support@elego.de -link 0 -split 0 -show_section_numbers -local_icons -no_subdir -prefix  m3-defn-complete m3-defn-complete.tex

m3-defn-complete.ps: m3-defn-complete.dvi
	dvips -o m3-defn-complete.ps  m3-defn-complete.dvi

m3-defn-complete.pdf: m3-defn-complete.dvi
	dvipdf m3-defn-complete.dvi

m3-defn-complete.dvi: m3-defn-complete.tex
	latex m3-defn-complete.tex
	latex m3-defn-complete.tex
	latex m3-defn-complete.tex
