
M3FILES = PLPlot.i3 PLPlot.m3 PLPlotRaw.i3 PLPlotRaw.m3
LONGREALM3FILES = $(patsubst %, LongReal%, $(M3FILES))

all:	plplot

install:
	cm3 -ship

plplot:	src/m3makefile $(patsubst %, src/%, $(M3FILES))
	cm3 -O

src/m3makefile:	swig/plplotmodula3.i swig/plplotcapi.i
	cd swig; swig -modula3 plplotmodula3.i
	for file in $(LONGREALM3FILES); do m3pp swig/$$file >src/$$file; rm swig/$$file; done
	mv swig/m3makefile src/
