
M3FILES = FFTW.i3 FFTW.m3 FFTWRaw.i3 FFTWRaw.m3

all:	fftw

install:
	cm3 -ship

fftw:	src/m3makefile $(patsubst %, src/%, $(M3FILES))
	cm3

src/m3makefile:	swig/fftw3.i
	cd swig; swig -modula3 fftw3.i
	for file in $(M3FILES); do m3pp swig/$$file >src/$$file; rm swig/$$file; done
	mv swig/m3makefile src/
