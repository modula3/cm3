
# M3FILES = FFTW.i3 FFTW.m3 FFTWRaw.i3 FFTWRaw.m3
M3FILES = FFTWLongReal.i3 FFTWLongReal.m3 FFTWLongRealRaw.i3 FFTWLongRealRaw.m3

all:	fftw

install:
	cm3 -ship

fftw:	src/m3makefile $(patsubst %, src/%, $(M3FILES))
	cm3

src/m3makefile:	swig/fftw3double.i swig/fftw3base.h
	cd swig; swig -modula3 fftw3double.i
	for file in $(M3FILES); do m3pp swig/$$file >src/$$file; rm swig/$$file; done
	mv swig/m3makefile src/

swig/fftw3base.h:	swig/fftw3.h
	egrep -v '^FFTW_DEFINE_API' $< > $@
