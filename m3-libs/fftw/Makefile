
M3FILEEXTS = FFTW.i3 FFTW.m3 FFTWRaw.i3 FFTWRaw.m3
M3FILES    = $(patsubst %, LongReal%, $(M3FILEEXTS)) \
             $(patsubst %, Real%,     $(M3FILEEXTS)) \
             $(patsubst %, Extended%, $(M3FILEEXTS))

all:	fftw

install:
	cm3 -ship

runtest:
	cd test && cm3 && LINUXLIBC6/test

fftw:	src/m3makefile $(patsubst %, src/%, $(M3FILES))
	cm3

src/LongRealFFTW.i3:	swig/fftw3double.i swig/fftw3float.i swig/fftw3longdouble.i \
                        swig/fftw3base.i swig/fftw3base.h
	(cd swig; \
	  swig -modula3 fftw3double.i; \
	  swig -modula3 fftw3float.i; \
	  swig -modula3 fftw3longdouble.i \
	)
	for file in $(M3FILES); do m3pp swig/$$file >src/$$file; rm swig/$$file; done

swig/fftw3base.h:	swig/fftw3.h
	egrep -v '^FFTW_DEFINE_API' $< > $@
