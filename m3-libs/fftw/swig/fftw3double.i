
%module FFTWLongReal

%insert(m3rawintf) %{
IMPORT LongReal AS R;
%}

%insert(m3wrapintf) %{
IMPORT LongReal AS R;
%}

%insert(m3wrapimpl) %{
IMPORT LongReal AS R, FFTWLongRealRaw AS Raw;
%}

#define FPREF(name) fftw_ ## name
#define REALTYPE double

%include fftw3base.i

/* We want only float type per module.
   Chose LONGREAL here. */
FFTW_DEFINE_API(FFTW_MANGLE_DOUBLE, double, fftw_complex)
