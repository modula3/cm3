
%module ExtendedFFTW

%insert(m3rawintf) %{
IMPORT Extended AS R;
%}

%insert(m3wrapintf) %{
IMPORT Extended AS R;
%}

%insert(m3wrapimpl) %{
IMPORT Extended AS R, ExtendedFFTWRaw AS Raw;
%}

#define FPREF(name) fftwl_ ## name
#define REALTYPE long double

%include fftw3base.i

/* We want only float type per module.
   Chose EXTENDED here. */

typedef long double long_double;

FFTW_DEFINE_API(FFTW_MANGLE_LONG_DOUBLE, long_double, fftwl_complex)
