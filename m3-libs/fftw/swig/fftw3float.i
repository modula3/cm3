
%module FFTWReal

%insert(m3rawintf) %{
IMPORT Real AS R;
%}

%insert(m3wrapintf) %{
IMPORT Real AS R;
%}

%insert(m3wrapimpl) %{
IMPORT Real AS R, FFTWRealRaw AS Raw;
%}

#define FPREF(name) fftwf_ ## name
#define REALTYPE float

%include fftw3base.i

/* We want only float type per module.
   Chose REAL here. */
FFTW_DEFINE_API(FFTW_MANGLE_FLOAT, float, fftwf_complex)
