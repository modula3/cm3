
%module FFTW

%ignore fftw_iodim_do_not_use_me;

%insert(m3rawintf) %{
TYPE
  Plan    <: ADDRESS;
  Complex = RECORD r, i: LONGREAL; END;
  IODim   = RECORD n, is, os: CARDINAL; END;
  FILE    = ADDRESS;
  R2RKind = {
    R2HC,    HC2R,    DHT,
    REDFT00, REDFT01, REDFT10, REDFT11,
    RODFT00, RODFT01, RODFT10, RODFT11
  };
%}

%typemap(m3rawintype)  fftw_plan       %{Plan%};
%typemap(m3rawintype)  fftw_complex *  %{ARRAY OF Complex%};
%typemap(m3rawintype)  fftw_iodim *    %{IODim%};
%typemap(m3rawintype)  fftw_r2r_kind * %{R2RKind%};
%typemap(m3rawrettype) fftw_plan       %{Plan%};

%typemap(m3rawintype)  fftwf_plan       %{Plan%};
%typemap(m3rawintype)  fftwf_complex *  %{ARRAY OF Complex%};
%typemap(m3rawintype)  fftwf_iodim *    %{IODim%};
%typemap(m3rawintype)  fftwf_r2r_kind * %{R2RKind%};
%typemap(m3rawrettype) fftwf_plan       %{Plan%};

%typemap(m3rawintype)  fftwl_plan       %{Plan%};
%typemap(m3rawintype)  fftwl_complex *  %{ARRAY OF Complex%};
%typemap(m3rawintype)  fftwl_iodim *    %{IODim%};
%typemap(m3rawintype)  fftwl_r2r_kind * %{R2RKind%};
%typemap(m3rawrettype) fftwl_plan       %{Plan%};

%typemap(m3rawintype)  void    %{ADDRESS%};
// fftw_export_wisdom
%typemap(m3rawintype)  void (*)(char c, void *)
  %{PROCEDURE (c:CHAR; buf:ADDRESS;)%};
// fftw_import_wisdom
%typemap(m3rawintype)  int (*)(void *)
  %{PROCEDURE (buf:ADDRESS;):CARDINAL%};

#ifdef GetWeirdThingsWorking
/* fftw3.h would generate functions for each
   float type thus we 'import' the definition
   instead of 'include'ing them. */
%import fftw3.h

/* We want only float type per module.
   Chose LONGREAL here. */
FFTW_DEFINE_API(FFTW_MANGLE_DOUBLE, double, fftw_complex)
#else
%include fftw3.h

#endif
