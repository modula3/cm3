
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




// adapt to Modula-3 conform names

%rename("Execute") fftw_execute;
%rename("PlanDFT") fftw_plan_dft;
%rename("PlanDFT1D") fftw_plan_dft_1d;
%rename("PlanDFT2D") fftw_plan_dft_2d;
%rename("PlanDFT3D") fftw_plan_dft_3d;
%rename("PlanManyDFT") fftw_plan_many_dft;
%rename("PlanGuruDFT") fftw_plan_guru_dft;
%rename("PlanGuruSplitDFT") fftw_plan_guru_split_dft;
%rename("ExecuteDFT") fftw_execute_dft;
%rename("ExecuteSplitDFT") fftw_execute_split_dft;
%rename("PlanManyDFTR2C") fftw_plan_many_dft_r2c;
%rename("PlanDFTR2C") fftw_plan_dft_r2c;
%rename("PlanDFTR2C1D") fftw_plan_dft_r2c_1d;
%rename("PlanDFTR2C2D") fftw_plan_dft_r2c_2d;
%rename("PlanDFTR2C3D") fftw_plan_dft_r2c_3d;
%rename("PlanManyDFTC2R") fftw_plan_many_dft_c2r;
%rename("PlanDFTC2R") fftw_plan_dft_c2r;
%rename("PlanDFTC2R1D") fftw_plan_dft_c2r_1d;
%rename("PlanDFTC2R2D") fftw_plan_dft_c2r_2d;
%rename("PlanDFTC2R3D") fftw_plan_dft_c2r_3d;
%rename("PlanGuruDFTR2C") fftw_plan_guru_dft_r2c;
%rename("PlanGuruDFTC2R") fftw_plan_guru_dft_c2r;
%rename("PlanGuruSplitDFTR2C") fftw_plan_guru_split_dft_r2c;
%rename("PlanGuruSplitDFTC2R") fftw_plan_guru_split_dft_c2r;
%rename("ExecuteDFTR2C") fftw_execute_dft_r2c;
%rename("ExecuteDFTC2R") fftw_execute_dft_c2r;
%rename("ExecuteSplitDFTR2C") fftw_execute_split_dft_r2c;
%rename("ExecuteSplitDFTC2R") fftw_execute_split_dft_c2r;
%rename("PlanManyR2R") fftw_plan_many_r2r;
%rename("PlanR2R") fftw_plan_r2r;
%rename("PlanR2R1D") fftw_plan_r2r_1d;
%rename("PlanR2R2D") fftw_plan_r2r_2d;
%rename("PlanR2R3D") fftw_plan_r2r_3d;
%rename("PlanGuruR2R") fftw_plan_guru_r2r;
%rename("ExecuteR2R") fftw_execute_r2r;
%rename("DestroyPlan") fftw_destroy_plan;
%rename("ForgetWisdom") fftw_forget_wisdom;
%rename("Cleanup") fftw_cleanup;
%rename("PlanWithNThreads") fftw_plan_with_nthreads;
%rename("InitThreads") fftw_init_threads;
%rename("CleanupThreads") fftw_cleanup_threads;
%rename("ExportWisdomToFile") fftw_export_wisdom_to_file;
%rename("ExportWisdomToString") fftw_export_wisdom_to_string;
%rename("ExportWisdom") fftw_export_wisdom;
%rename("ImportSystemWisdom") fftw_import_system_wisdom;
%rename("ImportWisdomFromFile") fftw_import_wisdom_from_file;
%rename("ImportWisdomFromString") fftw_import_wisdom_from_string;
%rename("ImportWisdom") fftw_import_wisdom;
%rename("FPrintPlan") fftw_fprint_plan;
%rename("PrintPlan") fftw_print_plan;
%rename("Malloc") fftw_malloc;
%rename("Free") fftw_free;
%rename("Flops") fftw_flops;

%ignore fftwf_execute;
%ignore fftwf_plan_dft;
%ignore fftwf_plan_dft_1d;
%ignore fftwf_plan_dft_2d;
%ignore fftwf_plan_dft_3d;
%ignore fftwf_plan_many_dft;
%ignore fftwf_plan_guru_dft;
%ignore fftwf_plan_guru_split_dft;
%ignore fftwf_execute_dft;
%ignore fftwf_execute_split_dft;
%ignore fftwf_plan_many_dft_r2c;
%ignore fftwf_plan_dft_r2c;
%ignore fftwf_plan_dft_r2c_1d;
%ignore fftwf_plan_dft_r2c_2d;
%ignore fftwf_plan_dft_r2c_3d;
%ignore fftwf_plan_many_dft_c2r;
%ignore fftwf_plan_dft_c2r;
%ignore fftwf_plan_dft_c2r_1d;
%ignore fftwf_plan_dft_c2r_2d;
%ignore fftwf_plan_dft_c2r_3d;
%ignore fftwf_plan_guru_dft_r2c;
%ignore fftwf_plan_guru_dft_c2r;
%ignore fftwf_plan_guru_split_dft_r2c;
%ignore fftwf_plan_guru_split_dft_c2r;
%ignore fftwf_execute_dft_r2c;
%ignore fftwf_execute_dft_c2r;
%ignore fftwf_execute_split_dft_r2c;
%ignore fftwf_execute_split_dft_c2r;
%ignore fftwf_plan_many_r2r;
%ignore fftwf_plan_r2r;
%ignore fftwf_plan_r2r_1d;
%ignore fftwf_plan_r2r_2d;
%ignore fftwf_plan_r2r_3d;
%ignore fftwf_plan_guru_r2r;
%ignore fftwf_execute_r2r;
%ignore fftwf_destroy_plan;
%ignore fftwf_forget_wisdom;
%ignore fftwf_cleanup;
%ignore fftwf_plan_with_nthreads;
%ignore fftwf_init_threads;
%ignore fftwf_cleanup_threads;
%ignore fftwf_export_wisdom_to_file;
%ignore fftwf_export_wisdom_to_string;
%ignore fftwf_export_wisdom;
%ignore fftwf_import_system_wisdom;
%ignore fftwf_import_wisdom_from_file;
%ignore fftwf_import_wisdom_from_string;
%ignore fftwf_import_wisdom;
%ignore fftwf_fprint_plan;
%ignore fftwf_print_plan;
%ignore fftwf_malloc;
%ignore fftwf_free;
%ignore fftwf_flops;

%ignore fftwl_execute;
%ignore fftwl_plan_dft;
%ignore fftwl_plan_dft_1d;
%ignore fftwl_plan_dft_2d;
%ignore fftwl_plan_dft_3d;
%ignore fftwl_plan_many_dft;
%ignore fftwl_plan_guru_dft;
%ignore fftwl_plan_guru_split_dft;
%ignore fftwl_execute_dft;
%ignore fftwl_execute_split_dft;
%ignore fftwl_plan_many_dft_r2c;
%ignore fftwl_plan_dft_r2c;
%ignore fftwl_plan_dft_r2c_1d;
%ignore fftwl_plan_dft_r2c_2d;
%ignore fftwl_plan_dft_r2c_3d;
%ignore fftwl_plan_many_dft_c2r;
%ignore fftwl_plan_dft_c2r;
%ignore fftwl_plan_dft_c2r_1d;
%ignore fftwl_plan_dft_c2r_2d;
%ignore fftwl_plan_dft_c2r_3d;
%ignore fftwl_plan_guru_dft_r2c;
%ignore fftwl_plan_guru_dft_c2r;
%ignore fftwl_plan_guru_split_dft_r2c;
%ignore fftwl_plan_guru_split_dft_c2r;
%ignore fftwl_execute_dft_r2c;
%ignore fftwl_execute_dft_c2r;
%ignore fftwl_execute_split_dft_r2c;
%ignore fftwl_execute_split_dft_c2r;
%ignore fftwl_plan_many_r2r;
%ignore fftwl_plan_r2r;
%ignore fftwl_plan_r2r_1d;
%ignore fftwl_plan_r2r_2d;
%ignore fftwl_plan_r2r_3d;
%ignore fftwl_plan_guru_r2r;
%ignore fftwl_execute_r2r;
%ignore fftwl_destroy_plan;
%ignore fftwl_forget_wisdom;
%ignore fftwl_cleanup;
%ignore fftwl_plan_with_nthreads;
%ignore fftwl_init_threads;
%ignore fftwl_cleanup_threads;
%ignore fftwl_export_wisdom_to_file;
%ignore fftwl_export_wisdom_to_string;
%ignore fftwl_export_wisdom;
%ignore fftwl_import_system_wisdom;
%ignore fftwl_import_wisdom_from_file;
%ignore fftwl_import_wisdom_from_string;
%ignore fftwl_import_wisdom;
%ignore fftwl_fprint_plan;
%ignore fftwl_print_plan;
%ignore fftwl_malloc;
%ignore fftwl_free;
%ignore fftwl_flops;






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
