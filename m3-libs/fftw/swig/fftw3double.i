
%module FFTWLongReal

%pragma(modula3) unsafe="true";
%pragma(modula3) library="m3fftw";

/*
  RTType.GetNDimensions and RTHeap.GetArrayShape
  for retrieving data from open arrays
  with unknown number of dimensions.
  RTHooks.AllocateOpenArray
  is probably not of great help.
  RTTypeMap.WalkRef walks through all
  fields of a RECORD(?), ARRAY(?).
*/

%ignore fftw_iodim_do_not_use_me;
%ignore fftw_r2r_kind_do_not_use_me;

%insert(m3rawintf) %{
TYPE
  Plan    <: ADDRESS;
  Complex = RECORD r, i: LONGREAL; END;
  IODim   = RECORD n, is, os: CARDINAL; END;
  R2RKind = {
    R2HC,    HC2R,    DHT,
    REDFT00, REDFT01, REDFT10, REDFT11,
    RODFT00, RODFT01, RODFT10, RODFT11
  };
%}

%insert(m3rawimpl) %{
REVEAL
  Plan = UNTRACED BRANDED REF RECORD END;
%}

%typemap(m3rawintype)  fftw_plan       %{Plan%};
%typemap(m3rawinmode)  fftw_plan       %{%};
%typemap(m3rawrettype) fftw_plan       %{Plan%};
%typemap(m3rawintype)  fftw_iodim *    %{IODim%};
%typemap(m3rawintype)  fftw_r2r_kind * %{R2RKind%};

%typemap(m3rawintype)  fftw_complex *  %{Complex%};
%typemap(m3rawintype)  fftw_complex_array_1d *  %{(* ARRAY OF *) Complex%};
%typemap(m3rawintype)  fftw_complex_array_2d *  %{(* ARRAY OF ARRAY OF *) Complex%};
%typemap(m3rawintype)  fftw_complex_array_3d *  %{(* ARRAY OF ARRAY OF ARRAY OF *) Complex%};

%typemap(m3rawinmode)  fftw_complex_array_1d *in,
                       fftw_complex_array_2d *in,
                       fftw_complex_array_3d *in,
		       double *in
		         %{READONLY%};


%typemap(m3rawintype)  void  * %{ADDRESS%};
// fftw_export_wisdom
%typemap(m3rawintype)  void (*)(char c, void *)
  %{PROCEDURE (c:CHAR; buf:ADDRESS;)%};
// fftw_import_wisdom
%typemap(m3rawintype)  int (*)(void *)
  %{PROCEDURE (buf:ADDRESS;):CARDINAL%};


%insert(m3wrapintf) %{
FROM FFTWLongRealRaw IMPORT R2RKind;

TYPE
  Plan    <: REFANY;
  Complex = RECORD r, i: LONGREAL; END;
  Dir = {forward, backward};

EXCEPTION
  SizeMismatch;

%}

%insert(m3wrapimpl) %{
FROM FFTWLongRealRaw IMPORT R2RKind;

CONST
  dirToSign = ARRAY Dir OF [-1..1] {-1,1};

REVEAL
  Plan = BRANDED REF FFTWLongRealRaw.Plan;

PROCEDURE CleanupPlan(<*UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
  BEGIN
    FFTWLongRealRaw.DestroyPlan(NARROW(r,Plan)^);
  END CleanupPlan;

%}

%typemap(m3wrapintype)   fftw_plan       %{Plan%};
%typemap(m3wrapinmode)   fftw_plan       %{%};
%typemap(m3wraprettype)  fftw_plan       %{Plan%};
%typemap(m3wrapargraw)   fftw_plan       %{$input^%};
%typemap(m3wrapretvar)   fftw_plan       %{plan:=NEW(Plan);%};
%typemap(m3wrapretraw)   fftw_plan       %{plan^%};
%typemap(m3wrapretcheck) fftw_plan       %{EVAL WeakRef.FromRef(plan,CleanupPlan);%};
%typemap(m3wrapretconv)  fftw_plan       %{plan%};
%typemap("m3wrapretcheck:import") fftw_plan %{WeakRef%};

%typemap(m3wrapintype)  double *        %{LONGREAL%};
%typemap(m3wrapintype)  fftw_iodim *    %{IODim%};

%typemap(m3wrapintype)  int sign        %{Dir%};
%typemap(m3wrapargraw)  int sign        %{dirToSign[$input]%};

%typemap(m3wrapintype)  fftw_r2r_kind * %{ARRAY OF R2RKind%};
%typemap(m3wrapintype)  fftw_r2r_kind   %{R2RKind%};
%typemap(m3wrapargraw)  fftw_r2r_kind   %{ORD($input)%};

%typemap(m3wrapintype)  fftw_complex_array_1d *  %{REF ARRAY OF Complex%};
%typemap(m3wrapintype)  fftw_complex_array_2d *  %{REF ARRAY OF ARRAY OF Complex%};
%typemap(m3wrapintype)  fftw_complex_array_3d *  %{REF ARRAY OF ARRAY OF ARRAY OF Complex%};
%typemap(m3wrapintype)  fftw_complex_tensor   *  %{REFANY (* Tensor of Complex *) %};

%typemap(m3wrapintype)  fftw_real_array_1d *  %{REF ARRAY OF LONGREAL%};
%typemap(m3wrapintype)  fftw_real_array_2d *  %{REF ARRAY OF ARRAY OF LONGREAL%};
%typemap(m3wrapintype)  fftw_real_array_3d *  %{REF ARRAY OF ARRAY OF ARRAY OF LONGREAL%};
%typemap(m3wrapintype)  fftw_real_tensor   *  %{REFANY (* Tensor of LONGREAL *) %};

%typemap(m3wrapargraw)  fftw_complex_array_1d *, fftw_real_array_1d *  %{$input[0]%};
%typemap(m3wrapargraw)  fftw_complex_array_2d *, fftw_real_array_2d *  %{$input[0,0]%};
%typemap(m3wrapargraw)  fftw_complex_array_3d *, fftw_real_array_3d *  %{$input[0,0,0]%};
%typemap(m3wrapargraw)  fftw_complex_tensor   *, fftw_real_tensor   *  %{$input%};

%typemap(m3wrapinmode)
	fftw_complex_array_1d *, fftw_real_array_1d *,
	fftw_complex_array_2d *, fftw_real_array_2d *,
	fftw_complex_array_3d *, fftw_real_array_3d *
	  "";

%typemap(m3wrapintype,numinputs=0) int n, int nx, int ny, int nz %{%}

%typemap(m3wrapargvar) fftw_complex_array_1d *in, fftw_real_array_1d *in
  %{n := NUMBER($input^);%};
%typemap(m3wrapargvar) fftw_complex_array_2d *in, fftw_real_array_2d *in
  %{nx := NUMBER($input^); ny := NUMBER($input[0]);%};
%typemap(m3wrapargvar) fftw_complex_array_3d *in, fftw_real_array_3d *in
  %{nx := NUMBER($input^); ny := NUMBER($input[0]); nz := NUMBER($input[0,0]);%};
%typemap(m3wrapargvar) fftw_complex_tensor   *in, fftw_real_tensor   *in
  %{$input%};

%typemap(m3wrapincheck) fftw_complex_array_1d *out, fftw_real_array_1d *out
  %{IF n # NUMBER($input^) THEN RAISE SizeMismatch; END;%};
%typemap(m3wrapincheck) fftw_complex_array_2d *out, fftw_real_array_2d *out
  %{IF nx # NUMBER($input^) OR ny # NUMBER($input[0]) THEN RAISE SizeMismatch; END;%};
%typemap(m3wrapincheck) fftw_complex_array_3d *out, fftw_real_array_3d *out
  %{IF nx # NUMBER($input^) OR ny # NUMBER($input[0]) OR nz # NUMBER($input[0,0]) THEN RAISE SizeMismatch; END;%};
%typemap(m3wrapincheck) fftw_complex_tensor   *out, fftw_real_tensor   *out
  %{$input%};

%typemap("m3wrapincheck:throws")
	fftw_complex_array_1d *out, fftw_real_array_1d *out,
	fftw_complex_array_2d *out, fftw_real_array_2d *out,
	fftw_complex_array_3d *out, fftw_real_array_3d *out
	  "SizeMismatch";

%typemap(m3wrapintype)  void  * %{REFANY%};
%typemap(m3wraprettype) void  * %{REFANY%};

// fftw_export_wisdom
%typemap(m3wrapintype)  void (*)(char c, void *)
  %{PROCEDURE (c:CHAR; buf:ADDRESS;)%};
// fftw_import_wisdom
%typemap(m3wrapintype)  int (*)(void *)
  %{PROCEDURE (buf:ADDRESS;):CARDINAL%};


%ignore FFTW_FORWARD;
%ignore FFTW_BACKWARD;
%ignore FFTW_MEASURE;
%ignore FFTW_DESTROY_INPUT;
%ignore FFTW_UNALIGNED;
%ignore FFTW_CONSERVE_MEMORY;
%ignore FFTW_EXHAUSTIVE;
%ignore FFTW_PRESERVE_INPUT;
%ignore FFTW_PATIENT;
%ignore FFTW_ESTIMATE;
%ignore FFTW_ESTIMATE_PATIENT;
%ignore FFTW_BELIEVE_PCOST;
%ignore FFTW_DFT_R2HC_ICKY;
%ignore FFTW_NONTHREADED_ICKY;
%ignore FFTW_NO_BUFFERING;
%ignore FFTW_NO_INDIRECT_OP;
%ignore FFTW_ALLOW_LARGE_GENERIC;
%ignore FFTW_NO_RANK_SPLITS;
%ignore FFTW_NO_VRANK_SPLITS;
%ignore FFTW_NO_VRECURSE;
%ignore FFTW_NO_SIMD;



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

/* ignore all functions that I have not considered so far */
%ignore fftw_plan_dft;
%ignore fftw_plan_many_dft;
%ignore fftw_plan_guru_dft;
%ignore fftw_plan_guru_split_dft;
%ignore fftw_execute_dft;
%ignore fftw_execute_split_dft;
%ignore fftw_plan_many_dft_r2c;
%ignore fftw_plan_dft_r2c;
%ignore fftw_plan_many_dft_c2r;
%ignore fftw_plan_dft_c2r;
%ignore fftw_plan_guru_dft_r2c;
%ignore fftw_plan_guru_dft_c2r;
%ignore fftw_plan_guru_split_dft_r2c;
%ignore fftw_plan_guru_split_dft_c2r;
%ignore fftw_execute_dft_r2c;
%ignore fftw_execute_dft_c2r;
%ignore fftw_execute_split_dft_r2c;
%ignore fftw_execute_split_dft_c2r;
%ignore fftw_plan_many_r2r;
%ignore fftw_plan_r2r;
%ignore fftw_plan_guru_r2r;
%ignore fftw_execute_r2r;
%ignore fftw_plan_with_nthreads;
%ignore fftw_init_threads;
%ignore fftw_cleanup_threads;
%ignore fftw_export_wisdom_to_file;
%ignore fftw_export_wisdom_to_string;
%ignore fftw_export_wisdom;
%ignore fftw_import_system_wisdom;
%ignore fftw_import_wisdom_from_file;
%ignore fftw_import_wisdom_from_string;
%ignore fftw_import_wisdom;
%ignore fftw_fprint_plan;
%ignore fftw_print_plan;
%ignore fftw_malloc;
%ignore fftw_free;
%ignore fftw_flops;


/* This is the original fftw3.h file
   without the FFTW_DEFINE_API macro calls. */
%include fftw3base.h

/* We want only float type per module.
   Chose LONGREAL here. */
FFTW_DEFINE_API(FFTW_MANGLE_DOUBLE, double, fftw_complex)
