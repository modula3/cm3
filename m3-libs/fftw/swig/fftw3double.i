
%module FFTWLongReal

%pragma(modula3) unsafe="true";
%pragma(modula3) library="m3fftw";

#define FPREF(name) fftw_ ## name

%insert(m3makefile) %{% compiled / works with with CM3 5.2.6 2003-06-27
import_lib("fftw3","/usr/lib")%}

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

%typemap(m3rawintype)  FPREF(plan)       %{Plan%};
%typemap(m3rawinmode)  FPREF(plan)       %{%};
%typemap(m3rawrettype) FPREF(plan)       %{Plan%};
%typemap(m3rawintype)  FPREF(iodim) *    %{IODim%};
%typemap(m3rawintype)  FPREF(r2r_kind) * %{R2RKind%};

%typemap(m3rawintype)  FPREF(complex) *  %{Complex%};
%typemap(m3rawintype)  FPREF(complex_array_1d) *  %{(* ARRAY OF *) Complex%};
%typemap(m3rawintype)  FPREF(complex_array_2d) *  %{(* ARRAY OF ARRAY OF *) Complex%};
%typemap(m3rawintype)  FPREF(complex_array_3d) *  %{(* ARRAY OF ARRAY OF ARRAY OF *) Complex%};

%typemap(m3rawinmode)  FPREF(complex_array_1d) *in,
                       FPREF(complex_array_2d) *in,
                       FPREF(complex_array_3d) *in,
		       double *in
		         %{READONLY%};


%typemap(m3rawintype)  void  * %{ADDRESS%};
// FPREF(export_wisdom)
%typemap(m3rawintype)  void (*)(char c, void *)
  %{PROCEDURE (c:CHAR; buf:ADDRESS;)%};
// FPREF(import_wisdom)
%typemap(m3rawintype)  int (*)(void *)
  %{PROCEDURE (buf:ADDRESS;):CARDINAL%};


%insert(m3wrapintf) %{
FROM FFTWLongRealRaw IMPORT R2RKind;

TYPE
  Plan    <: REFANY;
  Complex = RECORD r, i: LONGREAL; END;
  Dir = {Forward, Backward};

  FlagSet=SET OF Flag;
  Flag={
(* documented flags *)
DestroyInput,
Unaligned,
ConserveMemory,
Exhaustive, (* NO_EXHAUSTIVE is default *)
PreserveInput, (* cancels FFTW_DESTROY_INPUT *)
Patient, (* IMPATIENT is default *)
Estimate,

(* undocumented beyond-guru flags *)
EstimatePatient,
BelievePCost,
DFT_R2HC_Icky,
NonThreaded_Icky,
NoBuffering,
NoIndirectOp,
AllowLargeGeneric, (* NO_LARGE_GENERIC is default *)
NoRankSplits,
NoVRankSplits,
NoVRecurse,

NoSIMD};

CONST
  Measure=FlagSet{};

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

%typemap(m3wrapintype)   FPREF(plan)       %{Plan%};
%typemap(m3wrapinmode)   FPREF(plan)       %{%};
%typemap(m3wraprettype)  FPREF(plan)       %{Plan%};
%typemap(m3wrapargraw)   FPREF(plan)       %{$input^%};
%typemap(m3wrapretvar)   FPREF(plan)       %{plan:=NEW(Plan);%};
%typemap(m3wrapretraw)   FPREF(plan)       %{plan^%};
%typemap(m3wrapretcheck) FPREF(plan)       %{EVAL WeakRef.FromRef(plan,CleanupPlan);%};
%typemap(m3wrapretconv)  FPREF(plan)       %{plan%};
%typemap("m3wrapretcheck:import") FPREF(plan) %{WeakRef%};

%typemap(m3wrapintype)  double *        %{LONGREAL%};
%typemap(m3wrapintype)  FPREF(iodim) *    %{IODim%};

%typemap(m3wrapintype)    int sign        %{Dir%};
%typemap(m3wrapindefault) int sign        %{Dir.Backward%};
%typemap(m3wrapargraw)    int sign        %{dirToSign[$input]%};

%typemap(m3wrapintype)    unsigned flags  %{FlagSet%};
%typemap(m3wrapindefault) unsigned flags  %{FlagSet{Flag.Estimate}%};
%typemap(m3wrapargraw)    unsigned flags  %{LOOPHOLE($input,C.unsigned_int)%};
%typemap("m3wrapargraw:import") unsigned flags "Ctypes AS C"

%typemap(m3wrapintype)  FPREF(r2r_kind) * %{ARRAY OF R2RKind%};
%typemap(m3wrapintype)  FPREF(r2r_kind)   %{R2RKind%};
%typemap(m3wrapargraw)  FPREF(r2r_kind)   %{ORD($input)%};

%typemap(m3wrapintype)  FPREF(complex_array_1d) *  %{REF ARRAY OF Complex%};
%typemap(m3wrapintype)  FPREF(complex_array_2d) *  %{REF ARRAY OF ARRAY OF Complex%};
%typemap(m3wrapintype)  FPREF(complex_array_3d) *  %{REF ARRAY OF ARRAY OF ARRAY OF Complex%};
%typemap(m3wrapintype)  FPREF(complex_tensor)   *  %{REFANY (* Tensor of Complex *) %};

%typemap(m3wrapintype)  FPREF(real_array_1d) *  %{REF ARRAY OF LONGREAL%};
%typemap(m3wrapintype)  FPREF(real_array_2d) *  %{REF ARRAY OF ARRAY OF LONGREAL%};
%typemap(m3wrapintype)  FPREF(real_array_3d) *  %{REF ARRAY OF ARRAY OF ARRAY OF LONGREAL%};
%typemap(m3wrapintype)  FPREF(real_tensor)   *  %{REFANY (* Tensor of LONGREAL *) %};

%typemap(m3wrapargraw)  FPREF(complex_array_1d) *, FPREF(real_array_1d) *  %{$input[0]%};
%typemap(m3wrapargraw)  FPREF(complex_array_2d) *, FPREF(real_array_2d) *  %{$input[0,0]%};
%typemap(m3wrapargraw)  FPREF(complex_array_3d) *, FPREF(real_array_3d) *  %{$input[0,0,0]%};
%typemap(m3wrapargraw)  FPREF(complex_tensor)   *, FPREF(real_tensor)   *  %{$input%};

%typemap(m3wrapinmode)
	FPREF(complex_array_1d) *, FPREF(real_array_1d) *,
	FPREF(complex_array_2d) *, FPREF(real_array_2d) *,
	FPREF(complex_array_3d) *, FPREF(real_array_3d) *
	  "";

%typemap(m3wrapintype,numinputs=0) int n, int nx, int ny, int nz %{%}

%typemap(m3wrapargvar) FPREF(complex_array_1d) *in, FPREF(real_array_1d) *in
  %{n := NUMBER($input^);%};
%typemap(m3wrapargvar) FPREF(complex_array_2d) *in, FPREF(real_array_2d) *in
  %{nx := NUMBER($input^); ny := NUMBER($input[0]);%};
%typemap(m3wrapargvar) FPREF(complex_array_3d) *in, FPREF(real_array_3d) *in
  %{nx := NUMBER($input^); ny := NUMBER($input[0]); nz := NUMBER($input[0,0]);%};
%typemap(m3wrapargvar) FPREF(complex_tensor)   *in, FPREF(real_tensor)   *in
  %{$input%};

%typemap(m3wrapincheck) FPREF(complex_array_1d) *out, FPREF(real_array_1d) *out
  %{IF n # NUMBER($input^) THEN RAISE SizeMismatch; END;%};
%typemap(m3wrapincheck) FPREF(complex_array_2d) *out, FPREF(real_array_2d) *out
  %{IF nx # NUMBER($input^) OR ny # NUMBER($input[0]) THEN RAISE SizeMismatch; END;%};
%typemap(m3wrapincheck) FPREF(complex_array_3d) *out, FPREF(real_array_3d) *out
  %{IF nx # NUMBER($input^) OR ny # NUMBER($input[0]) OR nz # NUMBER($input[0,0]) THEN RAISE SizeMismatch; END;%};
%typemap(m3wrapincheck) FPREF(complex_tensor)   *out, FPREF(real_tensor)   *out
  %{$input%};

%typemap("m3wrapincheck:throws")
	FPREF(complex_array_1d) *out, FPREF(real_array_1d) *out,
	FPREF(complex_array_2d) *out, FPREF(real_array_2d) *out,
	FPREF(complex_array_3d) *out, FPREF(real_array_3d) *out
	  "SizeMismatch";

%typemap(m3wraprettype) void  * %{REFANY%};

// never tested if this is sensible
%typemap(m3wrapintype)  void  *data %{REFANY%};
%typemap(m3wrapargraw)  void  *data %{LOOPHOLE(data,ADDRESS)%};

// FPREF(export_wisdom)
%typemap(m3wrapintype)  void (*)(char c, void *)
  %{PROCEDURE (c:CHAR; buf:ADDRESS;)%};
// FPREF(import_wisdom)
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

%rename("Execute") FPREF(execute);
%rename("PlanDFT") FPREF(plan_dft);
%rename("PlanDFT1D") FPREF(plan_dft_1d);
%rename("PlanDFT2D") FPREF(plan_dft_2d);
%rename("PlanDFT3D") FPREF(plan_dft_3d);
%rename("PlanManyDFT") FPREF(plan_many_dft);
%rename("PlanGuruDFT") FPREF(plan_guru_dft);
%rename("PlanGuruSplitDFT") FPREF(plan_guru_split_dft);
%rename("ExecuteDFT") FPREF(execute_dft);
%rename("ExecuteSplitDFT") FPREF(execute_split_dft);
%rename("PlanManyDFTR2C") FPREF(plan_many_dft_r2c);
%rename("PlanDFTR2C") FPREF(plan_dft_r2c);
%rename("PlanDFTR2C1D") FPREF(plan_dft_r2c_1d);
%rename("PlanDFTR2C2D") FPREF(plan_dft_r2c_2d);
%rename("PlanDFTR2C3D") FPREF(plan_dft_r2c_3d);
%rename("PlanManyDFTC2R") FPREF(plan_many_dft_c2r);
%rename("PlanDFTC2R") FPREF(plan_dft_c2r);
%rename("PlanDFTC2R1D") FPREF(plan_dft_c2r_1d);
%rename("PlanDFTC2R2D") FPREF(plan_dft_c2r_2d);
%rename("PlanDFTC2R3D") FPREF(plan_dft_c2r_3d);
%rename("PlanGuruDFTR2C") FPREF(plan_guru_dft_r2c);
%rename("PlanGuruDFTC2R") FPREF(plan_guru_dft_c2r);
%rename("PlanGuruSplitDFTR2C") FPREF(plan_guru_split_dft_r2c);
%rename("PlanGuruSplitDFTC2R") FPREF(plan_guru_split_dft_c2r);
%rename("ExecuteDFTR2C") FPREF(execute_dft_r2c);
%rename("ExecuteDFTC2R") FPREF(execute_dft_c2r);
%rename("ExecuteSplitDFTR2C") FPREF(execute_split_dft_r2c);
%rename("ExecuteSplitDFTC2R") FPREF(execute_split_dft_c2r);
%rename("PlanManyR2R") FPREF(plan_many_r2r);
%rename("PlanR2R") FPREF(plan_r2r);
%rename("PlanR2R1D") FPREF(plan_r2r_1d);
%rename("PlanR2R2D") FPREF(plan_r2r_2d);
%rename("PlanR2R3D") FPREF(plan_r2r_3d);
%rename("PlanGuruR2R") FPREF(plan_guru_r2r);
%rename("ExecuteR2R") FPREF(execute_r2r);
%rename("DestroyPlan") FPREF(destroy_plan);
%rename("ForgetWisdom") FPREF(forget_wisdom);
%rename("Cleanup") FPREF(cleanup);
%rename("PlanWithNThreads") FPREF(plan_with_nthreads);
%rename("InitThreads") FPREF(init_threads);
%rename("CleanupThreads") FPREF(cleanup_threads);
%rename("ExportWisdomToFile") FPREF(export_wisdom_to_file);
%rename("ExportWisdomToString") FPREF(export_wisdom_to_string);
%rename("ExportWisdom") FPREF(export_wisdom);
%rename("ImportSystemWisdom") FPREF(import_system_wisdom);
%rename("ImportWisdomFromFile") FPREF(import_wisdom_from_file);
%rename("ImportWisdomFromString") FPREF(import_wisdom_from_string);
%rename("ImportWisdom") FPREF(import_wisdom);
%rename("FPrintPlan") FPREF(fprint_plan);
%rename("PrintPlan") FPREF(print_plan);
%rename("Malloc") FPREF(malloc);
%rename("Free") FPREF(free);
%rename("Flops") FPREF(flops);

/* ignore all functions that I have not considered so far */
%ignore FPREF(plan_dft);
%ignore FPREF(plan_many_dft);
%ignore FPREF(plan_guru_dft);
%ignore FPREF(plan_guru_split_dft);
%ignore FPREF(execute_dft);
%ignore FPREF(execute_split_dft);
%ignore FPREF(plan_many_dft_r2c);
%ignore FPREF(plan_dft_r2c);
%ignore FPREF(plan_many_dft_c2r);
%ignore FPREF(plan_dft_c2r);
%ignore FPREF(plan_guru_dft_r2c);
%ignore FPREF(plan_guru_dft_c2r);
%ignore FPREF(plan_guru_split_dft_r2c);
%ignore FPREF(plan_guru_split_dft_c2r);
%ignore FPREF(execute_dft_r2c);
%ignore FPREF(execute_dft_c2r);
%ignore FPREF(execute_split_dft_r2c);
%ignore FPREF(execute_split_dft_c2r);
%ignore FPREF(plan_many_r2r);
%ignore FPREF(plan_r2r);
%ignore FPREF(plan_guru_r2r);
%ignore FPREF(execute_r2r);
%ignore FPREF(malloc);
%ignore FPREF(free);

/* thread functions are not present in the libraries I have installed */
%ignore FPREF(plan_with_nthreads);
%ignore FPREF(init_threads);
%ignore FPREF(cleanup_threads);


/* This is the original fftw3.h file
   without the FFTW_DEFINE_API macro calls. */
%include fftw3base.h

/* We want only float type per module.
   Chose LONGREAL here. */
FFTW_DEFINE_API(FFTW_MANGLE_DOUBLE, double, fftw_complex)
