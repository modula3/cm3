INTERFACE MpfrP;
FROM Ctypes IMPORT int, char_star;
FROM Cstddef IMPORT size_t;
IMPORT Word;

CONST PrecFormat32 = 2; 
CONST PrecFormat64 = 3; 
      
TYPE T         = MpfrPtrT; (* mpfr_ptr_t_star *)
     Prec      = int;     (* MPFR_PREC_T -- iff GetPrecFormat returns 2. *)
     Rnd       = int;     (* enum for rounding mode *)
     Exp       = Prec;
     exp_star  = ADDRESS;
     MpfrPtrT  = ADDRESS; (* as good as any *)
     
<*EXTERNAL MpfrC__alloc*>
PROCEDURE alloc() : MpfrPtrT;

<*EXTERNAL MpfrC__free*>
PROCEDURE free(x : MpfrPtrT);

<*EXTERNAL MpfrC__deref*>
PROCEDURE deref(p : MpfrPtrT) : MpfrPtrT;

  
<*EXTERNAL mpfr_init2*>
PROCEDURE init2(t : T; prec : Prec);

<*EXTERNAL mpfr_get_d*>
PROCEDURE get_d(t : T; rnd : Rnd) : LONGREAL;  

<*EXTERNAL mpfr_set*>
PROCEDURE set(tgt, t : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_set_d*>
PROCEDURE set_d(tgt : T; v : LONGREAL; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_set_si*>
PROCEDURE set_si(tgt : T; v : int; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_swap*>
PROCEDURE swap(a, b : T);

<*EXTERNAL mpfr_get_prec*>
PROCEDURE get_prec(a : T) : Prec;

<*EXTERNAL mpfr_get_str*>
PROCEDURE get_str(str : char_star; expptr : exp_star; base : int; n : size_t; op : T; rnd : Rnd) : char_star;

<*EXTERNAL MpfrC__GetPrecFormat*>
PROCEDURE GetPrecFormat() : int; (* 2 for int *)

<*EXTERNAL mpfr_nan_p*>
PROCEDURE nan_p(a : T) : int;

<*EXTERNAL mpfr_inf_p*>
PROCEDURE inf_p(a : T) : int;

<*EXTERNAL mpfr_number_p*>
PROCEDURE number_p(a : T) : int;

<*EXTERNAL mpfr_zero_p*>
PROCEDURE zero_p(a : T) : int;

<*EXTERNAL mpfr_regular_p*>
PROCEDURE regular_p(a : T) : int;

<*EXTERNAL mpfr_sgn*>
PROCEDURE sgn(a : T) : int;

<*EXTERNAL mpfr_cmp_ui*>
PROCEDURE cmp_ui(a : T; to : Word.T) : int;
  
<*EXTERNAL mpfr_cmp*>
PROCEDURE cmp(a, b : T) : int;

<*EXTERNAL mpfr_greater_p*>
PROCEDURE greater_p(a, b : T) : int;

<*EXTERNAL mpfr_greaterequal_p*>
PROCEDURE greaterequal_p(a, b : T) : int;

<*EXTERNAL mpfr_less_p*>
PROCEDURE less_p(a, b : T) : int;

<*EXTERNAL mpfr_lessequal_p*>
PROCEDURE lessequal_p(a, b : T) : int;

<*EXTERNAL mpfr_equal_p*>
PROCEDURE equal_p(a, b : T) : int;

<*EXTERNAL mpfr_lessgreater_p*>
PROCEDURE lessgreater_p(a, b : T) : int;

<*EXTERNAL mpfr_add*>
PROCEDURE add(tgt, a, b : T; rnd : Rnd): int;  

<*EXTERNAL mpfr_sub*>
PROCEDURE sub(tgt, a, b : T; rnd : Rnd): int;
  
<*EXTERNAL mpfr_mul*>
PROCEDURE mul(tgt, a, b : T; rnd : Rnd): int;
  
<*EXTERNAL mpfr_div*>
PROCEDURE div(tgt, a, b : T; rnd : Rnd): int;  

<*EXTERNAL mpfr_pow*>
PROCEDURE pow(tgt, a, b : T; rnd : Rnd): int;  

<*EXTERNAL mpfr_sqrt*>
PROCEDURE sqrt(tgt, a : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_neg*>
PROCEDURE neg(tgt, a : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_abs*>
PROCEDURE abs(tgt, a : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_log*>
PROCEDURE log(tgt, a : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_exp*>
PROCEDURE exp(tgt, a : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_cos*>
PROCEDURE cos(tgt, a : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_sin*>
PROCEDURE sin(tgt, a : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_tan*>
PROCEDURE tan(tgt, a : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_gamma*>
PROCEDURE gamma(tgt, a : T; rnd : Rnd) : int;

<*EXTERNAL mpfr_const_log2*>
PROCEDURE const_log2(tgt : T; rnd : Rnd) : int;
  
<*EXTERNAL mpfr_const_pi*>
PROCEDURE const_pi(tgt : T; rnd : Rnd) : int;
  
END MpfrP.
