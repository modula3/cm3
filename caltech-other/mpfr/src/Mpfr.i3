INTERFACE Mpfr;

(* 

  Multiple-precision floating point 

  INRIA and GNU

  https://www.mpfr.org/

*)

IMPORT MpfrRoundingMode;

TYPE
  T <: REFANY;
  
  RM      = MpfrRoundingMode.T;
  Ternary = [ -1..1 ];
  PrintBase = [ 2..62 ];

PROCEDURE New(prec : CARDINAL) : T;

PROCEDURE Set     (tgt, v : T;            rnd := RM.N) : Ternary;
PROCEDURE SetLR   (tgt : T; v : LONGREAL; rnd := RM.N) : Ternary;
PROCEDURE SetInt  (tgt : T; v : INTEGER;  rnd := RM.N) : Ternary;

PROCEDURE GetLR   (from : T; rnd := RM.N) : LONGREAL;
  
PROCEDURE Swap(a, b : T);

PROCEDURE Compare(a, b : T) : Ternary;

PROCEDURE Format(v : T; base : PrintBase := 10; rnd := RM.N) : TEXT;
PROCEDURE FormatInt(v : T; base : PrintBase := 10; rnd := RM.N) : TEXT;

PROCEDURE NanP    (v : T) : BOOLEAN;
PROCEDURE InfP    (v : T) : BOOLEAN;
PROCEDURE NumberP (v : T) : BOOLEAN;
PROCEDURE ZeroP   (v : T) : BOOLEAN;
PROCEDURE RegularP(v : T) : BOOLEAN;
PROCEDURE Sign    (v : T) : Ternary;

PROCEDURE GreaterP     (v, w : T) : BOOLEAN;
PROCEDURE GreaterEqualP(v, w : T) : BOOLEAN;
PROCEDURE LessP        (v, w : T) : BOOLEAN;
PROCEDURE LessEqualP   (v, w : T) : BOOLEAN;
PROCEDURE EqualP       (v, w : T) : BOOLEAN;
PROCEDURE LessGreaterP (v, w : T) : BOOLEAN;
  
PROCEDURE Add(tgt, a, b : T; rnd := RM.N) : Ternary;
PROCEDURE Sub(tgt, a, b : T; rnd := RM.N) : Ternary;
PROCEDURE Mul(tgt, a, b : T; rnd := RM.N) : Ternary;
PROCEDURE Div(tgt, a, b : T; rnd := RM.N) : Ternary;
PROCEDURE Pow(tgt, a, b : T; rnd := RM.N) : Ternary;

PROCEDURE Sqrt(tgt, a : T; rnd := RM.N) : Ternary;
PROCEDURE Neg(tgt, a : T; rnd := RM.N) : Ternary;
PROCEDURE Abs(tgt, a : T; rnd := RM.N) : Ternary;

PROCEDURE Log(tgt, a : T; rnd := RM.N) : Ternary;
PROCEDURE Exp(tgt, a : T; rnd := RM.N) : Ternary;
PROCEDURE Cos(tgt, a : T; rnd := RM.N) : Ternary;
PROCEDURE Sin(tgt, a : T; rnd := RM.N) : Ternary;
PROCEDURE Tan(tgt, a : T; rnd := RM.N) : Ternary;

PROCEDURE Gamma(tgt, a : T; rnd := RM.N) : Ternary;

PROCEDURE ConstLog2(tgt : T; rnd := RM.N) : Ternary;
PROCEDURE ConstPi(tgt : T; rnd := RM.N) : Ternary;

CONST Brand = "Mpfr";

END Mpfr.
