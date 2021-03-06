GENERIC INTERFACE PolynomialFast(R, V);
(* Arithmetic for Modula-3, see doc for details *)

FROM Arithmetic IMPORT Error;

TYPE
  (* this is not only a reuse because of laziness, more than this, a
     polynomial can be treated as vector and behaves like a vector of
     arbitrary size *)
  TBody = V.TBody;
  T = V.T;
  QuotRem = RECORD quot, rem: T END;

CONST
  Compare = NIL;
  Mod     = NIL;                 (* dummy *)

PROCEDURE IsZero (x: T; ): BOOLEAN;
PROCEDURE Equal (x, y: T; ): BOOLEAN; (* x=y *)

PROCEDURE Add (x, y: T; ): T;    (* x+y *)
PROCEDURE Sub (x, y: T; ): T;    (* x-y *)

PROCEDURE Mul (x, y: T; ): T;    (* x*y *)
PROCEDURE Div (x, y: T; ): T RAISES {Error}; (* x/y if possible *)
(*PROCEDURE Mod(x,y:T;):T RAISES {Error}; (* x mod y *)*)
PROCEDURE DivMod (x, y: T; ): QuotRem RAISES {Error};
(* compute quotient x/y and remainder *)

PROCEDURE Eval (x : T;           (* evalulate this polynomial *)
                xi: R.T;         (* at this point *)
  ): R.T;

PROCEDURE Derive (x: T;          (* differentiate polynomial *)
  ): T;
PROCEDURE EvalDerivative
  (x : T;                        (* evaluate this polynomial *)
   xi: R.T;                      (* for this argument *)
   n : CARDINAL;                 (* with this number of derivatives *)
  ): REF ARRAY OF R.T;           (*ing x(xi), x'(xi)... *)

PROCEDURE Compose (x, y: T;      (* y(x) - apply y on the values of x *)
  ): T;

END PolynomialFast.
