GENERIC INTERFACE MatrixBasic(R, V);
(* Arithmetic for Modula-3, see doc for details *)

TYPE
  TRow = ARRAY OF R.T;
  TBody = ARRAY OF TRow;
  T = REF TBody;

PROCEDURE Add (x, y: T; ): T;    (* x + y *)
PROCEDURE Sub (x, y: T; ): T;    (* x - y *)
PROCEDURE IsZero (x: T; ): BOOLEAN;
PROCEDURE Equal (x, y: T; ): BOOLEAN; (* x=y, it is a checked run-time
                                         error, if the sizes don't match *)

PROCEDURE Scale (x: T; y: R.T; ): T; (* x:=x*factor *)
PROCEDURE Mul (x, y: T; ): T;    (* x * y *)
PROCEDURE MulV (x: T; y: V.T; ): V.T; (* x * y *)
PROCEDURE MulTV (x: T; y: V.T; ): V.T; (* x^T * y or y^T*x *)
PROCEDURE Adjoint (x: T; ): T;   (* x^* *)
PROCEDURE MulMMA (x: T; ): T;    (* x*x^* *)
PROCEDURE MulMAM (x: T; ): T;    (* x^**x *)

PROCEDURE Trace (x: T; ): R.T;   (* sum of the diagonal elements *)
PROCEDURE Determinant (x: T; ): R.T;

END MatrixBasic.
