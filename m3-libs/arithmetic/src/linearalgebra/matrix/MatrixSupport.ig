GENERIC INTERFACE MatrixSupport(R);
(* Arithmetic for Modula-3, see doc for details *)

(* Some generic routines that will be used from implementations of both
   MatrixBasic and MatrixFast. *)

TYPE
  TRow = ARRAY OF R.T;
  TBody = ARRAY OF TRow;
  T = REF TBody;

<* INLINE *>
PROCEDURE AssertEqualWidth (n, m: CARDINAL; );

PROCEDURE NewZero (m, n: CARDINAL; ): T; (* create zero matrix *)
PROCEDURE NewOne (n: CARDINAL; ): T; (* create identity matrix *)


PROCEDURE Transpose (x: T; ): T; (* x^T *)

PROCEDURE Determinant (x: T; ): R.T;

END MatrixSupport.
