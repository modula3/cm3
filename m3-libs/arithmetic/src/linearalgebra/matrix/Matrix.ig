GENERIC INTERFACE Matrix(R, V, MI);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Matrix math *)

CONST Brand = R.Brand & "Matrix";

TYPE
  (**
     text form: "M2x6{
                 V6{a00,a01,a02,a03,a04,a05},
                 V6{a10,a11,a12,a13,a14,a15}
                 }"
  **)
  TRow = MI.TRow;
  TBody = MI.TBody;
  T = MI.T;

  TMRow = ARRAY OF T;
  TMBody = ARRAY OF TMRow;       (* matrix of matrices *)

PROCEDURE New (m, n: CARDINAL; ): T; (* make New mxn matrix *)
PROCEDURE FromArray (READONLY x: TBody; ): T;
PROCEDURE RowFromArray (READONLY x: V.TBody; ): T;
PROCEDURE ColumnFromArray (READONLY x: V.TBody; ): T;
PROCEDURE DiagonalFromArray (READONLY x: V.TBody; ): T;
<* INLINE *>
PROCEDURE RowFromVector (x: V.T; ): T;
<* INLINE *>
PROCEDURE ColumnFromVector (x: V.T; ): T;
<* INLINE *>
PROCEDURE DiagonalFromVector (x: V.T; ): T;
PROCEDURE FromMatrixArray (READONLY x: TMBody; ): T;
PROCEDURE FromScalar (x: R.T; ): T;
PROCEDURE Copy (x: T; ): T;

PROCEDURE NewZero (m, n: CARDINAL; ): T; (* create zero matrix *)
PROCEDURE NewOne (n: CARDINAL; ): T; (* create identity matrix *)
PROCEDURE Cyclic (x: V.T; size: CARDINAL; shift: INTEGER := 1; ):
  T;                             (* each row is 'x' shifted by 'shift' to
                                    the right compared to the row above *)

PROCEDURE Transpose (x: T; ): T; (* x^T *)

CONST
  Add    = MI.Add;               (* x + y *)
  Sub    = MI.Sub;               (* x - y *)
  IsZero = MI.IsZero;
  Equal  = MI.Equal;             (* return v1=v2 *)

  Scale   = MI.Scale;            (* x:=x*factor *)
  Mul     = MI.Mul;              (* x * y *)
  MulV    = MI.MulV;             (* A * b *)
  MulTV   = MI.MulTV;            (* A^T * b or b^T*A *)
  Adjoint = MI.Adjoint;          (* x^* *)
  MulMMA  = MI.MulMMA;           (* x*x^* *)
  MulMAM  = MI.MulMAM;           (* x^**x *)

  Trace       = MI.Trace;        (* sum of the diagonal elements *)
  Determinant = MI.Determinant;

PROCEDURE GetRow (x: T; k: CARDINAL; ): V.T;
PROCEDURE GetColumn (x: T; k: CARDINAL; ): V.T;

TYPE
  ApplyFtn = PROCEDURE (x: R.T);
  MapFtn = PROCEDURE (x: R.T; ): R.T;
  ReduceFtn = PROCEDURE (x, y: R.T; ): R.T;

PROCEDURE Apply (x: T; f: ApplyFtn; );
PROCEDURE Map (x: T; f: MapFtn; ): T;
PROCEDURE ReduceRows (x: T; f: ReduceFtn; READONLY init: V.TBody; ): V.T;
PROCEDURE ReduceColumns (x: T; f: ReduceFtn; READONLY init: V.TBody; ):
  V.T;

END Matrix.
