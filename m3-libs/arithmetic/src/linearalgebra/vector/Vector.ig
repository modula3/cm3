GENERIC INTERFACE Vector(R);
(*Arithmetic for Modula-3, see doc for details

   Abstract: Vector math *)
FROM NADefinitions IMPORT Error;
(*==========================*)

CONST Brand = R.Brand & "Vector";

TYPE
  (*text form: "V6{a0,a1,a2,a3,a4,a5}"*)
  TBody = ARRAY OF R.T;
  T = REF TBody;

  TVBody = ARRAY OF T;

PROCEDURE New (n: CARDINAL): T;  (*make new vector with n components*)
PROCEDURE NewZero (n: CARDINAL):
  T;                             (*make new zero vector with n components*)
PROCEDURE NewUniform (n: CARDINAL; x: R.T):
  T;                             (*vector with all components set to x*)
<*INLINE*>
PROCEDURE FromArray (READONLY x: TBody): T;
PROCEDURE FromVectorArray (READONLY x: TVBody): T;
<*INLINE*>
PROCEDURE FromScalar (x: R.T): T;
<*INLINE*>
PROCEDURE Copy (x: T): T;

<*INLINE*>
PROCEDURE IsZero (x: T): BOOLEAN;
<*INLINE*>
PROCEDURE Equal (x, y: T): BOOLEAN RAISES {Error}; (*return x=y*)

<*INLINE*>
PROCEDURE Add (x, y: T): T RAISES {Error}; (*x+y*)
<*INLINE*>
PROCEDURE Sub (x, y: T): T RAISES {Error}; (*x-y*)
<*INLINE*>
PROCEDURE Neg (x: T): T;         (*return -x *)

<*INLINE*>
PROCEDURE Scale (x: T; y: R.T): T; (*x:=x*factor*)
<*INLINE*>
PROCEDURE Inner (x, y: T): R.T RAISES {Error}; (*<x,y>*)
<*INLINE*>
PROCEDURE Dot (x, y: T): R.T RAISES {Error}; (*x^T*y*)

(* should be generalized to finding an orthonormal basis of the space
   orthogonal to a given set of vectors

   PROCEDURE Cross(x,y:T):T RAISES {Error}; (*x x y*) *)

TYPE
  ApplyFtn = PROCEDURE (x: R.T) RAISES {Error};
  MapFtn = PROCEDURE (x: R.T): R.T RAISES {Error};
  ReduceFtn = PROCEDURE (x, y: R.T): R.T RAISES {Error};

PROCEDURE Apply (x: T; f: ApplyFtn) RAISES {Error};
PROCEDURE Map (x: T; f: MapFtn): T RAISES {Error};
PROCEDURE Reduce (x: T; f: ReduceFtn; init: R.T): R.T RAISES {Error};

PROCEDURE ArithSeq (num: CARDINAL; from: R.T; by: R.T): T;
PROCEDURE GeomSeq (num: CARDINAL; from: R.T; by: R.T): T;
PROCEDURE RecursiveSeq (num: CARDINAL; from: R.T; by: MapFtn): T
  RAISES {Error};

(*==========================*)
END Vector.
