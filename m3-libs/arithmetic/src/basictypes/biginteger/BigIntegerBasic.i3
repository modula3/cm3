INTERFACE BigIntegerBasic;
(*Copyright (c) 1996, m3na project

Abstract: Integers of arbitrary size

Daniel Beer
*)

(*==========================*)

FROM xUtils IMPORT Error;

TYPE
  Value <: REFANY;
  T     =  RECORD
             data : Value;
             size : INTEGER;  (*not all entries in 'data' may be used*)
             sign : BOOLEAN;
           END;

(*PROCEDURE Init (VAR r : LIST OF T);*)
PROCEDURE FromInteger (x : INTEGER) : T;
(*PROCEDURE SetZero (VAR r : LIST OF T);*)
(*PROCEDURE SetOne (VAR r : LIST OF T);*)
PROCEDURE Copy (READONLY x : T) : T;
(*PROCEDURE Swap (VAR x, y : T);*)
(*PROCEDURE Delete (VAR r : LIST OF T);*)

VAR
  (*CONST*)Zero,
  (*CONST*)One : T;

PROCEDURE Add (READONLY x, y : T) : T;
PROCEDURE Sub (READONLY x, y : T) : T;
<*INLINE*>PROCEDURE Neg (READONLY x : T) : T;
PROCEDURE Equal   (READONLY x, y : T) : BOOLEAN;
PROCEDURE Compare (READONLY x, y : T) : [-1..1];

PROCEDURE Mul (READONLY x, y : T) : T;
PROCEDURE Div (READONLY x, y : T) : T RAISES {Error};
PROCEDURE Mod (READONLY x, y : T) : T RAISES {Error};
PROCEDURE DivMod (READONLY x, y : T; VAR r : T) : T RAISES {Error};
PROCEDURE Write (READONLY x : T);
PROCEDURE WriteDC (READONLY x : T);

(*==========================*)
END BigIntegerBasic.
