(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 21 12:17:47 PDT 1993 by mcjones                  *)
(*      modified on Fri May  7 16:08:10 PDT 1993 by muller                   *)

MODULE ExtendedFloat;

(* This module implements the operations on IEEE extended precisionreals 
   that do not depend on the operating system. 
   We assume that EXTENDED = LONGREAL. *)

IMPORT LongFloat, FloatMode;

PROCEDURE Scalb (x: T; n: INTEGER): T RAISES {FloatMode.Trap} =
  BEGIN
    RETURN FLOAT (LongFloat.Scalb (FLOAT (x, LONGREAL), n), T);
  END Scalb;

PROCEDURE Logb (x: T): T RAISES {FloatMode.Trap} =
  BEGIN
    RETURN FLOAT (LongFloat.Logb (FLOAT (x, LONGREAL)), T);
  END Logb;

PROCEDURE ILogb (x: T): INTEGER =
  BEGIN
    RETURN LongFloat.ILogb (FLOAT (x, LONGREAL));
  END ILogb;

PROCEDURE NextAfter (x, y: T): T RAISES {FloatMode.Trap} =
  BEGIN
    RETURN FLOAT (LongFloat.NextAfter (FLOAT (x, LONGREAL), 
                                       FLOAT (y, LONGREAL)),
                  T);
  END NextAfter;

PROCEDURE CopySign (x, y: T): T =
  BEGIN
    RETURN FLOAT (LongFloat.CopySign (FLOAT (x, LONGREAL), 
                                      FLOAT (y, LONGREAL)), T);
  END CopySign;

PROCEDURE Finite (x: T): BOOLEAN =
  BEGIN
    RETURN LongFloat.Finite (FLOAT (x, LONGREAL));
  END Finite;

PROCEDURE IsNaN (x: T): BOOLEAN =
  BEGIN
    RETURN LongFloat.IsNaN (FLOAT (x, LONGREAL));
  END IsNaN;

PROCEDURE Sign (x: T): [0..1] =
  BEGIN
    RETURN LongFloat.Sign (FLOAT (x, LONGREAL));
  END Sign;

PROCEDURE Differs (x, y: T): BOOLEAN =
  BEGIN
    RETURN LongFloat.Differs (FLOAT (x, LONGREAL), FLOAT (y, LONGREAL));
  END Differs;

PROCEDURE Unordered (x, y: T): BOOLEAN =
  BEGIN
    RETURN LongFloat.Unordered (FLOAT (x, LONGREAL), FLOAT (y, LONGREAL));
  END Unordered;

PROCEDURE Sqrt (x: T): T RAISES {FloatMode.Trap} =
  BEGIN
    RETURN FLOAT (LongFloat.Sqrt (FLOAT (x, LONGREAL)), T);
  END Sqrt;

PROCEDURE Class (x: T): IEEEClass =
  BEGIN
    RETURN LongFloat.Class (FLOAT (x, LONGREAL));
  END Class;

PROCEDURE FromDecimal (
    sign: [0..1]; READONLY digits: ARRAY OF [0..9]; exp: INTEGER): T
    RAISES {FloatMode.Trap} =
  BEGIN
    RETURN FLOAT (LongFloat.FromDecimal (sign, digits, exp), T);
  END FromDecimal;

PROCEDURE ToDecimal(x: T): DecimalApprox =
  BEGIN
    RETURN LongFloat.ToDecimal (FLOAT (x, LONGREAL));
  END ToDecimal;

BEGIN
END ExtendedFloat.
