(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jul 27 07:45:15 PDT 1994 by kalsow     *)
(*      modified on Mon Jun 21 12:23:56 PDT 1993 by mcjones    *)
(*      modified on Thu Apr 29 16:14:50 PDT 1993 by muller     *)


UNSAFE MODULE ExtendedFloat;

IMPORT FPU, LongRealRep, FloatMode, LongFloat;

PROCEDURE Scalb(x: T; n: INTEGER): T =
  BEGIN
    RETURN FLOAT (FPU.ldexp (FLOAT (x, LONGREAL), n), T);
  END Scalb;

PROCEDURE Logb(<*UNUSED*> x: T): T =
  BEGIN
    <* ASSERT FALSE *>
  END Logb;

PROCEDURE ILogb(<*UNUSED*> x: T): INTEGER =
  BEGIN
    <* ASSERT FALSE *>
  END ILogb;

PROCEDURE NextAfter(<*UNUSED*> x, y: T): T =
  BEGIN
    <* ASSERT FALSE *>
  END NextAfter;

PROCEDURE CopySign(x, y: T): T =
  VAR res := x;
  BEGIN
    LOOPHOLE (res, LongRealRep.T).sign := LOOPHOLE (y, LongRealRep.T).sign;
    RETURN res;
  END CopySign;

PROCEDURE Finite(<*UNUSED*> x: T): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Finite;

PROCEDURE IsNaN(x: T): BOOLEAN =
  VAR xx := LOOPHOLE (x, LongRealRep.T);
  BEGIN
    RETURN (xx.sign # 0) AND (xx.exponent = 0);
  END IsNaN;

PROCEDURE Sign(x: T): [0..1] =
  VAR xx := LOOPHOLE (x, LongRealRep.T);
  BEGIN
    RETURN xx.sign;
  END Sign;

PROCEDURE Differs(x, y: T): BOOLEAN =
  BEGIN
    RETURN (NOT (IsNaN (x) OR IsNaN (y))) AND (x # y);
  END Differs;

PROCEDURE Unordered(x, y: T): BOOLEAN =
  BEGIN
    RETURN IsNaN (x) OR IsNaN (y);
  END Unordered;

PROCEDURE Sqrt(x: T): T =
  BEGIN
    RETURN FLOAT (FPU.sqrt (FLOAT (x, LONGREAL)), T);
  END Sqrt;
  
PROCEDURE Class(x: T): IEEEClass =
  BEGIN
    IF IsNaN (x) THEN
      RETURN IEEEClass.SignalingNaN;
    ELSIF (x = 0.0x+0) THEN
      RETURN IEEEClass.Zero;
    ELSE
      RETURN IEEEClass.Normal;
    END;
  END Class;

PROCEDURE FromDecimal(
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
