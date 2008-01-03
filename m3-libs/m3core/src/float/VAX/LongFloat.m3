(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jan 30 08:34:19 PST 1995 by kalsow     *)
(*      modified on Thu Nov  4 10:51:47 PST 1993 by heydon     *)
(*      modified on Mon Jun 21 12:22:32 PDT 1993 by mcjones    *)
(*      modified on Thu Apr 29 16:16:31 PDT 1993 by muller     *)

UNSAFE MODULE LongFloat;

IMPORT FPU, LongRealRep, Convert, DragonT, Ctypes, Word;

PROCEDURE Scalb(x: T; n: INTEGER): T =
  BEGIN
    RETURN FPU.ldexp (x, n);
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
    RETURN FPU.sqrt (x);
  END Sqrt;
  
PROCEDURE Class(x: T): IEEEClass =
  BEGIN
    IF IsNaN (x) THEN
      RETURN IEEEClass.SignalingNaN;
    ELSIF (x = 0.0d+0) THEN
      RETURN IEEEClass.Zero;
    ELSE
      RETURN IEEEClass.Normal;
    END;
  END Class;

PROCEDURE FromDecimal(
            sign: [0..1]; READONLY digits: ARRAY OF [0..9]; exp: INTEGER): T =
  <*FATAL Convert.Failed*>
  TYPE CharBuf = UNTRACED REF ARRAY OF Ctypes.char;
  CONST Sign = ARRAY [0..1] OF Ctypes.char { ORD ('+'), ORD ('-') };
  VAR
    ebuf: ARRAY [0..Word.Size] OF CHAR;
    buf: CharBuf;
    expLen, len: CARDINAL;
    res: T;
  BEGIN
    (* strategy:  build a C-style null terminated string and
       call the C runtime library to convert it to binary value. *)

    (* Allocate the buffer to hold the digits, the exponent value, and the
       four characters: 1) the leading sign, 2) the decimal point, 3) the 'e'
       character, and 4) the terminating 0 character. *)
    IF exp # 0 THEN expLen := Convert.FromInt(ebuf, exp) END;
    buf := NEW(CharBuf, NUMBER(digits) + expLen + 4);
    buf[0] := Sign [sign];              len := 1;
    buf[len] := ORD('0') + digits[0];   INC(len);
    buf[len] := ORD('.');               INC(len);
    FOR i := FIRST(digits) + 1 TO LAST(digits) DO
      buf[len] := ORD ('0') + digits [i];  INC (len);
    END;
    IF exp # 0 THEN
      buf[len] := ORD ('e');  INC (len);
      FOR i := 0 TO expLen - 1 DO
	buf[len] := ORD (ebuf[i]);  INC (len);
      END
    END;
    buf[len] := 0;

    res := FLOAT (DragonT.strtod (ADR(buf[0]), NIL), T);
    DISPOSE(buf);
    RETURN res
  END FromDecimal;

PROCEDURE ToDecimal(x: T): DecimalApprox =
  VAR
    xx := LOOPHOLE (x, LongRealRep.T);
    res: DecimalApprox;
    exp, sig0, sig1: INTEGER;
    count: CARDINAL;
  BEGIN
    res.class := Class (x);
    res.sign := Sign (x);

    IF (res.class # IEEEClass.Denormal) AND (res.class # IEEEClass.Normal) THEN
      RETURN res;
    END;

    exp  := xx.exponent - LongRealRep.ExponentBias;

    sig0 := Word.Shift (Word.Extract (xx.fraction2, 0, 12), 16)
                + xx.fraction3;

    sig1 := Word.Extract (xx.fraction2, 12, 4)
                + Word.Shift (xx.fraction1, 4)
                + Word.Shift (xx.fraction0, 20)
                + 16_8000000;

    DragonT.F (exp, sig1, sig0, 56, DragonT.CutoffMode.normal, 0, 
                 res.digits, count, res.exp);
    res.len := count;
    res.errorSign := 0;
    RETURN res;
  END ToDecimal;

BEGIN       
END LongFloat.

