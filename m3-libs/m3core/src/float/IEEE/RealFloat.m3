(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul 16 11:27:06 PDT 1996 by heydon                   *)
(*      modified on Thu Jan 26 12:59:31 PST 1995 by kalsow                   *)
(*      modified on Mon Jun 21 10:49:26 PDT 1993 by mcjones                  *)
(*      modified on Fri May  7 17:29:57 PDT 1993 by muller                   *)

UNSAFE MODULE RealFloat;

(* This module implements the operations on IEEE single precision reals 
   that do not depend on the operating system. *)

IMPORT RealRep AS Rep;
IMPORT DragonT, FPU, Word, Ctypes, Convert, Grisu;

PROCEDURE Scalb (x: T; n: INTEGER): T =
  BEGIN
    RETURN FLOAT (FPU.scalb (FLOAT (x, LONGREAL), n), T);
  END Scalb;
    
PROCEDURE Logb (x: T): T =
  CONST Log_of_zero = Rep.T{sign := 1, exponent := 16_ff, significand := 0};
  BEGIN
    CASE Class (x) OF
    | IEEEClass.SignalingNaN, 
      IEEEClass.QuietNaN =>
        RETURN x;
    | IEEEClass.Infinity => 
        RETURN ABS (x);
    | IEEEClass.Zero =>
        RETURN LOOPHOLE (Log_of_zero, T);
    | IEEEClass.Normal =>
        RETURN FLOAT (LOOPHOLE (x, Rep.T).exponent - 127, T);
    | IEEEClass.Denormal =>
        RETURN -126.0; END;
  END Logb;

PROCEDURE ILogb (x: T): INTEGER =
  VAR xx := LOOPHOLE (x, Rep.T);  v, w: Word.T;  n: INTEGER;
  BEGIN
    CASE Class (x) OF
    | IEEEClass.SignalingNaN,
      IEEEClass.QuietNaN =>
        (* RETURN 0; *)
        <* ASSERT FALSE*>
    | IEEEClass.Infinity =>
        RETURN (LAST (INTEGER));
    | IEEEClass.Zero =>
        RETURN (FIRST (INTEGER));
    | IEEEClass.Normal =>
        RETURN xx.exponent - 127;
    | IEEEClass.Denormal =>
        v := 16_400000;  n := - 127;  w := xx.significand;
        WHILE Word.And (v, w) = 0 DO
          v := Word.RightShift (v, 1);
          DEC (n);
        END;
        RETURN n;
    END; (*CASE*)
  END ILogb;

PROCEDURE NextAfter (x, y: T): T =
  VAR xx := LOOPHOLE (x, Rep.T);  yy := LOOPHOLE (y, Rep.T);
  BEGIN
    IF x = y                       THEN RETURN x; END;
    IF IsNaN (x) OR NOT Finite (x) THEN RETURN x; END;
    IF IsNaN (y)                   THEN RETURN y; END;

    IF x = 0.0   THEN
      RETURN LOOPHOLE (Rep.T {sign        := yy.sign,
                              exponent    := 0,
                              significand := 1}, T); END;
 
    IF (x > 0.0 AND x > y) OR (x < 0.0 AND x < y) THEN 
      IF xx.significand = 0 THEN
        xx.significand := 16_7fffff;
        DEC (xx.exponent);
        IF xx.exponent = 0 THEN
          RETURN (2.0 * x) / 2.0; (* generate underflow *) END;
      ELSE
        DEC (xx.significand); END;
    ELSE
      IF xx.significand = 16_7FFFFF THEN 
        xx.significand := 0;
        INC (xx.exponent);
        IF xx.exponent = 16_FF THEN
          RETURN (x + x);  (* generate overflow *) END;
      ELSE
        INC (xx.significand); END; END;

    RETURN LOOPHOLE (xx, T);
  END NextAfter;

PROCEDURE CopySign (x, y: T): T =
  VAR res := x;
  BEGIN
    LOOPHOLE (res, Rep.T).sign := LOOPHOLE (y, Rep.T).sign;
    RETURN res;
  END CopySign;

PROCEDURE Finite (x: T): BOOLEAN =
  VAR xx := LOOPHOLE (x, Rep.T);
  BEGIN
    RETURN xx.exponent # 16_FF; 
  END Finite;

PROCEDURE IsNaN (x: T): BOOLEAN =
  VAR xx := LOOPHOLE (x, Rep.T);
  BEGIN
    RETURN xx.exponent = 16_ff AND xx.significand # 0;
  END IsNaN;

PROCEDURE Sign (x: T): [0..1] =
  VAR xx := LOOPHOLE (x, Rep.T);
  BEGIN
    RETURN xx.sign;
  END Sign;

PROCEDURE Differs (x, y: T): BOOLEAN =
  BEGIN
    RETURN (x < y) OR (y < x);
  END Differs;

PROCEDURE Unordered (x, y: T): BOOLEAN =
  BEGIN
    RETURN NOT (x <= y OR y <= x);
  END Unordered;

PROCEDURE Sqrt (x: T): T =
  BEGIN
    RETURN FLOAT (FPU.sqrt (FLOAT (x, LONGREAL)), T);
  END Sqrt;

PROCEDURE Class (x: T): IEEEClass =
  VAR xx := LOOPHOLE (x, Rep.T);
  BEGIN
    IF xx.exponent = 0 THEN
      IF xx.significand = 0
        THEN RETURN IEEEClass.Zero;
        ELSE RETURN IEEEClass.Denormal;
      END;
    ELSIF xx.exponent # 16_FF THEN
      RETURN IEEEClass.Normal;
    ELSIF xx.significand = 0 THEN
      RETURN IEEEClass.Infinity;
    ELSIF Word.And (16_00400000, xx.significand) # 0 THEN
      RETURN IEEEClass.QuietNaN;
    ELSE 
      RETURN IEEEClass.SignalingNaN;
    END;
  END Class;

PROCEDURE FromDecimal (sign   : [0..1];
              READONLY digits : ARRAY OF [0..9];
                       exp    : INTEGER): T =
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
    xx := LOOPHOLE (x, Rep.T);
    res: DecimalApprox;
    exp, sig: INTEGER;
    count: CARDINAL;
    grisuMode := Grisu.FastDtoaMode.FAST_DTOA_SHORTEST_SINGLE;     
  BEGIN
    res.class := Class (x);
    res.sign := Sign (x);

    IF (res.class # IEEEClass.Denormal) AND (res.class # IEEEClass.Normal) THEN
      RETURN res;
    END;

    IF NOT Grisu.FastDtoa(FLOAT(ABS(x),LONGREAL), grisuMode, 0, res.digits, count, res.exp) THEN
      (* fallback on Dragon *)

      sig := xx.significand;

      IF xx.exponent = 0 THEN
        exp := -125;
      ELSE
        exp := xx.exponent - 126;
        sig := Word.Or (sig, 16_800000); (* add the implied 24th bit *)
      END;

      DragonT.F (exp, 0, 0, 0, sig, 24, DragonT.CutoffMode.normal, 0, 
                 res.digits, count, res.exp);
    END;
    res.len := count;
    res.errorSign := 0;
    RETURN res;
  END ToDecimal;

BEGIN
END RealFloat.
