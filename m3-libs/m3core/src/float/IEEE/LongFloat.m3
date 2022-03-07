(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul 16 11:27:56 PDT 1996 by heydon                   *)
(*      modified on Thu Jan 26 12:59:38 PST 1995 by kalsow                   *)
(*      modified on Mon Jun 21 12:17:44 PDT 1993 by mcjones                  *)
(*      modified on Fri May  7 17:31:14 PDT 1993 by muller                   *)

UNSAFE MODULE LongFloat;

(* This module implements the operations on IEEE double precision reals 
   that do not depend on the operating system. *)

IMPORT LongRealRep AS Rep;
IMPORT DragonT, FPU, Word, Ctypes, Convert, Grisu, Cstdlib;

PROCEDURE Scalb (x: T; n: INTEGER): T =
  BEGIN
    RETURN FLOAT (FPU.scalb (FLOAT (x, LONGREAL), n), T);
  END Scalb;

PROCEDURE Logb (x: T): T =
  CONST Log_of_zero = Rep.T {sign := 1, exponent := 16_7ff,
                        significand0 := 0, significand1 := 0};
  VAR ans: T;
  BEGIN
    CASE Class (x) OF
    | IEEEClass.SignalingNaN, 
      IEEEClass.QuietNaN =>
        RETURN x;
    | IEEEClass.Infinity => 
        RETURN ABS (x);
    | IEEEClass.Zero =>
        LOOPHOLE (ans, Rep.T) := Log_of_zero;
        RETURN ans;
    | IEEEClass.Normal =>
        RETURN FLOAT (LOOPHOLE (x, Rep.T).exponent - 1023, T);
    | IEEEClass.Denormal =>
        RETURN -1022.0d0;
    END;
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
        RETURN xx.exponent - 1023;
    | IEEEClass.Denormal =>
        IF xx.significand0 = 0
          THEN v := 16_80000000;  n := - 1043;  w := xx.significand1;
          ELSE v := 16_00080000;  n := - 1023;  w := xx.significand0;
        END;
        WHILE Word.And (v, w) = 0 DO
          v := Word.RightShift (v, 1);
          DEC (n);
        END;
        RETURN n;
    END; (*CASE*)
  END ILogb;

PROCEDURE NextAfter (x, y: T): T =
  CONST Ones0 = 16_fffff; (* BITSIZE (significand0) 1's *)
        Ones1 = -1; (* all 1's *)
  VAR xx := LOOPHOLE (x, Rep.T);  yy := LOOPHOLE (y, Rep.T);  z: T;
  BEGIN
    IF x = y                       THEN RETURN x; END;
    IF IsNaN (x) OR NOT Finite (x) THEN RETURN x; END;
    IF IsNaN (y)                   THEN RETURN y; END;

    IF x = 0.0d0   THEN
      LOOPHOLE(z, Rep.T).sign         := yy.sign;
      LOOPHOLE(z, Rep.T).exponent     := 0;
      LOOPHOLE(z, Rep.T).significand0 := 0;
      LOOPHOLE(z, Rep.T).significand1 := 1;
      RETURN z; END;
 
    IF (x > 0.0d0 AND x > y) OR (x < 0.0d0 AND x < y) THEN 
      IF xx.significand0 = 0 AND xx.significand1 = 0 THEN
        xx.significand0 := Ones0;
        xx.significand1 := Ones1;
        DEC (xx.exponent);
        IF xx.exponent = 0 THEN
          RETURN (2.0d0 * x) / 2.0d0; (* generate underflow *) END;
      ELSIF xx.significand1 = 0 THEN
        xx.significand1 := Ones1;
        DEC (xx.significand0);
      ELSE
        DEC (xx.significand1); END;
    ELSE
      IF xx.significand0 = Ones0 AND xx.significand1 = Ones1 THEN
        xx.significand0 := 0;
        xx.significand1 := 0;
        INC (xx.exponent);
        IF xx.exponent = 16_7ff THEN
          RETURN (x + x); (* generate overflow *) END;
      ELSIF xx.significand1 = Ones1 THEN
        xx.significand1 := 0;
        INC (xx.significand0);
      ELSE
        INC (xx.significand1); END; END;

    LOOPHOLE (z, Rep.T) := xx;
    RETURN z;
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
    RETURN xx.exponent # 16_7ff; 
  END Finite;

PROCEDURE IsNaN (x: T): BOOLEAN =
  VAR xx := LOOPHOLE (x, Rep.T);
  BEGIN
    RETURN xx.exponent = 16_7ff 
       AND (xx.significand0 # 0 OR xx.significand1 # 0);
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

PROCEDURE Class (x: T): IEEEClass =
  VAR xx := LOOPHOLE (x, Rep.T);
  BEGIN
    IF xx.exponent = 0 THEN
      IF xx.significand0 = 0 AND xx.significand1 = 0
        THEN RETURN IEEEClass.Zero;
        ELSE RETURN IEEEClass.Denormal;
      END;
    ELSIF xx.exponent # 16_7FF THEN
      RETURN IEEEClass.Normal;
    ELSIF xx.significand0 = 0 AND xx.significand1 = 0 THEN
      RETURN IEEEClass.Infinity;
    ELSIF Word.And (16_00080000, xx.significand0) # 0 THEN
      RETURN IEEEClass.QuietNaN;
    ELSE 
      RETURN IEEEClass.SignalingNaN;
    END;
  END Class;

PROCEDURE Sqrt (x: T): T =
  BEGIN
    RETURN FLOAT (FPU.sqrt (FLOAT (x, LONGREAL)), T);
  END Sqrt;

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

    res := FLOAT (Cstdlib.strtod (ADR(buf[0]), NIL), T);
    DISPOSE(buf);
    RETURN res
  END FromDecimal;

PROCEDURE ToDecimal(x: T): DecimalApprox =
  VAR
    xx := LOOPHOLE (x, Rep.T);
    res: DecimalApprox;
    exp, sig0, sig1: INTEGER;
    count: CARDINAL;
    grisuMode := Grisu.FastDtoaMode.FAST_DTOA_SHORTEST;
  BEGIN
    res.class := Class (x);
    res.sign := Sign (x);

    IF (res.class # IEEEClass.Denormal) AND (res.class # IEEEClass.Normal) THEN
      RETURN res;
    END;

    IF NOT Grisu.FastDtoa(ABS(x), grisuMode, 0, res.digits, count, res.exp) THEN
      (* fallback on Dragon *)
      
      (* we have the lower 32 bits in significand1, the upper 20 bits in
         significand0 and may be a bit to set at the top (if bits = 53) *)

      sig1 := Word.And (xx.significand1, 16_ffffffff);
      sig0 := Word.And (xx.significand0, 16_fffff);

      IF xx.exponent = 0 THEN
        exp := -1021;
      ELSE
        exp  := xx.exponent - 1022;
        sig0 := Word.Or (sig0, 16_100000);  (* add the implied 53rd bit *)
      END;

      DragonT.F (exp, 0, 0, sig0, sig1, 53, DragonT.CutoffMode.normal, 0, 
                res.digits, count, res.exp);
    END;
    res.len := count;
    res.errorSign := 0;
    RETURN res;
  END ToDecimal;

BEGIN
END LongFloat.

