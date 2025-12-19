(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul 16 11:27:56 PDT 1996 by heydon                   *)
(*      modified on Thu Jan 26 12:59:38 PST 1995 by kalsow                   *)
(*      modified on Mon Jun 21 12:17:44 PDT 1993 by mcjones                  *)
(*      modified on Fri May  7 17:31:14 PDT 1993 by muller                   *)

UNSAFE MODULE ExtendedFloat;

(* This module implements the operations on IEEE quad precision reals 
   that do not depend on the operating system. *)

IMPORT ExtendedRep AS Rep;
IMPORT DragonT, Word, Ctypes, Convert;

(* Users needing scalb or sqrt should consult QuadMath.i3 in libm3 *)
PROCEDURE Scalb (x : T; n: INTEGER) : T = <* NOWARN *>
  BEGIN
    <* ASSERT FALSE *>
  END Scalb;

PROCEDURE Sqrt (x: T): T = <* NOWARN *>
  BEGIN
    <* ASSERT FALSE *>
  END Sqrt;

PROCEDURE Logb (x: T): T =
  CONST Log_of_zero = Rep.T {sign := 1, exponent := 16_7FFF,
                             significand0 := 0, significand1 := 0,
                             significand2 := 0, significand3 := 0};
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
        RETURN FLOAT (LOOPHOLE (x, Rep.T).exponent - 16383, T);
    | IEEEClass.Denormal =>
        RETURN -16382.0X0;
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
        RETURN xx.exponent - 16383;
    | IEEEClass.Denormal =>
        IF xx.significand0 = 0 AND 
           xx.significand1 = 0 AND 
           xx.significand2 = 0 THEN
          v := 16_80000000;  n := - 16383 - 32 -32 -16; w := xx.significand3;
        ELSIF xx.significand0 = 0 AND 
              xx.significand1 = 0 THEN
          v := 16_80000000;  n := - 16383 -32 -16;  w := xx.significand2;
        ELSIF xx.significand0 = 0 THEN
          v := 16_80000000;  n := - 16383 -16;  w := xx.significand1;
        ELSE
          v := 16_00008000;  n := - 16383;  w := xx.significand0;
        END;
        WHILE Word.And (v, w) = 0 DO
          v := Word.RightShift (v, 1);
          DEC (n);
        END;
        RETURN n;
    END; (*CASE*)
  END ILogb;

PROCEDURE NextAfter (x, y: T): T =
  CONST Ones0 = 16_FFFF; (* BITSIZE (significand0) 1's *)
        Ones1 = -1; (* all 1's *)
  VAR xx := LOOPHOLE (x, Rep.T);  yy := LOOPHOLE (y, Rep.T);  z: T;
  BEGIN
    IF x = y                       THEN RETURN x; END;
    IF IsNaN (x) OR NOT Finite (x) THEN RETURN x; END;
    IF IsNaN (y)                   THEN RETURN y; END;

    IF x = 0.0x0   THEN
      LOOPHOLE(z, Rep.T).sign         := yy.sign;
      LOOPHOLE(z, Rep.T).exponent     := 0;
      LOOPHOLE(z, Rep.T).significand0 := 0;
      LOOPHOLE(z, Rep.T).significand1 := 0;
      LOOPHOLE(z, Rep.T).significand2 := 0;
      LOOPHOLE(z, Rep.T).significand3 := 1;
      RETURN z; 
    END;
 
    IF (x > 0.0X0 AND x > y) OR (x < 0.0X0 AND x < y) THEN 
      IF xx.significand0 = 0 AND xx.significand1 = 0 AND
         xx.significand2 = 0 AND xx.significand3 = 0 THEN
        xx.significand0 := Ones0;
        xx.significand1 := Ones1;
        xx.significand2 := Ones1;
        xx.significand3 := Ones1;
        DEC (xx.exponent);
        IF xx.exponent = 0 THEN
          RETURN (2.0X0 * x) / 2.0X0; (* generate underflow *) 
        END;
      ELSE
        IF xx.significand3 = 0 THEN
          IF xx.significand2 = 0 THEN
            IF xx.significand1 = 0 THEN
              xx.significand1 := Ones1;
              xx.significand2 := Ones1;
              xx.significand3 := Ones1;
              DEC (xx.significand0);
            ELSE
              DEC (xx.significand1); 
            END;
          ELSE
            DEC (xx.significand2); 
          END;             
        ELSE
          DEC (xx.significand3); 
        END;          
      END;      
    ELSE
      IF xx.significand0 = Ones0 AND xx.significand1 = Ones1 AND
         xx.significand2 = Ones1 AND xx.significand3 = Ones1 THEN
        xx.significand0 := 0;
        xx.significand1 := 0;
        xx.significand2 := 0;
        xx.significand3 := 0;
        INC (xx.exponent);
        IF xx.exponent = 16_7FFF THEN
          RETURN (x + x); (* generate overflow *) 
        END;
      ELSE
        IF xx.significand3 = Ones1 THEN
          IF xx.significand2 = Ones1 THEN
            IF xx.significand1 = Ones1 THEN
              xx.significand1 := 0;
              xx.significand2 := 0;
              xx.significand3 := 0;
              INC (xx.significand0);
            ELSE
              INC (xx.significand1); 
            END;
          ELSE
            INC (xx.significand2); 
          END;             
        ELSE
          INC (xx.significand3); 
        END;    
      END;
    END;
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
    RETURN xx.exponent # 16_7FFF;
  END Finite;

PROCEDURE IsNaN (x: T): BOOLEAN =
  VAR xx := LOOPHOLE (x, Rep.T);
  BEGIN
    RETURN xx.exponent = 16_7FFF 
       AND (xx.significand0 # 0 OR xx.significand1 # 0 OR
            xx.significand2 # 0 OR xx.significand3 # 0);
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
      IF xx.significand0 = 0 AND xx.significand1 = 0 AND
         xx.significand2 = 0 AND xx.significand3 = 0
        THEN RETURN IEEEClass.Zero;
        ELSE RETURN IEEEClass.Denormal;
      END;
    ELSIF xx.exponent # 16_7FFF THEN
      RETURN IEEEClass.Normal;
    ELSIF xx.significand0 = 0 AND xx.significand1 = 0 AND
          xx.significand2 = 0 AND xx.significand3 = 0 THEN
      RETURN IEEEClass.Infinity;
    ELSIF Word.And (16_00008000, xx.significand0) # 0 THEN
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
    used : INTEGER;
    res: T;
    pBuf : REF Convert.Buffer;
  BEGIN
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

    pBuf := LOOPHOLE(buf,REF Convert.Buffer);
    res := Convert.ToExtended (pBuf^, used);
    DISPOSE(buf);

    RETURN res
  END FromDecimal;

PROCEDURE ToDecimal(x: T): DecimalApprox =
  VAR
    xx := LOOPHOLE (x, Rep.T);
    res: DecimalApprox;
    exp, sig0, sig1, sig2, sig3: INTEGER;
    count: CARDINAL;
  BEGIN
    res.class := Class (x);
    res.sign := Sign (x);

    IF (res.class # IEEEClass.Denormal) AND (res.class # IEEEClass.Normal) THEN
      RETURN res;
    END;
      
    sig3 := Word.And (xx.significand3, 16_ffffffff);
    sig2 := Word.And (xx.significand2, 16_ffffffff);
    sig1 := Word.And (xx.significand1, 16_ffffffff);
    sig0 := Word.And (xx.significand0, 16_ffff);

    IF xx.exponent = 0 THEN
      exp := -16381;
    ELSE
      exp  := xx.exponent - 16382;
      sig0 := Word.Or (sig0, 16_10000);  (* add the implied 113th bit *)
    END;

    DragonT.F(exp, sig0, sig1, sig2, sig3, 113,
              DragonT.CutoffMode.normal, 0, res.digits, count, res.exp);
              
    res.len := count;
    res.errorSign := 0;

    RETURN res;
  END ToDecimal;

BEGIN
END ExtendedFloat.

