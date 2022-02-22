(* Slower form of LongFloat for endian neutral bootstrap. *)

UNSAFE MODULE LongFloatBoot EXPORTS LongFloat;

(* This module implements the operations on IEEE double precision reals
   that do not depend on the operating system. *)

IMPORT IEEE;
IMPORT DragonT, FPU, Word, Ctypes, Convert, Grisu, Cstdlib;

TYPE Float64 = IEEE.Float64TwoPartSignificand;
CONST Pack = IEEE.Pack64TwoPartSignificand;
CONST Unpack = IEEE.Unpack64TwoPartSignificand;

PROCEDURE Scalb (x: T; n: INTEGER): T =
  BEGIN
    RETURN FLOAT (FPU.scalb (FLOAT (x, LONGREAL), n), T);
  END Scalb;

VAR Log_of_zero := Pack (Float64 {sign := 1, exponent := 16_7ff,
                                    significand0 := 0, significand1 := 0});
PROCEDURE Logb (x: T): T =
  VAR xx := Unpack (x);
  BEGIN
    CASE Class (x) OF
    | IEEEClass.SignalingNaN,
      IEEEClass.QuietNaN =>
        RETURN x;
    | IEEEClass.Infinity =>
        RETURN ABS (x);
    | IEEEClass.Zero =>
        RETURN Log_of_zero;
    | IEEEClass.Normal =>
        xx.exponent := xx.exponent - 1023;
        RETURN Pack (xx);
    | IEEEClass.Denormal =>
        RETURN -1022.0d0;
    END;
  END Logb;

PROCEDURE ILogb (x: T): INTEGER =
  VAR xx := Unpack (x);  v, w: Word.T;  n: INTEGER;
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
  VAR xx := Unpack (x);
      yy := Unpack (y);
      zz := Float64 {};
  BEGIN
    IF x = y                       THEN RETURN x; END;
    IF IsNaN (x) OR NOT Finite (x) THEN RETURN x; END;
    IF IsNaN (y)                   THEN RETURN y; END;

    IF x = 0.0d0   THEN
      zz.sign         := yy.sign;
      zz.exponent     := 0;
      zz.significand0 := 0;
      zz.significand1 := 1;
      RETURN Pack (zz);
    END;

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

    RETURN Pack (xx);
  END NextAfter;

PROCEDURE CopySign (x, y: T): T =
  VAR res := Unpack (x);
  BEGIN
    res.sign := Unpack (y).sign;
    RETURN Pack (res);
  END CopySign;

PROCEDURE Finite (x: T): BOOLEAN =
  VAR xx := Unpack (x);
  BEGIN
    RETURN xx.exponent # 16_7ff;
  END Finite;

PROCEDURE IsNaN (x: T): BOOLEAN =
  VAR xx := Unpack (x);
  BEGIN
    RETURN xx.exponent = 16_7ff
       AND (xx.significand0 # 0 OR xx.significand1 # 0);
  END IsNaN;

PROCEDURE Sign (x: T): [0..1] =
  VAR xx := Unpack (x);
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
  VAR xx := Unpack (x);
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
  VAR xx := Unpack (x);
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
END LongFloatBoot.

