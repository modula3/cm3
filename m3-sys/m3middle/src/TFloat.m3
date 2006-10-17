(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TFloat.m3                                             *)
(* Last Modified On Thu Jul 28 10:18:03 PDT 1994 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

UNSAFE MODULE TFloat;

IMPORT Convert, TInt, TargetMap;
FROM Target IMPORT Int, Float, Precision;

PROCEDURE New (READONLY chars: ARRAY OF CHAR;  pre: Precision;
                                                       VAR f: Float): BOOLEAN =
  VAR used: INTEGER;
  BEGIN
    f.pre      := pre;
    f.exponent := 0;
    TRY f.fraction := Convert.ToExtended (chars, used);
    EXCEPT Convert.Failed => RETURN FALSE;
    END;
    RETURN (used = NUMBER (chars)) AND Normalize (f);
  END New;

PROCEDURE Prec (READONLY f: Float): Precision =
  BEGIN
    RETURN f.pre;
  END Prec;

PROCEDURE Add (READONLY a, b: Float;  VAR f: Float): BOOLEAN =
  BEGIN
    IF (a.pre # b.pre) THEN RETURN FALSE END;
    f.pre      := a.pre;
    f.exponent := 0;
    f.fraction := a.fraction + b.fraction;
    RETURN Normalize (f);
  END Add;

PROCEDURE Subtract (READONLY a, b: Float;  VAR f: Float): BOOLEAN =
  BEGIN
    IF (a.pre # b.pre) THEN RETURN FALSE END;
    f.pre      := a.pre;
    f.exponent := 0;
    f.fraction := a.fraction - b.fraction;
    RETURN Normalize (f);
  END Subtract;

PROCEDURE Multiply (READONLY a, b: Float;  VAR f: Float): BOOLEAN =
  BEGIN
    IF (a.pre # b.pre) THEN RETURN FALSE END;
    f.pre      := a.pre;
    f.exponent := 0;
    f.fraction := a.fraction * b.fraction;
    RETURN Normalize (f);
  END Multiply;

PROCEDURE Divide (READONLY a, b: Float;  VAR f: Float): BOOLEAN =
  BEGIN
    IF (a.pre # b.pre) THEN RETURN FALSE END;
    IF (b.fraction = 0.0x+0) THEN RETURN FALSE END;
    f.pre      := a.pre;
    f.exponent := 0;
    f.fraction := a.fraction / b.fraction;
    RETURN Normalize (f);
  END Divide;

PROCEDURE Mod (READONLY a, b: Float;  VAR f: Float): BOOLEAN =
  VAR x, y: EXTENDED;
  BEGIN
    IF (a.pre # b.pre) THEN RETURN FALSE END;
    x := a.fraction;
    y := b.fraction;
    IF (y = 0.0x+0) THEN RETURN FALSE END;
    f.pre      := a.pre;
    f.exponent := 0;
    f.fraction := x - y * FLOAT (FLOOR (x / y), EXTENDED);
    RETURN Normalize (f);
  END Mod;

PROCEDURE EQ (READONLY a, b: Float): BOOLEAN =
  BEGIN
    IF (a.pre # b.pre) THEN RETURN FALSE END;
    IF (a.exponent # b.exponent) THEN RETURN FALSE END;
    IF (a.fraction # b.fraction) THEN RETURN FALSE END;
    RETURN TRUE;
  END EQ;

PROCEDURE LT (READONLY a, b: Float): BOOLEAN =
  BEGIN
    IF (a.pre # b.pre) THEN RETURN FALSE END;
    RETURN (a.fraction < b.fraction);
  END LT;

PROCEDURE LE (READONLY a, b: Float): BOOLEAN =
  BEGIN
    IF (a.pre # b.pre) THEN RETURN FALSE END;
    RETURN (a.fraction <= b.fraction);
  END LE;

PROCEDURE FloatF (READONLY a: Float;  p: Precision;  VAR f: Float): BOOLEAN =
  BEGIN
    f.pre      := p;
    f.exponent := a.exponent;
    f.fraction := a.fraction;
    RETURN Normalize (f);
  END FloatF;

PROCEDURE FloatI (READONLY iI: Int;  p: Precision;  VAR f: Float): BOOLEAN =
  VAR i: INTEGER;
  BEGIN
    IF NOT TInt.ToInt (iI, i) THEN RETURN FALSE; END;
    f.pre      := p;
    f.exponent := 0;
    f.fraction := FLOAT (i, EXTENDED);
    RETURN Normalize (f);
  END FloatI;

PROCEDURE Trunc (READONLY a: Float;  VAR r: Int): BOOLEAN =
  BEGIN
    IF (a.pre = Precision.Short) THEN
      RETURN TInt.FromInt (TRUNC (ToReal (a)), r);
    ELSIF (a.pre = Precision.Long) THEN
      RETURN TInt.FromInt (TRUNC (ToLongreal (a)), r);
    ELSE
      RETURN TInt.FromInt (TRUNC (ToExtended (a)), r);
    END;
  END Trunc;

PROCEDURE Round (READONLY a: Float;  VAR r: Int): BOOLEAN =
  BEGIN
    IF (a.pre = Precision.Short) THEN
      RETURN TInt.FromInt (ROUND (ToReal (a)), r);
    ELSIF (a.pre = Precision.Long) THEN
      RETURN TInt.FromInt (ROUND (ToLongreal (a)), r);
    ELSE
      RETURN TInt.FromInt (ROUND (ToExtended (a)), r);
    END;
  END Round;

PROCEDURE Floor (READONLY a: Float;  VAR r: Int): BOOLEAN =
  BEGIN
    IF (a.pre = Precision.Short) THEN
      RETURN TInt.FromInt (FLOOR (ToReal (a)), r);
    ELSIF (a.pre = Precision.Long) THEN
      RETURN TInt.FromInt (FLOOR (ToLongreal (a)), r);
    ELSE
      RETURN TInt.FromInt (FLOOR (ToExtended (a)), r);
    END;
  END Floor;

PROCEDURE Ceiling (READONLY a: Float;  VAR r: Int): BOOLEAN =
  BEGIN
    IF (a.pre = Precision.Short) THEN
      RETURN TInt.FromInt (CEILING (ToReal (a)), r);
    ELSIF (a.pre = Precision.Long) THEN
      RETURN TInt.FromInt (CEILING (ToLongreal (a)), r);
    ELSE
      RETURN TInt.FromInt (CEILING (ToExtended (a)), r);
    END;
  END Ceiling;

PROCEDURE ToChars (READONLY f: Float;  VAR buf: ARRAY OF CHAR): INTEGER =
  <*FATAL Convert.Failed *>
  VAR zz: ARRAY [0..31] OF CHAR;  len: INTEGER;
  BEGIN
    <*ASSERT f.exponent = 0*>
    len := Convert.FromExtended (zz, f.fraction, 13, Convert.Style.Sci);
    IF (len > NUMBER (buf)) THEN RETURN -1 END;
    SUBARRAY (buf, 0, len) := SUBARRAY (zz, 0, len);
    RETURN len;
  END ToChars;

TYPE
  Ptr = UNTRACED REF ARRAY [0..BITSIZE(EXTENDED) DIV BITSIZE(Byte) - 1] OF Byte;

PROCEDURE ToBytes (READONLY f: Float;  VAR buf: ARRAY OF Byte): INTEGER =
  VAR
    x1  : REAL;
    x2  : LONGREAL;
    x3  : EXTENDED;
    adr : ADDRESS;
    ptr : Ptr;
    len := TargetMap.Float_types[f.pre].size DIV BITSIZE (Byte);
  BEGIN
    IF (NUMBER (buf) < len) THEN RETURN -1 END;
    IF    (f.pre = Precision.Short) THEN x1 := ToReal (f);     adr := ADR (x1);
    ELSIF (f.pre = Precision.Long)  THEN x2 := ToLongreal (f); adr := ADR (x2);
    ELSE                                 x3 := ToExtended (f); adr := ADR (x3);
    END;
    ptr := adr;
    SUBARRAY (buf, 0, len) := SUBARRAY (ptr^, 0, len);
    RETURN len;
  END ToBytes;

PROCEDURE FromBytes (READONLY buf: ARRAY OF Byte;  p: Precision;
                    VAR f: Float) =
  VAR
    len  := NUMBER (buf);
    ptr  : Ptr;
    x1   : REAL;
    x2   : LONGREAL;
    x3   : EXTENDED;
  BEGIN
    len := TargetMap.Float_types[p].size DIV BITSIZE (Byte);
    <*ASSERT len <= NUMBER (buf) *>

    f.pre      := p;
    f.exponent := 0;

    CASE p OF
    | Precision.Short =>
        ptr := ADR (x1);
        SUBARRAY (ptr^, 0, len) := SUBARRAY (buf, 0, len);
        f.fraction := FLOAT (x1, EXTENDED);
    | Precision.Long =>
        ptr := ADR (x2);
        SUBARRAY (ptr^, 0, len) := SUBARRAY (buf, 0, len);
        f.fraction := FLOAT (x2, EXTENDED);
    | Precision.Extended =>
        ptr := ADR (x3);
        SUBARRAY (ptr^, 0, len) := SUBARRAY (buf, 0, len);
        f.fraction := x3;
    END;
  END FromBytes;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE Normalize (VAR f: Float): BOOLEAN =
  BEGIN
    RETURN (f.exponent = 0);
  END Normalize;

PROCEDURE ToReal (READONLY f: Float): REAL =
  BEGIN
    RETURN FLOAT (f.fraction, REAL);
  END ToReal;

PROCEDURE ToLongreal (READONLY f: Float): LONGREAL =
  BEGIN
    RETURN FLOAT (f.fraction, LONGREAL);
  END ToLongreal;

PROCEDURE ToExtended (READONLY f: Float): EXTENDED =
  BEGIN
    RETURN f.fraction;
  END ToExtended;

BEGIN
END TFloat.
