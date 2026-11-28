(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TFloat.m3                                             *)
(* Last Modified On Thu Jul 28 10:18:03 PDT 1994 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

UNSAFE MODULE TFloat;

IMPORT Convert, TargetMap, TInt, Long;
FROM Target IMPORT Float, Precision;

PROCEDURE New (READONLY chars: ARRAY OF CHAR;  pre: Precision;
               VAR f: Float): BOOLEAN =
  VAR used: INTEGER;
  BEGIN
    f.pre      := pre;
    f.exponent := 0;
    TRY
      IF pre = Precision.Short THEN
        f.fraction := FLOAT(Convert.ToFloat (chars, used), EXTENDED);
      ELSIF pre = Precision.Long THEN
        f.fraction := FLOAT(Convert.ToLongFloat (chars, used), EXTENDED);
      ELSE
        f.fraction := Convert.ToExtended (chars, used);
      END;
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






(*------------------------------------------- results as target integers ---*)

CONST TwoTo63 = 9223372036854775808.0x0;  (* exact in any float format *)

PROCEDURE Ext (READONLY a: Float): EXTENDED =
  BEGIN
    CASE a.pre OF
    | Precision.Short    => RETURN FLOAT (ToReal (a), EXTENDED);
    | Precision.Long     => RETURN FLOAT (ToLongreal (a), EXTENDED);
    | Precision.Extended => RETURN ToExtended (a);
    END;
  END Ext;

(* r := v, a host LONGINT, as a target integer. *)
PROCEDURE FromLong (v: LONGINT;  VAR r: TInt.Int) =
  VAR neg := v < 0L;
  BEGIN
    FOR i := 0 TO 7 DO
      r[i] := VAL (Long.And (v, 16_FFL), INTEGER);
      v := Long.Shift (v, -8);
    END;
    FOR i := 8 TO LAST (r) DO
      IF neg THEN r[i] := 16_FF ELSE r[i] := 0 END;
    END;
  END FromLong;

(* FLOOR (x) for -2^63 <= x < 2^63, which LONGINT holds. *)
PROCEDURE FloorX (x: EXTENDED;  VAR r: TInt.Int): BOOLEAN =
  BEGIN
    IF NOT (-TwoTo63 <= x AND x < TwoTo63) THEN RETURN FALSE END;
    FromLong (FLOOR (x, LONGINT), r);
    RETURN TRUE;
  END FloorX;

(* v := r, a target integer, as a host LONGINT; FALSE if it does not fit. *)
PROCEDURE ToLong (READONLY r: TInt.Int;  VAR v: LONGINT): BOOLEAN =
  VAR sign := 0;
  BEGIN
    IF r[7] >= 16_80 THEN sign := 16_FF END;
    FOR i := 8 TO LAST (r) DO
      IF r[i] # sign THEN RETURN FALSE END;
    END;
    v := 0L;
    FOR i := 7 TO 0 BY -1 DO
      v := Long.Or (Long.Shift (v, 8), VAL (r[i], LONGINT));
    END;
    RETURN TRUE;
  END ToLong;

PROCEDURE FloatI (READONLY i: TInt.Int;  p: Precision;  VAR f: Float): BOOLEAN =
  VAR v: LONGINT;
  BEGIN
    IF NOT ToLong (i, v) THEN RETURN FALSE END;
    f.pre      := p;
    f.exponent := 0;
    f.fraction := FLOAT (v, EXTENDED);
    RETURN Normalize (f);
  END FloatI;

PROCEDURE Floor (READONLY a: Float;  VAR r: TInt.Int): BOOLEAN =
  BEGIN
    RETURN FloorX (Ext (a), r);
  END Floor;

PROCEDURE Ceiling (READONLY a: Float;  VAR r: TInt.Int): BOOLEAN =
  VAR f: TInt.Int;
  BEGIN
    (* -FLOOR (-x); the negation of -2^63 fits a target integer *)
    RETURN FloorX (-Ext (a), f) AND TInt.Subtract (TInt.Zero, f, r);
  END Ceiling;

PROCEDURE Trunc (READONLY a: Float;  VAR r: TInt.Int): BOOLEAN =
  BEGIN
    IF Ext (a) < 0.0x0 THEN RETURN Ceiling (a, r) END;
    RETURN Floor (a, r);
  END Trunc;

PROCEDURE Round (READONLY a: Float;  VAR r: TInt.Int): BOOLEAN =
  (* Ties go to the even neighbour, as FloatMode.RoundDefault
     (NearestElseEven) says, whatever ROUND does in the compiler that
     compiled this module: before #1281 the C back end took them away from
     zero, so a compiler it built folded ROUND (2.5) to 3.  For y >= 0,
     y - FLOOR (y) is exact (Sterbenz), so a tie is detected exactly. *)
  VAR x := Ext (a);  y := ABS (x);  g: TInt.Int;  d: EXTENDED;  odd: BOOLEAN;
  BEGIN
    IF NOT FloorX (y, g) THEN RETURN FALSE END;
    d := y - FLOAT (FLOOR (y, LONGINT), EXTENDED);
    odd := Long.And (FLOOR (y, LONGINT), 1L) # 0L;
    IF d > 0.5x0 OR (d = 0.5x0 AND odd) THEN
      IF NOT TInt.Add (g, TInt.One, g) THEN RETURN FALSE END;
    END;
    IF x < 0.0x0 THEN RETURN TInt.Subtract (TInt.Zero, g, r) END;
    r := g;
    RETURN TRUE;
  END Round;

PROCEDURE ToChars (READONLY f: Float;  VAR buf: ARRAY OF CHAR): INTEGER =
  <*FATAL Convert.Failed *>
  VAR zz: ARRAY [0..45] OF CHAR;  len: INTEGER;
  BEGIN
    <*ASSERT f.exponent = 0*>
    IF f.pre = Precision.Short THEN
      len := Convert.FromFloat (zz, FLOAT(f.fraction, REAL), 13, Convert.Style.Sci);
    ELSIF f.pre = Precision.Long THEN
      len := Convert.FromLongFloat (zz, FLOAT(f.fraction, LONGREAL), 17, Convert.Style.Sci);
    ELSE
      len := Convert.FromExtended (zz, f.fraction, 36, Convert.Style.Sci);
    END;
    IF (len > NUMBER (buf)) THEN RETURN -1 END;
    SUBARRAY (buf, 0, len) := SUBARRAY (zz, 0, len);
    RETURN len;
  END ToChars;

TYPE
  Ptr = UNTRACED REF ARRAY [0..4 * BITSIZE(REAL) DIV BITSIZE(Byte) - 1] OF Byte;

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
        ptr := LOOPHOLE(ADR (x1), Ptr);
        SUBARRAY (ptr^, 0, len) := SUBARRAY (buf, 0, len);
        f.fraction := FLOAT (x1, EXTENDED);
    | Precision.Long =>
        ptr := LOOPHOLE(ADR (x2), Ptr);
        SUBARRAY (ptr^, 0, len) := SUBARRAY (buf, 0, len);
        f.fraction := FLOAT (x2, EXTENDED);
    | Precision.Extended =>
        ptr := LOOPHOLE(ADR (x3), Ptr);
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
