(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar 28 14:15:55 PST 1996 by heydon                   *)
(*      modified on Tue Feb 21 14:22:42 PST 1995 by gnelson                  *)

UNSAFE MODULE JunoValue; (* for Hash() only *)

IMPORT Math, Formatter, Wr, Pickle, Fmt, ASCII, Text, Word;
IMPORT   FloatMode, RealFloat;

REVEAL
  Null = BRANDED "Juno-NIL" REF RECORD END;

PROCEDURE Unparse(wr: Wr.T; x: T; width, prec: CARDINAL) RAISES {Wr.Failure} =
  VAR f := Formatter.New(wr, width); BEGIN
    UnparseToFmt(f, x, prec);
    Formatter.Flush(f);
    Formatter.Close(f)
  END Unparse;

PROCEDURE UnparseToFmt(f: Formatter.T; x: T; prec: CARDINAL)
  RAISES {Wr.Failure} =

  PROCEDURE C(c: CHAR) RAISES {Wr.Failure} =
    BEGIN
      Formatter.PutChar(f, c)
    END C;

  PROCEDURE S(t: TEXT) RAISES {Wr.Failure} =
    BEGIN
      Formatter.PutText(f, t, raw := TRUE)
    END S;

  PROCEDURE UnpReal(r: Real) RAISES {Wr.Failure} =
    BEGIN
      Formatter.PutText(f, Fmt.Real(r, prec := prec), raw := TRUE)
    END UnpReal;

  PROCEDURE Octal(c: CHAR) RAISES {Wr.Failure} =
    BEGIN
      C('\\');
      S(Fmt.Pad(Fmt.Int(ORD(c), base := 8), length := 3, padChar := '0'))
    END Octal;

  PROCEDURE UnpText(t: TEXT) RAISES {Wr.Failure} =
    BEGIN
      C('"');
      FOR i := 0 TO Text.Length(t) - 1 DO
        VAR c := Text.GetChar(t, i); BEGIN
          IF c IN ASCII.Asciis THEN
            IF c IN ASCII.Controls THEN
              CASE c OF
                '\n' => C('\\'); C('n')
              | '\t' => C('\\'); C('t')
              | '\r' => C('\\'); C('r')
              | '\f' => C('\\'); C('f')
              ELSE Octal(c)
              END
            ELSE
              CASE c OF
              | '\\' => C('\\'); C(c)
              | '\"' => C('\\'); C(c)
              ELSE C(c)
              END
            END
          ELSE
            Octal(c)
          END
        END
      END;
      C('"')
    END UnpText;

  PROCEDURE UnpList(p: REF Pair) RAISES {Wr.Failure} =
    BEGIN
      C('[');
      Formatter.Begin(f);
      LOOP
        UnpValue(p.car);
        IF p.cdr = Nil THEN EXIT END;
        p := NARROW(p.cdr, REF Pair);
        S(", ");
        Formatter.UnitedBreak(f, 1)
      END;
      Formatter.End(f);
      C(']')
    END UnpList;

  PROCEDURE UnpPair(p: REF Pair) RAISES {Wr.Failure} =
    BEGIN
      C('(');
      Formatter.Begin(f);
      UnpValue(p.car);
      S(", ");
      Formatter.UnitedBreak(f, 1);
      UnpValue(p.cdr);
      Formatter.End(f);
      C(')')
    END UnpPair;

  PROCEDURE UnpNil() RAISES {Wr.Failure} =
    BEGIN
      Formatter.PutText(f, "NIL", raw := TRUE)
    END UnpNil;

  PROCEDURE UnpValue (x: T) RAISES {Wr.Failure} =
    BEGIN
      TYPECASE x OF <* NOWARN *>
      | NULL => <* ASSERT FALSE *>
      | Null => UnpNil()
      | REF REAL(r) => UnpReal(r^)
      | TEXT(t) => UnpText(t)
      | REF Pair(p) => IF ListLen(p) # -1 THEN UnpList(p) ELSE UnpPair(p) END
      END
    END UnpValue;

  BEGIN
    UnpValue(x)
  END UnparseToFmt;

PROCEDURE Equal(READONLY t1, t2: T): BOOLEAN =
  CONST Epsilon = 1.0E-3; BEGIN
    IF TYPECODE(t1) # TYPECODE(t2) THEN RETURN FALSE END;
    TYPECASE t1 OF <* NOWARN *>
    | Null => RETURN TRUE
    | REF Real(r1) => RETURN ABS(r1^ - NARROW(t2, REF Real)^) < Epsilon
    | TEXT(txt1) => RETURN Text.Equal(txt1, NARROW(t2, TEXT))
    | REF Pair(p1) =>
        VAR p2 := NARROW(t2, REF Pair); BEGIN
          RETURN Equal(p1.car, p2.car) AND Equal(p1.cdr, p2.cdr)
        END
    END
  END Equal;

PROCEDURE Hash(READONLY k: T): Word.T =
  (* This technique for loopholing Real's into Word.T's requires that
     "BITSIZE(Real) <= BITSIZE(Word.T)" *)
  CONST K = BITSIZE(Real); Max = Word.Minus(Word.Shift(1, K-1), 1);
  TYPE RealWord = BITS K FOR [-Max-1..Max];
  BEGIN
    TYPECASE k OF <* NOWARN *>
      Null => RETURN 0
    | TEXT (t) => RETURN Text.Hash(t)
    | REF Pair (p) => RETURN Word.Xor(Hash(p.car), Hash(p.cdr))
    | REF Real (r) => RETURN LOOPHOLE(r^, RealWord)
    END
  END Hash;

PROCEDURE Sin(x: Real): Real =
  BEGIN RETURN FLOAT(Math.sin(FLOAT(x, LONGREAL)), Real) END Sin;

PROCEDURE Cos(x: Real): Real =
  BEGIN RETURN FLOAT(Math.cos(FLOAT(x, LONGREAL)), Real) END Cos;

PROCEDURE Tan(x: Real): Real =
  BEGIN RETURN FLOAT(Math.tan(FLOAT(x, LONGREAL)), Real) END Tan;

PROCEDURE Asin(x: Real): Real =
  BEGIN RETURN FLOAT(Math.asin(FLOAT(x, LONGREAL)), Real) END Asin;

PROCEDURE Acos(x: Real): Real =
  BEGIN RETURN FLOAT(Math.acos(FLOAT(x, LONGREAL)), Real) END Acos;

PROCEDURE Atan(y, x: Real): Real =
  BEGIN
    RETURN FLOAT(Math.atan2(FLOAT(y, LONGREAL), FLOAT(x, LONGREAL)), Real)
  END Atan;

PROCEDURE Exp(x: Real): Real =
  BEGIN RETURN FLOAT(Math.exp(FLOAT(x, LONGREAL)), Real) END Exp;

PROCEDURE Ln(x: Real): Real =
  BEGIN RETURN FLOAT(Math.log(FLOAT(x, LONGREAL)), Real) END Ln;

PROCEDURE Sqrt(x: Real): Real =
  BEGIN RETURN FLOAT(Math.sqrt(FLOAT(x, LONGREAL)), Real) END Sqrt;

PROCEDURE RefReal(x: Real): REF Real =
  VAR res := NEW(REF Real); BEGIN
    res^ := x;
    RETURN res
  END RefReal;

PROCEDURE NewPoint(x, y: Real): REF Pair =
  BEGIN
    RETURN NEW(REF Pair, car := RefReal(x), cdr := RefReal(y))
  END NewPoint;

PROCEDURE ListFromVals(READONLY v: ARRAY OF T): T =
  VAR res: T := Nil; BEGIN
    FOR i := LAST(v) TO FIRST(v) BY -1 DO
      res := NEW(REF Pair, car := v[i], cdr := res)
    END;
    RETURN res
  END ListFromVals;

PROCEDURE IsList(v: T): BOOLEAN =
  BEGIN RETURN ListLen(v) > 0 END IsList;

PROCEDURE ListLen(t: T): INTEGER =
  VAR res := 0; BEGIN
    LOOP
      TYPECASE t OF
      | REF Pair (p) =>
        INC(res);
        t := p.cdr
      ELSE EXIT
      END
    END;
    IF t = Nil
      THEN RETURN res
      ELSE RETURN -1
    END
  END ListLen;

PROCEDURE NullWrite (
    <*UNUSED*> sp: Pickle.Special;
    <*UNUSED*> r: REFANY; 
    <*UNUSED*> writer: Pickle.Writer) =
  BEGIN
  END NullWrite;

PROCEDURE NullRead (
    <*UNUSED*> sp: Pickle.Special;
    <*UNUSED*> reader: Pickle.Reader;
    <*UNUSED*> id: Pickle.RefID) : REFANY =
  BEGIN
    RETURN Nil
  END NullRead;

BEGIN
  Nil := NEW(Null);
  Pickle.RegisterSpecial (NEW (Pickle.Special, sc := TYPECODE (Null),
    write := NullWrite,
    read  := NullRead));
  <* FATAL FloatMode.Trap *> BEGIN
    HalfEps := (RealFloat.NextAfter(1.0, 2.0) - 1.0) * 0.5
  END
END JunoValue.
