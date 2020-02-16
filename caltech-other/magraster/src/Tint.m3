(* $Id$ *)
MODULE Tint;
IMPORT TextReader;
IMPORT Scan, Fmt;
IMPORT FloatMode, Lex;
IMPORT Debug;
IMPORT Math;

PROCEDURE Parse(from : TEXT) : T RAISES { ParseError } =

  PROCEDURE InRange(READONLY x : INTEGER) : BOOLEAN = 
    BEGIN RETURN x >= 0 AND x <= 255 END InRange;

  VAR
    reader := NEW(TextReader.T).init(from);
    x : T;
    r, g, b : INTEGER;
  BEGIN
    Debug.Out("Tint.Parse(" & from & ")", 100);
    TRY
      r := Scan.Int(reader.nextE(Delims, skipNulls := TRUE));
      g := Scan.Int(reader.nextE(Delims, skipNulls := TRUE));
      b := Scan.Int(reader.nextE(Delims, skipNulls := TRUE));

      IF NOT (InRange(r) AND InRange(g) AND InRange(b)) THEN 
        RAISE ParseError 
      END;

      x[Channel.R] := FLOAT(r,LONGREAL)/255.0d0;
      x[Channel.G] := FLOAT(g,LONGREAL)/255.0d0;
      x[Channel.B] := FLOAT(b,LONGREAL)/255.0d0;
      x[Channel.alpha] := Scan.LongReal(reader.nextE(Delims, 
                                                     skipNulls := TRUE));
      RETURN x
    EXCEPT
      TextReader.NoMore, FloatMode.Trap, Lex.Error => RAISE ParseError
    END
  END Parse;

PROCEDURE Format(tint : T) : TEXT =
  VAR
    res := "{";
  BEGIN
    FOR i := FIRST(Channel) TO LAST(Channel) DO
      res := res & " " & Fmt.LongReal(tint[i])
    END;
    res := res & " }";
    RETURN res
  END Format;

PROCEDURE Gamma(READONLY tint : T; gamma : LONGREAL) : T =
  VAR
    res : T;
  CONST
    EpsMach = 0.0001d0;  
  BEGIN
    FOR i := Channel.R TO Channel.B DO
      <* ASSERT tint[i] >= 0.0d0 - EpsMach AND tint[i] <= 1.0d0 + EpsMach *>
      res[i] := Math.pow(tint[i],gamma)
    END;
    res[Channel.alpha] := tint[Channel.alpha];
    RETURN res
  END Gamma;

PROCEDURE Add(READONLY a, b : T) : T =
  VAR 
    res : T;
  BEGIN
    FOR i := FIRST(Channel) TO LAST(Channel) DO res[i] := a[i] + b[i] END;
    RETURN res
  END Add;


PROCEDURE Scale(READONLY a : T; by : LONGREAL) : T =
  VAR 
    res : T;
  BEGIN
    FOR i := FIRST(Channel) TO LAST(Channel) DO res[i] := a[i] * by END;
    RETURN res
  END Scale;

BEGIN END Tint.
