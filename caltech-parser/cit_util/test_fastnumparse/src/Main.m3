MODULE Main;
IMPORT FastNumParse;
IMPORT Debug, Text;
IMPORT Fmt;

PROCEDURE MakeChars(txt : TEXT) : REF ARRAY OF CHAR =
  VAR
    res := NEW(REF ARRAY OF CHAR, Text.Length(txt));
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      res[i] := Text.GetChar(txt, i)
    END;
    RETURN res
  END MakeChars;

PROCEDURE Chars(txt : TEXT) =
  BEGIN
    Debug.Out(Fmt.F("Chars \"%s\"", txt));
    chars := MakeChars(txt);
    p := 0;
  END Chars;

PROCEDURE DoInt() = 
  VAR
    z := FIRST(INTEGER);
  BEGIN
    WITH b = FastNumParse.Int(chars^, p, z) DO
      Debug.Out(Fmt.F("Int      %-6s %s", Fmt.Bool(b), Fmt.Int(z)))
    END
  END DoInt;

PROCEDURE DoLongReal() = 
  VAR
    z := FIRST(LONGREAL);
  BEGIN
    WITH b = FastNumParse.LongReal(chars^, p, z) DO
      Debug.Out(Fmt.F("LongReal %-6s %s", Fmt.Bool(b), Fmt.LongReal(z)))
    END
  END DoLongReal;

VAR
  chars : REF ARRAY OF CHAR;
  p : CARDINAL;
BEGIN
  Chars("1");
  DoInt();
  DoInt();

  Chars("11");
  DoInt();
  DoInt();

  Chars("11");
  DoLongReal();
  DoLongReal();

  Chars("-11");
  DoLongReal();
  DoLongReal();

  Chars("-11-11");
  DoLongReal();
  DoLongReal();

  Chars("-11e-11");
  DoLongReal();
  DoLongReal();

END Main.
