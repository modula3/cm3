MODULE BigIntegerFmtLex;
(*Copyright (c) 1996, m3na project

Abstract: Integers of arbitrary size

*)

IMPORT (*Rd, Thread, FloatMode, Lex AS L, TextRd,*)
       Fmt AS F, Text,
       Word;
IMPORT BigIntegerRep   AS BR;
IMPORT BigIntegerBasic AS BB;
FROM FmtLexSupport IMPORT Precedence;
FROM NADefinitions IMPORT Error, Err;

<*UNUSED*> CONST Module = "BigIntegerFmtLex.";
(*==========================*)

(*
<*FATAL Rd.Failure, Thread.Alerted*>
PROCEDURE Skip (txt: TEXT;  len, start: INTEGER;  blanks: BOOLEAN): INTEGER =
  (* Return the index of the first character of "txt" at or beyond "start"
     that's not in "chars". *)
  VAR
    i   : CARDINAL := NUMBER(buf);
    buf : ARRAY [0..63] OF CHAR;
  BEGIN
    LOOP
      IF (start >= len) THEN RETURN len; END;
      IF (i >= NUMBER (buf)) THEN i := 0; Text.SetChars (buf, txt, start);  END;
      IF (buf[i] IN Lex.Blanks) # blanks THEN RETURN start; END;
      INC (start);  INC (i);
    END;
  END Skip;

PROCEDURE ScanWord (txt: TEXT): Rd.T RAISES {Lex.Error} =
  (* Ensure that "txt" contains exactly one non-blank substring,
     and return its span [start..stop) *)
  VAR
    len    := Text.Length (txt);
    start  := Skip (txt, len, 0,     blanks := TRUE);
    stop   := Skip (txt, len, start, blanks := FALSE);
    finish := Skip (txt, len, stop,  blanks := TRUE);
  BEGIN
    IF finish < len THEN RAISE Lex.Error; END;
    RETURN TextRd.New (Text.Sub (txt, start, stop-start));
  END ScanWord;

PROCEDURE Lex(txt: TEXT; defaultBase: [2..16]): INTEGER
    RAISES {Lex.Error, FloatMode.Trap} =
  VAR rd := ScanWord(txt); res := Lex.Int(rd, defaultBase);
  BEGIN
    IF NOT Rd.EOF(rd) THEN RAISE Lex.Error END;
    RETURN res
  END Lex;
*)



PROCEDURE FastFmtU(READONLY x: T; base: F.Base; pad: [1..Word.Size]): TEXT =
  VAR
    txt : TEXT;
  BEGIN
    IF x.size=0 THEN
      RETURN "0";
    ELSE
      txt := F.Unsigned(x.data[x.size-1],base);
      FOR k:=x.size-2 TO 0 BY -1 DO
        txt := txt & F.Pad (F.Unsigned(x.data[k],base), pad, '0');
      END;
    END;
    RETURN txt;
  END FastFmtU;

(*can be optimized with a division routine that is specialised to small divisors*)
PROCEDURE SlowFmtU(x: T; base: F.Base): TEXT =
  VAR
    r, b  : T;
    txt   := "";
    digit : [0..LAST(F.Base)-1];
  BEGIN
    TRY
      b := BB.FromInteger(base);
      WHILE NOT BB.IsZero(x) DO
        x := BR.DivModU(x,b,r);
        <*ASSERT r.size<=1*>
        digit := r.data[0];
        IF digit<10 THEN
          txt := Text.FromChar(VAL(ORD('0')+digit,CHAR)) & txt;
        ELSE
          txt := Text.FromChar(VAL(ORD('a')+digit-10,CHAR)) & txt;
        END;
      END;
    EXCEPT
      | Error(err) => <*ASSERT err#Err.divide_by_zero*>
    END;
    IF Text.Empty(txt) THEN
      RETURN "0";
    ELSE
      RETURN txt;
    END;
  END SlowFmtU;

PROCEDURE Fmt(READONLY x: T; READONLY style := FmtStyle{}): TEXT =
  VAR
    txt : TEXT;
  BEGIN
    CASE style.base OF
      |  2 => txt := FastFmtU(x, 2,Word.Size);
      |  4 => txt := FastFmtU(x, 4,Word.Size DIV 2);
      | 16 => txt := FastFmtU(x,16,Word.Size DIV 4);
    ELSE
      txt := SlowFmtU(x,style.base);
    END;
    IF x.sign THEN
      RETURN "-" & txt;
    ELSE
      RETURN txt;
    END;
  END Fmt;

PROCEDURE Tex (x : T; READONLY style := TexStyle{}; <*UNUSED*> within : Precedence) : TEXT =
  BEGIN
    IF style.base=10 THEN
      RETURN Fmt (x, FmtStyle{base:=style.base});
    ELSE
      RETURN Fmt (x, FmtStyle{base:=style.base}) & "_{" & F.Int (style.base) & "}";
    END;
  END Tex;

(*==========================*)
BEGIN
END BigIntegerFmtLex.
