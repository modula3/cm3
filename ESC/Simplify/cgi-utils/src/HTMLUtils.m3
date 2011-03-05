(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Mon Dec  4 11:58:17 PST 1995 by detlefs
*)

MODULE HTMLUtils;

IMPORT TextWr, Word, Rd, Wr, Text;
IMPORT Thread;

<*FATAL Rd.Failure, Thread.Alerted, Wr.Failure *>

PROCEDURE FMakeWord(rd: Rd.T; stop: CHAR; VAR cl: INTEGER): TEXT =
  VAR res := NEW(TextWr.T).init(); BEGIN
    WHILE NOT Rd.EOF(rd) DO
      VAR c := Rd.GetChar(rd); BEGIN <*NOWARN*>
        IF c = stop THEN EXIT END (* IF *);
        DEC(cl);
        Wr.PutChar(res, c)
      END (* BEGIN *)
    END (* WHILE *);
    RETURN TextWr.ToText(res)
  END FMakeWord;

PROCEDURE PlusToSpace(str: TEXT) =
  VAR newStr := NEW(TextWr.T).init();
  BEGIN
    FOR i := 0 TO Text.Length(str) DO
      IF Text.GetChar(str, i) = '+' THEN
        (* replace the '+' with a ' ' *)
        Wr.PutText(newStr, Text.Sub(str, 0, i));
        Wr.PutChar(newStr, ' ');
        Wr.PutText(newStr, Text.Sub(str, i+1, Text.Length(str)-i-1));
        str := TextWr.ToText(newStr);
      END (* IF *)
    END (* FOR *)
  END PlusToSpace;

PROCEDURE UnEscapeURL(old: TEXT): TEXT =
  VAR res := NEW(TextWr.T).init();
      i := 0;
  BEGIN
    WHILE i < Text.Length(old) DO
      IF Text.GetChar(old, i) = '%' THEN
        Wr.PutChar(res, X2C(old, i+1));
        INC(i, 3);
      ELSE
        Wr.PutChar(res, Text.GetChar(old, i));
        INC(i)
      END (* IF *);
    END (* WHILE *);
    RETURN TextWr.ToText(res)
  END UnEscapeURL;

CONST Magic = 16_df;

(* Assumes ASCII encoding of CHAR *)
PROCEDURE X2C(READONLY text: TEXT; i: CARDINAL): CHAR =
  VAR res: INTEGER := 0;
      c := ORD(Text.GetChar(text, i));
  BEGIN
    IF Text.GetChar(text, i) >= 'A' THEN
      c := Word.And(c, Magic);
      res := res + (c - ORD('A')) + 10
    ELSE
      res := res + c - ORD('0')
    END (* IF *);
    res := res * 16;
    INC(i); c := ORD(Text.GetChar(text, i));
    IF Text.GetChar(text, i) >= 'A' THEN
      c := Word.And(c, Magic);
      res := res + (c - ORD('A')) + 10
    ELSE
      res := res + c - ORD('0')
    END (* IF *);
    RETURN VAL(res, CHAR)
  END X2C;

PROCEDURE MakeWord(VAR str: TEXT; stop: CHAR): TEXT =
  VAR res := NEW(TextWr.T).init();
      len := Text.Length(str);
  BEGIN
    FOR i := 0 TO len-1 DO
      IF Text.GetChar(str, i) # stop THEN
        Wr.PutChar(res, Text.GetChar(str, i))
      ELSE
        VAR j := i + 1;
            res2 := NEW(TextWr.T).init();
        BEGIN
          WHILE j < len DO
            Wr.PutChar(res2, Text.GetChar(str, j)); INC(j)
          END (* WHILE *);
          str := TextWr.ToText(res2);
          RETURN TextWr.ToText(res)
        END (* BEGIN *)
      END (* IF *)
    END (* FOR *);
    str := "";
    RETURN TextWr.ToText(res)
  END MakeWord;

BEGIN
END HTMLUtils.
