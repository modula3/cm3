(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Mon Dec  4 11:58:17 PST 1995 by detlefs
*)

MODULE HTMLUtils;

IMPORT TextWr, TextF, Word, Rd, Wr, Text;
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
  BEGIN
    FOR i := 0 TO LAST(str^) DO
      IF str^[i] = '+' THEN str^[i] := ' ' END (* IF *)
    END (* FOR *)
  END PlusToSpace;

PROCEDURE UnEscapeURL(old: TEXT): TEXT =
  VAR res := NEW(TextWr.T).init();
      i := 0;
  BEGIN
    WHILE i < LAST(old^) DO
      IF old^[i] = '%' THEN
        Wr.PutChar(res, X2C(old^, i+1));
        INC(i, 3);
      ELSE
        Wr.PutChar(res, old^[i]);
        INC(i)
      END (* IF *);
    END (* WHILE *);
    RETURN TextWr.ToText(res)
  END UnEscapeURL;

CONST Magic = 16_df;

(* Assumes ASCII encoding of CHAR *)
PROCEDURE X2C(READONLY ac: ARRAY OF CHAR; i: CARDINAL): CHAR =
  VAR res: INTEGER := 0;
      c := ORD(ac[i]);
  BEGIN
    IF ac[i] >= 'A' THEN
      c := Word.And(c, Magic);
      res := res + (c - ORD('A')) + 10
    ELSE
      res := res + c - ORD('0')
    END (* IF *);
    res := res * 16;
    INC(i); c := ORD(ac[i]);
    IF ac[i] >= 'A' THEN
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
      IF str^[i] # stop THEN
        Wr.PutChar(res, str^[i])
      ELSE
        VAR j := i + 1;
            res2 := NEW(TextWr.T).init();
        BEGIN
          WHILE j < len DO
            Wr.PutChar(res2, str^[j]); INC(j)
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
