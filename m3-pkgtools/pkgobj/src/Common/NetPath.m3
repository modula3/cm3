(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetPath.m3 *)
(* Last modified on Tue Jul 20 12:17:30 PDT 1993 by wobber *)

MODULE NetPath;

IMPORT Pathname, Text, TextList, TextSeq, Word;

CONST
  DirSepChar = '/';   (* this is not Unix, but our own path syntax *)
  DirSepCharStr = "/";

PROCEDURE Equal(t1, t2: T) : BOOLEAN =
  BEGIN
    WHILE t1 # NIL DO
      IF t2 = NIL THEN RETURN FALSE; END;
      IF NOT Text.Equal(t1.head, t2.head) THEN RETURN FALSE; END;
      t1 := t1.tail;
      t2 := t2.tail;
    END;
    RETURN (t2 = NIL);
  END Equal;

PROCEDURE Hash(t: T) : Word.T =
  BEGIN
    WHILE t.tail # NIL DO t := t.tail; END;
    RETURN Text.Hash(t.head);
  END Hash;

PROCEDURE Compare(t1, t2: T) : [-1..1] =
  VAR c:  [-1..1];
  BEGIN
    WHILE t1 # NIL DO
      IF t2 = NIL THEN RETURN -1; END;
      c := Text.Compare(t1.head, t2.head);
      IF c # 0 THEN RETURN c; END;
      t1 := t1.tail;
      t2 := t2.tail;
    END;
    IF t2 # NIL THEN RETURN 1; END;
    RETURN 0;
  END Compare;


PROCEDURE Check(t: T) : BOOLEAN =
  BEGIN
    WHILE t # NIL DO
      IF NOT CheckArc(t.head) THEN RETURN FALSE; END;
      t := t.tail;
    END;
    RETURN TRUE;
  END Check;

PROCEDURE CheckArc(arc: TEXT) : BOOLEAN =
  BEGIN
    (* check char sets here *)
    RETURN (arc = NIL) OR NOT Text.Empty(arc);
  END CheckArc;


PROCEDURE ToText(t: T) : TEXT =
  VAR text: TEXT := NIL;
  BEGIN
    IF t = NIL THEN RETURN ""; END;
    WHILE t # NIL DO
      IF text = NIL THEN
        text := t.head;
      ELSE
        text := text & DirSepCharStr & t.head;
      END;
      t := t.tail;
    END;
    RETURN text;
  END ToText;

PROCEDURE FromText(text: TEXT) : T RAISES {Invalid} =
  VAR i, ii := 0;
      len: CARDINAL;
      res: TextList.T := NIL;
  PROCEDURE NextSub(start, end: CARDINAL) RAISES {Invalid} =
    VAR t: TEXT;
    BEGIN
      IF start = end THEN RETURN; END;
      t := Text.Sub(text, start, end-start);
      IF NOT CheckArc(t) THEN RAISE Invalid; END;
      res := TextList.Cons(t, res);
    END NextSub;
  BEGIN
    text := StripOldArcs(text);
    len := Text.Length(text);
    WHILE ii < len DO
      i := Text.FindChar(text, DirSepChar, ii);
      IF i < 0 THEN EXIT; END;
      IF i = 0 THEN RAISE Invalid; END;
      NextSub(ii, i);
      ii := i + 1;
    END;
    NextSub(ii, Text.Length(text));
    RETURN TextList.ReverseD(res);
  END FromText;

PROCEDURE ToRelFN(t: T) : TEXT =
  VAR text: TEXT := NIL;
  BEGIN
    IF t = NIL THEN RETURN ""; END;
    WHILE t # NIL DO
      IF text = NIL THEN
        text := t.head;
      ELSE
        text := Pathname.Join(text, t.head, NIL);
      END;
      t := t.tail;
    END;
    RETURN text;
  END ToRelFN;

PROCEDURE FromRelFN(text: TEXT) : T RAISES {Invalid} =
  VAR s: TextSeq.T;
      res: TextList.T := NIL;
  BEGIN
    IF Pathname.Absolute(text) THEN RAISE Invalid; END;
    TRY
      s := Pathname.Decompose(text);
    EXCEPT
    | Pathname.Invalid => RAISE Invalid;
    END;
    FOR i := s.size()-1 TO 1 BY -1 DO
      VAR arc := s.get(i); BEGIN
        IF NOT Text.Empty(arc) THEN
          IF NOT CheckArc(arc) THEN RAISE Invalid; END;
          res := TextList.Cons(arc, res);
        END;
      END;
    END;
    RETURN res;
  END FromRelFN;

PROCEDURE Parent(t: T) : T =
  BEGIN
    IF t = NIL OR t.tail = NIL THEN RETURN NIL; END;
    RETURN TextList.Cons(t.head, Parent(t.tail));
  END Parent;


PROCEDURE EqualPN(pn1, pn2: PN) : BOOLEAN =
  BEGIN
    IF NOT Equal(pn1.dir, pn2.dir) THEN RETURN FALSE; END;
    RETURN Text.Equal(pn1.arc, pn2.arc);
  END EqualPN;

PROCEDURE PNToText(pn: PN) : TEXT =
  BEGIN
    IF pn.dir = NIL THEN RETURN pn.arc; END;
    RETURN ToText(pn.dir) & DirSepCharStr & pn.arc;
  END PNToText;

PROCEDURE PNFromText(text: TEXT) : PN RAISES {Invalid} =
  VAR pn: PN;
      l := FromText(text);
  BEGIN
    IF l = NIL THEN RAISE Invalid; END;
    IF l.tail = NIL THEN
      pn.dir := NIL;
      pn.arc := l.head;
    ELSE
      pn.dir := l;
      WHILE l.tail.tail # NIL DO l := l.tail; END;
      pn.arc := l.tail.head;
      l.tail := NIL;
    END;
    RETURN pn;
  END PNFromText;

CONST ProjStr = "/proj/";
      PkgStr = "/pkg/";

PROCEDURE StripOldArcs(t: TEXT) : TEXT =
    (* maps /proj/x/y/z -> x/y/z *)
    (* maps /proj/x/pkg/z -> x/z *)
  VAR i: INTEGER;
  BEGIN
    IF t = NIL OR Text.Empty(t) OR Text.GetChar(t, 0) # DirSepChar THEN
      RETURN t;
    END;
    IF NOT Text.Equal(ProjStr, Text.Sub(t, 0, Text.Length(ProjStr))) THEN
      RETURN t;
    END;
    t := Text.Sub(t, Text.Length(ProjStr), LAST(CARDINAL));
    i := Text.FindChar(t, DirSepChar);
    IF i > 0 AND Text.Equal(PkgStr, Text.Sub(t, i, Text.Length(PkgStr))) THEN
      t := Text.Sub(t, 0, i+1) &
           Text.Sub(t, i+Text.Length(PkgStr), LAST(CARDINAL));
    END;
    RETURN t;
  END StripOldArcs;

BEGIN
END NetPath.


