(* Copyright 1995-96 Critical Mass, Inc. All rights reserved. *)

(* This interface defines misc. TEXT manipulation routines. *)

MODULE Text2;

IMPORT ASCII, Text, Text8, Word;

PROCEDURE CIEqual (a, b: TEXT): BOOLEAN =
  VAR
    len1 := SleazyLen (a);
    len2 := SleazyLen (b);
    c1, c2: CHAR;
  BEGIN
    IF (len1 # len2) THEN RETURN FALSE; END;
    FOR i := 0 TO len1 - 1 DO
      c1 := ASCII.Upper [Text.GetChar (a, i)];
      c2 := ASCII.Upper [Text.GetChar (b, i)];
      IF (c1 # c2) THEN RETURN FALSE; END
    END;
    RETURN TRUE;
  END CIEqual;

PROCEDURE Trim (a: TEXT): TEXT =
  VAR start: CARDINAL := 0;  stop := SleazyLen (a);  c: CHAR;
  BEGIN
    WHILE (start < stop) DO
      c := Text.GetChar (a, start);
      IF (c # ' ') AND (c # '\t') AND (c # '\r') AND  (c # '\n') THEN EXIT; END;
      INC (start);
    END;
    WHILE (start < stop) DO
      c := Text.GetChar (a, stop-1);
      IF (c # ' ') AND (c # '\t') AND (c # '\r') AND  (c # '\n') THEN EXIT; END;
      DEC (stop);      
    END;
    IF (start < stop)
      THEN RETURN Text.Sub (a, start, stop - start);
      ELSE RETURN "";
    END;
  END Trim;

PROCEDURE EscapeString (a: TEXT): TEXT =
  VAR len: CARDINAL;   buf: ARRAY [0..255] OF CHAR;
  BEGIN
    IF (a = NIL) THEN a := ""; END;
    len := Text.Length (a);
    IF (len <= NUMBER (buf))
      THEN RETURN DoEscape (a, len, buf);
      ELSE RETURN DoEscape (a, len, NEW (REF ARRAY OF CHAR, len)^);
    END;
  END EscapeString;

PROCEDURE DoEscape (a: TEXT;  len: CARDINAL;  VAR buf: ARRAY OF CHAR): TEXT =
  CONST BackSlash = '\134';
  VAR
    n_special := 0;
    c: CHAR;
    b: Text8.T;
    bx, z: INTEGER;
  BEGIN
    Text.SetChars (buf, a);

    FOR i := 0 TO len-1 DO
      c := buf[i];
      IF (c = '\n') OR (c = '\"') OR (c = '\'') OR (c = BackSlash)
        OR (c = '\r') OR (c = '\t') OR (c = '\f') THEN
        INC (n_special);
      ELSIF (c < ' ') OR (c > '~') THEN
        INC (n_special, 3);
      END;
    END;

    b := Text8.Create (len + n_special + 2);
    b.contents[0] := '\"';  bx := 1;
    FOR i := 0 TO len-1 DO
      c := buf[i];
      IF (c = BackSlash) THEN
        b.contents[bx] := c; INC (bx); b.contents[bx] := c;  INC (bx);
      ELSIF (c = '\n') THEN
        b.contents[bx] := BackSlash; INC (bx); b.contents[bx] := 'n';  INC (bx);
      ELSIF (c = '\"') THEN
        b.contents[bx] := BackSlash; INC (bx); b.contents[bx] := c;  INC (bx);
      ELSIF (c = '\'') THEN
        b.contents[bx] := BackSlash; INC (bx); b.contents[bx] := c;  INC (bx);
      ELSIF (c = '\r') THEN
        b.contents[bx] := BackSlash; INC (bx); b.contents[bx] := 'r'; INC (bx);
      ELSIF (c = '\t') THEN
        b.contents[bx] := BackSlash; INC (bx); b.contents[bx] := 't'; INC (bx);
      ELSIF (c = '\f') THEN
        b.contents[bx] := BackSlash; INC (bx); b.contents[bx] := 'f'; INC (bx);
      ELSIF (c < ' ') OR (c > '~') THEN
        b.contents[bx] := BackSlash;  INC (bx);
        z := Word.RightShift (Word.And (ORD (c), 8_700), 6);
        b.contents[bx] := VAL(z + ORD('0'), CHAR);  INC (bx);
        z := Word.RightShift (Word.And (ORD (c), 8_070), 3);
        b.contents[bx] := VAL(z + ORD('0'), CHAR);  INC (bx);
        z := Word.And (ORD (c), 8_007);
        b.contents[bx] := VAL(z + ORD('0'), CHAR);  INC (bx);
      ELSE
        b.contents[bx] := c;  INC (bx);
      END;
    END;
    b.contents[bx] := '\"';  INC (bx);

    RETURN b;
  END DoEscape;

PROCEDURE SleazyLen (a: TEXT): CARDINAL =
  BEGIN
    IF (a = NIL) THEN RETURN 0; END;
    RETURN Text.Length (a);
  END SleazyLen;

BEGIN
END Text2.
