(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

MODULE LexMisc;

IMPORT Text, Word, IP;

PROCEDURE ReadUID (READONLY buf: ARRAY OF CHAR;  VAR cursor: INTEGER): INTEGER = 
  VAR eof := NUMBER (buf);  ch: CHAR;  uid: INTEGER := 0;  digit: INTEGER;
  BEGIN
    SkipBlanks (buf, cursor);
    WHILE (cursor < eof) DO
      ch := buf[cursor];
      IF ('0' <= ch) AND (ch <= '9') THEN
        digit := ORD (ch) - ORD ('0');
      ELSIF ('a' <= ch) AND (ch <= 'f') THEN
        digit := 10 + ORD (ch) - ORD ('a');
      ELSE
        EXIT;
      END;
      INC (cursor);
      uid := Word.LeftShift (uid, 4) + digit;
      uid := Word.And (uid, 16_ffffffff);
    END;
    RETURN uid;
  END ReadUID;

PROCEDURE SkipBlanks (READONLY buf: ARRAY OF CHAR;  VAR cur: INTEGER) =
  VAR eof := NUMBER (buf);
  BEGIN
    WHILE (cur < eof) AND (buf[cur] = ' ') DO INC (cur); END;
  END SkipBlanks;

PROCEDURE ReadName (READONLY buf: ARRAY OF CHAR;  VAR cursor: INTEGER): TEXT =
  VAR ch: CHAR;  eof := NUMBER (buf);  start := cursor;
  BEGIN
    SkipBlanks (buf, cursor);
    start := cursor;
    WHILE (cursor < eof) DO
      ch := buf[cursor];
      IF (ch = ' ') OR (ch = '\n') OR (ch = '\r') THEN EXIT END;
      INC (cursor);
    END;
    RETURN Text.FromChars (SUBARRAY (buf, start, cursor - start));
  END ReadName;

PROCEDURE ReadInt (READONLY buf: ARRAY OF CHAR;  VAR cursor: INTEGER): INTEGER =
  VAR ch: CHAR;  eof := NUMBER (buf);  val := 0;
  BEGIN
    SkipBlanks (buf, cursor);
    WHILE (cursor < eof) DO
      ch := buf[cursor];
      IF (ch < '0') OR ('9' < ch) THEN EXIT END;
      val := val * 10 + ORD (ch) - ORD ('0');
      INC (cursor);
    END;
    RETURN val;
  END ReadInt;

PROCEDURE ReadBrand (READONLY buf: ARRAY OF CHAR; VAR cursor: INTEGER): TEXT =
  VAR eof := NUMBER (buf);  start: INTEGER;
  BEGIN
    IF (buf[cursor] # ' ') THEN RETURN NIL END;
    INC (cursor);
    start := cursor;
    WHILE (cursor < eof) AND (buf[cursor] # '\r') AND (buf[cursor] # '\n') DO
      INC (cursor);
    END;
    RETURN Text.FromChars (SUBARRAY (buf, start, cursor - start));
  END ReadBrand;

CONST
  HexDigits = ARRAY [0..15] OF CHAR { '0','1','2','3','4','5','6','7',
                                      '8','9','a','b','c','d','e','f' };

PROCEDURE FmtUID (uid: INTEGER): TEXT =
  VAR buf: ARRAY [0..7] OF CHAR;
  BEGIN
    FOR i := 7 TO 0 BY -1 DO
      buf [i] := HexDigits [Word.And (uid, 16_f)];
      uid := Word.RightShift (uid, 4);
    END;
    RETURN Text.FromChars (buf);
  END FmtUID;

PROCEDURE ScanUID (txt: TEXT): INTEGER =
  VAR cursor := 0;  buf: ARRAY [0..31] OF CHAR;
  BEGIN
    Text.SetChars (buf, txt);
    RETURN ReadUID (buf, cursor);
  END ScanUID;

PROCEDURE ScanInt (txt: TEXT): INTEGER =
  VAR cursor := 0;  buf: ARRAY [0..31] OF CHAR;  len := Text.Length (txt);
  BEGIN
    Text.SetChars (buf, txt);
    RETURN ReadInt (SUBARRAY (buf, 0, MIN (NUMBER (buf), len)), cursor);
  END ScanInt;

PROCEDURE ScanIPAddress (txt: TEXT;  VAR(*OUT*) addr: IP.Address): BOOLEAN =
  VAR c: CHAR;  start, val, elt: INTEGER;
  BEGIN
    IF (txt = NIL) OR Text.Length (txt) <= 0 THEN
      addr := IP.NullAddress;
      RETURN TRUE;
    END;
    elt := 0;  val := 0;  start := 0;
    FOR i := 0 TO Text.Length (txt) - 1 DO
      c := Text.GetChar (txt, i);
      IF ('0' <= c) AND (c <= '9') THEN
        val := val * 10 + ORD (c) - ORD ('0');
      ELSIF (c = '.') THEN
        IF (start = i) OR (val > 255) OR (elt > LAST (addr.a)) THEN
          RETURN FALSE;
        END;
        addr.a [elt] := val;  INC (elt);
        val := 0;  start := i + 1;
      ELSE
        RETURN FALSE;
      END;
    END;
    IF (start = Text.Length (txt)) OR (val > 255) OR (elt # LAST (addr.a)) THEN
      RETURN FALSE;
    END;
    addr.a [elt] := val;
    RETURN TRUE;
  END ScanIPAddress;

PROCEDURE ScanString (txt: TEXT): TEXT =
  VAR buf := NEW (REF ARRAY OF CHAR, Text.Length (txt));
  BEGIN
    Text.SetChars (buf^, txt);
    RETURN ReadString (buf^);
  END ScanString;

PROCEDURE ReadString (VAR buf: ARRAY OF CHAR): TEXT =
  CONST Escape = '\134';
  VAR s0 := 0;  s1 := 0;  len := NUMBER (buf);  c, c2, c3: CHAR;
  BEGIN
    WHILE (s1 < len) DO
      c := buf[s1];  INC (s1);
      IF (c = Escape) THEN
        c := buf[s1];  INC (s1);
        IF    (c = 'n')    THEN  c := '\n';
        ELSIF (c = 'r')    THEN  c := '\r';
        ELSIF (c = 't')    THEN  c := '\t';
        ELSIF (c = 'f')    THEN  c := '\f';
        ELSIF (c = '\'')   THEN  c := '\'';
        ELSIF (c = '\"')   THEN  c := '\"';
        ELSIF (c = Escape) THEN  c := Escape;
        ELSIF (s1 + 1 <= LAST (buf)) THEN
          (* maybe its a three-digit octal value *)
          c2 := buf[s1];
          c3 := buf[s1+1];
          IF ('0' <= c) AND (c <= '3') AND
             ('0' <= c2) AND (c2 <= '7') AND
             ('0' <= c3) AND (c3 <= '7') THEN
            c := VAL ( (ORD (c) - ORD ('0')) * 64
                     + (ORD (c2) - ORD ('0')) * 8
                     + (ORD (c3) - ORD ('0')), CHAR);
            INC (s1, 2);
          ELSE
            c := Escape;
            DEC (s1);
          END;
        ELSE  (* I dunno... *)
          c := Escape;
          DEC (s1);
        END;
      END;
      buf[s0] := c;  INC (s0);
    END;
    RETURN Text.FromChars (SUBARRAY (buf, 0, s0));
  END ReadString;

BEGIN
END LexMisc.
