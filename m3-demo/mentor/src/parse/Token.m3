(* Copyright 1992 Digital Equipment Corporation.       *)
(* Distributed only by permission.                     *)
(* Last modified on Mon Aug  3 13:16:19 PDT 1992 by kalsow *)

MODULE Token;

IMPORT Text, Fmt;

CONST
  IdChars = SET OF CHAR { 'a'..'z', 'A'..'Z', '0'..'9'};

PROCEDURE Scan (input: TEXT;  VAR cursor: INTEGER;
                 VAR t: T;  VAR name: TEXT) =
  VAR
    len := Text.Length (input);
    ch: CHAR;
    buf: ARRAY [0..19] OF CHAR;
    i: INTEGER;
  BEGIN
    IF (cursor < 0) THEN  t := T.EOF; name := Name[T.EOF]; RETURN END;

    (* skip white space *)
    LOOP
      IF (cursor >= len) THEN  t := T.EOF; name := Name[T.EOF]; RETURN END;
      ch := Text.GetChar (input, cursor);
      IF (ch # ' ') AND (ch # '\n') AND (ch # '\t') THEN EXIT END;
      INC (cursor);
    END;

    IF (ch = '(') THEN
      t := T.LParen;  name := Name[T.LParen];  INC (cursor);
    ELSIF (ch = ')') THEN
      t := T.RParen;  name := Name[T.RParen];  INC (cursor);
    ELSIF (ch = '+') THEN
      t := T.Plus;  name := Name[T.Plus];  INC (cursor);
    ELSIF (ch = '*') THEN
      t := T.Star;  name := Name[T.Star];  INC (cursor);
    ELSIF (ch = '=') THEN
      t := T.Assign;  name := Name[T.Assign];  INC (cursor);
    ELSIF (ch = ';') THEN
      t := T.Semi;  name := Name[T.Semi];  INC (cursor);
    ELSIF (ch IN IdChars) THEN
      i := 0;
      WHILE (ch IN IdChars) DO
        IF (i < LAST (buf)) THEN buf[i] := ch; INC (i) END;
        INC (cursor);
        IF (cursor >= len) THEN EXIT END;
        ch := Text.GetChar (input, cursor);
      END;
      t := T.Id;  name := Text.FromChars (SUBARRAY (buf, 0, i));
    ELSE
      t := T.Id;  name := "?\\" & Fmt.Int (ORD (ch), 8) & "?"; INC (cursor);
    END;
  END Scan;

BEGIN
END Token.

