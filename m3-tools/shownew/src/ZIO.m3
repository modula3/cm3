(* Copyright (C) 1992, Digital Equipment Corporation       *)
(* All rights reserved.                                    *)
(* See the file COPYRIGHT for a full description.          *)
(*                                                         *)
(* Last modified on Wed Feb 16 09:25:13 PST 1994 by kalsow *)

UNSAFE MODULE ZIO;

IMPORT TextF, Rd, Thread, Stdio, Wr, Fmt;

PROCEDURE GetInt (): INTEGER RAISES {Rd.EndOfFile} =
  VAR
    i: INTEGER := 0;
    p: UNTRACED REF ARRAY [1..BYTESIZE(i)] OF CHAR := ADR (i);
    n: INTEGER;
  BEGIN
    TRY
      n := Rd.GetSub (Stdio.stdin, p^);
    EXCEPT Rd.Failure, Thread.Alerted =>
      n := 0;
    END;
    IF (n # BYTESIZE (i)) THEN
      RAISE Rd.EndOfFile;
    END;
    RETURN i;
  END GetInt;

VAR xx: INTEGER := 10;
PROCEDURE PutI (i: INTEGER) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    Wr.PutChar (Stdio.stderr, ' ');
    Wr.PutText (Stdio.stderr, Fmt.Int (i));
    DEC (xx);
    IF (xx <= 0) THEN Wr.PutChar (Stdio.stderr, '\n');  xx := 10; END;
  END PutI;

PROCEDURE PutT (t: TEXT) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    Wr.PutText (Stdio.stderr, t);
  END PutT;

PROCEDURE GetText (): TEXT =
  <*FATAL Rd.EndOfFile *>
  VAR
    len := GetInt ();
    txt := TextF.New (len);
  BEGIN
    TRY
      EVAL Rd.GetSub (Stdio.stdin, SUBARRAY (txt^, 0, len));
    EXCEPT Rd.Failure, Thread.Alerted =>
      txt := "**FAILED READ***";
    END;
    RETURN txt;
  END GetText;

BEGIN
END ZIO.

