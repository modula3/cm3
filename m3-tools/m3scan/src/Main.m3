(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Main.m3                                               *)
(* Last modified on Fri May 28 10:32:40 PDT 1993 by kalsow     *)

MODULE Main;

IMPORT Stdio, Wr, Params, Text, Convert;
IMPORT M3Token, M3Scanner;

TYPE TK = M3Token.T;

PROCEDURE DoIt () =
  VAR parm   : TEXT;
  VAR output := TRUE;
  VAR cmt    := TRUE;
  VAR prag   := FALSE;
  VAR scan   : M3Scanner.T;
  BEGIN
    FOR i := 1 TO Params.Count-1 DO
      parm := Params.Get (i);
      IF    Text.Equal (parm, "-comments") THEN cmt := FALSE;
      ELSIF Text.Equal (parm, "-pragmas")  THEN prag := TRUE;
      ELSIF Text.Equal (parm, "-quiet")    THEN output := FALSE;
      END;
    END;

    scan := NEW (M3Scanner.Default).init (Stdio.stdin, cmt, prag);
    IF (scan.token = TK.Comment) THEN scan.next () END;

    LOOP
      IF (output) THEN PrintToken (scan); END;
      IF (scan.token = TK.EOF) THEN EXIT END;
      scan.next ();
    END;
  END DoIt;

PROCEDURE PrintToken (t: M3Scanner.T) =
  VAR tag := M3Token.name [t.token];
  BEGIN
    Out  (tag); Pad (11 - Text.Length (tag));
    OutI (t.offset, 6);
    OutI (t.line, 6);
    OutI (t.column, 6);
    Out  ("  |");
    OutS (SUBARRAY (t.buffer^, t.start, t.length));
    Out  ("|\n");
    IF (t.token = TK.Error) THEN
      Out ("  !!!  ");
      Out (t.msg);
      Out("  !!!\n");
    END;
  END PrintToken;

PROCEDURE Out (t: TEXT) =
  <*FATAL ANY*>
  BEGIN
    Wr.PutText (Stdio.stdout, t);
  END Out;

PROCEDURE OutI (i, width: INTEGER) =
  <*FATAL ANY*>
  VAR buf: ARRAY [0..30] OF CHAR;  len := Convert.FromInt (buf, i);
  BEGIN
    Pad (width - len);
    Wr.PutString (Stdio.stdout, SUBARRAY (buf, 0, len));
  END OutI;

PROCEDURE OutS (READONLY x: ARRAY OF CHAR) =
  <*FATAL ANY*>
  BEGIN
    IF NUMBER (x) > 35 THEN
      Wr.PutString (Stdio.stdout, SUBARRAY (x, 0, 30));
      Wr.PutText   (Stdio.stdout, " ... ");
      Wr.PutString (Stdio.stdout, SUBARRAY (x, NUMBER (x)-5, 5));
    ELSE
      Wr.PutString (Stdio.stdout, x);
    END;
  END OutS;

PROCEDURE Pad (n: INTEGER) =
  <*FATAL ANY*>
  BEGIN
    WHILE (n > 0) DO Wr.PutChar (Stdio.stdout, ' ');  DEC (n) END;
  END Pad;

BEGIN
  DoIt ();
END Main.
