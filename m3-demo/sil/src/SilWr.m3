(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 11:02:30 PST 1994 by kalsow    *)

MODULE SilWr;

IMPORT FileWr, Wr, Fmt;
<*FATAL ANY*>

REVEAL
  T = Tx BRANDED "SilWr" OBJECT
    wr  : Wr.T;
  OVERRIDES
    putInt  := PutInt;
    putText := PutText;
    putStr  := PutStr;
    endLine := EndLine;
  END;

VAR next_gen := 1;

PROCEDURE Open (filename: TEXT): T =
  VAR t := NEW (T);
  BEGIN
    t.generation := next_gen;  INC (next_gen);
    t.wr := FileWr.Open (filename);
    RETURN t;
  END Open;

PROCEDURE Close (t: T) =
  BEGIN
    Wr.Close (t.wr);
    t.wr := NIL;
  END Close;

PROCEDURE PutInt (t: T;  i: INTEGER) =
  BEGIN
    Wr.PutText (t.wr, Fmt.Int (i));
    Wr.PutChar (t.wr, ' ');
  END PutInt;

PROCEDURE PutText (t: T;  txt: TEXT) =
  BEGIN
    Wr.PutText (t.wr, txt);
  END PutText;

PROCEDURE PutStr (t: T;  txt: TEXT) =
  BEGIN
    Wr.PutChar (t.wr, '\"');
    Wr.PutText (t.wr, txt);
    Wr.PutText (t.wr, "\" ");
  END PutStr;

PROCEDURE EndLine (t: T) =
  BEGIN
    Wr.PutText (t.wr, Wr.EOL);
  END EndLine;

BEGIN
END SilWr.
