(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 17 17:34:26 PDT 1994 by mhb                      *)
(*      modified on Tue Jun 16 16:46:28 PDT 1992 by muller                   *)

MODULE PrintUtil;

IMPORT Fmt, Stdio, Text, Thread, Wr;

<* FATAL Wr.Failure, Thread.Alerted *>

PROCEDURE PrintRealPair(text: Text.T; x: REAL; y: REAL) =
BEGIN
     Wr.PutText(Stdio.stdout, text);
     Wr.PutText(Stdio.stdout, "(");
     Wr.PutText(Stdio.stdout, Fmt.Real(x));
     Wr.PutText(Stdio.stdout, ",");
     Wr.PutText(Stdio.stdout, Fmt.Real(y));
     Wr.PutText(Stdio.stdout, ")");
END PrintRealPair;

PROCEDURE PrintReal(text: Text.T; r: REAL) =
BEGIN
     Wr.PutText(Stdio.stdout, text);
     Wr.PutText(Stdio.stdout, Fmt.Real(r));
END PrintReal;

PROCEDURE PrintIntPair(text: Text.T; i: INTEGER; j: INTEGER) =
BEGIN
     Wr.PutText(Stdio.stdout, text);
     Wr.PutText(Stdio.stdout, "(");
     Wr.PutText(Stdio.stdout, Fmt.Int(i));
     Wr.PutText(Stdio.stdout, ",");
     Wr.PutText(Stdio.stdout, Fmt.Int(j));
     Wr.PutText(Stdio.stdout, ")");
END PrintIntPair;

PROCEDURE PrintInt(text: Text.T; i: INTEGER) =
BEGIN
     Wr.PutText(Stdio.stdout, text);
     Wr.PutText(Stdio.stdout, Fmt.Int(i));
END PrintInt;

PROCEDURE NewLine() =
  BEGIN
    Wr.PutText(Stdio.stdout, "\n");
  END NewLine;

PROCEDURE PrintText(text: Text.T) =
  BEGIN
    Wr.PutText(Stdio.stdout, text);
  END PrintText;

BEGIN
END PrintUtil.
