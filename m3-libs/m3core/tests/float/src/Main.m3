(* Copyright (C) 1992, Xerox                                                 *)
(* All rights reserved.                                                      *)

(* Last modified on Mon Oct 12 14:23:00 PDT 1992 by muller                   *)
(*      modified on Fri Feb 28 21:23:10 PST 1992 by stolfi                   *)
(*      modified on Wed Sep 25 00:33:01 1991 by goldberg@xerox.parc.com      *)

UNSAFE MODULE Main;

IMPORT  RealTest, LongTest;
IMPORT Wr, Stdio, Fmt;

<* FATAL ANY *>

BEGIN
  Wr.PutText (Stdio.stderr, "---------------------------- REAL ---\n");
  RealTest.Test();
  (*Wr.PutText (Stdio.stderr, "---------------------------- threads ---\n");
  RealTest.TestThreads();
  Wr.PutText (Stdio.stderr, "---------------------------- LONGREAL ---\n");
  LongTest.Test ();*)
END Main.
