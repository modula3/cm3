(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1992, Xerox                                                 *)
(* All rights reserved.                                                      *)

(* Last modified on Thu Feb 27 10:40:55 PST 1992 by kalsow                   *)
(*      modified on Mon Feb 24 09:54:47 PST 1992 by muller                   *)
(*      modified on Wed Sep 25 00:33:01 1991 by goldberg@xerox.parc.com      *)

UNSAFE MODULE Main;

IMPORT  FloatTest, LongTest;
IMPORT Wr, Stdio;

<* FATAL ANY *>

BEGIN
  Wr.PutText (Stdio.stderr, "---------------------------- REAL ---\n");
  FloatTest.Test();
  Wr.PutText (Stdio.stderr, "---------------------------- threads ---\n");
  FloatTest.TestThreads();
  Wr.PutText (Stdio.stderr, "---------------------------- LONGREAL ---\n");
  LongTest.Test ();
END Main.
