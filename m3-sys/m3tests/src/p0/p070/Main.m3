(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Fri Jun 18 17:38:50 PDT 1993 by kalsow    *)

UNSAFE MODULE Main;

IMPORT Fingerprint, RTProcedure, RTProcedureSRC, Wr, Stdio, Fmt, Test;
<*FATAL ANY*>

TYPE Int32 = BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff];
TYPE FP = ARRAY [0..1] OF Int32;

VAR n := RTProcedureSRC.NumProcedures ();

PROCEDURE Toto () = BEGIN END Toto;

VAR fp: Fingerprint.T;
    proc: RTProcedure.Proc;
    xfp: FP;

BEGIN
  Wr.PutText (Stdio.stderr, Fmt.Int (n) & " procedures\n");

  proc := LOOPHOLE (Toto, RTProcedure.Proc);
  fp := RTProcedure.ToFingerprint (proc);
  xfp := LOOPHOLE (fp, FP);
  Wr.PutText (Stdio.stderr, 
	      "Toto fingerprint = {" & Fmt.Int (xfp[0]) & ", "
                 & Fmt.Int (xfp[1]) & "}\n");

  proc := RTProcedure.FromFingerprint (fp);
  Test.checkB (proc = LOOPHOLE (Toto, ADDRESS), TRUE);

  Test.done ();
END Main.
