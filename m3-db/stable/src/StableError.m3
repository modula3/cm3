(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:11:27 PST 1995 by kalsow     *)
(*      modified on Mon Sep 19 16:23:10 PDT 1994 by weich      *)

(* Stable object exceptions *)

MODULE StableError;

IMPORT RdUtils, Stdio, Wr, Thread, Process;

PROCEDURE ToText(code: Code): TEXT =
(* "RdUtils.FailureText" does the same conversion for Rd/Wr Excpetions. We can use it here: *)
  BEGIN
    RETURN RdUtils.FailureText(code);
  END ToText;

PROCEDURE Halt (msg: TEXT) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    Wr.PutText(Stdio.stderr, "****\n");
    Wr.PutText(Stdio.stderr, "**** fatal stable object error\n");
    Wr.PutText(Stdio.stderr, "****\n");
    Wr.PutText(Stdio.stderr, "****       " & msg & "\n");
    Wr.PutText(Stdio.stderr, "****\n\n\nProgram aborted\n");
    Process.Exit(1);
  END Halt;

BEGIN
END StableError.

