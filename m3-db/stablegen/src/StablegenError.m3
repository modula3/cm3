(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created by Carsten Weich                                    *)
(* Last modified on Fri Sep 23 15:25:36 PDT 1994 by weich      *)

MODULE StablegenError;

IMPORT Wr, Thread, Stdio, Process;

<*FATAL Thread.Alerted, Wr.Failure*>

PROCEDURE Warning (msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "stablegen (warning): ");
    Wr.PutText(Stdio.stderr, msg&"\n");
    Wr.Flush(Stdio.stderr);
  END Warning;

PROCEDURE Failure (msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "stablegen error: ");
    Wr.PutText(Stdio.stderr, msg&"\n");
    Wr.Flush(Stdio.stderr);
  END Failure;

PROCEDURE Fatal (msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "stablegen (warning): ");
    Wr.PutText(Stdio.stderr, msg&"\n");
    Wr.PutText(Stdio.stderr, "stablegen: aborted\n ");
    Process.Exit(1);
  END Fatal;
BEGIN
END StablegenError.
