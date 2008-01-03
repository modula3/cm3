(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Jan 17 16:16:50 PST 1993 by heydon                   *)
(*      modified on Fri Aug  7 21:51:54 PDT 1992 by myers                    *)

INTERFACE JunoHandleLexErr;

IMPORT JunoLex, Rd, Wr;

PROCEDURE HandleLexErr(
    err: JunoLex.ErrorRec;
    rd: Rd.T;
    wr: Wr.T;
    VAR start, (*OUT*) finish: INTEGER);
(* Assumes that it has received a reader "rd" in which "JunoParse.P" has just
   encountered a lexical error "err".  It produces output on the writer "wr"
   which represents this error, and returns writer indices "start" and
   "finish" that indicate the boundaries of the error. *)

END JunoHandleLexErr.
