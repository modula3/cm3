(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr  8 08:38:20 PDT 1994 by kalsow                   *)

INTERFACE MarkUp;

IMPORT Rd, Wr, Thread;

PROCEDURE Annotate (rd: Rd.T;  wr: Wr.T;  path: TEXT)
  RAISES {Thread.Alerted, Wr.Failure, Rd.Failure};
(* copy the Modula-3 source in "rd" to "wr" adding HTML annotations. *)

END MarkUp.
