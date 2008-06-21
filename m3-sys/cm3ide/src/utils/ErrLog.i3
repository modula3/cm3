(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May  6 10:37:59 PDT 1993 by kalsow                   *)

INTERFACE ErrLog;
IMPORT Wr;

PROCEDURE Note (msg: TEXT);

PROCEDURE Msg (a, b, c, d: TEXT := NIL);

PROCEDURE Redirect (wr: Wr.T);
PROCEDURE CancelRedirect ();

VAR (* READONLY, LL = log_mu *)
  log_mu   : MUTEX;
  log      : ARRAY [0..499] OF TEXT;
  log_head : INTEGER := 0;
  log_len  : INTEGER := 0;
  (* the last few logged messages, kept in a circular queue. *)

END ErrLog.
