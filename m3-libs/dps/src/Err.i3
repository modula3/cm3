(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 14 12:02:36 PST 1994 by kalsow                   *)

INTERFACE Err;

PROCEDURE Msg (a, b, c, d: TEXT := NIL);
(* writes the "a&b&c&d&<CRLF>" on stderr *)

END Err.
