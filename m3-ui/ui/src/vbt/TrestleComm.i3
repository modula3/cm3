(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:58:28 PST 1992 by muller   *)
(*      modified on Wed Aug 28 22:01:23 PDT 1991 by gnelson  *)
<*PRAGMA LL*>

INTERFACE TrestleComm;

EXCEPTION Failure;
(* Raised when communication to the window server fails. *)

END TrestleComm.
