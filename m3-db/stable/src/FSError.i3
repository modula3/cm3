(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Oct 11 15:12:21 PDT 1995 by najork                   *)
(*       Created on Wed Oct 11 15:02:48 PDT 1995 by najork                   *)

INTERFACE FSError;

IMPORT OSError;

PROCEDURE FileNotFound (err: OSError.Code): BOOLEAN;
(* Returns TRUE if "err.head" says "file not found". *)

END FSError.
