(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jul 26 17:41:06 PDT 1994 by najork                   *)
(*       Created on Fri Jul 22 16:25:15 PDT 1994 by najork                   *)


INTERFACE CB;

EXCEPTION BadMethod(TEXT);

(* The exception "BadMethod" is raised if the client did not implement the
   callback-handling method "invoke", or if an error occurred in the
   interpreter of the embedded language (e.g. Obliq) while interpreting
   a call to "invoke". *)

END CB.
