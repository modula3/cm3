(*| Copyright (C) 1993, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Thu Jun  3 16:59:45 PDT 1993 by kalsow     *)
(*|      modified on Sun Feb 21 14:14:12 PST 1993 by jdd        *)
(*|      modified on Sat Feb  6 12:28:37 PST 1993 by mjordan    *)

(* "RTThreadInit" is a private interface. It is implemented in
    the thread package. *)

INTERFACE RTThreadInit;

PROCEDURE Init ();
(* Initialise the threads world. *)

END RTThreadInit.

