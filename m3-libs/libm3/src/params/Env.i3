(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Wed Dec 15 15:12:34 PST 1993 by mcjones    *)
(*      modified on Wed Mar  4 08:31:11 1992 by kalsow         *)
(*      modified on Sat Sep 15 02:51:50 1990 by muller         *)
(*      modified on Mon Sep  5 17:21:33 1988 by stolfi         *)
(*      modified on Fri Jun 10 14:20:12 1988 by glassman       *)
(*      modified On Tue Sep 10 15:28:14 1985 By rovner         *)

(* This interface provides access to the environment variables given
   to a process when it is started (see "Process.Create").
   \index{environment variables}
   \index{process!environment variables}
*)

INTERFACE Env;

PROCEDURE Get(nm: TEXT): TEXT;
(* Return the value of the environment variable whose name is equal to
   "nm", or "NIL" if there is no such variable. *)

VAR (*CONST*) Count: CARDINAL;
(* Environment variables are indexed from "0" to "Count-1". *)

PROCEDURE GetNth(n: CARDINAL; VAR (*OUT*) nm, val: TEXT);
(* Set "nm" and "val" to the name and value of the environment
   variable with index "n".  It is a checked runtime error if "n >=
   Count".  *)

END Env.
