(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Wed Dec 15 15:12:45 PST 1993 by mcjones    *)
(*      modified on Wed Mar  4 08:31:11 1992 by kalsow         *)
(*      modified on Sat Sep 15 02:51:50 1990 by muller         *)
(*      modified on Mon Sep  5 17:21:33 1988 by stolfi         *)
(*      modified on Fri Jun 10 14:20:12 1988 by glassman       *)
(*      modified On Tue Sep 10 15:28:14 1985 By rovner         *)

(* This interface provides access to the command line arguments given
   to a process when it is started (see "Process.Create").
   \index{parameters of a process}
   \index{process!parameters}
*)

INTERFACE Params;

VAR (*CONST*) Count: CARDINAL;
(* Parameters are indexed from "0" (the command name) to "Count-1". *)

PROCEDURE Get(n: CARDINAL): TEXT;
(* Return the parameter with index "n".  It is a checked runtime error
   if "n >= Count". *)

END Params.

(* Parameters that begin with the characters "@M3" are reserved for
   use by the SRC Modula-3 runtime.  They are not included in the
   value of "Count" or in the sequence indexed by "Get". *)
