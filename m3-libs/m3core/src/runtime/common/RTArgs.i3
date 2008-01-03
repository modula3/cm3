(*| Copyright (C) 1990, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Thu Jun  3 13:33:57 PDT 1993 by kalsow     *)
(*|      modified on Wed Mar 17 10:11:11 PST 1993 by mjordan    *)
(*|      modified on Sun Feb 21 14:20:50 PST 1993 by jdd        *)

INTERFACE RTArgs;

(* This interface exports a procedural interface to the command line
   arguments/environment variable store, that is independent of
   the underlying representation. *)

PROCEDURE ArgC (): CARDINAL;
(* Returns the number of command line arguments, less those reserved by the
   M3 runtime. *)

PROCEDURE GetArg (n: CARDINAL): TEXT;
(* Returns the "n"th argument.  It is a checked runtime error if
   "n >= ArgC ()". *)

PROCEDURE EnvC (): CARDINAL;
(* Returns the count of environment variables. *)

PROCEDURE GetEnv (n: CARDINAL): TEXT;
(* Returns the "n"th environment variable.  It is a checked runtime error if
   "n >= EnvC ()". *)

END RTArgs.
