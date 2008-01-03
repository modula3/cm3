(*| Copyright (C) 1990, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)

(*| Last modified on Thu Jun  3 16:19:31 PDT 1993 by kalsow     *)
(*|      modified on Sun Feb 21 14:21:24 PST 1993 by jdd        *)

(* "RTParams" is a private interface. *)

INTERFACE RTParams;

(* This interface provides access to the "runtime" arguments from the
   command line.  Command line arguments that begin "@M3" are considered
   "runtime" arguments.  They are removed from the usual (argc, argv) list
   of arguments very early in initialization.  A runtime argument has the
   form "@M3x" or "@M3x=y".  "x" is the "name" of the argument, "y" is its
   "value", and "x=y" is the "full" argument.  If "=y" is missing, "" is
   the argument's value. *)

VAR (*READONLY*) NumParameters: CARDINAL;
(* Parameters are indexed from "0" to "NumParameters-1". *)

PROCEDURE Nth (n: INTEGER): TEXT;
(* returns the "n"-th "full" argument. If "n" is not in "[0..NumParameters-1]",
   "NIL" is returned. *)

PROCEDURE IsPresent (n: TEXT): BOOLEAN;
(* returns "TRUE" iff "n" is the name of a defined runtime argument. *)

PROCEDURE Value (n: TEXT): TEXT;
(* returns the value of the runtime argument with name "n".  If "n" is not
   the name of a runtime argument, NIL is returned.  If "n" is the name of
   an argument that was specified as "@M3n", the empty string "" is
   returned. *)

PROCEDURE RawValue (n: TEXT): ADDRESS;
(* returns the C "char*" that corresponds to the value. *)

PROCEDURE Init ();
(* called by the runtime initialization code *)

END RTParams.

