(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Nov 11 13:12:43 PST 1994 by kalsow     *)

(* RTLinker defines the values initialized by the linker and
   startup code.
*)

UNSAFE INTERFACE RTLinker;

TYPE
  LinkInfo = UNTRACED REF RECORD
    (* global module table *)
    n_modules  : INTEGER;
    modules    : ADDRESS; (* REF ARRAY [0..n_modules-1] OF RT0.ModulePtr *)

    (* external environment *)
    argc       : INTEGER;
    argv       : ADDRESS;
    envp       : ADDRESS;
    instance   : ADDRESS;  (* Windows "instance" handle *)

    (* initial thread bounds *)
    bottom_of_stack : ADDRESS;
    top_of_stack    : ADDRESS;
  END;

VAR info: LinkInfo := NIL;

PROCEDURE RunProgram (x: LinkInfo);
(* Called by the linker-generated main program to setup and
   run the Modula-3 program.  Initially, "x.modules^" is
   a list of RT0.Binder procedures which "RunProgram" calls
   to reinitialized the table. *)

END RTLinker.

