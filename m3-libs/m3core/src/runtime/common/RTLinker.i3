(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Nov 11 13:12:43 PST 1994 by kalsow     *)

(* RTLinker defines the values initialized by the linker and
   startup code.
*)

UNSAFE INTERFACE RTLinker;

IMPORT RT0;

VAR (* external environment *)
  argc       : INTEGER;
  argv       : ADDRESS;
  envp       : ADDRESS;
  instance   : ADDRESS;  (* Windows "instance" handle *)

VAR (* READONLY *)
  (* Does compiled code include GC checks? *)
  generational := TRUE;
  incremental  := TRUE;

PROCEDURE InitRuntime (argc: INTEGER;  argv, envp, instance: ADDRESS);
(* Initializes the runtime and the environment globals.  It must be
   called once before any other Modula-3 code is executed.  *)

PROCEDURE AddUnit (b: RT0.Binder);
(* Adds "b(0)" and any units it imports to the set of linked
   and initialized compilation units. *)

PROCEDURE AddUnitImports (b: RT0.Binder);
(* Adds any units "b(0)" imports but not "b(0)" itself to the set of linked
   and initialized compilation units. *)

PROCEDURE RunMainBody (m: RT0.ModulePtr);
(* Invokes "m"s main body if it hasn't already been done.
   Note: this procedure is only exported so that stack walkers
   can know when to quit looking at frames. *)

END RTLinker.

