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

<* EXTERNAL "RTLinker__GetEnvironmentStrings" *>
PROCEDURE GetEnvironmentStrings (EnvFromMain: ADDRESS): ADDRESS;

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

<*EXTERNAL RTLinker__PrintString*> PROCEDURE PrintString (a : ADDRESS);
<*EXTERNAL RTLinker__PrintText*> PROCEDURE PrintText(a : TEXT);
<*EXTERNAL RTLinker__PrintInt*> PROCEDURE PrintInt(a : INTEGER);
<*EXTERNAL RTLinker__PrintModule*> PROCEDURE PrintModule(a : ADDRESS);
(* TYPE Trace_t = { None, M3, C };
<*EXTERNAL RTLinker__traceInit*> VAR traceInit : Trace_t; *)

END RTLinker.

