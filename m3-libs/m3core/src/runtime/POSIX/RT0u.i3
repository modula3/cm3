(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jul 14 11:29:36 PDT 1994 by kalsow     *)

(* RT0u is almost "the bottom of the world".  It contains
   variables that are shared by multiple modules of the runtime
   and/or the compiler and linker.

   If you're using this interface, you're a wizard!

   This interface and its implemenation MUST NOT import any
   interface other than RT0.
*)

UNSAFE INTERFACE RT0u;

IMPORT RT0;

(*--------------------------------------------------- global module table ---*)

VAR (*CONST*) nModules: CARDINAL := 0;  (* == # compilation units in pgm *)
(* initialized by RTHooks' main body *)

VAR (*CONST*) modules: UNTRACED REF (*ARRAY OF*) RT0.ModulePtr := NIL;
(* allocated by the linker, its actual bounds are [0..nModules-1] *)

(*----------------------------------------------------- global type table ---*)

VAR (*CONST*) nTypes: CARDINAL := 0;   (* == max allocated typecode *)
(* initialized by RTType.Init. *)

VAR (*CONST*) types: UNTRACED REF (*ARRAY OF*) RT0.TypeDefn := NIL;
(* allocated by the startup code, its actual bounds are [0..nTypes-1] *)

(*------------------------------------------------------ mutual exclusion ---*)

<*EXTERNAL RT0u__inCritical*>
VAR inCritical: INTEGER;
(* inCritical provides low-level mutual exclusion between the thread
   runtime, garbage collector and the Unix signal that triggers thread
   preemption.  If inCritical is greater than zero, thread preemption
   is disabled.  We *ASSUME* that "INC(inCritical)" and "DEC(inCritical)"
   generate code that is atomic with respect to Unix signal delivery. *)

(*------------------------------------------------------- allocator stats ---*)

VAR (*CONST*) alloc_cnts : UNTRACED REF ARRAY RT0.Typecode OF INTEGER;
(* allocated by the startup code, its actual bounds are [0..nTypes-1] *)

VAR (*CONST*) alloc_bytes: UNTRACED REF ARRAY RT0.Typecode OF INTEGER;
(* allocated by the startup code, its actual bounds are [0..nTypes-1] *)

END RT0u.

