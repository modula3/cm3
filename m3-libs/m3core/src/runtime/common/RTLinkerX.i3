(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* Hooks to various modules' init code... *)

UNSAFE INTERFACE RTLinkerX;

IMPORT RT0;

<*EXTERNAL*> PROCEDURE RTLinker_M3      (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTLinker_I3      (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTSignal_I3      (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTParams_I3      (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapRep_I3     (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTThreadInit_I3  (mode: INTEGER): RT0.ModulePtr;
<*EXTERNAL*> PROCEDURE RTHeapInfo_I3    (mode: INTEGER): RT0.ModulePtr;

END RTLinkerX.

