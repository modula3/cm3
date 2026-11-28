(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Jun 20 10:51:26 PDT 1994 by kalsow     *)

(* RTHeapMap provides the routines needed by the collector
   to walk runtime values.
*)

INTERFACE RTHeapMap;

IMPORT RT0;

TYPE Visitor <: V_;  V_ = OBJECT METHODS apply (a: ADDRESS) END;
TYPE ObjectPtr = UNTRACED REF RT0.RefHeader;

PROCEDURE WalkRef (h: ObjectPtr;  v: Visitor);
(* For each traced reference at address 'x' in the object referenced by 'h',
   call 'v(x)'. *)

PROCEDURE DoWalkRef(t: RT0.TypeDefn;  a: ADDRESS;  v: Visitor);
(* For each traced reference at address 'x' in the object of type 't'
   starting at address 'a', call 'v(x)'. *)

PROCEDURE WalkMap (a, map: ADDRESS;  v: Visitor);
(* For each traced reference at address 'x' in the value at address 'a'
   whose layout is given by the garbage collector's map 'map', call
   'v(x)'. *)

PROCEDURE PrepareGlobals ();
(* Bring the map of the modules' globals up to date, allocating as needed.
   Call it before stopping the other threads: a thread stopped inside
   malloc holds malloc's lock, so the collector must not allocate or free
   while they are stopped. *)

PROCEDURE WalkGlobals (v: Visitor);
(* For each traced reference at address 'x' in a global var, call 'v(x)'.
   Allocates only if the map is out of date (see PrepareGlobals). *)

PROCEDURE WalkModuleGlobals (v: Visitor;  m: CARDINAL);
(* For each traced reference at address 'x' in a global variable of
   module 'm' (RTModule.Get(m)), call 'v(x)' *)

END RTHeapMap.

