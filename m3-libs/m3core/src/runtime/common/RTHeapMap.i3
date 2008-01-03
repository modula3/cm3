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

PROCEDURE WalkGlobals (v: Visitor);
(* For each traced reference at address 'x' in a global var, call 'v(x)'. *)

PROCEDURE WalkModuleGlobals (v: Visitor;  m: CARDINAL);
(* For each traced reference at address 'x' in a global variable of
   module 'm' (RTModule.Get(m)), call 'v(x)' *)
  
END RTHeapMap.

