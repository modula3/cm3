(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Aug  6 08:19:58 PDT 1993 by kalsow     *)
(*      modified on Tue Sep 25 00:38:09 1990 by muller         *)

(* This interface provides a safe interface to the runtime
   table of modules. *)

INTERFACE RTModule;

IMPORT RT0;

PROCEDURE Count (): CARDINAL;
(* returns the number of registered modules. They are indexed [0..Count-1]. *)

PROCEDURE Get (m: CARDINAL): RT0.ModulePtr;
(* returns a pointer to the module information for module 'm'. *)

END RTModule.
