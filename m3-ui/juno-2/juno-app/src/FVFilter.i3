(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  2 10:48:11 PDT 1994 by heydon                   *)
<* PRAGMA LL *>

INTERFACE FVFilter;

(* Procedures for controlling the reactivity of FormsVBT "Filter"
   components. *) 

IMPORT FormsVBT;

TYPE
  CursorKind = { Passive, Working };

PROCEDURE MakePassive(fv: FormsVBT.T; nm: TEXT; cursor := CursorKind.Passive);
(* Make the filter with name "nm" in the form "fv" passive, displaying the
   cursor determined by "cursor". *)

PROCEDURE MakeActive(fv: FormsVBT.T; nm: TEXT);
(* Make the filter with name "nm" in the form "fv" active. *)

END FVFilter.
