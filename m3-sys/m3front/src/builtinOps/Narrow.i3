(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Narrow.i3                                             *)
(* Last Modified On Fri Aug 21 17:25:03 PDT 1992 By kalsow     *)

INTERFACE Narrow;

IMPORT Type;

PROCEDURE Initialize ();

PROCEDURE Emit (tlhs, trhs: Type.T);
(* generate  "NARROW (s0.trhs, tlhs)"  -- Note: the generated code
   may include a procedure call. *)

END Narrow.
