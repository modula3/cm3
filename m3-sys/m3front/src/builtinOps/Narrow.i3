(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Narrow.i3                                             *)
(* Last Modified On Fri Aug 21 17:25:03 PDT 1992 By kalsow     *)

INTERFACE Narrow;

IMPORT Type, MSIR;

PROCEDURE Initialize ();

PROCEDURE Emit (tlhs, trhs: Type.T);
(* generate  "NARROW (s0.trhs, tlhs)"  -- Note: the generated code
   may include a procedure call. *)

PROCEDURE EmitMSIR (refVal: MSIR.Value;  tlhs, trhs: Type.T): MSIR.Value;
(* MSIR equivalent of Emit: checks refVal against tlhs at runtime,
   aborts on failure (RuntimeError.NarrowFailed = 5), returns refVal. *)

END Narrow.
