(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: EnumElt.i3                                            *)
(* Last Modified On Fri Jun 24 09:29:39 PDT 1994 By kalsow     *)

INTERFACE EnumElt;

IMPORT M3ID, Type, Value, Target;

PROCEDURE New (name: M3ID.T;  READONLY value: Target.Int;
               parent: Type.T): Value.T;

PROCEDURE IsEqual (va, vb: Value.T): BOOLEAN;

END EnumElt.
