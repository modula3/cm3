(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SetType.i3                                            *)
(* Last Modified On Fri Jun 24 09:28:30 PDT 1994 By kalsow     *)

INTERFACE SetType;

IMPORT Type;

PROCEDURE Parse (): Type.T;

PROCEDURE Split (t: Type.T;  VAR range: Type.T): BOOLEAN;

PROCEDURE IsSmallSet (t: Type.T): BOOLEAN;
(* Fits within a word.  Handled as a scalar. *)
(* This includes a packed small set type, regardless of its use. *)

END SetType.
