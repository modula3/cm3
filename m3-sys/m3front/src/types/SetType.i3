(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SetType.i3                                            *)
(* Last Modified On Fri Jun 24 09:28:30 PDT 1994 By kalsow     *)

INTERFACE SetType;

IMPORT Type;

PROCEDURE Parse (): Type.T;

PROCEDURE Split (t: Type.T;  VAR range: Type.T): BOOLEAN;

END SetType.
