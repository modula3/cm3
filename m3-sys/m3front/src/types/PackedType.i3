(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: PackedType.i3                                         *)
(* Last Modified On Fri Jun 24 09:26:58 PDT 1994 By kalsow     *)

INTERFACE PackedType;

IMPORT Type;

PROCEDURE Parse (): Type.T;

PROCEDURE New (size: INTEGER;  base: Type.T): Type.T;

PROCEDURE Is (t:Type.T): BOOLEAN;

PROCEDURE Split (t: Type.T;  VAR size: INTEGER;  VAR base: Type.T);

PROCEDURE Base (t: Type.T): Type.T;

END PackedType.
