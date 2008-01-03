(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SubrangeType.i3                                       *)
(* Last Modified On Fri Jun 24 09:28:42 PDT 1994 By kalsow     *)
(*      Modified On Sat Jan 20 01:13:51 1990 By muller         *)

INTERFACE SubrangeType;

IMPORT Type, Target;

PROCEDURE Parse (): Type.T;

PROCEDURE New (READONLY min, max: Target.Int;  base: Type.T;
               builtin: BOOLEAN): Type.T;

PROCEDURE Split (t: Type.T;  VAR min, max: Target.Int): BOOLEAN;

PROCEDURE Base (t: Type.T): Type.T;

END SubrangeType.
