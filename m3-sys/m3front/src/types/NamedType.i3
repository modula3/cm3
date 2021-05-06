(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: NamedType.i3                                          *)
(* Last Modified On Fri Jun 24 09:36:36 PDT 1994 By kalsow     *)
(*      Modified On Sat Aug 18 00:03:50 1990 By muller         *)

INTERFACE NamedType;

IMPORT M3, M3ID, Type, Value;

PROCEDURE Parse (): Type.T;

PROCEDURE New (t: Type.T): Type.T;
PROCEDURE Create (m, n: M3ID.T): Type.T;

PROCEDURE Strip (t: Type.T): Type.T;
PROCEDURE Split (t: Type.T;  VAR name: M3.QID): BOOLEAN;
PROCEDURE SplitV (t: Type.T;  VAR v: Value.T): BOOLEAN;

END NamedType.
