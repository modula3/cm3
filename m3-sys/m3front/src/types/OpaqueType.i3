(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OpaqueType.i3                                         *)
(* Last Modified On Tue Jun 28 10:18:16 PDT 1994 By kalsow     *)

INTERFACE OpaqueType;

IMPORT Type, Value;

PROCEDURE New (super: Type.T;  decl: Value.T): Type.T;

PROCEDURE Is (t: Type.T): BOOLEAN;

PROCEDURE Super (t: Type.T): Type.T;

PROCEDURE IsSubtype (a, b: Type.T): BOOLEAN;

PROCEDURE UID (t: Type.T): INTEGER;
(* if t is an opaque type, return it's uid, otherwise return zero *)

END OpaqueType.
