(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Tipe.i3                                               *)
(* Last Modified On Tue Dec 20 15:09:17 PST 1994 By kalsow     *)
(*      Modified On Fri Feb 23 03:41:39 1990 By muller         *)

INTERFACE Tipe;

IMPORT Type, Decl;

PROCEDURE Parse (READONLY att: Decl.Attributes);

PROCEDURE Define (name: TEXT;  t: Type.T;  reserved: BOOLEAN);

PROCEDURE DefineOpaque (name: TEXT;  super: Type.T): Type.T;

END Tipe.
