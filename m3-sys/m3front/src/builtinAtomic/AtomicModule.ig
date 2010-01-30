(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AtomicModule.ig                                       *)
(* Last Modified On Tue Mar  2 09:01:49 PST 1993 By kalsow     *)

GENERIC INTERFACE AtomicModule ();

IMPORT Module, Type, Expr;

VAR M: Module.T;

VAR T: Type.T;

VAR Order: Type.T;

VAR Sequential: Expr.T;

PROCEDURE Initialize (name: TEXT);

END AtomicModule.
