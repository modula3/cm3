(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Null.i3                                               *)
(* Last Modified On Mon Apr 25 08:20:04 PDT 1994 By kalsow     *)

INTERFACE Null;

IMPORT Type, Expr;

VAR
  T   : Type.T;
  Nil : Expr.T;

PROCEDURE Initialize ();

END Null.
