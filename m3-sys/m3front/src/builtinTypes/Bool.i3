(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Bool.i3                                               *)
(* Last Modified On Fri Jun 30 08:29:47 1989 By kalsow         *)

INTERFACE Bool;

IMPORT Type, Value, Expr;

VAR
  T     : Type.T;
  False : Value.T;
  True  : Value.T;
  Map   : ARRAY BOOLEAN OF Expr.T;

PROCEDURE Initialize ();

END Bool.
