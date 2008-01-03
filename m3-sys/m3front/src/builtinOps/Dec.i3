(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Dec.i3                                                *)
(* Last Modified On Mon Nov 22 13:34:22 PST 1993 By kalsow     *)
(*      Modified On Thu Jul 27 17:14:05 1989 By muller         *)

INTERFACE Dec;

IMPORT CallExpr, Expr;

PROCEDURE Initialize ();

PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T;  VAR cs: Expr.CheckState);

END Dec.
