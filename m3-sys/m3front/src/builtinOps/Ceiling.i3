(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Ceiling.i3                                            *)
(* Last Modified On Mon Nov 22 13:59:26 PST 1993 By kalsow         *)
(*      Modified On Thu Jul 27 17:14:07 1989 By muller         *)

INTERFACE Ceiling;

IMPORT CallExpr, Expr;

PROCEDURE Initialize ();

PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T;  VAR cs: Expr.CheckState);

END Ceiling.
