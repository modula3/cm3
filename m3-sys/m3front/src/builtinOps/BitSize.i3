(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: BitSize.i3                                            *)
(* Last Modified On Mon Nov 22 13:39:27 PST 1993 By kalsow     *)
(*      Modified On Thu Jul 27 17:14:08 1989 By muller         *)

INTERFACE BitSize;

IMPORT Expr, CallExpr;

PROCEDURE Initialize ();

PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T;  VAR cs: Expr.CheckState);

PROCEDURE DoPrep (e: Expr.T);
PROCEDURE DoCompile (e: Expr.T;  unit: INTEGER);

PROCEDURE DoFold (e: Expr.T;  unit: INTEGER): Expr.T;

END BitSize.
