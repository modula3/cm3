(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordPlus.i3                                           *)
(* Last Modified On Thu Aug 20 15:15:16 PDT 1992 By kalsow     *)

INTERFACE WordPlus;

IMPORT Expr, Target;

PROCEDURE GetArgs (args: Expr.List;  VAR i0, i1: Target.Int): BOOLEAN;

PROCEDURE Initialize ();

END WordPlus.
