(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Max.i3                                                *)
(* Last Modified On Mon Jul 27 22:02:26 1992 By kalsow     *)
(*      Modified On Thu Nov  2 18:19:04 1989 By muller         *)

INTERFACE Max;

IMPORT Type, CallExpr;

PROCEDURE Initialize ();

PROCEDURE TypeOf (ce: CallExpr.T): Type.T;
PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T);

END Max.
