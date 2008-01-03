(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: First.i3                                              *)
(* Last Modified On Mon Jul 27 20:56:19 1992 By kalsow         *)
(*      Modified On Fri Dec 21 01:17:49 1990 By muller         *)


INTERFACE First;

IMPORT Type, CallExpr;


PROCEDURE Initialize ();

PROCEDURE TypeOf  (ce: CallExpr.T): Type.T;
PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T);

END First.
