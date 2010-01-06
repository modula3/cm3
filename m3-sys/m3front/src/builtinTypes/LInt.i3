(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: LInt.i3                                               *)
(* Last Modified On Thu Jul 27 17:31:44 1989 By muller         *)
(*      Modified On Mon Jul 17 17:02:58 1989 By kalsow         *)

INTERFACE LInt;

IMPORT Type, CG;

VAR
  T: Type.T;
  Signed, Unsigned: CG.Type;
  Size: INTEGER;

PROCEDURE Initialize ();

END LInt.
