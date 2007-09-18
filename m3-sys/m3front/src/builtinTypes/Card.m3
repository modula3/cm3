(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Card.m3                                               *)
(* Last Modified On Thu Mar 10 09:44:11 PST 1994 By kalsow     *)
(*      Modified On Thu Jul 27 17:38:00 1989 By muller         *)

MODULE Card;

IMPORT SubrangeType, Target, TInt, Tipe, Int;

PROCEDURE Initialize () =
  BEGIN
    T := SubrangeType.New (TInt.Zero, Target.Integer.max, Int.T, TRUE);
    Tipe.Define ("CARDINAL", T, TRUE);
  END Initialize;

BEGIN
END Card.
