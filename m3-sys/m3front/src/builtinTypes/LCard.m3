(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: LCard.m3                                              *)
(* Last Modified On Thu Mar 10 09:44:11 PST 1994 By kalsow     *)
(*      Modified On Thu Jul 27 17:38:00 1989 By muller         *)

MODULE LCard;

IMPORT SubrangeType, Target, TInt, Tipe, LInt;

PROCEDURE Initialize () =
  BEGIN
    T := SubrangeType.New (TInt.Zero, Target.Longint.max, LInt.T, TRUE);
    Tipe.Define ("LONGCARD", T, TRUE);
  END Initialize;

BEGIN
END LCard.
