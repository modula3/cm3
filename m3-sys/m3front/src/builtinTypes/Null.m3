(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Null.m3                                               *)
(* Last Modified On Fri Jun 24 09:43:33 PDT 1994 By kalsow     *)
(*      Modified On Fri Feb  9 06:18:44 1990 By muller         *)

MODULE Null;

IMPORT RefType, Constant, Tipe, AddressExpr, Brand, TInt;

PROCEDURE Initialize () =
  BEGIN
    T := RefType.New (NIL, FALSE, Brand.New ("$null$"));
    Nil := AddressExpr.New (TInt.Zero);
    Tipe.Define ("NULL", T, TRUE);
    EVAL Constant.Declare ("NIL", Nil, TRUE);
  END Initialize;

BEGIN
END Null.
