(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Bool.m3                                               *)
(* Last Modified On Fri Jun 24 09:42:58 PDT 1994 By kalsow     *)
(*      Modified On Mon Nov 27 15:21:12 1989 By muller         *)

MODULE Bool;

IMPORT Expr, EnumType, EnumElt, Scope, M3;
IMPORT M3ID, Tipe, Constant, EnumExpr, TInt;

PROCEDURE Initialize () =
  VAR elts: Scope.T;  cs := M3.OuterCheckState;  true, false: Expr.T;
  BEGIN
    elts := Scope.PushNew (FALSE, M3ID.Add ("BOOLEAN"));
    T := EnumType.New (2, elts);

    False := EnumElt.New (M3ID.Add ("FALSE"), TInt.Zero, T);
    Scope.Insert (False);

    True := EnumElt.New (M3ID.Add ("TRUE"), TInt.One, T);
    Scope.Insert (True);

    Scope.PopNew ();
    Scope.TypeCheck (elts, cs);
    Tipe.Define ("BOOLEAN", T, TRUE);

    false := EnumExpr.New (T, TInt.Zero);
    true  := EnumExpr.New (T, TInt.One);

    EVAL Constant.Declare ("FALSE", false, TRUE);
    EVAL Constant.Declare ("TRUE", true, TRUE);

    Map[FALSE] := false;
    Map[TRUE] := true;
  END Initialize;

BEGIN
END Bool.
