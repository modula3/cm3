(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE Charr;

IMPORT M3, M3ID, EnumType, Tipe, Scope;

PROCEDURE Initialize () =
  VAR elts: Scope.T;  cs := M3.OuterCheckState;
  BEGIN
    elts := Scope.PushNew (FALSE, M3ID.Add ("CHAR"));
    T := EnumType.New (256, elts);
    Scope.PopNew ();
    Scope.TypeCheck (elts, cs);
    Tipe.Define ("CHAR", T, TRUE);
  END Initialize;

BEGIN
END Charr.
