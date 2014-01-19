(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE WCharr;

IMPORT M3, M3ID, EnumType, Tipe, Scope;

PROCEDURE Initialize () =
  VAR elts: Scope.T;  cs := M3.OuterCheckState;
  VAR NUMBER_WIDECHAR: INTEGER; 
  BEGIN
    elts := Scope.PushNew (FALSE, M3ID.Add ("WIDECHAR"));
    IF IsUnicode  
    THEN NUMBER_WIDECHAR := 16_110000
    ELSE NUMBER_WIDECHAR := 16_10000
    END; 
    T := EnumType.New (NUMBER_WIDECHAR, elts);
(* Widechar Tipe. *) 
    Scope.PopNew ();
    Scope.TypeCheck (elts, cs);
    Tipe.Define ("WIDECHAR", T, TRUE);
  END Initialize;

BEGIN
END WCharr.
