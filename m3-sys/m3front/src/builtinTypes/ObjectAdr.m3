(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ObjectAdr.m3                                          *)
(* Last Modified On Fri Jun 24 09:44:06 PDT 1994 By kalsow     *)
(*      Modified On Tue Nov 20 20:35:05 1990 By muller         *)

MODULE ObjectAdr;

IMPORT M3ID, ObjectType, Tipe, Scope;

PROCEDURE Initialize () =
  VAR s: Scope.T;
  BEGIN
    s := Scope.PushNew (FALSE, M3ID.NoID);
    Scope.PopNew ();
    T := NIL;   (* the value is used by ObjectType.New ! *)
    T := ObjectType.New (NIL, FALSE, NIL, s, s);
    Tipe.Define ("_UNTRACED_ROOT", T, FALSE);
  END Initialize;

BEGIN
END ObjectAdr.
