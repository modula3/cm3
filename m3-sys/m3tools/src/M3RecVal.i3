(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE M3RecVal;

IMPORT M3ID, M3Const;

TYPE
  T <: REFANY;

PROCEDURE NewEmpty (): T;

PROCEDURE SetField (t: T;  id: M3ID.T;  READONLY val: M3Const.T): T;

PROCEDURE Qualify (t: T;  id: M3ID.T;  VAR(*OUT*) val: M3Const.T): BOOLEAN;

PROCEDURE Compare (a, b: T): INTEGER;
(* (a = b) => 0, (a # b) => -99. *)

END M3RecVal.

