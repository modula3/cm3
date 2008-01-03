(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE M3ArrVal;

IMPORT M3Const;

TYPE
  T <: REFANY;

PROCEDURE NewEmpty (n_elts: CARDINAL): T;

PROCEDURE Set (t: T;  index: CARDINAL;  READONLY val: M3Const.T): BOOLEAN;

PROCEDURE Index (t: T;  index: CARDINAL;  VAR(*OUT*) val: M3Const.T): BOOLEAN;

PROCEDURE Compare (a, b: T): INTEGER;
(* (a = b) => 0, (a # b) => -99. *)

END M3ArrVal.

(* All array indicies are zero-based *)

