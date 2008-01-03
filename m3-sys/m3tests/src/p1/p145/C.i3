(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 13:49:41 PST 1992 by kalsow *)
(*      modified on Tue Apr  3 01:37:04 1990 by saxe       *)

INTERFACE C;

TYPE
  Moose = REF RECORD weight, height, age: REAL; lodgeNumber: INTEGER; END;
  MooseSelector = PROCEDURE (a, b: Moose): Moose;

VAR
  bullwinkle, thidwick: Moose;

PROCEDURE Elder(m1, m2: Moose): Moose;

PROCEDURE Younger(m1, m2: Moose): Moose;

PROCEDURE Taller(m1, m2: Moose): Moose;

PROCEDURE Shorter(m1, m2: Moose): Moose;

PROCEDURE Heavier(m1, m2: Moose): Moose;

PROCEDURE Lighter(m1, m2: Moose): Moose;

END C.
