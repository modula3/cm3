(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 13:53:06 PST 1992 by kalsow *)
(*      modified on Tue Apr  3 01:40:51 1990 by saxe       *)

INTERFACE A;

IMPORT C;

TYPE
  Moose = C.Moose;

(*--- unsupported Modula-2+ pass-thru variable ---
VAR
  moose: Moose = C.thidwick;
-------------------------------------------------*)

PROCEDURE MakeMoose(w, h, a: REAL; l: INTEGER): Moose;

CONST Bigger (*(m1, m2: Moose): Moose*) = C.Taller;

END A.
