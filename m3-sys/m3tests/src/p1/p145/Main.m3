(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 14:01:01 PST 1992 by kalsow *)
(*      modified on Wed Jan  9 18:59:17 1991 by saxe       *)

MODULE Main;

IMPORT Test, A, B, C;

VAR
  moose1: A.Moose;
  moose2: B.Moose;
  something: REFANY;
  somethingElse: REFANY;

BEGIN
  moose1 := C.thidwick (*A.moose*);
  moose2 := C.bullwinkle (*B.moose*);
  Test.checkI (moose1^.lodgeNumber, 12);
  Test.checkI (moose2^.lodgeNumber, 7);
  Test.check (A.Bigger(moose1, moose2) = moose1);
  Test.check (B.Bigger(moose1, moose2) = moose2);
  something := A.MakeMoose(46.0, 200.0, 0.5, 0);
  somethingElse := B.MakeMoose(46.0, 200.0, 0.5, 0);
  Test.checkI (TYPECODE(something), TYPECODE(somethingElse));
  Test.done ();
END Main.
