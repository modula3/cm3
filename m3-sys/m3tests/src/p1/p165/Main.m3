(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 16:12:30 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:19:22 PDT 1990 by ellis  *)
(*      modified on Mon Feb 12 15:48:54 1990 by saxe       *)

MODULE Main;
IMPORT Test;

TYPE
  RefInteger = REF INTEGER;
  SameRefInteger = RefInteger;
  DifferentRefInteger = BRANDED REF INTEGER;

BEGIN
  Test.checkI (TYPECODE(RefInteger), TYPECODE(SameRefInteger));
  Test.check  (TYPECODE(RefInteger) # TYPECODE(DifferentRefInteger));
  Test.done ();
END Main.
