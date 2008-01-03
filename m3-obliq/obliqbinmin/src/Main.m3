(* Copyright (C) 1993 Digital Equipment Corporation.                   *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)

(* Last modified on Mon Nov  1 10:01:12 1993 by luca *)

MODULE Main;
IMPORT ObliqOnline;

(* ========= Add or remove imports here ========= *)
(* ============================================== *)

  CONST Greetings = 
    "  obliq -min  (obliq with minimum libraries) (say \'help;\' for help)";

  PROCEDURE RegisterLibraries() =
  BEGIN
(* ========= Add or remove libraries here ========= *)
(* ================================================ *)
  END RegisterLibraries;

BEGIN
  ObliqOnline.Setup();
  RegisterLibraries();
  ObliqOnline.Interact(ObliqOnline.New(Greetings));
END Main.

