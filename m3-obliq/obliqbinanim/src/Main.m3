(* Copyright (C) 1993 Digital Equipment Corporation.                   *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)

(* Last modified on Mon Nov  1 10:02:54 1993 by luca *)

MODULE Main;
IMPORT ObliqOnline;

(* ========= Add or remove imports here ========= *)
IMPORT ObLibM3, ObLibM3Help; (* rd,wr,lex,fmt,pickle,process,thread *)
IMPORT ObLibUI, ObLibUIHelp; (* color,form *)
IMPORT ObLibAnim, ObLibAnimHelp; (* graph zeus *)
(* ============================================== *)

  CONST Greetings = 
    "  obliq -anim  (obliq with standard, ui, and animation libraries) (say \'help;\' for help)";

  PROCEDURE RegisterLibraries() =
  BEGIN
(* ========= Add or remove libraries here ========= *)
    ObLibM3.PackageSetup();    ObLibM3Help.Setup();
    ObLibUI.PackageSetup();    ObLibUIHelp.Setup();
    ObLibAnim.PackageSetup();  ObLibAnimHelp.Setup();
(* ================================================ *)
  END RegisterLibraries;

BEGIN
  ObliqOnline.Setup();
  RegisterLibraries();
  ObliqOnline.Interact(ObliqOnline.New(Greetings));
END Main.

