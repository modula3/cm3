(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 22 11:22:45 PDT 1995 by najork                   *)
(*       Created on Tue Jan 18 09:39:01 PST 1994 by najork                   *)


MODULE Main;
IMPORT ObliqOnline;

(* ========= Add or remove imports here ========= *)

IMPORT ObLibM3, ObLibM3Help;     (* rd,wr,lex,fmt,pickle,process,thread *)
IMPORT ObLibUI, ObLibUIHelp;     (* color,form *)
IMPORT ObLibAnim, ObLibAnimHelp; (* graph zeus *)
IMPORT ObLib3D;                  (* anim3D     *)

(* ============================================== *)

  CONST Greetings = 
    "Obliq-3D    22-Aug-95 Beta Release (say \'help;\' for help)\nCopyright (c) 1995 Digital Equipment Corporation";

  PROCEDURE RegisterLibraries() =
  BEGIN
(* ========= Add or remove libraries here ========= *)
    ObLibM3.PackageSetup();    ObLibM3Help.Setup();
    ObLibUI.PackageSetup();    ObLibUIHelp.Setup();
    ObLibAnim.PackageSetup();  ObLibAnimHelp.Setup();
    ObLib3D.PackageSetup();    
(* ================================================ *)
  END RegisterLibraries;

BEGIN
  ObliqOnline.Setup();
  RegisterLibraries();
  ObliqOnline.Interact (ObliqOnline.New(Greetings));
END Main.
