(* Copyright (C) 1993 Digital Equipment Corporation.                   *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)

(* Last modified on Thu Mar  3 13:58:52 1994 by luca *)

MODULE Main;
IMPORT ObliqOnline, OSError, TextRd, SynWr, FileWr;

(* ========= Add or remove imports here ========= *)
IMPORT ObLibM3; (* rd,wr,lex,fmt,pickle,process,thread *)
IMPORT ObLibUI; (* color,form *)
(* ============================================== *)

  CONST Greetings = "obliq-ui server";

  PROCEDURE RegisterLibraries() =
  BEGIN
(* ========= Add or remove libraries here ========= *)
    ObLibM3.PackageSetup();
    ObLibUI.PackageSetup();
(* ================================================ *)
  END RegisterLibraries;

CONST ServerScript =
  "process_new(processor, [\"netobjd\"], true); \n" &
  "loop try \n" &
  "  net_exportEngine(\"uiObliqEngine\", \"\", sys_address); \n" &
  "  exit; \n" &
  "except net_failure => pause(2.0); \n" &
  "end end; \n" &
  "let mu = mutex(); \n" &
  "lock mu do wait(mu, condition()) end; \n";

VAR logWr: SynWr.T;

BEGIN
  ObliqOnline.Setup();
  RegisterLibraries();
  TRY 
    logWr := SynWr.New(FileWr.Open("/tmp/obliqsrvui.log"));
  EXCEPT OSError.E =>
    logWr := SynWr.out;
  END;
  ObliqOnline.Interact(
    ObliqOnline.New(Greetings, logWr, FALSE),
    "<server script>", TextRd.New(ServerScript));
END Main.

