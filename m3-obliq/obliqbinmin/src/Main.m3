(* Copyright (C) 1993 Digital Equipment Corporation.                   *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)

MODULE Main;
IMPORT ObliqOnline, SynWr, Wr, Thread, Bundle, CompileTime;

(* ========= Add or remove imports here ========= *)
(* ============================================== *)

VAR Greetings := 
    "Repo (with minimum libraries), compiled on " & 
    Bundle.Get(CompileTime.Get(),"compiletime.txt") &
    "(say \'help;\' for help)\nCopyright (c) 1995-1998 Columbia University";

  PROCEDURE RegisterLibraries() =
  BEGIN
(* ========= Add or remove libraries here ========= *)
(* ================================================ *)
  END RegisterLibraries;

VAR console: SynWr.T;
    wr: Wr.T;

BEGIN
  SynWr.Setup();
  console := SynWr.out;
  wr := SynWr.UnderlyingWr(console);

  TRY
    Wr.PutText(wr, "[Repo Runtime Initializing]\r");
    Wr.Flush(wr);
    SynWr.PushSilence (console);
    ObliqOnline.Setup(console);
    SynWr.PopSilence (console);
    RegisterLibraries();
    SynWr.Flush(console);
    Wr.PutText(wr, "                                                    \r");
    Wr.Flush(wr);
    ObliqOnline.Interact(ObliqOnline.New(console, Greetings));
  EXCEPT Wr.Failure, Thread.Alerted => END;
END Main.
