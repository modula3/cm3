(* Copyright (C) 1993 Digital Equipment Corporation.                   *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)

(* Last modified on Wed Oct 27 14:28:44 1993 by luca *)

MODULE Main;
IMPORT ObliqOnline, SynWr, Wr, Thread, Params, Text, OSError, FileWr,
       TextRd, Bundle, CompileTime;

(* ========= Add or remove imports here ========= *)
IMPORT ObLibM3, ObLibM3Help; (* rd,wr,lex,fmt,pickle,process,thread *)
(* ============================================== *)

VAR Greetings := 
    "Repo (with standard libraries), compiled on " & 
    Bundle.Get(CompileTime.Get(),"compiletime.txt") &
    "(say \'help;\' for help)\nCopyright (c) 1995-1998 Columbia University";

  PROCEDURE RegisterLibraries() RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
(* ========= Add or remove libraries here ========= *)
    SynWr.Flush(console);
    Wr.PutText(wr, "[Repo Runtime Initializing] [Basic libraries]\r");
    Wr.Flush(wr);
    SynWr.PushSilence (console);
    ObLibM3.PackageSetup();    ObLibM3Help.Setup();
    SynWr.PopSilence (console);
(* ================================================ *)
  END RegisterLibraries;

CONST ServerScript =
  "process_new(processor, [\"netobjd\"], true, \"\"); \n" &
  "loop try \n" &
  "  net_exportEngine(\"stdRepoEngine\", \"\", sys_address); \n" &
  "  exit; \n" &
  "except net_failure => pause(2.0); \n" &
  "end end; \n" &
  "let mu = mutex(); \n" &
  "lock mu do wait(mu, condition()) end; \n";

VAR console: SynWr.T;
    wr: Wr.T;
    i := 1;
    last := Params.Count-1;
    srv := FALSE;

BEGIN
  SynWr.Setup();
  console := SynWr.out;

  WHILE i <= last DO
    WITH param = Params.Get(i) DO
      INC(i);
      IF Text.Equal ("-RepoSrv", param) THEN
        srv := TRUE;
      END;
    END;
  END;

  IF srv THEN
    TRY 
      console := SynWr.New(FileWr.Open("/tmp/reposrvstd.log"));
    EXCEPT OSError.E => END;
  END;

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

    IF srv THEN
      ObliqOnline.Interact(
          ObliqOnline.New(console, Greetings, FALSE),
          "<server script>", TextRd.New(ServerScript));
    ELSE
      ObliqOnline.Interact(ObliqOnline.New(console, Greetings));
    END;
  EXCEPT Wr.Failure, Thread.Alerted => END;
END Main.
