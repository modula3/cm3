(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Wed Oct 11 14:32:36 PDT 1995 by najork     *)
(*      modified on Thu Jan 19 12:17:17 PST 1995 by kalsow     *)
(*      modified on Tue Sep 27 18:30:00 PDT 1994 by weich      *)

(* This is for mere debugging purposes: Do some printout so that one
   can see what the program is doing.

   There are several levels of logging: Everything with a less or lower lev-
   parameter than the global 'level'-variable will be printed.
   Use 'Log.level:= 0' to switch off logging *)

MODULE Log;

IMPORT Wr, Fmt, Thread, Process;
FROM Stdio IMPORT stdout;

<*FATAL Wr.Failure, Thread.Alerted*>

CONST indent = "  ";		(* Text used for each level of indentation *)
VAR proccount: CARDINAL:= 0;	(* indendation-level (set by 'In/ExitProc') *)
  first:= TRUE;			(* first to write into a new line *)

PROCEDURE InProc(name: TEXT; lev:= 1) =
  BEGIN
    IF lev <= level THEN
      PutText("------- "&name&"-----------");
      INC(proccount);
      Nl(lev:=lev);
    END (*IF*)
  END InProc;

PROCEDURE ExitProc(comment: TEXT:= ""; lev:= 1) =
  BEGIN
    IF lev <= level THEN
      DEC(proccount);
      PutText("------- "&comment);
      Nl(lev:= lev);
    END (*IF*)
  END ExitProc;

PROCEDURE PutText(t: TEXT; lev:= 1) =
  BEGIN
    IF lev <= level THEN
      IF first THEN
	first:= FALSE; 
        Wr.PutText(stdout, linestart);
        FOR i:= 1 TO proccount DO Wr.PutText(stdout, indent) END;
      END; (*IF*)
      Wr.PutText(stdout, t); Wr.Flush(stdout);
    END (*IF*)
  END PutText;

PROCEDURE PutInt(i: INTEGER; lev:= 1) =
  BEGIN
    IF lev <= level THEN
      PutText(Fmt.Int(i));
    END (*IF*)
  END PutInt;

PROCEDURE Nl(lev:= 1) =
  BEGIN
    IF lev <= level THEN
      Wr.PutText(stdout, "\n"); Wr.Flush(stdout);
      first:= TRUE;
    END (*IF*)
  END Nl;

PROCEDURE Separate(lev:= 1) =
  BEGIN
    IF lev <= level THEN
      Nl(lev:= lev);
      Wr.PutText(stdout, "------------------");
      Nl(lev:= lev);
    END (*IF*)
  END Separate;


PROCEDURE CrashPoint(nr: INTEGER) =
  BEGIN
    IF crash = nr THEN
      PutText("crash-point "); PutInt(nr); PutText(" reached"); Nl();
      Process.Exit(nr);
    END;
  END CrashPoint;

BEGIN
  level := 0
END Log.
