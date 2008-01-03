(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 12:16:31 PST 1995 by kalsow     *)
(*      modified on Tue Sep 27 18:30:00 PDT 1994 by weich      *)

(* This is for mere debugging purposes: Do some printout so that one
   can see what the program is doing.

   There are several levels of logging: Everything with a less or lower lev-
   parameter than the global 'level'-variable will be printed.
   Use 'Log.level:= 0' to switch off logging *)

INTERFACE Log;

PROCEDURE InProc(name: TEXT; lev:= 1);
  (* Write out something to indicate that program is in procedure 'name'
     the following output will be intendet by a few spaces (so use Nl()!) *)

PROCEDURE ExitProc(comment: TEXT:= ""; lev:= 1);
  (* This is merely to end the indentation started by 'InProc'. 'comment'
     will be printed out (perhaps to specify the exit point if the procedure
     has several RETURN-statements) *)

PROCEDURE PutText(t: TEXT; lev:= 1);
  (* Write out a text string *)

PROCEDURE PutInt(i: INTEGER; lev:= 1);
  (* Write out an integer *)

PROCEDURE Nl(lev:= 1);
  (* Make a newline on output *)

PROCEDURE Separate(lev:= 1);
  (* Write out some space or horizontal line *)


(* Crash-Simulation *)

VAR crash:= 0;
(* set to a number to crash at a certain point in the program *)
  PROCEDURE CrashPoint(nr: INTEGER);
(* Halts the program (uses Unix.exit) if "nr=crash". *)


VAR
  level:= 1;           (* default is to do logging *)
  linestart:= "> ";    (* every logging line will start with this characters 
			  in order to distiguish logging information from
			  program output *)
END Log.
