(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


MODULE Err;

IMPORT Thread;
IMPORT Params, Wr, Stdio, Process;


PROCEDURE DefaultPrint(msg: TEXT)=
  BEGIN
    TRY
      WITH err = Stdio.stderr DO
        Wr.PutText(err, msg);
        Wr.Flush(err);
      END;
    EXCEPT
    | Wr.Failure, Thread.Alerted => (* give up *)
    END;
  END DefaultPrint;


EXCEPTION
  Disaster;

VAR progname_g: TEXT := NIL;


PROCEDURE Print(
    msg: TEXT;
    severity: Severity := Severity.Fatal;
    newline: BOOLEAN := TRUE)
    RAISES {} =
  VAR
    first, second, third: TEXT;
  BEGIN
    IF severity = Severity.Continue THEN
      first := "";
    ELSE
      first := progname_g & ": ";
    END;
    CASE severity OF
    | Severity.Warning => second := "(Warning) ";
    | Severity.Error => second := "(Error) ";
    | Severity.Fatal => second := "(Fatal error) ";
    | Severity.Disaster => second := "(Disaster) ";
    ELSE second := "";
    END;
    IF newline THEN third := "\n" ELSE third := "" END;
    DefaultPrint(first & second & msg & third);
    IF severity = Severity.Fatal THEN
      Process.Exit(1);
    ELSIF severity = Severity.Disaster THEN
      <*FATAL Disaster*> BEGIN RAISE Disaster; END;
    END;
  END Print;

PROCEDURE SetProgramName(n: TEXT): TEXT=
  VAR prev := progname_g;
  BEGIN
    progname_g := n;
    RETURN prev;
  END SetProgramName;

BEGIN
  progname_g := Params.Get(0);
END Err.
