(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Wed Dec 21 13:47:07 PST 1994 by kalsow     *)
(*      modified on Sat Jun 27 22:22:30 PDT 1992 by muller     *)

UNSAFE MODULE RTProcess;

IMPORT RTHeapRep, RTException, RTIO, RTOS;

(*-------------------------------- program startup/shutdown -----------------*)

TYPE
  Exitor = UNTRACED BRANDED "RTProcess.Exitor" REF RECORD
             proc: PROCEDURE ();
             next: Exitor;
           END;

(* Exitors are untraced because we don't want to depend on the
   collector during a crash.  Since they're never disposed, we
   have a small memory leak, probably about (3 * 16 bytes)/process. *)

VAR
  exitors: Exitor := NIL;

PROCEDURE RegisterExitor (p: PROCEDURE ()) =
  BEGIN
    exitors := NEW (Exitor, proc := p, next := exitors);
  END RegisterExitor;

PROCEDURE InvokeExitors () =
  VAR tmp: Exitor;
  BEGIN
    (* run the registered "exit" routines *)
    WHILE exitors # NIL DO
      (* to ensure progress, remove an element from the list before
         invoking it *)
      tmp := exitors;
      exitors := exitors.next;
      tmp.proc ();
    END;
  END InvokeExitors;

PROCEDURE Exit (n: INTEGER) =
  BEGIN
    InvokeExitors ();
    RTOS.Exit (n);
  END Exit;

PROCEDURE Crash (msg: TEXT) =
  BEGIN
    IF (msg # NIL) THEN
      RTIO.PutText ("\n*** ");
      RTIO.PutText (msg);
      RTIO.PutText ("\n");
    END;
    RTException.DumpStack ();
    RTIO.Flush ();

    (* run the registered "exit" routines *)
    InvokeExitors ();

    (* crash *)
    EVAL RTHeapRep.Crash ();
    RTOS.Crash ();
  END Crash;

(*------------------------------------------------------ Ctl-C interrupts ---*)

VAR cur_handler: InterruptHandler := NIL;

PROCEDURE OnInterrupt (p: InterruptHandler): InterruptHandler =
  (* This procedure should be atomic... but I doubt anyone cares. *)
  VAR old := cur_handler;
  BEGIN
    cur_handler := p;
    RETURN old;
  END OnInterrupt;

BEGIN
END RTProcess.



