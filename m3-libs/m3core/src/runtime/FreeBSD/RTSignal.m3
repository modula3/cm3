(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov 21 10:31:19 PST 1994 by kalsow    *)
(*      modified on Mon Mar 16 18:10:15 PST 1992 by muller    *)

UNSAFE MODULE RTSignal;

IMPORT RTMisc, RTProcess, Csignal, Usignal, Uprocess;
FROM Ctypes IMPORT int;

VAR
  DefaultHandler   : Csignal.Handler;
  IgnoreSignal     : Csignal.Handler;
  initial_handlers : ARRAY [0..5] OF Csignal.Handler;

PROCEDURE InstallHandlers () =
  BEGIN
    DefaultHandler := LOOPHOLE (0, Csignal.Handler);
    IgnoreSignal   := LOOPHOLE (1, Csignal.Handler);

    SetHandler (0, Usignal.SIGHUP,  Shutdown);
    SetHandler (1, Usignal.SIGINT,  Interrupt);
    SetHandler (2, Usignal.SIGQUIT, Quit);
    SetHandler (3, Usignal.SIGSEGV, SegV);
    SetHandler (4, Usignal.SIGPIPE, IgnoreSignal);
    SetHandler (5, Usignal.SIGTERM, Shutdown);
  END InstallHandlers;

PROCEDURE SetHandler (id: INTEGER;  sig: int;  handler: Csignal.Handler) =
  VAR old := Csignal.signal (sig, handler);
  BEGIN
    initial_handlers[id] := old;
    IF (old # DefaultHandler) THEN
      (* don't override inherited, non-default handlers *)
      EVAL Csignal.signal (sig, old);
    END;
  END SetHandler;

PROCEDURE RestoreHandlers () =
  BEGIN
    RestoreHandler (0, Usignal.SIGHUP);
    RestoreHandler (1, Usignal.SIGINT);
    RestoreHandler (2, Usignal.SIGQUIT);
    RestoreHandler (3, Usignal.SIGSEGV);
    RestoreHandler (4, Usignal.SIGPIPE);
    RestoreHandler (5, Usignal.SIGTERM);
  END RestoreHandlers;

PROCEDURE RestoreHandler (id: INTEGER;  sig: int) =
  BEGIN
    EVAL Csignal.signal (sig, initial_handlers[id]);
  END RestoreHandler;

PROCEDURE Shutdown (sig: int) =
  BEGIN
    RTProcess.InvokeExitors ();                   (* flush stdio... *)
    EVAL Csignal.signal (sig, DefaultHandler);    (* restore default handler *)
    EVAL Usignal.kill (Uprocess.getpid (), sig);  (* and resend the signal *)
  END Shutdown;

PROCEDURE Interrupt (sig: int) =
  VAR h := RTProcess.OnInterrupt (NIL);
  BEGIN
    IF (h = NIL) THEN
      Shutdown (sig);
    ELSE
      EVAL RTProcess.OnInterrupt (h); (* reinstall the handler *)
      h ();
    END;
  END Interrupt;

PROCEDURE Quit (<*UNUSED*> sig: int) =
  BEGIN
    RTMisc.FatalErrorI ("aborted", 0);
  END Quit;

PROCEDURE SegV (<*UNUSED*> sig: int) =
  BEGIN
    RTMisc.FatalErrorI (
      "Segmentation violation - possible attempt to dereference NIL", 0);
  END SegV;

BEGIN
END RTSignal.
