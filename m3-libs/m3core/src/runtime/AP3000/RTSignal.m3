(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov 21 10:30:17 PST 1994 by kalsow    *)
(*      modified on Mon Mar 16 18:10:15 PST 1992 by muller    *)

UNSAFE MODULE RTSignal;

IMPORT RTMisc, RTProcess, Usignal, Uprocess;
FROM Ctypes IMPORT int;

TYPE
  SigInfo = UNTRACED REF Usignal.struct_sigcontext;

VAR
  DefaultHandler   : Usignal.SignalHandler;
  IgnoreSignal     : Usignal.SignalHandler;
  initial_handlers : ARRAY [0..5] OF Usignal.struct_sigvec;

PROCEDURE InstallHandlers () =
  BEGIN
    DefaultHandler := LOOPHOLE (0, Usignal.SignalHandler);
    IgnoreSignal   := LOOPHOLE (1, Usignal.SignalHandler);

    SetHandler (0, Usignal.SIGHUP,  Shutdown);
    SetHandler (1, Usignal.SIGINT,  Interrupt);
    SetHandler (2, Usignal.SIGQUIT, Quit);
    SetHandler (3, Usignal.SIGSEGV, SegV);
    SetHandler (4, Usignal.SIGPIPE, IgnoreSignal);
    SetHandler (5, Usignal.SIGTERM, Shutdown);
  END InstallHandlers;

PROCEDURE SetHandler (id: INTEGER; sig: int;  handler: Usignal.SignalHandler) =
  (* Note: we use the LOOPHOLE to prevent the runtime check for
     nested procedure.  The runtime check crashes when
     handler = IgnoreSignal = 1. *)
  VAR new: Usignal.struct_sigvec;
  BEGIN
    new.sv_handler := LOOPHOLE (handler, Usignal.SignalHandler);
    new.sv_mask    := Usignal.empty_sv_mask;
    new.sv_flags   := 0;
    EVAL Usignal.sigvec (sig, new, initial_handlers[id]);
    IF (initial_handlers[id].sv_handler # DefaultHandler) THEN
      (* don't override inherited, non-default handlers *)
      EVAL Usignal.sigvec (sig, initial_handlers[id], new);
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
  VAR old: Usignal.struct_sigvec;
  BEGIN
    EVAL Usignal.sigvec (sig, initial_handlers[id], old);
  END RestoreHandler;

PROCEDURE Shutdown (sig: int; <*UNUSED*> code: int; <*UNUSED*> scp: SigInfo) =
  VAR new, old: Usignal.struct_sigvec;
  BEGIN
    new.sv_handler := DefaultHandler;
    new.sv_mask    := Usignal.empty_sv_mask;
    new.sv_flags   := 0;
    RTProcess.InvokeExitors ();                   (* flush stdio... *)
    EVAL Usignal.sigvec (sig, new, old);          (* restore default handler *)
    EVAL Usignal.kill (Uprocess.getpid (), sig);  (* and resend the signal *)
  END Shutdown;

PROCEDURE Interrupt (sig: int;  code: int;  scp: SigInfo) =
  VAR h := RTProcess.OnInterrupt (NIL);
  BEGIN
    IF (h = NIL) THEN
      Shutdown (sig, code, scp);
    ELSE
      EVAL RTProcess.OnInterrupt (h); (* reinstall the handler *)
      h ();
    END;
  END Interrupt;

PROCEDURE Quit (<*UNUSED*> sig, code: int; scp: SigInfo) =
  VAR pc := 0;
  BEGIN
    IF (scp # NIL) THEN pc := scp.sc_pc END;
    RTMisc.FatalErrorPC (pc, "aborted");
  END Quit;

PROCEDURE SegV (<*UNUSED*> sig, code: int; scp: SigInfo) =
  VAR pc := 0;
  BEGIN
    IF (scp # NIL) THEN pc := scp.sc_pc END;
    RTMisc.FatalErrorPC (pc,
      "Segmentation violation - possible attempt to dereference NIL");
  END SegV;

BEGIN
END RTSignal.
