(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov 21 10:30:38 PST 1994 by kalsow    *)
(*      modified on Mon Mar 16 18:10:15 PST 1992 by muller    *)

UNSAFE MODULE RTSignal;

IMPORT RTError, RTProcess, Usignal, Uprocess, Uucontext;
FROM Ctypes IMPORT int;
FROM Usignal IMPORT SignalAction;

TYPE
  SigInfo = Usignal.struct_siginfo_star;
  SigContext = Uucontext.struct_ucontext_star;

VAR
  DefaultHandler   : SignalAction;
  IgnoreSignal     : SignalAction;
  initial_handlers : ARRAY [0..6] OF Usignal.struct_sigaction;

PROCEDURE InstallHandlers () =
  BEGIN
    DefaultHandler := LOOPHOLE (0, SignalAction);
    IgnoreSignal   := LOOPHOLE (1, SignalAction);

    SetHandler (0, Usignal.SIGHUP,  Shutdown);
    SetHandler (1, Usignal.SIGINT,  Interrupt);
    SetHandler (2, Usignal.SIGQUIT, Quit);
    SetHandler (3, Usignal.SIGSEGV, SegV);
    SetHandler (4, Usignal.SIGPIPE, IgnoreSignal);
    SetHandler (5, Usignal.SIGTERM, Shutdown);
    SetHandler (6, Usignal.SIGBUS,  SegV);
  END InstallHandlers;

PROCEDURE SetHandler (id: INTEGER; sig: int;  handler: SignalAction) =
  (* Note: we use the LOOPHOLE to prevent the runtime check for
     nested procedure.  The runtime check crashes when
     handler = IgnoreSignal = 1. *)
  VAR new: Usignal.struct_sigaction;
  BEGIN
    new.sa_sigaction := LOOPHOLE (handler, SignalAction);
    new.sa_flags     := Usignal.SA_SIGINFO;
    WITH i = Usignal.sigemptyset(new.sa_mask) DO
      <*ASSERT i = 0*>
    END;
    WITH i = Usignal.sigaction (sig, new, initial_handlers[id]) DO
      <*ASSERT i = 0*>
    END;
    IF (initial_handlers[id].sa_sigaction # DefaultHandler) THEN
      (* don't override inherited, non-default handlers *)
      WITH i = Usignal.sigaction (sig, initial_handlers[id], new) DO
        <*ASSERT i = 0*>
      END;
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
  VAR old: Usignal.struct_sigaction;
  BEGIN
    EVAL Usignal.sigaction (sig, initial_handlers[id], old);
  END RestoreHandler;

PROCEDURE Shutdown (sig: int;
                    <*UNUSED*> sip: SigInfo;
                    <*UNUSED*> scp: SigContext) =
  VAR new, old: Usignal.struct_sigaction;
  BEGIN
    new.sa_sigaction := DefaultHandler;
    new.sa_flags     := 0;
    EVAL Usignal.sigemptyset(new.sa_mask);
    RTProcess.InvokeExitors ();                   (* flush stdio... *)
    EVAL Usignal.sigaction (sig, new, old);       (* restore default handler *)
    EVAL Usignal.kill (Uprocess.getpid (), sig);  (* and resend the signal *)
  END Shutdown;

PROCEDURE Interrupt (sig: int; sip: SigInfo; scp: SigContext) =
  VAR h := RTProcess.OnInterrupt (NIL);
  BEGIN
    IF (h = NIL) THEN
      Shutdown (sig, sip, scp);
    ELSE
      EVAL RTProcess.OnInterrupt (h); (* reinstall the handler *)
      h ();
    END;
  END Interrupt;

PROCEDURE Quit (<*UNUSED*> sig: int; sip: SigInfo; scp: SigContext) =
  VAR pc := 0;
  BEGIN
    IF (sip # NIL AND scp # NIL)
     AND sip.si_addr = LOOPHOLE(scp.uc_mcontext.ss.eip, ADDRESS) THEN
      pc := scp.uc_mcontext.ss.eip;
    END;
    RTError.MsgPC (pc, "aborted");
  END Quit;

PROCEDURE SegV (<*UNUSED*> sig: int; sip: SigInfo; scp: SigContext) =
  VAR pc := 0;
  BEGIN
    IF (sip # NIL AND scp # NIL)
     AND sip.si_addr = LOOPHOLE(scp.uc_mcontext.ss.eip, ADDRESS) THEN
      pc := scp.uc_mcontext.ss.eip;
    END;
    RTError.MsgPC (pc,
      "Segmentation violation - possible attempt to dereference NIL");
  END SegV;

BEGIN
END RTSignal.
