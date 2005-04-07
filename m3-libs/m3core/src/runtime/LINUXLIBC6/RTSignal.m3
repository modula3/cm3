(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov 21 11:26:41 PST 1994 by kalsow    *)
(*      modified on Mon Mar 16 18:10:15 PST 1992 by muller    *)

UNSAFE MODULE RTSignal;

IMPORT RTError, RTProcess, Usignal, Uprocess, Uucontext;
FROM Ctypes IMPORT int;

VAR
  DefaultHandler   : Usignal.SignalActionHandler;
  IgnoreSignal     : Usignal.SignalActionHandler;
  initial_handlers : ARRAY [0..5] OF Usignal.struct_sigaction;

PROCEDURE InstallHandlers () =
  BEGIN
    DefaultHandler := LOOPHOLE (0, Usignal.SignalActionHandler);
    IgnoreSignal   := LOOPHOLE (1, Usignal.SignalActionHandler);
    (* Note: we cannot use Usignal.SIG_DFL and Usignal.SIG_IGN because
       they may not be initialized when this module is kicked into action
       by the low-level runtime startup code... *)

    SetHandler (0, Usignal.SIGHUP,  Shutdown);
    SetHandler (1, Usignal.SIGINT,  Interrupt);
    SetHandler (2, Usignal.SIGQUIT, Quit);
    SetHandler (3, Usignal.SIGSEGV, SegV);
    SetHandler (4, Usignal.SIGPIPE, IgnoreSignal);
    SetHandler (5, Usignal.SIGTERM, Shutdown);
  END InstallHandlers;

PROCEDURE SetHandler (id: INTEGER; sig: int;  handler: Usignal.SignalActionHandler) =
  VAR new: Usignal.struct_sigaction;
  BEGIN
    new.sa_sigaction := LOOPHOLE (handler, Usignal.SignalActionHandler);
    new.sa_flags := Usignal.SA_SIGINFO;
    WITH i = Usignal.sigemptyset(ADR(new.sa_mask)) DO
      <*ASSERT i = 0*>
    END;
    WITH i = Usignal.sigaction (sig, ADR(new), ADR(initial_handlers[id])) DO
      <*ASSERT i = 0*>
    END;
    IF (initial_handlers[id].sa_sigaction # DefaultHandler) THEN
      (* don't override inherited, non-default handlers *)
      WITH i = Usignal.sigaction (sig, ADR(initial_handlers[id]), ADR(new)) DO
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
  BEGIN
    EVAL Usignal.sigaction (sig, ADR(initial_handlers[id]), NIL);
  END RestoreHandler;

PROCEDURE Shutdown (sig: int;
         <*UNUSED*> sip: Usignal.siginfo_t_star;
         <*UNUSED*> uap: Uucontext.ucontext_t_star) =
  VAR new: Usignal.struct_sigaction;
  BEGIN
    new.sa_sigaction := DefaultHandler;
    new.sa_flags := 0;
    EVAL Usignal.sigemptyset(ADR(new.sa_mask));
    RTProcess.InvokeExitors ();                   (* flush stdio... *)
    (* restore default handler *)
    EVAL Usignal.sigaction (sig, ADR(new), NIL);
    EVAL Usignal.kill (Uprocess.getpid (), sig);  (* and resend the signal *)
  END Shutdown;

PROCEDURE Interrupt (sig: int;
                     sip: Usignal.siginfo_t_star;
                     uap: Uucontext.ucontext_t_star) =
  VAR h := RTProcess.OnInterrupt (NIL);
  BEGIN
    IF (h = NIL) THEN
      Shutdown (sig, sip, uap);
    ELSE
      EVAL RTProcess.OnInterrupt (h); (* reinstall the handler *)
      h ();
    END;
  END Interrupt;

PROCEDURE Quit (<*UNUSED*> sig: int;
                <*UNUSED*> sip: Usignal.siginfo_t_star;
                           uap: Uucontext.ucontext_t_star) =
  VAR pc := 0;
  BEGIN
    IF (uap # NIL) THEN pc := uap.uc_mcontext.gregs[Uucontext.REG_EIP]; END;
    RTError.MsgPC (pc, "aborted");
  END Quit;

PROCEDURE SegV (<*UNUSED*> sig: int;
                <*UNUSED*> sip: Usignal.siginfo_t_star;
                           uap: Uucontext.ucontext_t_star) =
  VAR pc := 0;
  BEGIN
    IF (uap # NIL) THEN pc := uap.uc_mcontext.gregs[Uucontext.REG_EIP]; END;
    RTError.MsgPC (pc,
      "Segmentation violation - possible attempt to dereference NIL");
  END SegV;

BEGIN
END RTSignal.
