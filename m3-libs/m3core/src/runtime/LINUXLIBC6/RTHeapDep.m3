(* Copyright (C) 1993, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(*| Last modified on Fri Jan 20 09:39:09 PST 1995 by kalsow *)
(*|      modified on Thu Jan 28 19:24:55 PST 1993 by jdd    *)

UNSAFE MODULE RTHeapDep;

IMPORT ThreadF, RTHeapRep, RTCollectorSRC, RTMachine, RTVM;
IMPORT Cstdlib, Ctypes, Umman, Unix, Usignal;
IMPORT Utypes, Uucontext, Word;

VAR
  initialized := FALSE;
  (* true iff "Init" has been called *)

  defaultSIGSEGV: Usignal.SignalActionHandler := NIL;
  (* original handler for "SIGSEGV" signal; set by "Init" *)

PROCEDURE Protect (p: Page; n: CARDINAL; readable, writable: BOOLEAN) =
  BEGIN
    IF NOT initialized THEN Init(); initialized := TRUE; END;
    IF NOT readable THEN writable := FALSE; END; (* processor limitation *)
    VAR prot: Ctypes.int := 0;
    BEGIN
      IF readable THEN prot := Word.Or(prot, Umman.PROT_READ); END;
      IF writable THEN prot := Word.Or(prot, Umman.PROT_WRITE); END;
      VAR
        addr := LOOPHOLE(p * BytesPerPage, Utypes.caddr_t);
        ret := Umman.mprotect(addr, n * BytesPerPage, prot);
      BEGIN
        <* ASSERT ret = 0 *>
      END;
    END;
  END Protect;

(* Init establishes a handler for SIGSEGV, caused by VM faults, and for all
   other signals that cause core dumps. *)

PROCEDURE Init () =
  BEGIN
    (* check that "BytesPerPage" is an acceptable value *)
    VAR vmPageBytes := Unix.getpagesize();
    BEGIN
      <* ASSERT BytesPerPage >= vmPageBytes *>
      <* ASSERT BytesPerPage MOD vmPageBytes = 0 *>
    END;

    (* establish SIGSEGV handler; remember previous handler *)
    VAR
      act, oact : Usignal.struct_sigaction;
      ret: Ctypes.int;
    BEGIN
      act.sa_flags := Word.Or(Usignal.SA_NODEFER,
                              Word.Or(Usignal.SA_RESTART, Usignal.SA_SIGINFO));
      act.sa_sigaction := LOOPHOLE(Fault, Usignal.SignalActionHandler);
      EVAL Usignal.sigemptyset(act.sa_mask);
      (* block the "SIGVTALRM" signal when signal handlers are called *)
      EVAL Usignal.sigaddset(act.sa_mask, Usignal.SIGVTALRM);
      ret := Usignal.sigaction(Usignal.SIGSEGV, act, oact);
      <* ASSERT ret = 0 *>
      defaultSIGSEGV := oact.sa_sigaction;
    END;

    (* establish signal handler for all other signals that dump core, if no
       handler exists *)
    PROCEDURE OverrideDefault (sig: Ctypes.int) =
      VAR
        act, oact: Usignal.struct_sigaction;
        ret: Ctypes.int;
      BEGIN
        act.sa_flags := Usignal.SA_SIGINFO;
        act.sa_sigaction := Core;
        EVAL Usignal.sigemptyset(act.sa_mask);
        EVAL Usignal.sigaddset(act.sa_mask, Usignal.SIGVTALRM);
        ret := Usignal.sigaction(sig, act, oact);
        <* ASSERT ret = 0 *>
        (* If the old handler was not the default, restore it. *)
        IF oact.sa_sigaction # Usignal.SIG_DFL THEN
          ret := Usignal.sigaction(sig, oact, act);
          <* ASSERT ret = 0 *>
        END;
      END OverrideDefault;
    BEGIN
      (* override signal handling for all signals that normally dump core *)
      OverrideDefault(Usignal.SIGQUIT);
      OverrideDefault(Usignal.SIGILL);
      OverrideDefault(Usignal.SIGTRAP);
      OverrideDefault(Usignal.SIGIOT);
      OverrideDefault(Usignal.SIGFPE);
      OverrideDefault(Usignal.SIGBUS);
      OverrideDefault(Usignal.SIGSYS);
    END;
  END Init;

(* Fault is called upon a SIGSEGV signal, caused by a VM fault.  If
   RTHeapRep.Fault is not able to handle the fault, it invokes the previous
   action. *)

PROCEDURE Fault (sig : Ctypes.int;
                 sip : Usignal.siginfo_t_fault_star;
                 uap : Uucontext.ucontext_t_star) =
  BEGIN
    (* try handling memory fault using "RTHeapRep.Fault" *)
    IF RTHeapRep.Fault(LOOPHOLE(uap.uc_mcontext.cr2, ADDRESS)) THEN
      RETURN;
    END;
    (* otherwise, use "defaultSIGSEGV" to handle the fault *)
    IF defaultSIGSEGV = Usignal.SIG_IGN THEN RETURN;
    ELSIF defaultSIGSEGV = Usignal.SIG_DFL THEN Core(sig, sip, uap);
    ELSE defaultSIGSEGV(sig, sip, uap);
    END;
  END Fault;

(* Core is a signal handler for signals that dump core, to complete the
   current collection before dumping core.  This makes core files easier to
   debug, and avoids an Ultrix bug that creates incomplete core files if
   heap pages are read-protected. *)

VAR dumped_core := FALSE;

PROCEDURE Core (sig : Ctypes.int;
     <*UNUSED*> sip : Usignal.siginfo_t_star;
     <*UNUSED*> uap : Uucontext.ucontext_t_star) =
  BEGIN
    ThreadF.SuspendOthers();
    IF NOT dumped_core THEN
      (* indicate that this thread will dump core *)
      dumped_core := TRUE;

      (* clean up the heap *)
      EVAL RTHeapRep.Crash();

      (* establish default action *)
      VAR
        act, oact: Usignal.struct_sigaction;
      BEGIN
        act.sa_flags := 0;
        act.sa_sigaction := Usignal.SIG_DFL;
        EVAL Usignal.sigemptyset(act.sa_mask);
        EVAL Usignal.sigaction(sig, act, oact);
      END;

      (* unblock signals *)
      VAR set, oset: Uucontext.sigset_t;
      BEGIN
        EVAL Usignal.sigemptyset(set);
        EVAL Usignal.sigprocmask(Usignal.SIG_SETMASK, set, oset);
      END;

      (* now, dump core *)
      Cstdlib.abort ();
      <* ASSERT FALSE *>
    END;
    ThreadF.ResumeOthers();
  END Core;

(* System-call faults are handled in RTHeapDepC.c *)

PROCEDURE VMFaultTime (): REAL =
  BEGIN
    RETURN 0.010;                (* guess 10ms to handle a page fault *)
  END VMFaultTime;

BEGIN
  VM := RTVM.VMHeap();
  AtomicWrappers := RTVM.AtomicWrappers();
  IF VM THEN
    RTMachine.RTHeapRep_Fault  := LOOPHOLE (RTHeapRep.Fault, ADDRESS);
    RTMachine.RTCSRC_FinishVM  := LOOPHOLE (RTCollectorSRC.FinishVM, ADDRESS);
  END;
END RTHeapDep.
