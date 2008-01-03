(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)
(*      modified on Thu Dec 24 15:36:37 PST 1992 by jdd     *)

UNSAFE MODULE RTHeapDep;

IMPORT ThreadF, RTHeapRep, RTCollectorSRC, RTProcedureSRC, RTMachine, RTVM;
IMPORT Cstdlib, Ctypes, Umman, Unix, Usignal;
IMPORT Utypes, Uucontext, Word;

VAR
  initialized := FALSE;
  (* true iff "Init" has been called *)

  defaultSIGSEGV: Usignal.SignalAction := NIL;
  (* original handler for "SIGSEGV" signal; set by "Init" *)

PROCEDURE Protect (p: Page; n: CARDINAL; readable, writable: BOOLEAN) =
  BEGIN
    IF NOT initialized THEN Init(); initialized := TRUE; END;
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
      vec, ovec : Usignal.struct_sigaction;
      ret: Ctypes.int;
    BEGIN
      vec.sa_flags := Word.Or(Usignal.SA_RESTART, Usignal.SA_SIGINFO);
      vec.sa_sigaction := LOOPHOLE(Fault, Usignal.SignalAction);
      EVAL Usignal.sigemptyset(vec.sa_mask);
      (* block the "SIGVTALRM" signal when signal handlers are called *)
      EVAL Usignal.sigaddset(vec.sa_mask, Usignal.SIGVTALRM);
      ret := Usignal.sigaction(Usignal.SIGSEGV, vec, ovec);
      <* ASSERT ret = 0 *>
      defaultSIGSEGV := ovec.sa_sigaction;
    END;

    (* establish signal handler for all other signals that dump core, if no
       handler exists *)
    PROCEDURE OverrideDefault (sig: Ctypes.int) =
      VAR
        vec, ovec: Usignal.struct_sigaction;
        ret: Ctypes.int;
      BEGIN
        vec.sa_flags := Usignal.SA_SIGINFO;
        vec.sa_sigaction := Core;
        EVAL Usignal.sigemptyset(vec.sa_mask);
        EVAL Usignal.sigaddset(vec.sa_mask, Usignal.SIGVTALRM);
        ret := Usignal.sigaction(sig, vec, ovec);
        <* ASSERT ret = 0 *>
        (* If the old handler was not the default, restore it. *)
        IF ovec.sa_sigaction # LOOPHOLE(Usignal.SIG_DFL, Usignal.SignalAction) THEN
          ret := Usignal.sigaction(sig, ovec, vec);
          <* ASSERT ret = 0 *>
        END;
      END OverrideDefault;
    BEGIN
      (* override signal handling for all signals that normally dump core *)
      OverrideDefault(Usignal.SIGQUIT);
      OverrideDefault(Usignal.SIGILL);
      OverrideDefault(Usignal.SIGTRAP);
      OverrideDefault(Usignal.SIGIOT);
      OverrideDefault(Usignal.SIGEMT);
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
    IF sig = Usignal.SIGSEGV AND sip # NIL THEN
      <*ASSERT sig = sip.si_signo*>
      IF RTHeapRep.Fault(sip.si_addr) THEN RETURN; END;
    END;
    (* otherwise, use "defaultSIGSEGV" to handle the fault *)
    IF defaultSIGSEGV = LOOPHOLE(Usignal.SIG_IGN, Usignal.SignalAction) THEN RETURN;
    ELSIF defaultSIGSEGV = LOOPHOLE(Usignal.SIG_DFL, Usignal.SignalAction) THEN
      Core(sig, sip, uap);
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
        vec, ovec: Usignal.struct_sigaction;
      BEGIN
        vec.sa_flags := 0;
        vec.sa_sigaction := LOOPHOLE(Usignal.SIG_DFL, Usignal.SignalAction);
        EVAL Usignal.sigemptyset(vec.sa_mask);
        EVAL Usignal.sigaction(sig, vec, ovec);
      END;

      (* unblock signals *)
      VAR set: Uucontext.sigset_t;
      BEGIN
        EVAL Usignal.sigemptyset(set);
        EVAL Usignal.sigprocmask(Usignal.SIG_SETMASK, set);
      END;

      (* now, dump core *)
      Cstdlib.abort ();
      <* ASSERT FALSE *>
    END;
    ThreadF.ResumeOthers();
  END Core;

(* System-call faults are handled in RTHeapDepC.c *)

BEGIN
  RTMachine.RTHeapDep_Fault := LOOPHOLE(Fault, ADDRESS);
  RTMachine.RTProcedureSRC_FromPC := LOOPHOLE (RTProcedureSRC.FromPC, ADDRESS);
  VM := RTVM.VMHeap();
  AtomicWrappers := RTVM.AtomicWrappers();
  IF VM THEN
    RTMachine.RTHeapRep_Fault  := LOOPHOLE (RTHeapRep.Fault, ADDRESS);
    RTMachine.RTCSRC_FinishVM  := LOOPHOLE (RTCollectorSRC.FinishVM, ADDRESS);
  END;
END RTHeapDep.
