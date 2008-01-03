(* Copyright (C) 1993, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(*| Last modified on Fri Jan 20 09:39:09 PST 1995 by kalsow *)
(*|      modified on Thu Jan 28 19:24:55 PST 1993 by jdd    *)

UNSAFE MODULE RTHeapDep;

IMPORT ThreadF, RTMachine, RTHeapRep, RTCollectorSRC, RTVM;
IMPORT Cstdlib, Ctypes, Umman, Unix, Uresource, Usignal, Utime, Utypes, Word;

VAR
  initialized                           := FALSE;
  defaultActionSIGSEGV: Usignal.SignalActionHandler := NIL;
  defaultSIGSEGV: Usignal.SignalHandler := NIL; (* previous handler *)

PROCEDURE Protect (p: Page; n: CARDINAL; readable, writable: BOOLEAN) =
  BEGIN
    IF NOT initialized THEN Init(); initialized := TRUE; END;
    IF NOT readable THEN writable := FALSE; END; (* processor limitation *)
    VAR prot: Ctypes.int := 0;
    BEGIN
      IF readable THEN prot := Word.Or(prot, Umman.PROT_READ); END;
      IF writable THEN prot := Word.Or(prot, Umman.PROT_WRITE); END;
      VAR
        ret := Umman.mprotect(LOOPHOLE(p * BytesPerPage, Utypes.caddr_t),
                              n * BytesPerPage, prot);
      BEGIN
        <* ASSERT ret = 0 *>
      END;
    END;
  END Protect;

(* Init establishes a handler for SIGSEGV, caused by VM faults, and for all
   other signals that cause core dumps. *)

PROCEDURE Init () =
  BEGIN
    (* sanity check *)
    VAR vmPageBytes := Unix.getpagesize();
    BEGIN
      <* ASSERT BytesPerPage >= vmPageBytes *>
      <* ASSERT BytesPerPage MOD vmPageBytes = 0 *>
    END;

    (* establish SIGSEGV handler; remember previous handler *)
    VAR
      newHandler := LOOPHOLE(Fault,Usignal.SignalActionHandler);
      vec := Usignal.struct_sigaction{
               sa_handler := newHandler, sa_mask :=
               Word.LeftShift(1, Usignal.SIGVTALRM - 1), 
               sa_flags := Usignal.SA_RESTART,
               sa_restorer := NIL};
      ovec: Usignal.struct_sigaction;
      ret, tmp: Ctypes.int;

    BEGIN
      tmp := Usignal.SA_RESTART;
      ret := Usignal.sigaction(Usignal.SIGSEGV, ADR(vec), ADR(ovec));
      <* ASSERT ret = 0 *>
      defaultActionSIGSEGV := ovec.sa_handler;
      defaultSIGSEGV := LOOPHOLE(defaultActionSIGSEGV,Usignal.SignalHandler);
    END;

    (* establish signal handler for all other signals that dump core, if no
       handler exists *)
    PROCEDURE OverrideDefault (sig: Ctypes.int) =
      VAR
        vec := Usignal.struct_sigaction{
                 sa_handler := Core, sa_mask :=
                 Word.LeftShift(1, Usignal.SIGVTALRM - 1), 
                 sa_flags := Usignal.SA_RESTART,
                 sa_restorer := NIL};
        ovec: Usignal.struct_sigaction;
        ret := Usignal.sigaction(sig, ADR(vec), ADR(ovec));
      BEGIN
        <* ASSERT ret = 0 *>
        IF ovec.sa_handler # Usignal.SIG_DFL THEN
          ret := Usignal.sigaction(sig, ADR(ovec), ADR(vec));
          <* ASSERT ret = 0 *>
        END;
      END OverrideDefault;
    BEGIN
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
      <*NOWARN*> scp : Usignal.struct_sigcontext;
                 code: Ctypes.int) =
  BEGIN
    IF RTHeapRep.Fault(LOOPHOLE(scp.cr2, ADDRESS)) THEN
      RETURN;
    END;
    IF defaultActionSIGSEGV = Usignal.SIG_IGN THEN RETURN; END;
    IF defaultActionSIGSEGV = Usignal.SIG_DFL THEN
      Core(sig);
    ELSE
      defaultSIGSEGV(sig, scp, code);
    END;
  END Fault;

(* Core is a signal handler for signals that dump core, to complete the
   current collection before dumping core.  This makes core files easier to
   debug, and avoids an Ultrix bug that creates incomplete core files if
   heap pages are read-protected. *)

VAR dumped_core := FALSE;

PROCEDURE Core (sig : Ctypes.int) =
  VAR
    ovec: Usignal.struct_sigaction;
    vec := Usignal.struct_sigaction{sa_handler := Usignal.SIG_DFL,
        sa_mask := 0, sa_flags := Usignal.SA_RESTART, sa_restorer := NIL};
  BEGIN
    ThreadF.SuspendOthers();
    IF NOT dumped_core THEN
      dumped_core := TRUE;
      EVAL RTHeapRep.Crash();      (* clean up the heap *)
      EVAL Usignal.sigaction(sig, ADR(vec), ADR(ovec)); (* establish default action *)
      EVAL Usignal.sigsetmask(0);
      (** EVAL Usignal.kill(Uprocess.getpid(), sig); (* dump core *) **)
      Cstdlib.abort (); (* dump core *)
      <* ASSERT FALSE *>
    END;
    ThreadF.ResumeOthers();
  END Core;

(* System-call faults are handled in RTHeapDepC.c *)

BEGIN
  VM := RTVM.VMHeap();
  AtomicWrappers := RTVM.AtomicWrappers();
  IF VM THEN
    RTMachine.RTHeapRep_Fault  := LOOPHOLE (RTHeapRep.Fault, ADDRESS);
    RTMachine.RTCSRC_FinishVM  := LOOPHOLE (RTCollectorSRC.FinishVM, ADDRESS);
  END;
END RTHeapDep.
