(* Copyright (C) 1992, 1996 Digital Equipment Corporation    *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Nov 14 14:25:40 PST 1996 by heydon   *)

UNSAFE MODULE RTHeapDep;

IMPORT RT0u, RTHeapRep, RTCollectorSRC, RTHeapDepC, Cstdlib;
IMPORT Ctypes, Umman, Unix, Uresource, Usignal, Utime, Utypes, Word, Uucontext;
FROM Usignal IMPORT SIGQUIT, SIGILL, SIGTRAP, SIGEMT, SIGFPE, SIGIOT;
FROM Usignal IMPORT SIGBUS, SIGSEGV, SIGSYS, SIGVTALRM, SIG_DFL;

VAR
  initialized := FALSE;
  orig: ARRAY [0..Usignal.NSIG] OF Usignal.struct_sigaction;

PROCEDURE Fault (sig: Ctypes.int;
                 sip: Usignal.struct_siginfo_star;
                 scp: Uucontext.struct_ucontext_star) =
(* Fault is called upon a SIGSEGV signal caused by a VM fault.  If
   RTHeapRep.Fault is not able to handle the fault, it invokes the
   previous action installed in "orig[SIGSEGV]". *)
  BEGIN
    IF (sig = SIGSEGV OR sig = SIGBUS) AND sip # NIL THEN
      IF sig = sip.si_signo THEN
        IF sip.si_addr = LOOPHOLE(scp.uc_mcontext.ss.srr0, ADDRESS) THEN
          IF RTHeapRep.Fault(LOOPHOLE(scp.uc_mcontext.es.dar, ADDRESS)) THEN
            RETURN;
          END;
        END;
      END;
    END;

    VAR old: Usignal.struct_sigaction;
    BEGIN
      (* otherwise, restore original handler and retry *)
      EVAL Usignal.sigaction(sig, orig[sig], old);
      RETURN;
    END;
  END Fault;

(* record if core is already being dumped *)
VAR dumped_core := FALSE;

PROCEDURE Core (             sig : Ctypes.int;
                <* UNUSED *> sip : Usignal.struct_siginfo_star;
                <* UNUSED *> scp : Uucontext.struct_ucontext_star) =
(* Core is a signal handler for signals that dump core; it completes the
   current collection before dumping core.  This makes core files easier to
   debug, and avoids an Ultrix bug that creates incomplete core files if
   heap pages are read-protected. *)
  BEGIN
    Cstdlib.abort();
    INC(RT0u.inCritical);
    IF NOT dumped_core THEN
      (* indicate that this thread will dump core *)
      dumped_core := TRUE;

      (* clean up the heap and install default handler *)
      EVAL RTHeapRep.Crash();

      (* establish default handler *)
      VAR old: Usignal.struct_sigaction;
      BEGIN
        EVAL Usignal.sigaction(sig, orig[sig], old);
      END;
      RETURN;
    END;
    DEC(RT0u.inCritical);
  END Core;

PROCEDURE Init () =
(* Init establishes a handler for SIGSEGV, caused by VM faults,
   and for all other signals that cause core dumps. System-call
   faults are handled in "RTHeapDepC.c". *)
  BEGIN
    (* check that "BytesPerPage" is an acceptable value *)
    VAR vmPageBytes := Unix.getpagesize(); BEGIN
      <* ASSERT BytesPerPage >= vmPageBytes *>
      <* ASSERT BytesPerPage MOD vmPageBytes = 0 *>
    END;

    (* establish SIGSEGV handler; remember previous handler *)
    VAR new: Usignal.struct_sigaction;
    BEGIN
      WITH flags = Usignal.SA_SIGINFO,
           flags = Word.Or(flags, Usignal.SA_NODEFER),
           flags = Word.Or(flags, Usignal.SA_RESTART) DO
        new.sa_flags := flags;
      END;
      new.sa_sigaction := Fault;
      WITH i = Usignal.sigemptyset(new.sa_mask) DO
        <*ASSERT i = 0*>
      END;
      (* block the "SIGVTALRM" signal when signal handlers are called *)
      WITH i = Usignal.sigaddset(new.sa_mask, SIGVTALRM) DO
        <*ASSERT i = 0*>
      END;
      WITH i = Usignal.sigaction(SIGSEGV, new, orig[SIGSEGV]) DO
        <*ASSERT i = 0*>
      END;
      WITH i = Usignal.sigaction(SIGBUS, new, orig[SIGBUS]) DO
        <*ASSERT i = 0*>
      END;
    END;

    PROCEDURE OverrideDefaultWithCore(sig: Ctypes.int) =
    (* If no handler currently exists for signal "sig",
       install a handler for "sig" that dumps core. *)
      VAR new: Usignal.struct_sigaction;
      BEGIN
        new.sa_flags := Usignal.SA_SIGINFO;
        new.sa_sigaction := Core;
        WITH i = Usignal.sigemptyset(new.sa_mask) DO
          <*ASSERT i = 0*>
        END;
        WITH i = Usignal.sigaddset(new.sa_mask, SIGVTALRM) DO
          <*ASSERT i = 0*>
        END;
        WITH i = Usignal.sigaction(sig, new, orig[sig]) DO
          <*ASSERT i = 0*>
        END;
        (* If the old handler was not the default, restore it. *)
        IF orig[sig].sa_sigaction # SIG_DFL THEN
          WITH i = Usignal.sigaction(sig, orig[sig], new) DO
            <*ASSERT i = 0*>
          END;
        END;
      END OverrideDefaultWithCore;
    BEGIN
      (* override signal handling for all signals that normally dump core *)
      OverrideDefaultWithCore(SIGQUIT);
      OverrideDefaultWithCore(SIGILL);
      OverrideDefaultWithCore(SIGTRAP);
      OverrideDefaultWithCore(SIGIOT);
      OverrideDefaultWithCore(SIGEMT);
      OverrideDefaultWithCore(SIGFPE);
      OverrideDefaultWithCore(SIGSYS);
    END;
  END Init;

PROCEDURE Protect(p: Page; n: CARDINAL; readable, writable: BOOLEAN) =
  BEGIN
    IF NOT initialized THEN Init(); initialized := TRUE; END;
    VAR prot: Ctypes.int := 0; BEGIN
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

PROCEDURE TimevalSecs(READONLY t: Utime.struct_timeval): REAL =
(* Return the number of seconds represented by "t" as a floating-
   point number. *)
  BEGIN
    RETURN FLOAT(t.tv_sec) + (FLOAT(t.tv_usec) / 1.0e6)
  END TimevalSecs;

PROCEDURE TimeUsed (): REAL =
  VAR
    usage: Uresource.struct_rusage;
    ret := Uresource.getrusage(Uresource.RUSAGE_SELF, usage);
  BEGIN
    <* ASSERT ret = 0 *>
    RETURN TimevalSecs(usage.ru_utime) + TimevalSecs(usage.ru_stime);
  END TimeUsed;

PROCEDURE VMFaultTime (): REAL =
  BEGIN
    RETURN 0.010; (* guess 10ms to handle a page fault *)
  END VMFaultTime;

BEGIN
  IF VM THEN
    RTHeapDepC.set_RTHeapRep_Fault(LOOPHOLE(RTHeapRep.Fault, ADDRESS));
    RTHeapDepC.set_RTCSRC_FinishVM(LOOPHOLE(RTCollectorSRC.FinishVM, ADDRESS));
  END;
END RTHeapDep.
