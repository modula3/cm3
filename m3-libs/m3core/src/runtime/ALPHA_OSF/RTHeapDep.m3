(* Copyright (C) 1992, 1996 Digital Equipment Corporation    *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Nov 14 14:25:40 PST 1996 by heydon   *)

UNSAFE MODULE RTHeapDep;

IMPORT RT0u, RTHeapRep, RTCollectorSRC, RTMachine;
IMPORT Cstdlib, Ctypes, Umman, Unix, Uresource, Usignal, Utime, Utypes, Word;

VAR
  (* true iff "Init" has been called *)
  initialized := FALSE;

  (* original handler for "SIGSEGV" signal; set by "Init" *)
  origSIGSEGV: Usignal.SignalHandler := NIL;

PROCEDURE Fault (sig: Ctypes.int; code: Ctypes.int;
  scp: UNTRACED REF Usignal.struct_sigcontext) =
(* Fault is called upon a SIGSEGV signal caused by a VM fault.  If
   RTHeapRep.Fault is not able to handle the fault, it invokes the
   previous action installed in "origSIGSEGV". *)
  BEGIN
    (* try handling memory fault using "RTHeapRep.Fault" *)
    (* The field "sc_traparg_a0" of a "Usignal.struct_sigcontext"
       is the virtual address that caused the fault. *)
    IF scp # NIL AND RTHeapRep.Fault(LOOPHOLE(scp.sc_traparg_a0, ADDRESS)) THEN
      RETURN
    END;
    (* otherwise, use "origSIGSEGV" to handle the fault *)
    IF origSIGSEGV = Usignal.SIG_IGN THEN
      RETURN
    ELSIF origSIGSEGV = Usignal.SIG_DFL THEN
      Core(sig, code, scp);
    ELSE
      origSIGSEGV(sig, code, scp);
    END;
  END Fault;

(* record if core is already being dumped *)
VAR dumped_core := FALSE;

PROCEDURE Core (sig: Ctypes.int; <*UNUSED*> code: Ctypes.int;
  <*UNUSED*> scp: UNTRACED REF Usignal.struct_sigcontext) =
(* Core is a signal handler for signals that dump core; it completes the
   current collection before dumping core.  This makes core files easier to
   debug, and avoids an Ultrix bug that creates incomplete core files if
   heap pages are read-protected. *)
  VAR
    (* default signal handler *)
    in_vec := Usignal.struct_sigvec{
      sv_handler := Usignal.SIG_DFL, sv_mask := 0, sv_flags := 0};
    dummy_vec: Usignal.struct_sigvec;
  BEGIN
    INC(RT0u.inCritical);
    IF NOT dumped_core THEN
      (* indicate that this thread will dump core *)
      dumped_core := TRUE;

      (* clean up the heap and install default handler *)
      EVAL RTHeapRep.Crash();
      EVAL Usignal.sigvec(sig, in_vec, (*OUT*) dummy_vec);
      EVAL Usignal.sigsetmask(0);

      (* now, dump core *)
      Cstdlib.abort ();
      <* ASSERT FALSE *>
    END;
    DEC(RT0u.inCritical);
  END Core;

PROCEDURE Init () =
(* Init establishes a handler for SIGSEGV, caused by VM faults,
   and for all other signals that cause core dumps. System-call
   faults are handled in "RTHeapDepC.c". *)
  CONST
    (* block the "SIGVTALRM" signal when signal handlers are called *)
    Mask = Word.LeftShift(1, Usignal.SIGVTALRM - 1);
  BEGIN
    (* check that "BytesPerPage" is an acceptable value *)
    VAR vmPageBytes := Unix.getpagesize(); BEGIN
      <* ASSERT BytesPerPage >= vmPageBytes *>
      <* ASSERT BytesPerPage MOD vmPageBytes = 0 *>
    END;

    (* establish SIGSEGV handler; remember previous handler *)
    VAR
      in_vec := Usignal.struct_sigvec{
        sv_handler := Fault, sv_mask := Mask, sv_flags := 0};
      out_vec: Usignal.struct_sigvec;
      ret := Usignal.sigvec(Usignal.SIGSEGV, in_vec, (*OUT*) out_vec);
    BEGIN
      <* ASSERT ret = 0 *>
      origSIGSEGV := out_vec.sv_handler;
    END;

    PROCEDURE OverrideDefaultWithCore(sig: Ctypes.int) =
    (* If no handler currently exists for signal "sig",
       install a for "sig" that dumps core. *)
      VAR
        in_vec := Usignal.struct_sigvec{
          sv_handler := Core, sv_mask := Mask, sv_flags := 0};
        out_vec: Usignal.struct_sigvec;
        ret := Usignal.sigvec(sig, in_vec, (*OUT*) out_vec);
      BEGIN
        <* ASSERT ret = 0 *>
        (* If the old handler was not the default, restore it. *)
        IF out_vec.sv_handler # Usignal.SIG_DFL THEN
          VAR dummy_vec: Usignal.struct_sigvec; BEGIN
            ret := Usignal.sigvec(sig, out_vec, (*OUT*) dummy_vec);
            <* ASSERT ret = 0 *>
          END
        END;
      END OverrideDefaultWithCore;

    BEGIN
      (* override signal handling for all signals that normally dump core *)
      OverrideDefaultWithCore(Usignal.SIGQUIT);
      OverrideDefaultWithCore(Usignal.SIGILL);
      OverrideDefaultWithCore(Usignal.SIGTRAP);
      OverrideDefaultWithCore(Usignal.SIGIOT);
      OverrideDefaultWithCore(Usignal.SIGEMT);
      OverrideDefaultWithCore(Usignal.SIGFPE);
      OverrideDefaultWithCore(Usignal.SIGBUS);
      OverrideDefaultWithCore(Usignal.SIGSYS);
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
    ret := Uresource.getrusage(Uresource.RUSAGE_SELF, (*OUT*) ADR(usage));
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
    RTMachine.RTHeapRep_Fault  := LOOPHOLE (RTHeapRep.Fault, ADDRESS);
    RTMachine.RTCSRC_FinishVM  := LOOPHOLE (RTCollectorSRC.FinishVM, ADDRESS);
  END;
END RTHeapDep.
