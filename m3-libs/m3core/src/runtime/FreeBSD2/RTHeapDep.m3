(* Copyright (C) 1994, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(*| Last modified on Fri Jan 20 09:41:09 PST 1995 by kalsow      *)
(*|      modified on Mon Oct 31 00:04:38 MET 1994 by Olaf Wagner *)
(*|      modified on Thu Jan 28 19:24:55 PST 1993 by jdd         *)

UNSAFE MODULE RTHeapDep;

IMPORT RT0u, RTMachine, RTHeapRep, RTCollectorSRC;
IMPORT Cstdlib, Ctypes, Umman, Unix, Uresource, Usignal, Utypes, Word;

VAR
  initialized                           := FALSE;
  defaultSIGSEGV: Usignal.SignalHandler := NIL; (* previous handler *)
  defaultSIGBUS:  Usignal.SignalHandler := NIL; (* previous handler *)

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
        (*
        <* ASSERT ret = n * BytesPerPage *>
        *)
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
      vec := Usignal.struct_sigvec{
               sv_handler := Fault, sv_mask :=
               Word.LeftShift(1, Usignal.SIGVTALRM - 1), sv_flags := 0};
      ovec: Usignal.struct_sigvec;
      ret := Usignal.sigvec(Usignal.SIGSEGV, vec, ovec);
      vecb := Usignal.struct_sigvec{
               sv_handler := Fault, sv_mask :=
               Word.LeftShift(1, Usignal.SIGVTALRM - 1), sv_flags := 0};
      ovecb: Usignal.struct_sigvec;
      retb := Usignal.sigvec(Usignal.SIGBUS, vecb, ovecb);
    BEGIN
      <* ASSERT ret = 0 *>
      <* ASSERT retb = 0 *>
      defaultSIGSEGV := ovec.sv_handler;
      defaultSIGBUS := ovecb.sv_handler;
    END;

    (* establish signal handler for all other signals that dump core, if no
       handler exists *)
    PROCEDURE OverrideDefault (sig: Ctypes.int) =
      VAR
        vec := Usignal.struct_sigvec{
                 sv_handler := Core, sv_mask :=
                 Word.LeftShift(1, Usignal.SIGVTALRM - 1), sv_flags := 0};
        ovec: Usignal.struct_sigvec;
        ret                         := Usignal.sigvec(sig, vec, ovec);
      BEGIN
        <* ASSERT ret = 0 *>
        IF ovec.sv_handler # Usignal.SIG_DFL THEN
          ret := Usignal.sigvec(sig, ovec, vec);
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
      OverrideDefault(Usignal.SIGSYS);
    END;
  END Init;

(* Fault is called upon a SIGSEGV signal, caused by a VM fault.  If
   RTHeapRep.Fault is not able to handle the fault, it invokes the previous
   action. *)

PROCEDURE Fault (sig : Ctypes.int;
                 code: Ctypes.int;
                 scp : UNTRACED REF Usignal.struct_sigcontext) =
  VAR sf_addr_addr: UNTRACED REF ARRAY[0..1] OF ADDRESS;
      sf_addr     : ADDRESS;
  (*
   * Signal frame of FreeBSD 1.1.5
   *
   *  struct sigframe {
   *    int     sf_signum;
   *    int     sf_code;
   *    struct  sigcontext *sf_scp;
   *    char    *sf_addr;            <-- this is the faulting address
   *    sig_t   sf_handler;
   *    struct  sigcontext sf_sc;    <-- this address is passed in scp
   *   };
   *)
                                                    
  BEGIN
    sf_addr_addr := scp - ( 2 * BYTESIZE(ADDRESS));
    sf_addr      := sf_addr_addr[0];
    IF scp # NIL AND RTHeapRep.Fault(sf_addr) THEN
      RETURN;
    END;
    IF defaultSIGSEGV = Usignal.SIG_IGN THEN RETURN; END;
    IF defaultSIGSEGV = Usignal.SIG_DFL THEN
      Core(sig, code, scp);
    ELSE
      defaultSIGSEGV(sig, code, scp);
    END;
  END Fault;

(* Core is a signal handler for signals that dump core, to complete the
   current collection before dumping core.  This makes core files easier to
   debug, and avoids an Ultrix bug that creates incomplete core files if
   heap pages are read-protected. *)

VAR dumped_core := FALSE;

PROCEDURE Core (             sig : Ctypes.int;
                <* UNUSED *> code: Ctypes.int;
                <* UNUSED *> scp : UNTRACED REF Usignal.struct_sigcontext) =
  VAR
    ovec: Usignal.struct_sigvec;
    vec := Usignal.struct_sigvec{sv_handler := Usignal.SIG_DFL,
                                 sv_mask := 0, sv_flags := 0};
  BEGIN
    INC(RT0u.inCritical);
    IF NOT dumped_core THEN
      dumped_core := TRUE;
      EVAL RTHeapRep.Crash();      (* clean up the heap *)
      EVAL Usignal.sigvec(sig, vec, ovec); (* establish default action *)
      EVAL Usignal.sigsetmask(0);
      (** EVAL Usignal.kill(Uprocess.getpid(), sig); (* dump core *) **)
      Cstdlib.abort (); (* dump core *)
      <* ASSERT FALSE *>
    END;
    DEC(RT0u.inCritical);
  END Core;

(* System-call faults are handled in RTHeapDepC.c *)

PROCEDURE TimeUsed (): REAL =
  VAR usage: Uresource.struct_rusage;
  BEGIN
    VAR ret := Uresource.getrusage(Uresource.RUSAGE_SELF, ADR(usage));
    BEGIN
      <* ASSERT ret # -1 *>
    END;
    RETURN (FLOAT(usage.ru_utime.tv_sec)
              + FLOAT(usage.ru_utime.tv_usec) / 1000000.0)
             + (FLOAT(usage.ru_utime.tv_sec)
                  + FLOAT(usage.ru_utime.tv_usec) / 1000000.0);
  END TimeUsed;

PROCEDURE VMFaultTime (): REAL =
  BEGIN
    RETURN 0.010;                (* guess 10ms to handle a page fault *)
  END VMFaultTime;

BEGIN
  IF VM THEN
    RTMachine.RTHeapRep_Fault  := LOOPHOLE (RTHeapRep.Fault, ADDRESS);
    RTMachine.RTCSRC_FinishVM  := LOOPHOLE (RTCollectorSRC.FinishVM, ADDRESS);
  END;
END RTHeapDep.
