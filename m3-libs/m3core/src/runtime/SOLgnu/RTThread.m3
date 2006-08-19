(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking   *)
(*      modified on Mon Nov 21 13:12:29 PST 1994 by kalsow    *)
(*      modified on Wed Dec 23 17:24:49 PST 1992 by jdd       *)
(*      modified on Thu Nov 12 15:56:32 PST 1992 by muller    *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks;

IMPORT Uframe, Usignal, Unix, Umman, Uucontext, RTMisc;

PROCEDURE SP (READONLY s: State): ADDRESS =
  BEGIN
    RETURN LOOPHOLE (s.uc_mcontext.gregs.sp, ADDRESS);
  END SP;

(*--------------------------------------------------------- thread stacks ---*)

VAR page_bytes : CARDINAL := 0;
VAR stack_slop : CARDINAL;

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*)s: Stack) =
  VAR i: INTEGER;  start: ADDRESS;
  BEGIN
    IF page_bytes = 0 THEN
      page_bytes := Unix.getpagesize ();
      stack_slop := 2 * (page_bytes DIV BYTESIZE (INTEGER));
    END;

    (* allocate enough so that we're guaranteed to get a full, aligned page *)
    INC (size, stack_slop);
    s.words := NEW (StackSpace, size);

    (* find the aligned page and unmap it *)
    start := RTMisc.Align (ADR (s.words[0]), page_bytes);
    i := Umman.mprotect (start, page_bytes, Umman.PROT_NONE);
    <* ASSERT i = 0 *>
    (* finally, set the bounds of the usable region *)
    s.first := start + page_bytes;
    s.last  := ADR (s.words[0]) + size * ADRSIZE (s.words[0]);
  END NewStack;

PROCEDURE DisposeStack (VAR s: Stack) =
  VAR i: INTEGER;  start := RTMisc.Align (ADR (s.words[0]), page_bytes);
  BEGIN
    (* find the aligned page and re-map it *)
    i := Umman.mprotect (start, page_bytes, Umman.PROT_READ+Umman.PROT_WRITE);
    <* ASSERT i = 0 *>

    (* and finally, free the storage *)
    DISPOSE (s.words);
    s.words := NIL;
    s.first := NIL;
    s.last  := NIL;
  END DisposeStack;

PROCEDURE FlushStackCache () =
  VAR d: State;
  BEGIN
    EVAL Save(d);
  END FlushStackCache;

(*-------------------------------------------------- modifying the models ---*)

PROCEDURE UpdateStateForNewSP (VAR s: State; offset: INTEGER) =
  BEGIN
    INC (s.uc_mcontext.gregs.sp, offset);
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (a: ADDRESS; <*UNUSED*> offset: INTEGER) =
  VAR f := LOOPHOLE(a, Uframe.struct_frame_star);
  BEGIN
    INC (f.fr_savfp, offset);
    f.fr_savpc := 0;
  END UpdateFrameForNewSP;

(*------------------------------------ manipulating the SIGVTALRM handler ---*)

VAR
  ThreadSwitchSignal: Uucontext.sigset_t;

PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler) =
  VAR sv, osv: Usignal.struct_sigaction;  i: INTEGER;
  BEGIN
    sv.sa_sigaction := LOOPHOLE(handler, Usignal.SignalAction);
    sv.sa_flags := Usignal.SA_RESTART;
    i := Usignal.sigemptyset (sv.sa_mask);
    <* ASSERT i = 0 *>
    i := Usignal.sigaction (Usignal.SIGVTALRM, sv, osv);
    <* ASSERT i = 0 *>
  END setup_sigvtalrm;

PROCEDURE allow_sigvtalrm () =
  BEGIN
    WITH i = Usignal.sigprocmask(Usignal.SIG_UNBLOCK, ThreadSwitchSignal) DO
      <* ASSERT i = 0 *>
    END;
  END allow_sigvtalrm;

PROCEDURE disallow_sigvtalrm () =
  BEGIN
    WITH i = Usignal.sigprocmask(Usignal.SIG_BLOCK, ThreadSwitchSignal) DO
      <* ASSERT i = 0 *>
    END;
  END disallow_sigvtalrm;

BEGIN
  WITH i = Usignal.sigemptyset(ThreadSwitchSignal) DO
    <* ASSERT i = 0 *>
  END;
  WITH i = Usignal.sigaddset(ThreadSwitchSignal, Usignal.SIGVTALRM) DO
    <* ASSERT i = 0 *>
  END;
END RTThread.
