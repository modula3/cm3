(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Nov 21 11:28:44 PST 1994 by kalsow     *)
(*      modified on Tue May  4 18:49:28 PDT 1993 by muller     *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks, ThreadPThread;

IMPORT Usignal, Unix, RTMisc, Umman, Word, Upthread;
FROM Usignal
IMPORT sigprocmask, sigemptyset, sigaddset, SIGVTALRM, SIG_BLOCK, SIG_UNBLOCK;

PROCEDURE SP (READONLY s: State): ADDRESS =
  BEGIN
    RETURN LOOPHOLE (s.sp, ADDRESS);
  END SP;

(*--------------------------------------------------------- thread stacks ---*)

VAR page_bytes : CARDINAL := 0;
VAR stack_slop : CARDINAL;

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*)s: Stack) =
  VAR i: INTEGER; start: ADDRESS;
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
    i := Umman.mprotect (start, page_bytes, Umman.PROT_READ);
    <* ASSERT i = 0 *>
    (* The protection should be 0, but making the page read-only
       is good enough to prevent unchecked runtime errors *)

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
    Transfer (d, d);
  END FlushStackCache;

(*-------------------------------------------------- modifying the models ---*)

PROCEDURE UpdateStateForNewSP (VAR s: State; offset: INTEGER) =
  BEGIN
    INC (s.sp, offset);
    INC (s.bp, offset);
    (* Zero the return address and previous frame pointer to mark the
       thread stack end. *)
    LOOPHOLE(s.bp, UNTRACED REF Word.T)^ := 0;
    LOOPHOLE(s.bp + BYTESIZE(ADDRESS), UNTRACED REF Word.T)^ := 0;
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (<*UNUSED*> a: ADDRESS;
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
  END UpdateFrameForNewSP;

(*------------------------------------------------------ pthreads support ---*)

PROCEDURE SuspendSignal (): INTEGER =
  BEGIN
    RETURN Usignal.libc_current_sigrtmin()+7;
  END SuspendSignal;

PROCEDURE RestartSignal (): INTEGER =
  BEGIN
    RETURN Usignal.SIGXCPU;
  END RestartSignal;

PROCEDURE SuspendThread (act: Activation): BOOLEAN =
  BEGIN
    WITH r = Upthread.kill(act.handle, SuspendSignal()) DO <*ASSERT r=0*> END;
    RETURN TRUE;			 (* signalling *)
  END SuspendThread;

PROCEDURE RestartThread (act: Activation) =
  BEGIN
    WITH r = Upthread.kill(act.handle, RestartSignal()) DO <*ASSERT r=0*> END;
  END RestartThread;

(*------------------------------------ manipulating the SIGVTALRM handler ---*)

VAR
  ThreadSwitchSignal: Usignal.sigset_t;

PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler) =
  BEGIN
    EVAL Usignal.signal(SIGVTALRM, handler);
  END setup_sigvtalrm;

PROCEDURE allow_sigvtalrm () =
  VAR old: Usignal.sigset_t;
  BEGIN
    WITH i = sigprocmask(SIG_UNBLOCK, ThreadSwitchSignal, old) DO
      <* ASSERT i = 0 *>
    END;
  END allow_sigvtalrm;

PROCEDURE disallow_sigvtalrm () =
  VAR old: Usignal.sigset_t;
  BEGIN
    WITH i = sigprocmask(SIG_BLOCK, ThreadSwitchSignal, old) DO
      <* ASSERT i = 0 *>
    END;
  END disallow_sigvtalrm;

BEGIN
  WITH i = sigemptyset(ThreadSwitchSignal) DO <* ASSERT i = 0 *> END;
  WITH i = sigaddset(ThreadSwitchSignal, SIGVTALRM) DO <* ASSERT i = 0 *> END;
END RTThread.
