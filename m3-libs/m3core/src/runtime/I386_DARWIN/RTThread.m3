(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Nov 23 13:00:57 PST 1994 by kalsow                   *)
(*      modified on Tue Apr 20 16:19:54 PDT 1993 by muller                   *)

UNSAFE MODULE RTThread EXPORTS RTThread;

IMPORT Word, Usignal, Unix, RTMisc, Umman;
FROM Usignal
IMPORT sigprocmask, sigemptyset, sigaddset, SIGVTALRM, SIG_BLOCK, SIG_UNBLOCK;

CONST
  SP_pos = 2;
  FP_pos = 3;

PROCEDURE SP (READONLY s: State): ADDRESS =
  BEGIN
    RETURN LOOPHOLE (s [SP_pos], ADDRESS);
  END SP;

(*--------------------------------------------------------- thread stacks ---*)

VAR page_bytes : CARDINAL := 0;
VAR stack_slop : CARDINAL;

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*)s: Stack) =
  VAR start: ADDRESS;
  BEGIN
    IF (page_bytes = 0) THEN
      page_bytes := Unix.getpagesize ();
      stack_slop := 2 * (page_bytes DIV BYTESIZE (INTEGER));
    END;

    (* allocate enough so that we're guaranteed to get a full, aligned page *)
    INC (size, stack_slop);
    s.words := NEW (StackSpace, size);

    (* find the aligned page and unmap it *)
    start := RTMisc.Align (ADR (s.words[0]), page_bytes);
    WITH i = Umman.mprotect (start, page_bytes, Umman.PROT_NONE) DO
      <* ASSERT i = 0 *>
    END;

    (* finally, set the bounds of the usable region *)
    s.first := start + page_bytes;
    s.last  := ADR (s.words[0]) + size * ADRSIZE (s.words[0]);
  END NewStack;

PROCEDURE DisposeStack (VAR s: Stack) =
  VAR start := RTMisc.Align (ADR (s.words[0]), page_bytes);
  BEGIN
    (* find the aligned page and re-map it *)
    WITH i = Umman.mprotect (start, page_bytes,
                             Word.Or(Umman.PROT_READ, Umman.PROT_WRITE)) DO
      <* ASSERT i = 0 *>
    END;

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
    INC (s [SP_pos], offset);
    INC (s [FP_pos], offset);
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (<*UNUSED*> a: ADDRESS;
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
  END UpdateFrameForNewSP;

(*------------------------------------ manipulating the SIGVTALRM handler ---*)

VAR ThreadSwitchSignal: Usignal.sigset_t;

PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler) =
  VAR old := Usignal.signal(Usignal.SIGVTALRM, handler);
  BEGIN
    <*ASSERT old # LOOPHOLE(Usignal.SIG_ERR,Usignal.SignalHandler) *>
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
  WITH i = sigaddset(ThreadSwitchSignal, SIGVTALRM) DO <* ASSERT i=0 *> END;
END RTThread.
