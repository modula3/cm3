(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Nov 23 13:00:57 PST 1994 by kalsow                   *)
(*      modified on Tue Apr 20 16:19:54 PDT 1993 by muller                   *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks;

IMPORT Word, Usignal, Unix, Umman, RTMisc;

CONST 
  SP_pos = 34;
  FP_pos = 19;

PROCEDURE SP (READONLY s: State): ADDRESS =
  BEGIN
    RETURN LOOPHOLE (s [SP_pos], ADDRESS);
  END SP;

(*--------------------------------------------------------- thread stacks ---*)

VAR page_bytes : CARDINAL := 0;
VAR stack_slop : CARDINAL;

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*)s: Stack) =
  VAR i: INTEGER;  start: ADDRESS;
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
    i := Umman.mprotect (start, page_bytes, Umman.PROT_READ);
    <* ASSERT i = 0 *>
    (* The protection should be 0, but a bug in MIPS/Ultrix 4.2 (vmdup)
       causes kernel panics when it is.  Making the page read-only 
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
    INC (s [SP_pos], offset);
    INC (s [FP_pos], offset);
    (* NOTE: unconditionally updating the frame pointer is a hack.
       Each procedure decides independently if r15 is used as a frame pointer.
       For the critical procedure, Thread.DetermineContext, it appears
       that the current gcc-based back-end does use r15 as a frame pointer. *)
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (<*UNUSED*> a: ADDRESS; 
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
  END UpdateFrameForNewSP;

(*------------------------------------ manipulating the SIGVTALRM handler ---*)

VAR
  ThreadSwitchSignal: Usignal.sigset_t;

PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler) =
  VAR new, old: Usignal.struct_sigaction;
  BEGIN
    new.sa_handler := handler;
    new.sa_flags := Word.Or(Usignal.SA_RESTART, Usignal.SA_SIGINFO);
    WITH i = Usignal.sigemptyset(new.sa_mask) DO
      <* ASSERT i = 0 *>
    END;
    WITH i = Usignal.sigaction (Usignal.SIGVTALRM, new, old) DO
      <* ASSERT i = 0 *>
    END;
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

(*--------------------------------------------- exception handling support --*)

PROCEDURE GetCurrentHandlers (): ADDRESS=
  BEGIN
    RETURN handlerStack;
  END GetCurrentHandlers;

PROCEDURE SetCurrentHandlers (h: ADDRESS)=
  BEGIN
    handlerStack := h;
  END SetCurrentHandlers;

(*RTHooks.PushEFrame*)
PROCEDURE PushEFrame (frame: ADDRESS) =
  TYPE Frame = UNTRACED REF RECORD next: ADDRESS END;
  VAR f := LOOPHOLE (frame, Frame);
  BEGIN
    f.next := handlerStack;
    handlerStack := f;
  END PushEFrame;

(*RTHooks.PopEFrame*)
PROCEDURE PopEFrame (frame: ADDRESS) =
  BEGIN
    handlerStack := frame;
  END PopEFrame;

BEGIN
  WITH i = Usignal.sigemptyset(ThreadSwitchSignal) DO
    <* ASSERT i = 0 *>
  END;
  WITH i = Usignal.sigaddset(ThreadSwitchSignal, Usignal.SIGVTALRM) DO
    <* ASSERT i = 0 *>
  END;
END RTThread.

