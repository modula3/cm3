(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Nov 21 11:28:44 PST 1994 by kalsow     *)
(*      modified on Tue May  4 18:49:28 PDT 1993 by muller     *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks;

IMPORT Usignal, Unix, RTMisc, Umman, Word;

PROCEDURE SP (READONLY s: State): ADDRESS =
  BEGIN
    RETURN LOOPHOLE (s.sp, ADDRESS);
  END SP;

VAR page_bytes : CARDINAL := 0;
VAR stack_slop : CARDINAL;

(*--------------------------------------------------------- thread stacks ---*)

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*)s: Stack) =
  VAR i: INTEGER; start: ADDRESS;
  BEGIN
    IF (page_bytes = 0) THEN
      page_bytes := Unix.getpagesize ();
      stack_slop := 2 * (page_bytes DIV BYTESIZE (INTEGER));
    END;

    (* allocate enough so that we're guaranteed to get a full, aligned page *)
    INC (size, stack_slop);
    s.words := NEW (StackSpace, size);
    (* 
    s.first := ADR (s.words[0]);
    s.last  := s.first + size * ADRSIZE (s.words[0]);
    *)

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
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (a: ADDRESS;
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
    (* Zero the return address and previous frame pointer to mark the
       thread stack end. *)
    LOOPHOLE(a,UNTRACED REF Word.T)^ := 0;
    LOOPHOLE(a + BYTESIZE(ADDRESS),UNTRACED REF Word.T)^ := 0;
  END UpdateFrameForNewSP;

(*------------------------------------ manipulating the SIGVTALRM handler ---*)

PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler) =
  VAR x: Usignal.struct_sigaction;
  BEGIN
    x.sa_handler := LOOPHOLE (handler, Usignal.SignalActionHandler);
    x.sa_mask := Usignal.empty_sigset_t;
    x.sa_flags := Usignal.SA_RESTART;
    EVAL Usignal.sigaction (Usignal.SIGVTALRM, ADR (x), NIL);
  END setup_sigvtalrm;

PROCEDURE allow_sigvtalrm () =
  BEGIN
    EVAL Usignal.sigprocmask(Usignal.SIG_UNBLOCK,ADR(sigvtalrmMask),NIL);
  END allow_sigvtalrm;

PROCEDURE disallow_sigvtalrm () =
  BEGIN
    EVAL Usignal.sigprocmask(Usignal.SIG_BLOCK,ADR(sigvtalrmMask),NIL);
  END disallow_sigvtalrm;

VAR
  sigvtalrmMask: Usignal.sigset_t;

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
  sigvtalrmMask.val[0] := Usignal.sigmask(Usignal.SIGVTALRM);
END RTThread.
