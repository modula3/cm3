(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov 23 13:01:14 PST 1994 by kalsow    *)
(*      modified on Tue May  4 18:49:28 PDT 1993 by muller    *)
(* ow 16.09.1994 *)
(* ow 11.10.1994 *)

UNSAFE MODULE RTThread;

IMPORT Usignal, Unix, Umman, RTMisc;

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
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (<*UNUSED*> a: ADDRESS;
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
  END UpdateFrameForNewSP;

(*------------------------------------ manipulating the SIGVTALRM handler ---*)

PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler) =
  VAR sv, osv: Usignal.struct_sigvec;  i: INTEGER;
  BEGIN
    sv.sv_handler := handler;
    sv.sv_mask    := Usignal.empty_sv_mask;
    sv.sv_flags   := 0;
    i := Usignal.sigvec (Usignal.SIGVTALRM, sv, osv);
    <*ASSERT i = 0*>
  END setup_sigvtalrm;

PROCEDURE allow_sigvtalrm () =
  VAR svt : Usignal.sigset_t := Usignal.sigmask(Usignal.SIGVTALRM);
      old : Usignal.sigset_t;
      i   : INTEGER;
  BEGIN
    i := Usignal.sigprocmask(Usignal.SIG_UNBLOCK, svt, old);
    <*ASSERT i = 0 *>
  END allow_sigvtalrm;

PROCEDURE disallow_sigvtalrm () =
  VAR svt : Usignal.sigset_t := Usignal.sigmask(Usignal.SIGVTALRM);
      old : Usignal.sigset_t;
      i   : INTEGER;
  BEGIN
    i := Usignal.sigprocmask(Usignal.SIG_BLOCK, svt, old);
    <*ASSERT i = 0 *>
  END disallow_sigvtalrm;

BEGIN
END RTThread.

