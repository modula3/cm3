(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov 21 13:17:16 PST 1994 by kalsow    *)
(*      modified on Wed Dec 23 17:24:49 PST 1992 by jdd       *)
(*      modified on Thu Nov 12 15:56:32 PST 1992 by muller    *)

UNSAFE MODULE RTThread;

IMPORT Usignal;

CONST 
  SP_pos = 1;
  FP_pos = 3;

PROCEDURE SP (READONLY s: State): ADDRESS =
  BEGIN
    RETURN LOOPHOLE (s [SP_pos], ADDRESS);
  END SP;

(*--------------------------------------------------------- thread stacks ---*)

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*)s: Stack) =
  BEGIN
    s.words := NEW (StackSpace, size);
    s.first := ADR (s.words[0]);
    s.last  := s.first + size * ADRSIZE (s.words[0]);
  END NewStack;

PROCEDURE DisposeStack (VAR s: Stack) =
  BEGIN
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

VAR
  ThreadSwitchSignal: Usignal.sigset_t;

PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler) =
  VAR sv, osv: Usignal.struct_sigaction;  i: INTEGER;
  BEGIN
    sv.sa_handler := handler;
    sv.sa_flags   := 0;
    i := Usignal.sigaction (Usignal.SIGVTALRM, sv, osv);
    <* ASSERT i = 0 *>
  END setup_sigvtalrm;

PROCEDURE allow_sigvtalrm () =
  VAR i : Usignal.sigset_t;
  BEGIN
    EVAL Usignal.sigprocmask(Usignal.SIG_UNBLOCK, ThreadSwitchSignal, i)
  END allow_sigvtalrm;

PROCEDURE disallow_sigvtalrm () =
  VAR i : Usignal.sigset_t;
  BEGIN
    EVAL Usignal.sigprocmask(Usignal.SIG_BLOCK, ThreadSwitchSignal, i)
  END disallow_sigvtalrm;

BEGIN
  (*mask_sigvtalrm(ThreadSwitchSignal); This was in the  from IBMR2 *)
END RTThread.

