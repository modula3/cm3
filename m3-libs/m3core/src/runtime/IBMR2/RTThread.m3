(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov 21 11:08:09 PST 1994 by kalsow    *)
(*      modified on Wed Dec 23 17:24:54 PST 1992 by jdd       *)
(*      modified on Thu Nov 12 10:50:07 PST 1992 by muller    *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks;

IMPORT Usignal;

CONST 
  SP_pos = 3;
  SP_copy_pos = 23;  (* gcc uses r31 for stack ops *)

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
  BEGIN
  END FlushStackCache;

(*-------------------------------------------------- modifying the models ---*)

PROCEDURE UpdateStateForNewSP (VAR s: State; offset: INTEGER) =
  BEGIN
    INC (s [SP_pos], offset);
    INC (s [SP_copy_pos], offset);
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (<*UNUSED*> a: ADDRESS; 
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
  END UpdateFrameForNewSP;

(*------------------------------------ manipulating the SIGVTALRM handler ---*)

VAR
  ThreadSwitchSignal: Usignal.sigset_t;

PROCEDURE mask_sigvtalrm (VAR i: Usignal.sigset_t) =
  BEGIN
    IF Usignal.SIGVTALRM <= 32 THEN
      i.losigs := Usignal.sigmask (Usignal.SIGVTALRM);
      i.hisigs := 0
    ELSE
      i.losigs := 0;
      i.hisigs := Usignal.sigmask (Usignal.SIGVTALRM-32)
    END
  END mask_sigvtalrm;
  
PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler) =
  VAR sa, osa: Usignal.struct_sigaction;  i: INTEGER;
  BEGIN
    sa.sa_handler := handler;
    sa.sa_mask    := Usignal.empty_sigset_t;
    sa.sa_flags   := Usignal.SA_RESTART;
    i := Usignal.sigaction (Usignal.SIGVTALRM, sa, osa);
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
  mask_sigvtalrm(ThreadSwitchSignal);
END RTThread.


