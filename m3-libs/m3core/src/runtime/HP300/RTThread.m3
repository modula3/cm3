(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov 21 10:56:40 PST 1994 by kalsow    *)
(*      modified on Wed Dec 23 17:25:33 PST 1992 by jdd       *)
(*      modified on Thu Nov 12 15:56:29 PST 1992 by muller    *)

UNSAFE MODULE RTThread;

IMPORT Usignal, Word;

CONST 
  SP_pos = 12;
  FP_pos = 11;

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
  VAR i : Word.T;
  BEGIN
    i := Usignal.sigsetmask (0);
    i := Word.And (i, Word.Not (Usignal.sigmask (Usignal.SIGVTALRM)));
    EVAL Usignal.sigsetmask (i); 
  END allow_sigvtalrm;

PROCEDURE disallow_sigvtalrm () =
  VAR i : Word.T;
  BEGIN
    i := Usignal.sigsetmask (0);
    i := Word.Or (i, Usignal.sigmask (Usignal.SIGVTALRM));
    EVAL Usignal.sigsetmask (i); 
  END disallow_sigvtalrm;

BEGIN
END RTThread.


