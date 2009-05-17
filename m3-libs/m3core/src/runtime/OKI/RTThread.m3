(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov 21 11:57:12 PST 1994 by kalsow    *)
(*      modified on Wed Dec 23 15:37:36 PST 1992 by jdd       *)
(*      modified on Thu Nov 12 15:56:28 PST 1992 by muller    *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks;

CONST 
  SP_pos = 1;
  FP_pos = 2;

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

(*---------------------------------------------------------------------------*)

BEGIN
END RTThread.
