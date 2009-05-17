(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

UNSAFE MODULE RTThread EXPORTS RTThread;

PROCEDURE SP (<*UNUSED*> READONLY s: State): ADDRESS =
  BEGIN
     <* ASSERT(FALSE) *>
  END SP;

(*--------------------------------------------------------- thread stacks ---*)

PROCEDURE NewStack (<*UNUSED*> size: INTEGER;
                    <*UNUSED*> VAR(*OUT*)s: Stack) =
  BEGIN
     <* ASSERT(FALSE) *>
  END NewStack;

PROCEDURE DisposeStack (<*UNUSED*> VAR s: Stack) =
  BEGIN
    <* ASSERT(FALSE) *>
  END DisposeStack;

PROCEDURE FlushStackCache () =
  (*VAR d: State;*)
  BEGIN
    (*Transfer (d, d);*)
    <* ASSERT(FALSE) *>
  END FlushStackCache;

(*-------------------------------------------------- modifying the models ---*)

PROCEDURE UpdateStateForNewSP (<*UNUSED*> VAR s: State;
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
    <* ASSERT(FALSE) *>
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (<*UNUSED*> a: ADDRESS;
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
    <* ASSERT(FALSE) *>
  END UpdateFrameForNewSP;

(*---------------------------------------------------------------------------*)

BEGIN
END RTThread.
