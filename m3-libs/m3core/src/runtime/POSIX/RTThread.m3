(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

UNSAFE MODULE RTThread EXPORTS RTThread;

IMPORT Unix, RTMisc, Umman;

PROCEDURE SP (<*UNUSED*> READONLY s: State): ADDRESS =
  BEGIN
     <* ASSERT(FALSE) *>
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
    i := Umman.mprotect (start, page_bytes, Umman.PROT_NONE);
    <* ASSERT i = 0 *>

    (* finally, set the bounds of the usable region *)
    s.first := start + page_bytes;
    s.last  := ADR (s.words[0]) + size * ADRSIZE (s.words[0]);
  END NewStack;

PROCEDURE DisposeStack (VAR s: Stack) =
  VAR i: INTEGER;  start := RTMisc.Align (ADR (s.words[0]), page_bytes);
  BEGIN
    (* find the aligned page and re-map it *)
    i := Umman.mprotect (start, page_bytes, Umman.PROT_READ + Umman.PROT_WRITE);
    <* ASSERT i = 0 *>

    (* and finally, free the storage *)
    DISPOSE (s.words);
    s.words := NIL;
    s.first := NIL;
    s.last  := NIL;
  END DisposeStack;

PROCEDURE FlushStackCache () =
  (*VAR d: State;*)
  BEGIN
    (*Transfer (d, d);*)
     <* ASSERT(FALSE) *>
  END FlushStackCache;

(*-------------------------------------------------- modifying the models ---*)

PROCEDURE UpdateStateForNewSP (<*UNUSED*> VAR s: State; <*UNUSED*> offset: INTEGER) =
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
