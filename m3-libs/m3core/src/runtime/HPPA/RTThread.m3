(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks;

IMPORT Word, Usignal, Unix, Umman, RTMisc;

PROCEDURE SP (READONLY s: State): ADDRESS =
  BEGIN
    RETURN LOOPHOLE (s.SP, ADDRESS);
  END SP;

(*--------------------------------------------------------- thread stacks ---*)

VAR page_bytes : CARDINAL := 0;
VAR stack_slop : CARDINAL;

TYPE
  Header = RECORD (* for an open array of integer *)
    heap_hdr   : RT0.RefHeader;
    data_start : ADDRESS;
    n_words    : INTEGER;
  END;

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*)s: Stack) =
  CONST
    NEWMEM    = Umman.MAP_PRIVATE + Umman.MAP_VARIABLE + Umman.MAP_ANONYMOUS;
    READWRITE = Umman.PROT_READ + Umman.PROT_WRITE;
  VAR
    i       : INTEGER;
    n_bytes : INTEGER;
    start   : ADDRESS;
    base    : ADDRESS;
    hdr     : UNTRACED REF Header;
  BEGIN
    IF (page_bytes = 0) THEN
      page_bytes := Unix.getpagesize ();
      stack_slop := (2 * page_bytes + BYTESIZE (Header)) DIV BYTESIZE (INTEGER);
    END;

    (* allocate enough so that we're guaranteed to get a full, aligned page *)
    INC (size, stack_slop);
    n_bytes := size * BYTESIZE (INTEGER);

    (* on HPUX you must use mmap() to acquire memory that mprotect() can use... *)
    base := mmap (NIL, n_bytes, READWRITE, NEWMEM, 0, 0);
    <* ASSERT base # LOOPHOLE (-1, ADDRESS) *>

    (* build the open array descriptor *)
    hdr           := base;
    hdr.heap_hdr  := RT0.RefHeader {};
    hdr.heap_hdr.typecode := TYPECODE (StackSpace);
    hdr.data_addr := base + ADRSIZE (Header);
    hdr.n_words   := (n_bytes - BYTESIZE (Header)) DIV BYTESIZE (INTEGER);
    s.words       := ADR (hdr.data_addr);
    
    (* find the last aligned page *)
    start := RTMisc.Align (base + n_bytes, page_bytes);
    WHILE (start + page_bytes > base + n_bytes) DO
      DEC (start, page_bytes);
    END;

    (* and turn it into a stack guard page *)
    i := Umman.mprotect (start, page_bytes, Umman.PROT_NONE);
    <* ASSERT i = 0 *>

    (* finally, set the bounds of the usable region *)
    s.first := ADR (s.words[0]);
    s.last  := start;
  END NewStack;

PROCEDURE DisposeStack (VAR s: Stack) =
  VAR
    i       : INTEGER;
    base    := ADR (s.words[0]) - ADRSIZE (Header);
    n_bytes := BYTESIZE (Header) + NUMBER (s.words^) * BYTESIZE (INTEGER);
  BEGIN
    (* free the storage *)
    i := munmap (base, n_bytes);
    <* ASSERT i = 0 *>

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
    INC (s.SP, offset);
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
    <* ASSERT i = 0 *>
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
END RTThread.

