(* Copyright according to COPYRIGHT-CMASS. *)

UNSAFE MODULE RTThread EXPORTS RTThread, RTHooks;

IMPORT Usignal, Unix, RTMisc;
(* IMPORT Umman; later, see below *) 

CONST 
  SP_pos = 0;
  SP_copy_pos = 19;

PROCEDURE SP (READONLY s: State): ADDRESS =
  BEGIN
    RETURN LOOPHOLE (s.regs [SP_pos], ADDRESS);
  END SP;

(*--------------------------------------------------------- thread stacks ---*)

VAR page_bytes : CARDINAL := 0;
VAR stack_slop : CARDINAL;

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*)s: Stack) =
  VAR (* i: INTEGER; see below *) start: ADDRESS;
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
    (* FIXME: stack protection seems not to be as easy as this on PPC.
              Switched off until we properly implement use of memory
              protection. (Leads to crashes in XEventsQueued)
    i := Umman.mprotect (start, page_bytes, Umman.PROT_NONE);
    <* ASSERT i = 0 *>
    *)

    (* finally, set the bounds of the usable region *)
    s.first := start + page_bytes;
    s.last  := ADR (s.words[0]) + size * ADRSIZE (s.words[0]);
  END NewStack;

PROCEDURE DisposeStack (VAR s: Stack) =
  (* VAR i: INTEGER;  start := RTMisc.Align (ADR (s.words[0]), page_bytes); *)
  BEGIN
    (* find the aligned page and re-map it *)
    (* see above
    i := Umman.mprotect (start, page_bytes, Umman.PROT_READ+Umman.PROT_WRITE);
    <* ASSERT i = 0 *>
    *)

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
    INC (s.regs [SP_pos], offset);
    INC (s.regs [SP_copy_pos], offset);
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (<*UNUSED*> a: ADDRESS;
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
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

