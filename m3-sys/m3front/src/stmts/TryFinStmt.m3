(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TryFinStmt.m3                                         *)
(* Last modified on Fri May 19 07:50:09 PDT 1995 by kalsow     *)
(*      modified on Thu Dec  5 17:19:13 PST 1991 by muller     *)

MODULE TryFinStmt;

IMPORT M3ID, CG, Token, Scanner, Stmt, StmtRep, Marker, Target, Type, Addr;
IMPORT RunTyme, Procedure, ProcBody, M3RT, Scope, Fmt, Host, TryStmt, Module;
IMPORT Jmpbufs;
IMPORT MSIR, MSIRBuilder, CaptureAnalysis;
FROM Stmt IMPORT Outcome;

TYPE
  P = Stmt.T OBJECT
        body     : Stmt.T;
        finally  : Stmt.T;
        forigin  : INTEGER;
        viaProc  : BOOLEAN;
        scope    : Scope.T;
        handler  : HandlerProc;
        jmpbufs  : Jmpbufs.Try;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
        compileMSIR := CompileMSIR;
        capture  := Capture;
      END;

TYPE
  HandlerProc = ProcBody.T OBJECT
    self: P;
    activation: CG.Var;
    jmpbufs : Jmpbufs.Proc;
  OVERRIDES
    gen_decl := EmitDecl;
    gen_body := EmitBody;
  END;

VAR
  last_name : INTEGER := 0;
  next_uid  : INTEGER := 0;

PROCEDURE Parse (body: Stmt.T;  ): Stmt.T =
  TYPE TK = Token.T;
  VAR p := NEW (P);
  BEGIN
    StmtRep.Init (p);
    p.body := body;
    Scanner.Match (TK.tFINALLY);
    p.forigin := Scanner.offset;
    IF Target.Has_stack_walker THEN
      p.viaProc := FALSE;
      p.scope   := NIL;
      p.finally := Stmt.Parse ();
    ELSE
      p.handler := NEW (HandlerProc, self := p);
      ProcBody.Push (p.handler);
      p.scope := Scope.PushNew (TRUE, M3ID.NoID);
      p.finally := Stmt.Parse ();
      Scope.PopNew ();
      ProcBody.Pop ();
    END;
    Scanner.Match (TK.tEND);
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR zz: Scope.T;  oc: Stmt.Outcomes;  name: INTEGER;
  BEGIN
    Jmpbufs.CheckTry (cs.jmpbufs, p.jmpbufs);
    Marker.PushFinally (CG.No_label, CG.No_label, CG.No_label, NIL);
    Stmt.TypeCheck (p.body, cs);
    Marker.Pop ();
    TryStmt.PushHandler (NIL, 0, FALSE);
    IF Target.Has_stack_walker THEN
      Stmt.TypeCheck (p.finally, cs);
    ELSE
      oc := Stmt.GetOutcome (p.finally);
      IF (Stmt.Outcome.Exits IN oc) OR (Stmt.Outcome.Returns IN oc) THEN
        p.viaProc := FALSE;
        Stmt.TypeCheck (p.finally, cs);
      ELSE
        p.viaProc := TRUE;
        name := p.forigin MOD 10000;
        p.handler.name := HandlerName (name);
        IF (name = last_name) THEN
          INC (next_uid);
          p.handler.name := p.handler.name & "_" & Fmt.Int (next_uid);
        ELSE
          last_name := name;
          next_uid := 0;
        END;
        zz := Scope.Push (p.scope);
          p.handler.jmpbufs := Jmpbufs.CheckProcPush (cs.jmpbufs,
                                                      M3ID.Add (p.handler.name));
          Scope.TypeCheck (p.scope, cs);
          Stmt.TypeCheck (p.finally, cs);
          Jmpbufs.CheckProcPop (cs.jmpbufs, p.handler.jmpbufs);
        Scope.Pop (zz);
      END;
    END;
    TryStmt.PopHandler ();
  END Check;

PROCEDURE HandlerName (uid: INTEGER): TEXT =
  CONST Insert = ARRAY BOOLEAN OF TEXT { "_M3_LINE_", "_I3_LINE_" };
  BEGIN
    RETURN M3ID.ToText (Module.Name (NIL))
           & Insert [Module.IsInterface ()]
           & Fmt.Int (uid);
  END HandlerName;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  BEGIN
    IF Target.Has_stack_walker THEN RETURN Compile1 (p);
    ELSIF p.viaProc            THEN RETURN Compile2 (p);
    ELSE                            RETURN Compile3 (p);
    END;
  END Compile;

PROCEDURE Compile1 (p: P): Stmt.Outcomes =
  VAR
    oc, xc, o: Stmt.Outcomes;
    lab, xx: CG.Label;
    info: CG.Var;
    proc: Procedure.T;
    returnSeen, exitSeen : BOOLEAN;
    catches := ARRAY[0..0] OF CG.TypeUID{0};
  BEGIN
    (* declare and initialize the info record *)
    info := CG.Declare_local (M3ID.NoID, Target.Address.size, Target.Address.align,
                              CG.Type.Addr, 0, in_memory := TRUE,
                              up_level := FALSE, f := CG.Never);
    CG.Load_nil ();
    CG.Store_addr (info, M3RT.EA_exception);

    (* compile the body *)
    lab := CG.Next_label (4);
    CG.Set_label (lab, barrier := TRUE);
    CG.Start_try ();

    Marker.PushFinally (lab, lab+1, lab+2, info);
    Marker.SaveFrame ();
      oc := Stmt.Compile (p.body);
    Marker.PopFinally (returnSeen, exitSeen);

    CG.Jump (lab+2);

    (* End the try block before the landing pad so that the finally handler
       code (which follows at lab+2) is OUTSIDE the inner try region.  This
       prevents exceptions raised inside the finally clause (e.g. by
       ResumeRaiseEx) from being re-caught by this same catch, which would
       create an infinite loop.  Any such exceptions propagate naturally to
       an enclosing outer try block instead. *)
    CG.End_try ();

    CG.Set_label (lab+1);
    CG.Landing_pad(lab+1, catches);
    CG.Store_addr (info);
    CG.Set_label (lab+2);

    (* set the "Compiler.ThisException()" globals *)
    TryStmt.PushHandler (info, 0, direct := FALSE);

    (* compile the handler *)
    Scanner.offset := p.forigin;
    CG.Gen_location (p.forigin);
      xc := Stmt.Compile (p.finally);

    IF (Outcome.FallThrough IN xc) THEN
      (* exceptional outcome? *)
      CG.Load_addr (info, M3RT.EA_exception, Target.Address.align);
      CG.Load_nil ();
      CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, lab+3, CG.Always);

      IF (exitSeen) THEN
        xx := CG.Next_label ();
        CG.Load_addr (info, M3RT.EA_exception, Target.Address.align);
        CG.Loophole (CG.Type.Addr, Target.Integer.cg_type );

        CG.Load_intt (Marker.Exit_exception);
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, xx, CG.Always);
        Marker.EmitExit ();
        CG.Set_label (xx);
      END;

      IF (returnSeen) THEN
        xx := CG.Next_label ();
        CG.Load_addr (info, M3RT.EA_exception, Target.Address.align);
        CG.Loophole (CG.Type.Addr, Target.Integer.cg_type );

        CG.Load_intt (Marker.Return_exception);
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, xx, CG.Always);
        Marker.EmitReturn (NIL, fromFinally := TRUE);
        CG.Set_label (xx);
      END;

      (* resume the exception *)
      proc := RunTyme.LookUpProc (RunTyme.Hook.ResumeRaiseEx);
      Procedure.StartCall (proc);
      CG.Load_addr (info, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
      Procedure.EmitCall (proc);
      CG.Set_label (lab+3, barrier := TRUE);
    END;

    (* restore the "Compiler.ThisException()" globals *)
    TryStmt.PopHandler ();

    o := Stmt.Outcomes {};
    IF Outcome.FallThrough IN xc THEN o := oc END;
    IF Outcome.Exits IN xc   THEN o := o + Stmt.Outcomes {Outcome.Exits} END;
    IF Outcome.Returns IN xc THEN o := o + Stmt.Outcomes {Outcome.Returns} END;
    RETURN o;
  END Compile1;

PROCEDURE Compile2 (p: P): Stmt.Outcomes =
  VAR
    oc, xc, o: Stmt.Outcomes;
    lab: CG.Label;
    frame: CG.Var;
  BEGIN
    <*ASSERT p.viaProc*>

    (* declare and initialize the info record *)
    frame := CG.Declare_local (M3ID.NoID, M3RT.EF2_SIZE, Target.Address.align,
                               CG.Type.Struct, 0, in_memory := TRUE,
                               up_level := FALSE, f := CG.Never);
    CG.Load_procedure (p.handler.cg_proc);
    CG.Store_addr (frame, M3RT.EF2_handler);
    CG.Load_static_link (p.handler.cg_proc);
    CG.Store_addr (frame, M3RT.EF2_frame);

    (* compile the body *)
    lab := CG.Next_label (2);
    CG.Set_label (lab, barrier := TRUE);
    Marker.PushFrame (frame, M3RT.HandlerClass.FinallyProc);
    Marker.PushFinallyProc (lab, lab+1, frame, p.handler.cg_proc, p.handler.level);
      oc := Stmt.Compile (p.body);
    Marker.Pop ();
    IF (Outcome.FallThrough IN oc) THEN
      Marker.PopFrame (frame);
      CG.Start_call_direct (p.handler.cg_proc, p.handler.level, CG.Type.Void);
      (* Shouldn't we pass the activation parameter here?
         What value do we pass? *)
      CG.Call_direct (p.handler.cg_proc, CG.Type.Void);
    END;
    CG.Set_label (lab+1, barrier := TRUE);

    (* set the "Compiler.ThisException()" globals *)
    TryStmt.PushHandler (p.handler.activation, 0, direct := FALSE);

    Scanner.offset := p.forigin;
    CG.Gen_location (p.forigin);
    IF (Host.inline_nested_procs) THEN
      CG.Begin_procedure (p.handler.cg_proc);
      Jmpbufs.CompileProcAllocateJmpbufs (p.handler.jmpbufs);
      xc := Stmt.Compile (p.finally);
      CG.Exit_proc (CG.Type.Void);
      CG.End_procedure (p.handler.cg_proc);
    ELSE
      CG.Note_procedure_origin (p.handler.cg_proc);
      xc := Stmt.GetOutcome (p.finally);
    END;

    (* restore the "Compiler.ThisException()" globals *)
    TryStmt.PopHandler ();

    o := Stmt.Outcomes {};
    IF Outcome.FallThrough IN xc THEN o := oc END;
    IF Outcome.Exits IN xc   THEN o := o + Stmt.Outcomes {Outcome.Exits} END;
    IF Outcome.Returns IN xc THEN o := o + Stmt.Outcomes {Outcome.Returns} END;
    RETURN o;
  END Compile2;

PROCEDURE EmitDecl (x: HandlerProc) =
  VAR p := x.self;  par: CG.Proc := NIL;
  BEGIN
    IF (p.viaProc) THEN
      IF (x.parent # NIL) THEN par := x.parent.cg_proc; END;
      x.cg_proc := CG.Declare_procedure (M3ID.Add (x.name), 1, CG.Type.Void,
                                         x.level, Target.DefaultCall,
                                         exported := FALSE, parent := par);
      x.activation := CG.Declare_param (M3ID.NoID, Target.Address.size,
                                        Target.Address.align, CG.Type.Addr,
                                        Type.GlobalUID (Addr.T),
                                        in_memory := FALSE, up_level := FALSE,
                                        f := CG.Always);
    END;
  END EmitDecl;

PROCEDURE EmitBody (x: HandlerProc) =
  VAR p := x.self;
  BEGIN
    IF (p.viaProc) AND (NOT Host.inline_nested_procs) THEN

      (* set the "Compiler.ThisException()" globals *)
      TryStmt.PushHandler (x.activation, 0, direct := FALSE);

      (* generate the actual procedure *)
      Scanner.offset := p.forigin;
      CG.Gen_location (p.forigin);
      CG.Begin_procedure (x.cg_proc);
      Jmpbufs.CompileProcAllocateJmpbufs (x.jmpbufs);
      EVAL Stmt.Compile (p.finally);
      CG.Exit_proc (CG.Type.Void);
      CG.End_procedure (x.cg_proc);

      (* restore the "Compiler.ThisException()" globals *)
      TryStmt.PopHandler ();

    END;
  END EmitBody;

PROCEDURE Compile3 (p: P): Stmt.Outcomes =
  VAR
    oc, xc, o: Stmt.Outcomes;
    lab, xx: CG.Label;
    frame: CG.Var;
    returnSeen, exitSeen: BOOLEAN;
    proc: Procedure.T;
  BEGIN
    <* ASSERT NOT p.viaProc *>

    (* declare and initialize the info record *)
    frame := CG.Declare_local (M3ID.NoID, M3RT.EF1_SIZE, Target.Address.align,
                               CG.Type.Struct, 0, in_memory := TRUE,
                               up_level := FALSE, f := CG.Never);
    CG.Load_nil ();
    CG.Store_addr (frame, M3RT.EF1_info + M3RT.EA_exception);

    lab := CG.Next_label (3);
    CG.Set_label (lab, barrier := TRUE);
    Marker.PushFrame (frame, M3RT.HandlerClass.Finally);
    Marker.CaptureState (frame, Jmpbufs.CompileTryGetJmpbuf (p.jmpbufs), lab+1);

    (* compile the body *)
    Marker.PushFinally (lab, lab+1, CG.No_label, frame);
      oc := Stmt.Compile (p.body);
    Marker.PopFinally (returnSeen, exitSeen);
    IF (Outcome.FallThrough IN oc) THEN
      Marker.PopFrame (frame);
    END;
    CG.Set_label (lab+1, barrier := TRUE);

    (* set the "Compiler.ThisException()" globals *)
    TryStmt.PushHandler (frame, M3RT.EF1_info, direct := TRUE);

    (* compile the handler *)
    Scanner.offset := p.forigin;
    CG.Gen_location (p.forigin);
    xc := Stmt.Compile (p.finally);

    IF (Outcome.FallThrough IN xc) THEN
      (* generate the bizzare end-tests *)

      (* exceptional outcome? *)
      CG.Load_addr
        (frame, M3RT.EF1_info + M3RT.EA_exception, Target.Address.align);
      CG.Load_nil ();
      CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, lab+2, CG.Always);

      IF (exitSeen) THEN
        xx := CG.Next_label ();
        CG.Load_int (Target.Integer.cg_type,
                     frame, M3RT.EF1_info + M3RT.EA_exception);
        CG.Load_intt (Marker.Exit_exception);
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, xx, CG.Always);
        Marker.EmitExit ();
        CG.Set_label (xx);
      END;

      IF (returnSeen) THEN
        xx := CG.Next_label ();
        CG.Load_int (Target.Integer.cg_type,
                     frame, M3RT.EF1_info + M3RT.EA_exception);
        CG.Load_intt (Marker.Return_exception);
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, xx, CG.Always);
        Marker.EmitReturn (NIL, fromFinally := TRUE);
        CG.Set_label (xx);
      END;

      (* ELSE, a real exception is being raised => resume it *)
      proc := RunTyme.LookUpProc (RunTyme.Hook.ResumeRaiseEx);
      Procedure.StartCall (proc);
      CG.Load_addr_of (frame, M3RT.EF1_info, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
      Procedure.EmitCall (proc);

      CG.Set_label (lab+2, barrier := TRUE);
    END;

    (* restore the "Compiler.ThisException()" globals *)
    TryStmt.PopHandler ();

    o := Stmt.Outcomes {};
    IF Outcome.FallThrough IN xc THEN o := oc END;
    IF Outcome.Exits IN xc   THEN o := o + Stmt.Outcomes {Outcome.Exits} END;
    IF Outcome.Returns IN xc THEN o := o + Stmt.Outcomes {Outcome.Returns} END;
    RETURN o;
  END Compile3;

PROCEDURE CompileMSIR (p: P) =
  (* TRY body FINALLY finally END.
     The finally must run on BOTH the normal exit and the exceptional exit, and
     on the exceptional exit the in-flight exception must continue to the
     enclosing handler.  We model this exactly as the C backend does — as
     `catch (...) { finally; throw; }`:

       - The landing pad is a CATCH-ALL (isCleanup := FALSE → `catch _M3Exc`),
         NOT a cleanup.  A cleanup is invisible to the Itanium phase-1 search,
         so a raise in the body would find no handler and std::terminate before
         the finally ran (and bypassing any enclosing TRY/EXCEPT in this frame).
         A catch-all is a real handler, so phase 1 stops here.
       - On the exceptional path we claim the exception (__cxa_begin_catch), set
         the selector to Sel_Exc, run the finally, then __cxa_rethrow to re-raise
         it.  The rethrow is an INVOKE unwinding to the ENCLOSING try context's
         landing pad (so a nested TRY/EXCEPT in the same frame catches it); with
         no enclosing handler it is a plain call that propagates to the caller.
       - A non-local EXIT in the body branches here too (selector = Sel_Exit, via
         MSIRBuilder.EmitExitMSIR routing through the registered Finally cleanup
         frame); after the finally runs, the epilogue continues the EXIT outward.
         That arm is emitted ONLY when an EXIT actually routed through this
         finally (CurrentFinallyExitSeen) — otherwise there is no loop to
         continue to and EmitExitMSIR would (wrongly) abandon.
       - On the normal path the selector stays Sel_Normal → merge. *)
  VAR
    lpad:        MSIR.Block;
    finBody:     MSIR.Block;
    rethrow:     MSIR.Block;
    chkExit:     MSIR.Block;
    exitCont:    MSIR.Block;
    chkReturn:   MSIR.Block;
    rcont:       MSIR.Block;
    merge:       MSIR.Block;
    enclosing:   MSIR.Block;
    selector:    MSIR.Value;
    actSlot:     MSIR.Value;
    retSlot:     MSIR.Value;
    lpVal:       MSIR.Value;
    excHeader:   MSIR.Value;
    excObjPtr:   MSIR.Value;
    actPtr:      MSIR.Value;
    selV:        MSIR.Value;
    retV:        MSIR.Value;
    i32:         MSIR.T;
    ptrT:        MSIR.T;
    retT:        MSIR.T;
    exitSeen:    BOOLEAN;
    returnSeen:  BOOLEAN;
  BEGIN
    IF NOT MSIRBuilder.InProc() THEN RETURN END;

    i32  := MSIR.TI(32);
    ptrT := MSIR.TPtr(MSIR.TVoid());

    (* Allocas go in the current (pre-try) block so they land in the entry. *)
    selector := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", i32);
    MSIR.BuildStore(MSIRBuilder.CurrentBlock(),
                    MSIR.ConstInt(i32, MSIRBuilder.Sel_Normal), selector);
    actSlot := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", ptrT);

    (* Allocate a retSlot for a pending RETURN value if this proc returns a
       value.  When a RETURN in the TRY body routes through this finally,
       ReturnStmt stores the value here; the epilogue loads and returns it. *)
    retT    := MSIRBuilder.CurrentResultType();
    IF retT = NIL THEN retT := MSIR.ProcResultType(MSIRBuilder.CurrentProc()) END;
    IF retT # NIL AND MSIR.Kind(retT) # MSIR.TypeKind.Void THEN
      retSlot := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", retT);
    ELSE
      retSlot := NIL;
    END;

    lpad    := MSIRBuilder.NewBlock("fin.lpad");
    finBody := MSIRBuilder.NewBlock("fin.body");
    rethrow := MSIRBuilder.NewBlock("fin.rethrow");
    merge   := MSIRBuilder.NewBlock("fin.done");

    (* Compile body with the finally landing pad as unwind target AND a Finally
       cleanup frame so a non-local EXIT/RETURN in the body runs the finally first. *)
    MSIRBuilder.PushTryContext(lpad);
    MSIRBuilder.PushFinallyCleanup(finBody, selector, retSlot);
    Stmt.CompileMSIR(p.body);
    exitSeen   := MSIRBuilder.CurrentFinallyExitSeen();
    returnSeen := MSIRBuilder.CurrentFinallyReturnSeen();
    MSIRBuilder.PopFinallyCleanup();
    MSIRBuilder.PopTryContext();
    (* After Pop, the current unwind block is the ENCLOSING try context (NIL if
       this finally is outermost in the procedure). *)
    enclosing := MSIRBuilder.CurrentUnwindBlock();

    (* Normal fall-through → finBody (selector stays Sel_Normal). *)
    IF NOT MSIRBuilder.CurrentBlockTerminated() THEN
      MSIR.BuildBr(MSIRBuilder.CurrentBlock(), finBody, ARRAY OF MSIR.Value{});
    END;

    (* Exceptional path: catch-all (so phase 1 finds a handler), but do NOT
       __cxa_begin_catch.  We only PEEK at the exception object to recover the
       M3 RaiseActivation (separately heap-allocated, so it survives), save it,
       set Sel_Exc, and run the finally.  Re-raising later via RTHooks.ResumeRaise
       is a FRESH throw — so a finally that itself raises a new exception leaves
       no half-claimed C++ exception dangling (which otherwise hangs the unwinder
       when the new exception passes through an intermediate non-matching
       handler, e.g. p0/p004).  Mirrors the C backend, which likewise re-raises
       via ResumeRaiseEx rather than a C++ rethrow. *)
    lpVal     := MSIR.BuildLandingPad(lpad, "", isCleanup := FALSE);
    excHeader := MSIR.BuildExtractValue(lpad, "", lpVal, 0);
    excObjPtr := MSIR.BuildCall(lpad, "", MSIRBuilder.CxaGetExceptionPtr(),
                                ARRAY OF MSIR.Value{excHeader});
    actPtr    := MSIR.BuildLoad(lpad, "", ptrT, excObjPtr); (* _M3Exc.act *)
    MSIR.BuildStore(lpad, actPtr, actSlot);
    MSIR.BuildStore(lpad, MSIR.ConstInt(i32, MSIRBuilder.Sel_Exc), selector);
    MSIR.BuildBr(lpad, finBody, ARRAY OF MSIR.Value{});

    (* Finally body (shared by all entry paths). *)
    MSIRBuilder.SetCurrentBlock(finBody);
    Stmt.CompileMSIR(p.finally);

    (* After finally, dispatch on the selector: Sel_Exc → rethrow; Sel_Exit →
       continue the EXIT (only if an EXIT routed through here); else → merge. *)
    IF NOT MSIRBuilder.CurrentBlockTerminated() THEN
      selV := MSIR.BuildLoad(MSIRBuilder.CurrentBlock(), "", i32, selector);
      (* Chain of selector checks: Sel_Exc → rethrow; Sel_Exit → continue EXIT
         (only if seen); Sel_Return → emit ret with saved value (only if seen);
         else → merge (normal fall-through). *)
      VAR notExc := MSIRBuilder.NewBlock("fin.notexc");
      BEGIN
        MSIR.BuildCondBr(MSIRBuilder.CurrentBlock(),
                         MSIR.BuildICmp(MSIRBuilder.CurrentBlock(), "",
                           MSIR.CmpPred.Eq, selV,
                           MSIR.ConstInt(i32, MSIRBuilder.Sel_Exc)),
                         rethrow, ARRAY OF MSIR.Value{},
                         notExc,  ARRAY OF MSIR.Value{});
        MSIRBuilder.SetCurrentBlock(notExc);
      END;
      IF exitSeen THEN
        chkExit  := MSIRBuilder.NewBlock("fin.chkexit");
        exitCont := MSIRBuilder.NewBlock("fin.exitcont");
        MSIR.BuildCondBr(MSIRBuilder.CurrentBlock(),
                         MSIR.BuildICmp(MSIRBuilder.CurrentBlock(), "",
                           MSIR.CmpPred.Eq, selV,
                           MSIR.ConstInt(i32, MSIRBuilder.Sel_Exit)),
                         exitCont, ARRAY OF MSIR.Value{},
                         chkExit,  ARRAY OF MSIR.Value{});
        (* This finally's frame is already popped, so EmitExitMSIR targets the
           next outer finally or the loop exit. *)
        MSIRBuilder.SetCurrentBlock(exitCont);
        MSIRBuilder.EmitExitMSIR();
        MSIRBuilder.SetCurrentBlock(chkExit);
      END;
      IF returnSeen THEN
        chkReturn := MSIRBuilder.NewBlock("fin.chkreturn");
        MSIR.BuildCondBr(MSIRBuilder.CurrentBlock(),
                         MSIR.BuildICmp(MSIRBuilder.CurrentBlock(), "",
                           MSIR.CmpPred.Eq, selV,
                           MSIR.ConstInt(i32, MSIRBuilder.Sel_Return)),
                         chkReturn, ARRAY OF MSIR.Value{},
                         merge,     ARRAY OF MSIR.Value{});
        MSIRBuilder.SetCurrentBlock(chkReturn);
        (* Pending RETURN: load the saved value, route through any enclosing
           LOCK cleanup (mutex release) before actually returning. *)
        IF retSlot # NIL AND retT # NIL THEN
          retV := MSIR.BuildLoad(chkReturn, "", retT, retSlot);
        ELSE
          retV := NIL;
        END;
        IF NOT MSIRBuilder.EmitReturnThroughFinally(retV) THEN
          MSIR.BuildRet(chkReturn, retV);
        END;
      ELSE
        MSIR.BuildBr(MSIRBuilder.CurrentBlock(), merge, ARRAY OF MSIR.Value{});
      END;
    END;

    (* Re-raise the saved activation (a fresh throw via RTHooks.ResumeRaise) to
       the enclosing handler — or to the caller if outermost. *)
    MSIRBuilder.SetCurrentBlock(rethrow);
    VAR
      resumeHook := MSIRBuilder.HookProc(RunTyme.Hook.ResumeRaiseEx);
      actV       := MSIR.BuildLoad(rethrow, "", ptrT, actSlot);
    BEGIN
      IF enclosing # NIL THEN
        rcont := MSIRBuilder.NewBlock("fin.rethrow.cont");
        EVAL MSIR.BuildInvoke(rethrow, "", resumeHook,
                              ARRAY OF MSIR.Value{actV}, rcont, enclosing);
        MSIRBuilder.SetCurrentBlock(rcont);
        MSIR.BuildUnreachable(rcont);
      ELSE
        EVAL MSIR.BuildCall(rethrow, "", resumeHook, ARRAY OF MSIR.Value{actV});
        MSIR.BuildUnreachable(rethrow);
      END;
    END;

    MSIRBuilder.SetCurrentBlock(merge);
  END CompileMSIR;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  VAR oc, xc, o: Stmt.Outcomes;
  BEGIN
    oc := Stmt.GetOutcome (p.body);
    xc := Stmt.GetOutcome (p.finally);
    o := Stmt.Outcomes {};
    IF Outcome.FallThrough IN xc THEN o := oc END;
    IF Outcome.Exits IN xc THEN o := o + Stmt.Outcomes {Outcome.Exits} END;
    IF Outcome.Returns IN xc THEN o := o + Stmt.Outcomes {Outcome.Returns} END;
    RETURN o;
  END GetOutcome;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    Stmt.Capture (p.body,    ca);
    Stmt.Capture (p.finally, ca);
  END Capture;

BEGIN
END TryFinStmt.
