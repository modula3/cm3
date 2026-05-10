(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: LockStmt.m3                                           *)
(* Last modified on Fri Jun 24 15:50:01 PDT 1994 by kalsow     *)
(*      modified on Sun Jan 21 07:49:28 1990 by muller         *)

MODULE LockStmt;

IMPORT M3ID, Expr, Mutex, Error, Type, Stmt, StmtRep, Token, Marker;
IMPORT CG, Target, M3RT, Scanner, Procedure, RunTyme;
IMPORT MSIR, MSIRBuilder;
FROM Scanner IMPORT Match;

TYPE
  P = Stmt.T OBJECT
        mutex   : Expr.T;
        body    : Stmt.T;
        tail    : INTEGER;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
        compileMSIR := CompileMSIR;
      END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR p := NEW (P);
  BEGIN
    StmtRep.Init (p);
    Match (TK.tLOCK);
    p.mutex := Expr.Parse ();
    Match (TK.tDO);
    p.body := Stmt.Parse ();
    p.tail := Scanner.offset;
    Match (TK.tEND);
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;
  BEGIN
    Expr.TypeCheck (p.mutex, cs);
    t := Expr.TypeOf (p.mutex);
    IF NOT Type.IsSubtype (t, Mutex.T) THEN
      Error.Msg ("expression must be a mutex");
    END;
    Marker.PushLock (CG.No_label, CG.No_label, NIL);
    Stmt.TypeCheck (p.body, cs);
    Marker.Pop ();
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  BEGIN
    IF Target.Has_stack_walker
      THEN RETURN Compile1 (p);
      ELSE RETURN Compile2 (p);
    END;
  END Compile;

PROCEDURE Compile1 (p: P): Stmt.Outcomes =
  VAR
    oc: Stmt.Outcomes;
    lab, xx: CG.Label;
    info, mu: CG.Var;
    returnSeen, exitSeen : BOOLEAN;
    proc: Procedure.T;
    catches := ARRAY[0..0] OF CG.TypeUID{0};
  BEGIN
    (* declare and initialize the info record *)
    info := CG.Declare_local (M3ID.NoID, Target.Address.size, Target.Address.align,
                              CG.Type.Addr, 0, in_memory := TRUE,
                              up_level := FALSE, f := CG.Never);

    CG.Load_nil ();
    CG.Store_addr (info, M3RT.EA_exception);

    (* capture the mutex expression *)
    Expr.Prep (p.mutex);
    Expr.Compile (p.mutex);
    mu := CG.Declare_local (M3ID.NoID, Target.Address.size,
                            Target.Address.align, CG.Type.Addr,
                            Type.GlobalUID (Mutex.T), in_memory := TRUE,
                            up_level := FALSE, f := CG.Never);
    CG.Store_addr (mu);

    (* acquire the lock *)
    Marker.SetLock (TRUE, mu, 0);
    Expr.NoteWrite (p.mutex);

    (* compile the body *)
    lab := CG.Next_label (4);
    CG.Set_label (lab, barrier := TRUE);
    CG.Start_try ();

    Marker.PushFinally (lab, lab+1, lab+2, info);
    Marker.SaveFrame ();
      oc := Stmt.Compile (p.body);
    Marker.PopFinally (returnSeen, exitSeen);

    (* jump over the exc handler *)
    CG.Jump (lab+2);

    (* End the try block here so that the lock-release and re-raise code
       (which follows at lab+2) is OUTSIDE the try region.  This prevents
       exceptions from Thread.Release or ResumeRaiseEx from being re-caught
       by this same catch clause.  Any such exceptions propagate naturally
       to an enclosing outer try block instead. *)
    CG.End_try ();

    CG.Set_label (lab+1, barrier := TRUE);
    CG.Landing_pad(lab+1, catches);
    CG.Store_addr (info);
    CG.Set_label (lab+2);

    (* release the lock *)
    Marker.SetLock (FALSE, mu, 0);
    Expr.NoteWrite (p.mutex);

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
    CG.Load_addr (info, M3RT.EA_exception, Target.Address.align);
    CG.Load_nil ();
    CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, lab+3, CG.Always);
    proc := RunTyme.LookUpProc (RunTyme.Hook.ResumeRaiseEx);
    Procedure.StartCall (proc);
    CG.Load_addr (info, 0, Target.Address.align);
    CG.Pop_param (CG.Type.Addr);
    Procedure.EmitCall (proc);

    CG.Gen_location (p.tail);
    CG.Set_label (lab+3, barrier := TRUE);
    RETURN oc;
  END Compile1;

PROCEDURE Compile2 (p: P): Stmt.Outcomes =
  VAR oc: Stmt.Outcomes;  frame: CG.Var;  l: CG.Label;
  BEGIN
    (* capture the mutex expression *)
    Expr.Prep (p.mutex);
    Expr.Compile (p.mutex);
    frame := CG.Declare_local (M3ID.NoID, M3RT.EF4_SIZE, Target.Address.align,
                               CG.Type.Struct, 0, in_memory := TRUE,
                               up_level := FALSE, f := CG.Never);
    CG.Store_addr (frame, M3RT.EF4_mutex);

    (* acquire the lock *)
    Marker.SetLock (TRUE, frame, M3RT.EF4_mutex);
    Expr.NoteWrite (p.mutex);

    (* set the barrier and link the frame *)
    l := CG.Next_label (2);
    CG.Set_label (l, barrier := TRUE);
    Marker.PushFrame (frame, M3RT.HandlerClass.Lock);

    (* compile the body *)
    Marker.PushLock (l, l+1, frame);
      oc := Stmt.Compile (p.body);
    Marker.Pop ();

    CG.Gen_location (p.tail);
    CG.Set_label (l+1, barrier := TRUE);

    IF (Stmt.Outcome.FallThrough IN oc) THEN
      (* unlink the frame and release the lock *)
      Marker.PopFrame (frame);
      Marker.SetLock (FALSE, frame, M3RT.EF4_mutex);
      Expr.NoteWrite (p.mutex);
    END;

    RETURN oc;
  END Compile2;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.GetOutcome (p.body);
  END GetOutcome;

PROCEDURE CompileMSIR (p: P) =
  (* LOCK mu DO body END  ≡  mu.acquire(); TRY body FINALLY mu.release() END
     MUTEX vtable slot = M3RT.MUTEX_acquire|release (byte offset) / bytes-per-addr
     M3RT.MUTEX_acquire = 0 * AP → slot 0
     M3RT.MUTEX_release = 1 * AP → slot 1  where AP = Target.Address.bytes *)
  VAR
    mu:        MSIR.Value;
    lpad:      MSIR.Block;
    finBody:   MSIR.Block;
    resumeBlk: MSIR.Block;
    merge:     MSIR.Block;
    lpSlot:    MSIR.Value;
    excFlag:   MSIR.Value;
    lpVal:     MSIR.Value;
    excFlagV:  MSIR.Value;
    lpLoaded:  MSIR.Value;
    lpType:    MSIR.T;
    zero, one: MSIR.Value;
  BEGIN
    IF NOT MSIRBuilder.InProc() THEN RETURN END;

    (* Compile the mutex expression. *)
    mu := Expr.CompileMSIR(p.mutex);
    IF mu = NIL THEN RETURN END;

    (* Acquire the mutex: mu.acquire() — vtable slot M3RT.MUTEX_acquire / AP. *)
    EVAL MSIRBuilder.EmitMethodCall(
           "", mu,
           VAL(M3RT.MUTEX_acquire, LONGINT) DIV VAL(Target.Address.bytes, LONGINT),
           MSIR.TVoid(), ARRAY OF MSIR.Value{});
    IF NOT MSIRBuilder.InProc() THEN RETURN END;

    (* TRY body FINALLY mu.release() END — mirrors TryFinStmt.CompileMSIR. *)
    lpType := MSIR.TLandingPad();
    zero   := MSIR.ConstInt(MSIR.TI1(), 0L);
    one    := MSIR.ConstInt(MSIR.TI1(), 1L);

    lpSlot  := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", lpType);
    excFlag := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", MSIR.TI1());
    MSIR.BuildStore(MSIRBuilder.CurrentBlock(), zero, excFlag);

    lpad      := MSIRBuilder.NewBlock("lock.lpad");
    finBody   := MSIRBuilder.NewBlock("lock.fin");
    resumeBlk := MSIRBuilder.NewBlock("lock.resume");
    merge     := MSIRBuilder.NewBlock("lock.done");

    (* Body inside TRY (lpad = cleanup unwind target). *)
    MSIRBuilder.PushTryContext(lpad);
    Stmt.CompileMSIR(p.body);
    MSIRBuilder.PopTryContext();

    IF NOT MSIRBuilder.CurrentBlockTerminated() THEN
      MSIR.BuildBr(MSIRBuilder.CurrentBlock(), finBody, ARRAY OF MSIR.Value{});
    END;

    (* Landing pad: save lp value, set exceptional flag. *)
    lpVal := MSIR.BuildLandingPad(lpad, "", isCleanup := TRUE);
    MSIR.BuildStore(lpad, lpVal, lpSlot);
    MSIR.BuildStore(lpad, one, excFlag);
    MSIR.BuildBr(lpad, finBody, ARRAY OF MSIR.Value{});

    (* Finally body: release the mutex — vtable slot M3RT.MUTEX_release / AP.
       Release is called outside our own lpad (already popped), so if it throws
       the exception propagates to any enclosing TRY rather than looping back. *)
    MSIRBuilder.SetCurrentBlock(finBody);
    EVAL MSIRBuilder.EmitMethodCall(
           "", mu,
           VAL(M3RT.MUTEX_release, LONGINT) DIV VAL(Target.Address.bytes, LONGINT),
           MSIR.TVoid(), ARRAY OF MSIR.Value{});
    IF NOT MSIRBuilder.InProc() THEN RETURN END;

    IF NOT MSIRBuilder.CurrentBlockTerminated() THEN
      excFlagV := MSIR.BuildLoad(MSIRBuilder.CurrentBlock(), "",
                                 MSIR.TI1(), excFlag);
      MSIR.BuildCondBr(MSIRBuilder.CurrentBlock(), excFlagV,
                       resumeBlk, ARRAY OF MSIR.Value{},
                       merge,     ARRAY OF MSIR.Value{});
    END;

    (* Resume: reload saved landing pad and resume unwinding. *)
    MSIRBuilder.SetCurrentBlock(resumeBlk);
    lpLoaded := MSIR.BuildLoad(resumeBlk, "", lpType, lpSlot);
    MSIR.BuildResume(resumeBlk, lpLoaded);

    MSIRBuilder.SetCurrentBlock(merge);
  END CompileMSIR;

BEGIN
END LockStmt.
