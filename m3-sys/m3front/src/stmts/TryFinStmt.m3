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
    Marker.PushFinally (CG.No_label, CG.No_label, NIL);
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

(*
PROCEDURE Compile1 (p: P): Stmt.Outcomes =
  VAR
    oc, xc, o: Stmt.Outcomes;
    lab: CG.Label;
    info: CG.Var;
    proc: Procedure.T;
  BEGIN
    (* declare and initialize the info record *)
    info := CG.Declare_local (M3ID.NoID, M3RT.EA_SIZE, Target.Address.align,
                              CG.Type.Struct, 0, in_memory := TRUE,
                              up_level := FALSE, f := CG.Never);
    CG.Load_nil ();
    CG.Store_addr (info, M3RT.EA_exception);

    (* compile the body *)
    lab := CG.Next_label (2);
    CG.Set_label (lab, barrier := TRUE);
    Marker.PushFinally (lab, lab+1, info);
    Marker.SaveFrame ();
      oc := Stmt.Compile (p.body);
    Marker.Pop ();
    CG.Set_label (lab+1, barrier := TRUE);

    (* set the "Compiler.ThisException()" globals *)
    TryStmt.PushHandler (info, 0, direct := TRUE);

    (* compile the handler *)
    Scanner.offset := p.forigin;
    CG.Gen_location (p.forigin);
      xc := Stmt.Compile (p.finally);

    (* generate the bizzare end-tests *)

    IF (Outcome.Returns IN oc) THEN
      lab := CG.Next_label ();
      CG.Load_int (Target.Integer.cg_type, info, M3RT.EA_exception);
      CG.Load_intt (Marker.Return_exception);
      CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, lab, CG.Always);
      Marker.EmitReturn (NIL, fromFinally := TRUE);
      CG.Set_label (lab);
    END;

    IF (Outcome.Exits IN oc) THEN
      lab := CG.Next_label ();
      CG.Load_int (Target.Integer.cg_type, info, M3RT.EA_exception);
      CG.Load_intt (Marker.Exit_exception);
      CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, lab, CG.Always);
      Marker.EmitExit ();
      CG.Set_label (lab);
    END;

    (* resume the exception *)
    proc := RunTyme.LookUpProc (RunTyme.Hook.ResumeRaiseEx);
    lab := CG.Next_label ();
    CG.Load_addr (info, M3RT.EA_exception, Target.Address.align);
    CG.Load_nil ();
    CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, lab, CG.Always);
    Procedure.StartCall (proc);
    CG.Load_addr_of (info, 0, Target.Address.align);
    CG.Pop_param (CG.Type.Addr);
    Procedure.EmitCall (proc);
    CG.Set_label (lab);

    (* restore the "Compiler.ThisException()" globals *)
    TryStmt.PopHandler ();

    o := Stmt.Outcomes {};
    IF Outcome.FallThrough IN xc THEN o := oc END;
    IF Outcome.Exits IN xc   THEN o := o + Stmt.Outcomes {Outcome.Exits} END;
    IF Outcome.Returns IN xc THEN o := o + Stmt.Outcomes {Outcome.Returns} END;
    RETURN o;
  END Compile1;
*)

PROCEDURE Compile1 (p: P): Stmt.Outcomes =
  VAR
    oc, xc, o: Stmt.Outcomes;
    lab, xx: CG.Label;
    info: CG.Var;
    returnSeen, exitSeen : BOOLEAN;
    proc: Procedure.T;
  BEGIN
    (* declare and initialize the info record *)
    info := CG.Declare_local (M3ID.NoID, M3RT.EA_SIZE, Target.Address.align,
                              CG.Type.Struct, 0, in_memory := TRUE,
                              up_level := FALSE, f := CG.Never);
    CG.Load_nil ();
    CG.Store_addr (info, M3RT.EA_exception);

    (* compile the body *)
    lab := CG.Next_label (3);
    CG.Set_label (lab, barrier := TRUE);
    Marker.PushFinally (lab, lab+1, info);
    Marker.SaveFrame ();
      oc := Stmt.Compile (p.body);
    Marker.PopFinally (returnSeen, exitSeen);
    CG.Set_label (lab+1, barrier := TRUE);

    (* set the "Compiler.ThisException()" globals *)
    TryStmt.PushHandler (info, 0, direct := TRUE);

    (* compile the handler *)
    Scanner.offset := p.forigin;
    CG.Gen_location (p.forigin);
      xc := Stmt.Compile (p.finally);

    IF (Outcome.FallThrough IN xc) THEN
      (* generate the bizzare end-tests *)

      (* exceptional outcome? *)
      CG.Load_addr (info, M3RT.EA_exception, Target.Address.align);
      CG.Load_nil ();
      CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, lab+2, CG.Always);

      IF (exitSeen) THEN
        xx := CG.Next_label ();
        CG.Load_int (Target.Integer.cg_type, info, M3RT.EA_exception);
        CG.Load_intt (Marker.Exit_exception);
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, xx, CG.Always);
        Marker.EmitExit ();
        CG.Set_label (xx);
      END;

      IF (returnSeen) THEN
        xx := CG.Next_label ();
        CG.Load_int (Target.Integer.cg_type, info, M3RT.EA_exception);
        CG.Load_intt (Marker.Return_exception);
        CG.If_compare (Target.Integer.cg_type, CG.Cmp.NE, xx, CG.Always);
        Marker.EmitReturn (NIL, fromFinally := TRUE);
        CG.Set_label (xx);
      END;

      (* resume the exception *)
      proc := RunTyme.LookUpProc (RunTyme.Hook.ResumeRaiseEx);
      Procedure.StartCall (proc);
      CG.Load_addr_of (info, 0, Target.Address.align);
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
    Marker.PushFinally (lab, lab+1, frame);
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

BEGIN
END TryFinStmt.
