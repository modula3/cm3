(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

MODULE DebugStmt;

IMPORT CG, Expr, Token, Scanner, Stmt, StmtRep, Error, M3RT;
IMPORT Host, EnumExpr, Type, Bool, Target, TInt, ErrType;
IMPORT Textt, Procedure, NarrowExpr, Module, AssignStmt, RunTyme;

TYPE
  P = Stmt.T OBJECT
        cond   : Expr.T;
        n_msgs : INTEGER;
        msgs   : REF ARRAY OF Expr.T;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    Scanner.Match (Token.T.tDEBUG);
    p.cond   := NIL;
    p.n_msgs := 0;
    p.msgs   := NEW (REF ARRAY OF Expr.T, 4);
    WHILE (Scanner.cur.token # Token.T.tENDPRAGMA) DO
      IF (p.n_msgs >= NUMBER (p.msgs^)) THEN ExpandMsgs (p); END;
      p.msgs[p.n_msgs] := Expr.Parse ();
      INC (p.n_msgs);
      IF (p.n_msgs = 1) THEN
        IF Scanner.cur.token = Token.T.tWITH THEN
          p.cond := p.msgs[0];  p.n_msgs := 0;
        ELSIF Scanner.cur.token # Token.T.tCOMMA THEN
          EXIT;
        END;
      ELSE
        IF Scanner.cur.token # Token.T.tCOMMA THEN EXIT; END;
      END;
      Scanner.GetToken ();  (* "," or "WITH" *)
    END;
    IF (Scanner.cur.token # Token.T.tENDPRAGMA) THEN
      Scanner.Fail ("missing \'*>\'");
    END;
    Scanner.cur.token := Token.T.tSEMI;  (* for the statement parser *)
    RETURN p;
  END Parse;

PROCEDURE ExpandMsgs (p: P) =
  VAR n := NUMBER (p.msgs^);  new := NEW (REF ARRAY OF Expr.T, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := p.msgs^;
    p.msgs := new;
  END ExpandMsgs;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;  shift := FALSE;
  BEGIN
    IF (p.cond # NIL) THEN
      Expr.TypeCheck (p.cond, cs);
      t := Type.Base (Expr.TypeOf (p.cond));
      IF (t # Bool.T) AND (t # ErrType.T) THEN
        Error.Msg ("ASSERT condition must be a BOOLEAN");
      END;
    END;

    FOR i := 0 TO p.n_msgs-1 DO
      Expr.TypeCheck (p.msgs[i], cs);
      t := Type.Base (Expr.TypeOf (p.msgs[i]));
      IF (i = 0) AND (p.cond = NIL) AND (t = Bool.T) THEN
        (* the first "msg" is really the condition to test *)
        shift := TRUE;
      ELSIF Type.IsAssignable (Textt.T, t) THEN
        p.msgs[i] := CheckArg (Textt.T, t, p.msgs[i], cs);
      ELSE
        Error.Msg ("DEBUG message must be assignable to TEXT");
      END;
    END;

    IF (shift) THEN
      p.cond := p.msgs[0];
      FOR i := 0 TO p.n_msgs-1 DO
        p.msgs[i] := p.msgs[i+1];
      END;
      DEC (p.n_msgs);
    END;
  END Check;

PROCEDURE CheckArg (tlhs, trhs: Type.T;  e: Expr.T;
                    VAR cs: Stmt.CheckState): Expr.T =
  BEGIN
    AssignStmt.Check (tlhs, e, cs);
    IF Host.doNarrowChk AND NOT Type.IsSubtype (trhs, tlhs) THEN
      e := NarrowExpr.New (e, tlhs);
      Expr.TypeCheck (e, cs);
    END;
    RETURN e;
  END CheckArg;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR v: Expr.T;  i: Target.Int;  u: Type.T;   skip: CG.Label;
  BEGIN
    IF Host.doDebugs THEN
      i := TInt.MOne;
      v := Expr.ConstValue (p.cond);
      IF (p.cond = NIL) THEN
        (* DEBUG with no test condition *)
        PrepMsgs (p);
        EmitDebug (p);
      ELSIF (v = NIL) THEN
        (* DEBUG with non-constant test condition *)
        skip := CG.Next_label ();
        PrepMsgs (p);
        Expr.PrepBranch (p.cond, CG.No_label, skip, CG.Always);
        Expr.CompileBranch (p.cond, CG.No_label, skip, CG.Always);
        EmitDebug (p);
        CG.Set_label (skip);
      ELSIF EnumExpr.Split (v, i, u) AND TInt.EQ (i, TInt.Zero) THEN
        (* DEBUG (FALSE) *)
      ELSE <* ASSERT TInt.EQ (i, TInt.One) *>
        (* Debug (TRUE) *)
        PrepMsgs (p);
        EmitDebug (p);
      END;
    END;
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END Compile;

PROCEDURE PrepMsgs (p: P) =
  BEGIN
    FOR i := 0 TO p.n_msgs-1 DO Expr.Prep (p.msgs[i]); END;
  END PrepMsgs;

PROCEDURE EmitDebug (p: P) =
  VAR
    proc      : Procedure.T;
    this_file : TEXT;
    this_line : INTEGER;
    msgs      := CG.Declare_temp (Target.Address.pack + Target.Integer.pack
                                    + p.n_msgs*Target.Address.pack,
                                  Target.Address.align, CG.Type.Struct,
                                  in_memory := TRUE);
    offset: INTEGER;
  BEGIN
    Scanner.Here (this_file, this_line);

    (* initialize the open-array pointer to the msgs *)
    CG.Load_addr_of (msgs, M3RT.OA_size_1, Target.Address.align);
    CG.Store_addr (msgs, M3RT.OA_elt_ptr);

    (* initialize the count of array sizes *)
    CG.Load_intt (p.n_msgs);
    CG.Store_int (msgs, M3RT.OA_size_0);

    (* initialize each message *)
    offset := M3RT.OA_size_1;
    FOR i := 0 TO p.n_msgs-1 DO
      Expr.Compile (p.msgs[i]);
      CG.Store_addr (msgs, offset);
      INC (offset, Target.Address.pack);
    END;

    proc := RunTyme.LookUpProc (RunTyme.Hook.DebugMsg);
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      (* module data pointer *)
        CG.Load_addr_of (Module.GlobalData (FALSE), 0, CG.Max_alignment);
        CG.Pop_param (CG.Type.Addr);
      (* line number *)
        CG.Load_intt (this_line);
        CG.Pop_param (Target.Integer.cg_type);
      (* messages *)
        CG.Load_addr_of (msgs, 0, Target.Address.align);
        CG.Pop_param (CG.Type.Addr);
    ELSE
      (* messages *)
        CG.Load_addr_of (msgs, 0, Target.Address.align);
        CG.Pop_param (CG.Type.Addr);
      (* line number *)
        CG.Load_intt (this_line);
        CG.Pop_param (Target.Integer.cg_type);
      (* module data pointer *)
        CG.Load_addr_of (Module.GlobalData (FALSE), 0, CG.Max_alignment);
        CG.Pop_param (CG.Type.Addr);
    END;
    Procedure.EmitCall (proc);
    CG.Free_temp (msgs);
  END EmitDebug;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END GetOutcome;

BEGIN
END DebugStmt.
