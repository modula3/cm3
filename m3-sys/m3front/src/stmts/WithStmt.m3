(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WithStmt.m3                                           *)
(* Last modified on Tue Jun 20 09:28:07 PDT 1995 by kalsow     *)
(*      modified on Fri Jun 16 12:48:09 PDT 1995 by ericv      *)
(*      modified on Tue Jun 26 08:01:23 1990 by muller         *)

MODULE WithStmt;

IMPORT M3ID, CG, Expr, Scope, Value, Variable, OpenArrayType;
IMPORT Type, Stmt, StmtRep, Token, M3RT, Target, Tracer, AssignStmt;
IMPORT ArrayExpr;
FROM Scanner IMPORT Match, MatchID, GetToken, cur;
IMPORT MSIR, MSIRBuilder, MSIRType, CaptureAnalysis;

TYPE
  Kind = {designator, openarray, structure, other};

  P = Stmt.T OBJECT
        var     : Variable.T;
        expr    : Expr.T;
        scope   : Scope.T;
        body    : Stmt.T;
        kind    : Kind;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
        compileMSIR := CompileMSIR;
        capture  := Capture;
      END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  BEGIN
    Match (TK.tWITH);
    RETURN ParseTail ();
  END Parse;

PROCEDURE ParseTail (): Stmt.T =
  TYPE TK = Token.T;
  VAR p: P;  id: M3ID.T;  trace: Tracer.T;
  VAR u: BOOLEAN := FALSE; (* WITH-bound Id has UNUSED pragma. *) 
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    IF cur.token = TK.tUNUSED THEN
      u := TRUE; 
      GetToken (); (* UNUSED *)
      Match (TK.tENDPRAGMA);
    END; 
    id := MatchID ();
    trace := Variable.ParseTrace ();
    p.var := Variable.New (id, u);
    Match (TK.tEQUAL);
    p.expr := Expr.Parse ();
    p.scope := Scope.New1 (p.var);
    Variable.BindTrace (p.var, trace);
    IF (cur.token = TK.tCOMMA) THEN
      GetToken (); (* , *)
      p.body := ParseTail ();
    ELSE
      Match (TK.tDO);
      p.body := Stmt.Parse ();
      Match (TK.tEND);
    END;
    Scope.PopNew ();
    RETURN p;
  END ParseTail;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;  zz: Scope.T;
  BEGIN
    Expr.TypeCheck (p.expr, cs);
    t := Expr.TypeOf (p.expr);

    IF OpenArrayType.Is (t) THEN
      p.kind := Kind.openarray;
      Variable.NeedsAddress (p.var);
      ArrayExpr.NoteTargetType (p.expr, t);
    ELSIF Expr.IsDesignator (p.expr) THEN
      p.kind := Kind.designator;
      Expr.NeedsAddress (p.expr);
    ELSIF Type.IsStructured (t) THEN
      p.kind := Kind.structure;
      Variable.NeedsAddress (p.var);
      AssignStmt.Check (t, p.expr, cs);
    ELSE
      p.kind := Kind.other;
    END;
      
    Variable.BindType (p.var, t, indirect := (p.kind = Kind.designator),
                       readonly := NOT Expr.IsWritable (p.expr, traced := FALSE),
                       open_array_ok := TRUE,  needs_init := FALSE);

    Scope.TypeCheck (p.scope, cs);
    zz := Scope.Push (p.scope);
      Stmt.TypeCheck (p.body, cs);
    Scope.Pop (zz);
    Scope.WarnUnused (p.scope);
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR
    oc: Stmt.Outcomes;
    zz: Scope.T;
    info: Type.Info;
    val: CG.Val;
    dope_size: INTEGER;
    t, tlhs: Type.T;
    global, indirect, traced: BOOLEAN;
  BEGIN
    t := Type.CheckInfo (Value.TypeOf (p.var), info);

    (* evaluate the expr outside the new scope and capture its value *)
    EVAL Expr.CheckUseFailure (p.expr);
    CASE p.kind OF
    | Kind.designator =>
        Variable.Split (p.var, tlhs, global, indirect, traced);
        Expr.PrepLValue (p.expr, traced);
        Expr.CompileAddress (p.expr, traced);
        val := CG.Pop ();
    | Kind.structure =>
        tlhs := Value.TypeOf (p.var);
        ArrayExpr.NoteUseTargetVar (p.expr);
        AssignStmt.PrepForEmit (tlhs, p.expr, initializing := TRUE);
    | Kind.openarray, Kind.other =>
        Expr.Prep (p.expr);
        Expr.Compile (p.expr);
        val := CG.Pop ();
    END;

    (* open the new scope *)
    zz := Scope.Push (p.scope);
      Scope.Enter (p.scope);

      (* initialize the variable *)
      CASE p.kind OF
      | Kind.designator =>
          CG.Push (val);
          Variable.SetLValue (p.var);
          CG.Free (val);
      | Kind.openarray =>
          (* Copy dope-only into p.var. *)
          dope_size := OpenArrayType.OpenDepth(t) * Target.Integer.pack;
          (* p.var will have been declared with independently-computed
             size equal to dope_size.  See Variable.Declare. *)
          INC (dope_size, M3RT.OA_sizes);
          Variable.LoadLValue (p.var);
          CG.Push (val);
          CG.Copy (dope_size, overlap := FALSE);
          CG.Free (val);

(* CHECK: p.expr can, if it's a truly dynamic open array constructor, be
          compiled as a heap-allocated temporary, complete with contiguous
          dope.  This will keep a pointer only to its interior, i.e., its
          elements.  Is this enough to protect it from CG? See heapTempPtr
          in ArrayExpr.m3, which maintains a pointer to the dope, but it
          will remain until the containing procedure is exited -- overly
          long. *)
          
      | Kind.structure =>
          Variable.LoadLValue (p.var);
          AssignStmt.DoEmit (tlhs, p.expr, initializing := TRUE);
      | Kind.other =>
          Variable.LoadLValue (p.var);
          CG.Push (val);
          CG.Store_indirect (info.stk_type, 0, info.size);
          CG.Free (val);
      END;
      Variable.ScheduleTrace (p.var);

      oc := Stmt.Compile (p.body);
      Scope.Exit (p.scope);
    Scope.Pop (zz);
    RETURN oc;
  END Compile;

PROCEDURE CompileMSIR (p: P) =
  VAR
    addr:               MSIR.Value;
    val:                MSIR.Value;
    mt:                 MSIR.T;
    t:                  Type.T;
    global, indirect,
    lhs:                BOOLEAN;
  BEGIN
    CASE p.kind OF
    | Kind.designator, Kind.openarray =>
        (* Bind the alias variable to the lvalue address.
           For open arrays, the lvalue is a pointer to the dope vector. *)
        addr := Expr.LValueMSIR (p.expr);
        IF addr = NIL THEN
          MSIRBuilder.Abandon ("WITH designator/openarray: cannot get lvalue in MSIR");
          RETURN;
        END;
        Variable.Split (p.var, t, global, indirect, lhs);
        mt := MSIRType.Translate (t);
        IF mt = NIL THEN
          MSIRBuilder.Abandon ("WITH designator/openarray: unsupported type");
          RETURN;
        END;
        MSIRBuilder.BindVarAddr (p.var, addr, mt);

    | Kind.other =>
        val := Expr.CompileMSIR (p.expr);
        IF val = NIL THEN RETURN END;
        IF NOT Variable.AddLocalMSIR (p.var, MSIRBuilder.CurrentBlock()) THEN RETURN END;
        addr := MSIRBuilder.LookupVarAddr (p.var);
        IF addr = NIL THEN RETURN END;
        MSIR.BuildStore (MSIRBuilder.CurrentBlock (), val, addr);

    ELSE
        MSIRBuilder.Abandon ("WITH: structure kind not yet in MSIR");
        RETURN;
    END;

    Stmt.CompileMSIR (p.body);
  END CompileMSIR;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.GetOutcome (p.body);
  END GetOutcome;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    Expr.Capture (p.expr, ca);
    Stmt.Capture (p.body, ca);
    (* p.var is the WITH-bound alias — not an outer capture *)
  END Capture;

BEGIN
END WithStmt.
