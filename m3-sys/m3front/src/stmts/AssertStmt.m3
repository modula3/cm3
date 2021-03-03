(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AssertStmt.m3                                         *)
(* Last modified on Sat Nov 19 10:15:44 PST 1994 by kalsow     *)
(*      modified on Sat Mar 16 02:01:05 1991 by muller         *)

MODULE AssertStmt;

IMPORT CG, Expr, Token, Scanner, Stmt, StmtRep, Error;
IMPORT Host, EnumExpr, Type, Bool, Target, TInt, ErrType;
IMPORT Textt, Procedure, NarrowExpr, Module, AssignStmt, RunTyme;
IMPORT TextExpr, M3String;

TYPE
  P = Stmt.T OBJECT
        cond : Expr.T;
        msg  : Expr.T;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
      END;

CONST includeTextInAsserts = TRUE;

PROCEDURE Parse (): Stmt.T =
  VAR p: P;
      text: REF ARRAY OF CHAR := NIL;
      size := 0;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    IF includeTextInAsserts THEN
      Scanner.EnableTextCapture ();
    END;
    Scanner.Match (Token.T.tASSERT);
    p.cond := Expr.Parse ();
    p.msg  := NIL;
    IF (Scanner.cur.token = Token.T.tWITH)
      OR (Scanner.cur.token = Token.T.tCOMMA) THEN
      Scanner.GetToken ();  (* "," or "WITH" *)
      p.msg := Expr.Parse ();
    END;
    IF includeTextInAsserts THEN
      Scanner.DisableTextCapture (text, size);
      IF p.msg = NIL AND size > 3 THEN (* 3 is to remove "*>" *)
        p.msg := TextExpr.New8 (M3String.FromStr (text^, size - 3));
      END;
    END;
    IF (Scanner.cur.token # Token.T.tENDPRAGMA) THEN
      Scanner.Fail ("missing \'*>\'");
    END;
    Scanner.cur.token := Token.T.tSEMI;  (* for the statement parser *)
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;
  BEGIN
    Expr.TypeCheck (p.cond, cs);
    t := Type.Base (Expr.TypeOf (p.cond));
    IF (t # Bool.T) AND (t # ErrType.T) THEN
      Error.Msg ("ASSERT condition must be a BOOLEAN");
    END;
    IF (p.msg # NIL) THEN
      Expr.TypeCheck (p.msg, cs);
      t := Type.Base (Expr.TypeOf (p.msg));
      IF Type.IsAssignable (Textt.T, t) THEN
        p.msg := CheckArg (Textt.T, t, p.msg, cs);
      ELSE
        Error.Msg ("ASSERT message must be assignable to TEXT");
      END;
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
    IF Host.doAsserts THEN
      i := TInt.MOne;
      v := Expr.ConstValue (p.cond);
      IF (v = NIL) THEN
        skip := CG.Next_label ();
        IF (p.msg # NIL) THEN Expr.Prep (p.msg); END;
        Expr.PrepBranch (p.cond, skip, CG.No_label, CG.Always);
        Expr.CompileBranch (p.cond, skip, CG.No_label, CG.Always);
        Crash (p);
        CG.Set_label (skip);
      ELSIF EnumExpr.Split (v, i, u) AND TInt.EQ (i, TInt.Zero) THEN
        (* ASSERT (FALSE) *)
        IF (p.msg # NIL) THEN Expr.Prep (p.msg); END;
        Crash (p);
        RETURN Stmt.Outcomes {};
      ELSE <* ASSERT TInt.EQ (i, TInt.One) *>
        (* ASSERT (TRUE) *)
      END;
    END;
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END Compile;

PROCEDURE Crash (p: P) =
  VAR proc: Procedure.T;  this_file: TEXT;  this_line: INTEGER;
  BEGIN
    IF (p.msg = NIL) THEN
      CG.Abort (CG.RuntimeError.AssertFailed);
      RETURN;
    END;

    Scanner.Here (this_file, this_line);
    proc := RunTyme.LookUpProc (RunTyme.Hook.AssertFailed);
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      (* module data pointer *)
        CG.Load_addr_of (Module.GlobalData (FALSE), 0, CG.Max_alignment);
        CG.Pop_param (CG.Type.Addr);
      (* line number *)
        CG.Load_intt (this_line);
        CG.Pop_param (Target.Integer.cg_type);
      (* message *)
        Expr.Compile (p.msg);
        CG.Pop_param (CG.Type.Addr);
    ELSE
      (* message *)
        Expr.Compile (p.msg);
        CG.Pop_param (CG.Type.Addr);
      (* line number *)
        CG.Load_intt (this_line);
        CG.Pop_param (Target.Integer.cg_type);
      (* module data pointer *)
        CG.Load_addr_of (Module.GlobalData (FALSE), 0, CG.Max_alignment);
        CG.Pop_param (CG.Type.Addr);
    END;
    Procedure.EmitCall (proc);
  END Crash;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END GetOutcome;

BEGIN
END AssertStmt.
