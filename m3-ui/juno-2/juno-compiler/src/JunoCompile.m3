(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 27 18:44:43 PST 1997 by heydon                   *)
(*      modified on Tue Feb 21 14:47:07 PST 1995 by gnelson                  *)
(*      modified on Fri Aug  7 21:54:03 PDT 1992 by myers                    *)

MODULE JunoCompile EXPORTS JunoCompile, JunoCompileRep;

IMPORT BuiltInSlots, JunoAST, JunoChkBNF, JunoAssemble, JunoScope;
IMPORT   JunoUnparse, JunoCompileNF;
FROM JunoCompileErr IMPORT Error, ErrVal, Raise;
IMPORT JunoRT, JunoDisassem;
IMPORT StackTbl, JunoASTUtils AS Utils;
IMPORT Thread, AtomIntTbl;

(* Note on Error Reporting:

   The procedure "JunoCompileErr.Raise" is used to report compilation errors.
   In general, the philosophy of the compiler is to report errors as soon as
   possible. When a command is compiled, there are roughly 4 stages:

|    1) JunoChkBNF: check that the AST conforms to the grammar
|    2) JunoCompile.AnnotateAtoms
|    3) main compilation work
|    4) JunoAssemble.Cmd: assemble AST into bytestream

   "JunoChkBNF" only reports errors to indicate that the AST does not conform
   to the Juno-2 grammar. This is necessary because the parser actually allows
   a superset of the grammar. Many of the compilation errors are reported by
   "AnnotateAtoms", but some are also produced by the routines that do the
   compilation work. "JunoAssemble.Cmd" does not report any errors. *)

IMPORT Atom, Wr, Rd, Lex, Fmt, FloatMode;
FROM Stdio IMPORT stderr;
<* FATAL Wr.Failure, Thread.Alerted *>
VAR debug := 0;
(* = 0 => No debugging output,
| >= 1 => Show when a pred, proc, or func is compiled
| >= 2 => Show the predicate/command to be compiled
| >= 3 => Show the command produced by the compilation
| >= 4 => Show the disassembled version of the assembled command
*) 

VAR mu := NEW(MUTEX);
(* The procedures exported by the "JunoCompile" interface
   are protected by "mu". *)

VAR varTbl := NEW(AtomIntTbl.Default);
(* This table is reused in "AnnotateAtoms_NearVarList" to determine if the
   same variable appears more than once in an existential quantification or
   projection command. *)

VAR (* CONST *)
  saveName := Atom.FromText("Save");
  restoreName := Atom.FromText("Restore");

(* ============================ Declarations =============================== *)

VAR (* CONST *)
  dummy_mod := Atom.FromText("DUMMY_MOD");
  dummy_var := Atom.FromText("DUMMY_VAR");
  dummy_slot := JunoRT.GetVarIndex(dummy_mod, dummy_var);
  dummy_qid := NEW(JunoAST.QId, id0 := dummy_mod, id1 := dummy_var,
    type := JunoAST.IdType.Var, index := dummy_slot);
  dummy_assign := NEW(JunoAST.Assign, vars := Utils.NewQIdList(dummy_qid));

VAR (* CONST *)
  CompStackSize := 2 * Thread.GetDefaultStackSize();

TYPE
  CompClosure = Thread.SizedClosure OBJECT  (* Class only -- do not NEW *)
    error: BOOLEAN := FALSE;
    errVal: ErrVal;
  END;

(* We fork a separate thread for compilations so as to use a larger thread
   stack. The closure in each case is a subtype of a "CompClosure".

   The field "error" is true iff the compilation resulted in the exception
   "JunoCompileErr.Error". In this case, "errVal" is the argument to the
   "Error" exception. *)

  ExprClosure = CompClosure OBJECT
    expr: JunoAST.Expr;
    scp: JunoScope.T;
    pure: BOOLEAN;
    res: JunoRT.ByteStream;
  END;

PROCEDURE Expr(
    expr: JunoAST.Expr;
    scp: JunoScope.T;
    nm: JunoAST.Id;
    VAR (*OUT*) val_slot: CARDINAL;
    pure := FALSE):
    JunoRT.ByteStream RAISES {Error} =
  VAR cl: ExprClosure; BEGIN
    val_slot := dummy_slot;
    LOCK mu DO
      IF debug > 0 THEN
	Wr.PutText(stderr, "\nCompiling expression for \"");
        Wr.PutText(stderr, Atom.ToText(nm) & "\"...\n");
	Wr.Flush(stderr)
      END;
      cl := NEW(ExprClosure, stackSize := CompStackSize,
        expr := expr, scp := scp, pure := pure, apply := ExprApply);
      EVAL Thread.Join(Thread.Fork(cl));
      IF cl.error THEN RAISE Error(cl.errVal) END;
      DebugDisassemble(cl.res);
      RETURN cl.res
    END
  END Expr;

PROCEDURE ExprApply(cl: ExprClosure): REFANY =
  VAR tbl := NEW(StackTbl.T).init(); BEGIN
    TRY
      JunoChkBNF.Expr(cl.expr);
      AnnotateAtoms(cl.expr, tbl, cl.scp, ASTKind.Expr, cl.pure);
      dummy_assign.exprs := Utils.NewExprList(cl.expr);
      cl.res := JunoAssemble.Cmd(
    	Cmd(dummy_assign, cl.scp, tbl, annotate := FALSE),
    	cl.scp, temp_cnt := tbl.next_index - 1,
    	type := JunoAssemble.CmdType.Proc)
    EXCEPT
      Error (errVal) => cl.error := TRUE; cl.errVal := errVal
    END;
    RETURN NIL
  END ExprApply;

TYPE
  PredClosure = CompClosure OBJECT
    pred: JunoScope.Pred;
    scp: JunoScope.T;
  END;

PROCEDURE PredDecl(nm: JunoAST.Id; pred: JunoScope.Pred; scp: JunoScope.T)
    RAISES {Error} =
  VAR cl: PredClosure; BEGIN
    LOCK mu DO
      IF debug > 0 THEN
	Wr.PutText(stderr, "\nCompiling predicate \"");
        Wr.PutText(stderr, Atom.ToText(nm) & "\"...\n");
	Wr.Flush(stderr)
      END;
      cl := NEW(PredClosure, stackSize := CompStackSize,
        pred := pred, scp := scp, apply := PredApply);
      EVAL Thread.Join(Thread.Fork(cl));
      IF cl.error THEN RAISE Error(cl.errVal) END;
      DebugDisassemble(JunoRT.code_tbl[pred.index])
    END
  END PredDecl;

PROCEDURE PredApply(cl: PredClosure): REFANY =
  BEGIN
    TRY
      WITH pred = cl.pred DO
      	(* Check for legality; convert to normal form *)
      	JunoChkBNF.Formula(pred.body);
      	AnnotateAtoms(pred.body, pred.tbl, cl.scp, ASTKind.Pred, pure := TRUE);
      	pred.normal_form := JunoCompileNF.Normalize(pred.body, pred.tbl);
      	IF debug > 1 THEN
	  Wr.PutText(stderr, "\n  Original Predicate:\n");
	  DebugUnparse(pred.body);
      	  Wr.PutText(stderr, "\n  Normal Form:\n");
      	  DebugUnparse(pred.normal_form)
      	END;
      	(* Compile command and install in global code table *)
      	VAR cmd := NormalForm(pred.normal_form, cl.scp, pred.tbl); BEGIN
	  JunoRT.code_tbl[pred.index] := JunoAssemble.Cmd(cmd, cl.scp,
      	    temp_cnt := pred.tbl.next_index - 1,
      	    type := JunoAssemble.CmdType.Pred)
      	END
      END
    EXCEPT
      Error (errVal) => cl.error := TRUE; cl.errVal := errVal
    END;
    RETURN NIL
  END PredApply;

TYPE
  FuncClosure = CompClosure OBJECT
    func: JunoScope.Func;
    scp: JunoScope.T;
  END;

PROCEDURE FuncDecl(nm: JunoAST.Id; func: JunoScope.Func; scp: JunoScope.T)
    RAISES {Error} =
  VAR cl: FuncClosure; BEGIN
    LOCK mu DO
      IF debug > 0 THEN
	Wr.PutText(stderr, "\nCompiling function \"");
        Wr.PutText(stderr, Atom.ToText(nm) & "\"...\n");
	Wr.Flush(stderr)
      END;
      cl := NEW(FuncClosure, stackSize := CompStackSize,
        func := func, scp := scp, apply := FuncApply);
      EVAL Thread.Join(Thread.Fork(cl));
      IF cl.error THEN RAISE Error(cl.errVal) END;
      DebugDisassemble(JunoRT.code_tbl[func.index])
    END
  END FuncDecl;

PROCEDURE FuncApply(cl: FuncClosure): REFANY =
  BEGIN
    TRY
      WITH func = cl.func DO
      	(* Check for legality; convert to normal form *)
      	JunoChkBNF.Formula(func.body);
      	AnnotateAtoms(func.body, func.tbl, cl.scp, ASTKind.Pred, pure := TRUE);
      	func.normal_form := JunoCompileNF.Normalize(func.body, func.tbl);
      	IF debug > 1 THEN
	  Wr.PutText(stderr, "\n  Original Predicate:\n");
	  DebugUnparse(func.body);
      	  Wr.PutText(stderr, "\n  Normal Form:\n");
      	  DebugUnparse(func.normal_form)
      	END;
      	(* Compile command and install in global code table *)
      	CONST
          outSet = SET OF JunoScope.ArgKind{JunoScope.ArgKind.Out};
      	VAR
      	  outList := JunoScope.LocalArgs(func.formals, outSet);
	  cmd := NormalForm(func.normal_form, cl.scp, func.tbl,
      	    xtra_vars := Utils.IdListToNearVarList(outList));
      	BEGIN
	  JunoRT.code_tbl[func.index] := JunoAssemble.Cmd(cmd, cl.scp,
      	    temp_cnt := func.tbl.next_index - 1,
      	    type := JunoAssemble.CmdType.Func)
      	END
      END
    EXCEPT
      Error (errVal) => cl.error := TRUE; cl.errVal := errVal
    END;
    RETURN NIL
  END FuncApply;

TYPE
  ProcClosure = CompClosure OBJECT
    proc: JunoScope.Proc;
    scp: JunoScope.T;
    res: Result;
  END;

PROCEDURE ProcDecl(nm: JunoAST.Id; proc: JunoScope.Proc; scp: JunoScope.T):
    JunoAST.Cmd RAISES {Error} =
  VAR cl: ProcClosure; BEGIN
    LOCK mu DO
      IF debug > 0 THEN
	Wr.PutText(stderr, "\nCompiling procedure \"");
        Wr.PutText(stderr, Atom.ToText(nm) & "\"...\n");
	Wr.Flush(stderr)
      END;
      cl := NEW(ProcClosure, stackSize := CompStackSize,
        proc := proc, scp := scp, apply := ProcApply);
      EVAL Thread.Join(Thread.Fork(cl));
      IF cl.error THEN RAISE Error(cl.errVal) END;
      DebugDisassemble(JunoRT.code_tbl[proc.index]);
      RETURN cl.res.cmd
    END
  END ProcDecl;

PROCEDURE ProcApply(cl: ProcClosure): REFANY =
  BEGIN
    TRY
      WITH proc = cl.proc DO
      	JunoChkBNF.TotalCmd(proc.body);
      	cl.res := Cmd(proc.body, cl.scp, proc.tbl);
      	JunoRT.code_tbl[proc.index] := JunoAssemble.Cmd(cl.res, cl.scp,
      	  temp_cnt := proc.tbl.next_index - 1,
      	  type := JunoAssemble.CmdType.Proc)
      END
    EXCEPT
      Error (errVal) =>
        cl.error := TRUE;
        cl.errVal := errVal
    END;
    RETURN NIL
  END ProcApply;

PROCEDURE DebugDisassemble(stream: JunoRT.ByteStream) =
  BEGIN
    IF debug > 3 THEN
      Wr.PutText(stderr, "\n  Assembled Command:\n");
      JunoDisassem.P(stream, stderr);
      Wr.Flush(stderr)
    END
  END DebugDisassemble;

(* ========================== Annotate Atoms =============================== *)

TYPE ASTKind = { Pred, Expr, Cmd };

PROCEDURE AnnotateAtoms(
    ast: JunoAST.T;
    t: StackTbl.T;
    scp: JunoScope.T;
    kind: ASTKind;
    pure := FALSE)
  RAISES {Error} =
(* Destructively annotate the index of every node in "ast" that has an index,
   and set the "bp" field of each node to point to itself (except
   "JunoAST.GroupedExpr" and "JunoAST.GroupedCmd" nodes). The "kind" argument
   indicates the top-level class of "ast". The "ast" may contain query nodes.

   When a projected or existential variable is encountered, assign it the new
   index "t.next_index", and push the variable onto the stack table. When a
   variable use "x" is encountered, it is assigned the index associated with
   "x" in "t".

   Raises "Error" if "ast" contains a compilation error. Also raises "error"
   if "pure" is set and "ast" contains a reference to a procedure (according
   to "scp"). *)

  PROCEDURE Pred0(ast: JunoAST.Formula) RAISES {Error} =
    BEGIN
      TYPECASE Utils.Ungroup(ast) OF <* NOWARN *>
      | JunoAST.LitPred => RETURN
      | JunoAST.And (e) => Pred0(e.f1); Pred0(e.f2)
      | JunoAST.Or (e) => Pred0(e.f1); Pred0(e.f2)
      | JunoAST.Not (e) => Pred0(e.f);
      | JunoAST.Exists (e) => 
	  StackTbl.Mark(t);
          NearVarList(e.vars);
	  Pred0(e.f);
	  StackTbl.PopToMark(t)
      | JunoAST.BIUPred (e) => Expr0(e.e)
      | JunoAST.Relation (e) => Expr0(e.e1); Expr0(e.e2)
      | JunoAST.Call (c) =>
          VAR ent := QIdInScope(c.name); pred: JunoScope.Pred; BEGIN
            TYPECASE ent OF
	      NULL => Raise("Undefined predicate", c)
            | JunoScope.Pred (p) => pred := p; c.normal_form := p.normal_form
            ELSE Raise("Not a predicate", c)
	    END;
	    (* Typecheck "c" *)
	    IF c.inouts.size # 0 THEN
	      Raise("Predicates don't have INOUT parameters", c)
	    ELSIF pred.in_cnt # c.ins.size THEN
	      Raise("Wrong number of IN parameters", c)
	    END
          END;
          LValueList(c.inouts);
          ExprList0(c.ins)
      END
    END Pred0;

  PROCEDURE QId0(qid: JunoAST.QId; lvalue: BOOLEAN) RAISES {Error} =
    BEGIN
      IF qid.id0 = JunoAST.NilId THEN
        VAR i := StackTbl.Lookup(t, qid.id1); BEGIN
          IF i # 0 THEN
            qid.type := JunoAST.IdType.Local;
            qid.index := i;
            RETURN
          END
        END
      END;
      (* Qualified id or unqualified non-local variable *)
      EVAL QIdInScope(qid);
      IF lvalue THEN
        CASE qid.type OF
          JunoAST.IdType.None, JunoAST.IdType.Const, JunoAST.IdType.Pred,
          JunoAST.IdType.Func, JunoAST.IdType.Proc, JunoAST.IdType.ExtProc =>
            Raise("You can only assign values to variables", qid)
        ELSE (* SKIP *)
        END
      ELSE
        CASE qid.type OF
          JunoAST.IdType.None, JunoAST.IdType.Pred, JunoAST.IdType.Func =>
            Raise("This is not a valid expression", qid)
        ELSE (* SKIP *)
        END
      END
    END QId0;

  PROCEDURE QIdInScope(qid: JunoAST.QId): JunoScope.Entity RAISES {Error} =
  (* Annotate the variable "qid" to be either a local variable, formal
     argument, constant, predicate, function, global variable, or procedure,
     depending on the type of entity it is bound to in "scp". Also annotate
     the variable's index.

     Raises "Error" if "qid" is either an undeclared identifier or is an
     unqualified module name, or if "pure" is set and "qid" refers to a
     procedure.

     Return the entity associated with "qid" in "scp". *)
    VAR
      unit: JunoScope.Entity;
      ent := JunoScope.LookupQId(scp, qid, unit);
    BEGIN
      TYPECASE ent OF <* NOWARN *>
        NULL =>
          IF unit = NIL AND qid.id0 # JunoAST.NilId
	    THEN Raise("Undefined module", qid)
	    ELSE Raise("Undefined identifier", qid)
	  END
      | JunoScope.Mod => Raise("This identifies a module", qid)
      | JunoScope.LocalValue (local) =>
          qid.type := JunoAST.IdType.Local;
          qid.index := local.offset
      | JunoScope.Const (const) =>
          qid.type := JunoAST.IdType.Const;
          qid.index := const.index
      | JunoScope.Pred (pred) =>
          qid.type := JunoAST.IdType.Pred;
          qid.index := pred.index
      | JunoScope.Func (func) =>
          qid.type := JunoAST.IdType.Func;
          qid.index := func.index
      | JunoScope.Var (var) =>
          qid.type := JunoAST.IdType.Var;
          qid.index := var.index
      | JunoScope.Proc (proc) =>
          IF pure THEN
            Raise("You can't call a procedure in a constraint", qid)
          END;
          IF proc.external
            THEN qid.type := JunoAST.IdType.ExtProc
            ELSE qid.type := JunoAST.IdType.Proc
          END;
          qid.index := proc.index
      END;
      RETURN ent
    END QIdInScope;

  PROCEDURE CheckInCnts(c: JunoAST.T; nm: JunoAST.QId;
      realInoutCnt, realInCnt, inoutCnt, inCnt: CARDINAL)
      RAISES {Error} =
  (* Assume "c" is a "JunoAST.Call" or "JunoAST.ProcCall" to the
     procedure/predicate/function named "nm" with "realInCnt" and
     "realInoutCnt" IN and INOUT arguments.

     If "nm" is a call to "APPLY", then it is simply required to have at least
     one IN parameter. If it is a call to "CLOSE", it is required to have
     "inoutCnt" INOUT parameters (namely, 0), and at least one IN parameter.
     Otherwise, it is required to have "inCnt" IN parameters, and "inoutCnt"
     INOUT parameters. If any of these conditions is violoated, "Error" is
     raised with an appropriate error message and associated AST "c". *)
    BEGIN
      (* check APPLY case *)
      IF BuiltInSlots.IsApplyProc(nm) THEN
        IF realInCnt < 1 THEN
          Raise("APPLY requires at least one IN parameter", c)
        END
      (* check INOUT parameters *)
      ELSIF realInoutCnt # inoutCnt THEN
	Raise("Wrong number of INOUT parameters", c)
      (* check CLOSE case *)
      ELSIF BuiltInSlots.IsCloseProc(nm) THEN
        IF realInCnt < 1 THEN
          Raise("CLOSE requires at least IN parameter", c)
        END
      (* check IN parameters *)
      ELSIF realInCnt # inCnt THEN
        Raise("Wrong number of IN parameters", c)
      END
    END CheckInCnts;

  PROCEDURE CheckArgCnts(c: JunoAST.T; nm: JunoAST.QId;
    realOutCnt, realInoutCnt, realInCnt, outCnt, inoutCnt, inCnt: CARDINAL)
    RAISES {Error} =
  (* Like "CheckInCnts" above, but also checks that the number of OUT
     parameters also agree. That is, so long as "nm" is not "APPLY",
     "realOutCnt" must agree with "outCnt". *)
    BEGIN
      CheckInCnts(c, nm, realInoutCnt, realInCnt, inoutCnt, inCnt);
      IF realOutCnt # outCnt AND NOT BuiltInSlots.IsApplyProc(nm) THEN
        Raise("Wrong number of OUT parameters", c)
      END
    END CheckArgCnts;

  PROCEDURE Expr0(ast: JunoAST.Expr) RAISES {Error} =
    BEGIN
      TYPECASE Utils.Ungroup(ast) OF <* NOWARN *>
      | JunoAST.Text (e) => e.index := JunoRT.GetValueIndex(e.val)
      | JunoAST.LitValue => (* SKIP JunoAST.Number and JunoAST.Nil *)
      | JunoAST.QId (e) => QId0(e, lvalue := FALSE)
      | JunoAST.BIUFunc (e) => Expr0(e.e);
      | JunoAST.BIBFunc (e) => Expr0(e.e1); Expr0(e.e2)
      | JunoAST.List (e) => ExprList0(e.elts);
      | JunoAST.Call (c) =>
          VAR ent := QIdInScope(c.name); BEGIN
            TYPECASE ent OF
              NULL => Raise("Undefined function", c)
            | JunoScope.Func (f) =>
                c.normal_form := f.normal_form;
                CheckInCnts(c, c.name, c.inouts.size, c.ins.size, 0, f.in_cnt)
            | JunoScope.Proc (p) =>
                IF p.out_cnt # 1 AND NOT BuiltInSlots.IsApplySlot(p.index) THEN
                  Raise("Not a functional procedure", c)
                END;
                CheckInCnts(c, c.name, c.inouts.size, c.ins.size,
                  p.inout_cnt, p.in_cnt)
            ELSE
                Raise("Not a function", c)
	    END
          END;
          LValueList(c.inouts);
          ExprList0(c.ins)
      END
    END Expr0;

  PROCEDURE LValueList(el: JunoAST.ExprList) RAISES {Error} =
    VAR curr := el.head; BEGIN
      WHILE curr # NIL DO
        QId0(curr.expr, lvalue := TRUE);
        curr := curr.next
      END
    END LValueList;

  PROCEDURE ExprList0(el: JunoAST.ExprList) RAISES {Error} =
    VAR curr := el.head; BEGIN
      WHILE curr # NIL DO
        Expr0(curr.expr);
        curr := curr.next
      END
    END ExprList0;

  PROCEDURE Cmd0(ast: JunoAST.T) RAISES {Error} =
    BEGIN
      TYPECASE Utils.Ungroup(ast) OF <* NOWARN *>
	JunoAST.Skip, JunoAST.Abort, JunoAST.Halt => RETURN
      | JunoAST.Assign (c) =>
          IF c.vars.size # c.exprs.size THEN
            Raise("The number of variables on the left\n"
              & "is different from the\nnumber of terms on the right", c)
          END;
          LValueList(c.vars);
          ExprList0(c.exprs)
      | JunoAST.If (c) => Cmd0(c.body)
      | JunoAST.Do (c) => Cmd0(c.body)
      | JunoAST.Save (c) =>
          c.save := NEW(JunoAST.QId, bp := c.nm,
            id0 := c.nm.id1, id1 := saveName);
          c.restore := NEW(JunoAST.QId, bp := c.nm,
            id0 := c.nm.id1, id1 := restoreName);
          EVAL QIdInScope(c.save);
          EVAL QIdInScope(c.restore);
          Cmd0(c.body)
      | JunoAST.Proj (c) => 
	  StackTbl.Mark(t);
          NearVarList(c.vars);
	  Cmd0(c.body);
	  StackTbl.PopToMark(t)
      | JunoAST.Seq (c) =>
          (* The following is equivalent to "Cmd0(c.c1); Cmd0(c.c2)"
             except that it uses fewer stack frames when "c" is a long
             right-associative list of semi-colon-separated commands. *)
          Cmd0(c.c1);
          LOOP
            TYPECASE Utils.Ungroup(c.c2) OF JunoAST.Seq (newC) =>
              Cmd0(newC.c1);
              c := newC
            ELSE EXIT
            END
          END;
          Cmd0(c.c2)
      | JunoAST.Guard (c) => Pred0(c.grd); Cmd0(c.body)
      | JunoAST.Else (c) => Cmd0(c.c1); Cmd0(c.c2)
      | JunoAST.ProcCall (c) =>
          <* ASSERT NOT pure *>
          VAR ent := QIdInScope(c.name); proc: JunoScope.Proc; BEGIN
            TYPECASE ent OF
              NULL => Raise("Undefined procedure", c)
            | JunoScope.Proc (p) => proc := p
            ELSE Raise("Not a procedure", c)
	    END;
            CheckArgCnts(c, c.name, c.outs.size, c.inouts.size, c.ins.size,
              proc.out_cnt, proc.inout_cnt, proc.in_cnt)
          END;
          LValueList(c.outs);
          LValueList(c.inouts);
          ExprList0(c.ins)
      | JunoAST.Query (c) => Pred0(c.f); QueryVars(c.vars)
      END;
    END Cmd0;

  PROCEDURE NearVarList(nvl: JunoAST.NearVarList) RAISES {Error} =
  (* Annotate the variables in the list "head" and their hint expressions
     (if any). This procedure works in two passes. First, it annotates all
     of the variables; then it annotates the hints. This is necessary
     because a hint may refer to a variable introduced later in the list.
     Raise "Error" if the same variable appears more than once in the list. *)
    VAR head := nvl.head; BEGIN
      (* Mark the variables *)
      EVAL varTbl.init(sizeHint := 20);
      VAR l: JunoAST.NearVarLink := head; BEGIN
        WHILE l # NIL DO
          IF varTbl.put(l.id, 0) THEN
            Raise("The variable \"" & Atom.ToText(l.id)
              & "\" appears more than once", nvl)
          END;
          l.index := t.next_index;
          StackTbl.Push(t, l.id);
          l := l.next
        END
      END;
      (* annotate their hints *)
      VAR l: JunoAST.NearVarLink := head; BEGIN
        WHILE l # NIL DO
          IF l.hint # JunoAST.NilExpr THEN Expr0(l.hint) END;
          l := l.next
        END
      END
    END NearVarList;

  PROCEDURE QueryVars(nv: JunoAST.NearVarList) RAISES {Error} =
  (* Annotate the indices of the query variables in "nv" from the stack
     table "t". Any hints in this list are *not* annotated. Raise "Error" if
     any of the variables is not bound to an index in "t". *)
    VAR curr := nv.head; BEGIN
      WHILE curr # NIL DO
        VAR i := StackTbl.Lookup(t, curr.id); BEGIN
          IF i = 0 THEN Raise("Query variable is not a local", nv) END;
          curr.index := i
        END;
        curr := curr.next
      END
    END QueryVars;

  (* AnnotateAtoms *)
  BEGIN
    CASE kind OF <*NOWARN*>
      ASTKind.Pred => Pred0(ast)
    | ASTKind.Expr => Expr0(ast)
    | ASTKind.Cmd  => Cmd0(ast)
    END
  END AnnotateAtoms;

(* =============================== Cmd ===================================== *)

PROCEDURE Cmd(
    cmd: JunoAST.Cmd;
    scp: JunoScope.T;
    stack_tbl: StackTbl.T;
    annotate := TRUE;
    pure := FALSE):
  Result RAISES {Error} =

  PROCEDURE C1(cmd: JunoAST.Cmd): JunoAST.Cmd RAISES {Error} =
  (* Returns a command equivalent to "cmd" in which all commands to the
     right of guard arrows are total, and in which all "Near" and "Equals"
     constraints on projected variables have been extracted (hence, all
     projected variables are unhinted with reset "frozen" bits).

     Here are the rewrite rules embodied by this procedure:

|      C1(DO A OD) == DO C1(A) OD                            (rule C1.06)
|      C1(IF A FI) == IF C1(A) FI                            (rule C1.07)
|      C1(SAVE M IN A END) == SAVE M IN C1(A) END            (rule C1.08)
|      C1(A ; B) == C1(A) ; C1(B)                            (rule C1.09)
|      C1(A | B) == C1(A) | C1(B)                            (rule C1.10)
|      C1(VAR x IN A END) == VAR x IN C1(A) END              (rule C1.11)
|      C1(P -> A) == C1Grd(P -> A)                           (see C1Grd below)
|      C1(A) == A (SKIP, ABORT, HALT, Assignment, ProcCall, Query)

     In rule C1.11 (projection), any hints associated with the projected
     variables "x" are stripped out and incorporated as a new guard on the
     command "A". For example:

|      C1(VAR x ~ f(y) IN A END) == VAR x IN C1(x ~ f(y) -> A) END

     Queries are only allowed to appear in "cmd" under certain conditions. It
     is assumed that a query of the form "P?(vlist)" will appear in "cmd"
     if and only if all of the variables "vlist" are all locally projected
     variables in scope. Furthermore, for each projection command in "cmd",
     the guard of the command, when computed according to the rewrite rules
     below, may not contain an expression of the form "Grd(<query>)". The
     guard rewrite rules are:

|      Grd(A ; B)  == Grd(A)               (Grd(B) must be True)
|      Grd(A | B)  == Grd(A) OR Grd(B)
|      Grd(x :: S) == (E x: Grd(S))
|      Grd(P -> S) == P => Grd(S)

     All other commands are total, i.e., Grd(S) == True. *)
    BEGIN
      cmd := Utils.Ungroup(cmd);
      TYPECASE cmd OF <* NOWARN *>
	JunoAST.Skip, JunoAST.Abort, JunoAST.Halt, JunoAST.Assign,
        JunoAST.ProcCall, JunoAST.Query =>
	  RETURN cmd
      | JunoAST.Do (c) =>				       (* rule C1.06 *)
	  RETURN NEW(JunoAST.Do, bp := c, body := C1(c.body))
      | JunoAST.If (c) =>				       (* rule C1.07 *)
	  RETURN NEW(JunoAST.If, bp := c, body := C1(c.body))
      | JunoAST.Save (c) =>				       (* rule C1.08 *)
	  RETURN NEW(JunoAST.Save, bp := c, nm := c.nm, body := C1(c.body),
            save := c.save, restore := c.restore)
      | JunoAST.Seq (c) =>				       (* rule C1.09 *)
	  RETURN C1Seq(c)
      | JunoAST.Else (c) =>				       (* rule C1.10 *)
	  RETURN NEW(JunoAST.Else, bp := c, c1 := C1(c.c1), c2 := C1(c.c2))
      | JunoAST.Proj (c) =>				       (* rule C1.11 *)
          VAR
            hints := Utils.ExtractHints(c.vars);
            body := c.body; uv := c.vars;
          BEGIN
            IF hints # JunoAST.TrueVal THEN
              body := NEW(JunoAST.Guard, bp := c, grd := hints, body := body);
              uv := Utils.StripHints(uv)
            END;
	    RETURN NEW(JunoAST.Proj, bp := c, vars := uv, body := C1(body))
          END
      | JunoAST.Guard (g) => RETURN C1Grd(g)
      END
    END C1;

  PROCEDURE C1Seq(seq: JunoAST.Seq): JunoAST.Seq RAISES { Error } =
  (* Equivalent to:
|
|      RETURN NEW(JunoAST.Seq, bp := seq,
|        c1 := C1(seq.c1), c2 := C1(seq.c2))
|
     except that it uses fewer stack frames when "seq" is a long
     right-associative list of semi-colon-separated commands. *)
    VAR
      res := NEW(JunoAST.Seq, bp := seq, c1 := C1(seq.c1));
      curr := res;
    BEGIN
      LOOP
        TYPECASE Utils.Ungroup(seq.c2) OF JunoAST.Seq (newSeq) =>
          curr.c2 := NEW(JunoAST.Seq, bp := newSeq, c1 := C1(newSeq.c1));
          seq := newSeq;
          curr := curr.c2
        ELSE EXIT
        END
      END;
      curr.c2 := C1(seq.c2);
      RETURN res
    END C1Seq;

  PROCEDURE C1Grd(g: JunoAST.Guard): JunoAST.Cmd RAISES {Error} =
  (* This is the procedure "C1" above specialized to compiling guarded
     commands only. Here are the compilation rules:

|      C1(P -> VAR x IN A END) == VAR x IN C1(P -> A) END      (rule C1.12)
|      C1(P -> { A | B }) == C1(P -> A) | C1(P -> B)           (rule C1.13)
|      C1(P -> { A ; B }) == C1(P -> A) | C1(B)                (rule C1.14)
|      C1(P -> { Q -> A }) == C1((P AND Q) -> A)               (rule C1.15)
|      C1(P -> A) == P -> C1(A) (all other cases)              (rule C1.11)

    In the first rule, any hints associated with the projected variables "x"
    are stripped out and incorporated into "P", and all variables in the
    resulting projected list of variables are made unhinted with reset
    "frozen" bits.

    Since we know that "C1(P -> A) == C1Grd(P -> A)", we invoke "C1Grd"
    directly for those right-hand sides in the rules above that contain
    statements of the form "C1(P -> A)". This saves a stack frame, procedure
    call, and the work of doing the TYPECASE in "C1".
  *)
    VAR gBody := Utils.Ungroup(g.body); BEGIN
      TYPECASE gBody OF
        JunoAST.Proj (c) =>                                (* rule C1.12 *)
          VAR
            hints := Utils.ExtractHints(c.vars);
            grd := g.grd; uv := c.vars;
          BEGIN
            IF hints # JunoAST.TrueVal THEN
              grd := NEW(JunoAST.And, bp := g, f1 := hints, f2 := grd);
              uv := Utils.StripHints(uv)
            END;
            RETURN NEW(JunoAST.Proj, bp := g, vars := uv,
              body := C1Grd(NEW(JunoAST.Guard, bp := g,
                grd := grd, body := c.body)))
          END
      | JunoAST.Else (c) =>                                (* rule C1.13 *)
          RETURN NEW(JunoAST.Else, bp := c,
            c1 := C1Grd(NEW(JunoAST.Guard,
              bp := g, grd := g.grd, body := c.c1)),
            c2 := C1Grd(NEW(JunoAST.Guard,
              bp := g, grd := g.grd, body := c.c2)))
      | JunoAST.Seq (c) =>                                 (* rule C1.14 *)
          RETURN NEW(JunoAST.Seq, bp := c,
            c1 := C1Grd(NEW(JunoAST.Guard,
              bp := g, grd := g.grd, body := c.c1)),
            c2 := C1(c.c2))
      | JunoAST.Guard (c) =>                               (* rule C1.15 *)
          RETURN C1Grd(NEW(JunoAST.Guard, bp := c,
            grd := NEW(JunoAST.And, bp := g, f1 := g.grd, f2 := c.grd),
            body := c.body))
      | JunoAST.Query => <* ASSERT FALSE *>
      ELSE                                                 (* rule C1.11 *)
        RETURN NEW(JunoAST.Guard, bp := g,
          grd := g.grd, body := C1(gBody))
      END
    END C1Grd;
  
  PROCEDURE C2(cmd: JunoAST.Cmd): JunoAST.Cmd RAISES {Error} =
  (* Returns a command equivalent to "cmd" in which:

|     - guard arrows are replaced by queries
|     - constraints are compiled into normal form
|     - projections and E quantifications are eliminated
|     - SAVE commands are eliminated

     This procedure requires that all projected variables are unhinted with
     reset "frozen" bits, but ``de-sugars'' hints on existentially quantified
     variables.

     Here are the compilation rules implemented by this procedure:

|      C2(DO A OD) == DO C2(A) OD                               (rule C2.05)
|      C2(IF A FI) == IF C2(A) FI                               (rule C2.06)
|      C2(SAVE M IN A END) == M.Save(); C2(A); M.Restore()      (rule C2.07)
|      C2(A ; B) == C2(A) ; C2(B)                               (rule C2.08)
|      C2(A | B) == C2(A) | C2(B)                               (rule C2.09)
|      C2(P -> A) == C2q(P) ; C2(A)                             (rule C2.10)
|      C2(VAR x IN A END) == C2p(A, x)                          (rule C2.11)
|      C2(P?(v)) == C2pp(P, {v})
|      C2(A) == A (SKIP, ABORT, HALT, Assignment, Procedure Call)
  *)
    BEGIN
      cmd := Utils.Ungroup(cmd);
      TYPECASE cmd OF <* NOWARN *>
	JunoAST.Skip, JunoAST.Abort, JunoAST.Halt,
        JunoAST.Assign, JunoAST.ProcCall =>
	  RETURN cmd
      | JunoAST.Do (c) =>				       (* rule C2.05 *)
	  RETURN NEW(JunoAST.Do, bp := c, body := C2(c.body))
      | JunoAST.If (c) =>				       (* rule C2.06 *)
	  RETURN NEW(JunoAST.If, bp := c, body := C2(c.body))
      | JunoAST.Save (c) =>				       (* rule C2.07 *)
          VAR
            save := NEW(JunoAST.ProcCall, bp := c, name := c.save,
              outs := JunoAST.EmptyQIdList, inouts := JunoAST.EmptyQIdList,
              ins := JunoAST.EmptyExprList);
            restore := NEW(JunoAST.ProcCall, bp := c, name := c.restore,
              outs := JunoAST.EmptyQIdList, inouts := JunoAST.EmptyQIdList,
              ins := JunoAST.EmptyExprList);
          BEGIN
            RETURN NEW(JunoAST.Seq, c1 := save,
              c2 := NEW(JunoAST.Seq, c1 := C2(c.body), c2 := restore))
          END
      | JunoAST.Seq (c) =>				       (* rule C2.08 *)
          RETURN C2Seq(c)
      | JunoAST.Else (c) =>				       (* rule C2.09 *)
	  RETURN NEW(JunoAST.Else, bp := c, c1 := C2(c.c1), c2 := C2(c.c2))
      | JunoAST.Guard (c) =>				       (* rule C2.10 *)
	  RETURN NEW(JunoAST.Seq, bp := c, c1 := C2q(c.grd), c2 := C2(c.body))
      | JunoAST.Proj (c) =>				       (* rule C2.11 *)
          (* NOTE: We can drop the projection here since all
	     variables have been annotated and their hints extracted. *)
          RETURN C2p(c.body, c.vars)
      | JunoAST.Query (c) =>
          RETURN C2pp(c.f, c.vars)
      END
    END C2;

  PROCEDURE C2Seq(seq: JunoAST.Seq): JunoAST.Seq RAISES { Error } =
  (* Equivalent to:
|
|      RETURN NEW(JunoAST.Seq, bp := seq,
|        c1 := C2(seq.c1), c2 := C2(seq.c2))
|
     except that it uses fewer stack frames when "seq" is a long
     right-associative list of semi-colon-separated commands. *)
    VAR
      res := NEW(JunoAST.Seq, bp := seq, c1 := C2(seq.c1));
      curr := res;
    BEGIN
      LOOP
        TYPECASE Utils.Ungroup(seq.c2) OF
          JunoAST.Seq (newSeq) =>
            curr.c2 := NEW(JunoAST.Seq, bp := newSeq, c1 := C2(newSeq.c1));
            seq := newSeq;
            curr := curr.c2
        ELSE EXIT
        END
      END;
      curr.c2 := C2(seq.c2);
      RETURN res
    END C2Seq;

  PROCEDURE C2q(pred: JunoAST.Formula): JunoAST.Cmd RAISES {Error} =
  (* Returns a command equivalent to the command "pred?()". Furthermore, the
     resulting command does not contain any predicates of the form "NOT p",
     "e1 # e2", or "(E x ~= e :: p)". Here are the compilation rules
     implemented by this procedure:

|      C2q(TRUE) == SKIP                                         (rule C2.12)
|      C2q(FALSE) == FAIL                                        (rule C2.13)
|      C2q(NOT P) == FLIP(C2q(P))                                (rule C2.14)
|      C2q(P OR Q) == C2q(P) | C2q(Q)                            (rule C2.15)
|      C2q(P AND Q) == C2q(P) ; C2q(Q)                           (rule C2.16)
|      C2q(e1 # e2) == FLIP((e1 = e2)?())                        (rule C2.17)
|      C2q((E x :: P)) == C2pp((E x :: P), {})                   (rule C2.20)
|      C2q(P) == (P)?() (all other cases)                        (rule C2.21)
*)
    BEGIN
      TYPECASE Utils.Ungroup(pred) OF
	JunoAST.True =>					       (* rule C2.12 *)
	  RETURN JunoAST.SkipVal
      | JunoAST.False =>				       (* rule C2.13 *)
	  RETURN JunoAST.FailVal
      | JunoAST.Not (p) =>				       (* rule C2.14 *)
	  RETURN NEW(JunoAST.Flip, bp := p, body := C2q(p.f))
      | JunoAST.Or (p) =>				       (* rule C2.15 *)
	  RETURN NEW(JunoAST.Else, bp := p, c1 := C2q(p.f1), c2 := C2q(p.f2))
      | JunoAST.And (p) =>				       (* rule C2.16 *)
	  RETURN NEW(JunoAST.Seq, bp := p, c1 := C2q(p.f1), c2 := C2q(p.f2))
      | JunoAST.Differs (p) =>				       (* rule C2.17 *)
	  RETURN NEW(JunoAST.Flip, body := NEW(JunoAST.Query,
	    f := NEW(JunoAST.Equals, bp := p, e1 := p.e1, e2 := p.e2),
	    vars := JunoAST.EmptyNVList))
      | JunoAST.Exists (p) =>				       (* rule C2.20 *)
	  RETURN C2pp(p, vars := JunoAST.EmptyNVList)
      ELSE						       (* rule C2.21 *)
	  RETURN NEW(JunoAST.Query, f := pred, vars := JunoAST.EmptyNVList)
      END
    END C2q;
  
  PROCEDURE C2p(cmd: JunoAST.Cmd; vars: JunoAST.NearVarList): JunoAST.Cmd
      RAISES {Error} =
  (* Returns a command that first solves the projected variables "vars" for
     the constraint "Grd(cmd)", and then executes the ``body'' of "cmd".
     Requires that hints have been extracted from any projected variables in
     "cmd", that all variables in "vars" are unhinted with reset "frozen"
     bits, and that the "evar" fields of all variables in "vars" are false.

     Here are the compilation rules implemented by this procedure:

|      C2p(VAR v' IN A END, v) == C2p(A, v \union v')            (rule C2.23)
|      C2p(A | B, v) == C2p(A, v) | C2p(B, v)                    (rule C2.24)
|      C2p(A ; B, v) == C2p(A, v) ; C2(B)                        (rule C2.25)
|      C2p(P -> A, v) == C2pp(P, v) ; C2(A)                      (rule C2.26)
|      C2p(A, v) == C2(A) (all other cases)                      (rule C2.22)

     In rule C2.24, a copy of the variable list "v" is made so any
     side-effects on one list do not effect the other.
*)
    BEGIN
      (* Check the pre-condition *)
      VAR l := vars.head; BEGIN
        WHILE l # NIL DO
          <* ASSERT l.frozen = FALSE AND l.hint = JunoAST.NilExpr *>
          <* ASSERT l.evar = FALSE *>
          l := l.next
        END
      END;
      cmd := Utils.Ungroup(cmd);
      TYPECASE cmd OF
	JunoAST.Proj (c) =>				       (* rule C2.23 *)
          (* NOTE: We can drop the projection here since all
	     variables have been annotated and their hints extracted. *)
          RETURN C2p(c.body, Utils.NearVarListUnion(vars, c.vars))
      | JunoAST.Else (c) =>				       (* rule C2.24 *)
          VAR varsCopy := Utils.NearVarListCopy(vars); BEGIN
	    RETURN NEW(JunoAST.Else, bp := c,
              c1 := C2p(c.c1, vars), c2 := C2p(c.c2, varsCopy))
          END
      | JunoAST.Seq (c) =>				       (* rule C2.25 *)
	  RETURN NEW(JunoAST.Seq, bp := c,
            c1 := C2p(c.c1, vars), c2 := C2(c.c2))
      | JunoAST.Guard (c) =>				       (* rule C2.26 *)
	  RETURN NEW(JunoAST.Seq, bp := c,
	    c1 := C2pp(c.grd, vars), c2 := C2(c.body))
      ELSE						       (* rule C2.22 *)
	  RETURN C2(cmd)
      END
    END C2p;
  
  PROCEDURE C2pp(f: JunoAST.Formula; vars: JunoAST.NearVarList): JunoAST.Cmd
      RAISES {Error} =
  (* Returns a command that solves the projected variables "vars" for the
     constraint "f". Requires that all variables in "vars" are unhinted, and
     that the "evar" fields of all variables in "vars" are false. The
     "frozen" bits of the variables should be set on entry iff the
     corresponding variable has been initialized; the values of these bits are
     undefined on return. *)
    BEGIN
      RETURN JunoCompileNF.ToCmd(JunoCompileNF.Normalize(f, stack_tbl),
        scp, stack_tbl, xtra_vars := vars);
    END C2pp;

  (* Cmd *)
  BEGIN
    IF annotate THEN
      AnnotateAtoms(cmd, stack_tbl, scp, ASTKind.Cmd, pure := pure)
    END;
    IF debug > 1 THEN
      Wr.PutText(stderr, "\n  Original Command:\n");
      DebugUnparse(cmd)
    END;
    VAR res := NEW(Result, cmd := C2(C1(cmd))); BEGIN
      IF debug > 2 THEN
        Wr.PutText(stderr, "\n  Compiled Command:\n");
        DebugUnparse(res.cmd)
      END;
      RETURN res
    END
  END Cmd;

(* ============================== Normal Form ============================== *)

PROCEDURE NormalForm(
    nf: JunoAST.NormalForm;
    scp: JunoScope.T;
    stack_tbl: StackTbl.T;
    xtra_vars: JunoAST.NearVarList := NIL):
  Result RAISES {Error} =
(* Compiles the normal-form query "nf" in isolation. See the description for
   the procedure "JunoCompileNF.ToCmd". *)
  VAR res := NEW(Result, cmd :=
    JunoCompileNF.ToCmd(nf, scp, stack_tbl, xtra_vars));
  BEGIN
    IF debug > 2 THEN
      Wr.PutText(stderr, "\n  Compiled Command:\n");
      DebugUnparse(res.cmd)
    END;
    RETURN res
  END NormalForm;

PROCEDURE DebugUnparse(ast: JunoAST.T) =
  BEGIN
    JunoUnparse.Debug(ast);
    Wr.Flush(stderr)
  END DebugUnparse;

PROCEDURE SaveSlots(wr: Wr.T) =
  BEGIN
    Wr.PutText(wr, Fmt.Int(dummy_slot) & "\n");
    BuiltInSlots.Save(wr);
  END SaveSlots;

PROCEDURE RestoreSlots(rd: Rd.T) =
  <* FATAL FloatMode.Trap, Lex.Error, Rd.Failure *>
  BEGIN
    dummy_slot := Lex.Int(rd);
    BuiltInSlots.Restore(rd);
  END RestoreSlots;

BEGIN
END JunoCompile.
