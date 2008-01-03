(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 16:46:56 PST 1997 by heydon                   *)
(*      modified on Sat Oct 17 18:03:41 PDT 1992 by gnelson                  *)
(*      modified on Fri Aug  7 21:54:07 PDT 1992 by myers                    *)

MODULE JunoAssemble;

IMPORT BuiltInSlots, JunoAST, JunoCompileRep, JunoScope, JunoASTUtils AS Utils;
IMPORT JunoByteCode AS BC, JunoRT, JunoRTError, JunoValue, WriteVal;
IMPORT IntSeq;

TYPE
  IntList = REF RECORD
    val: INTEGER;
    next: IntList
  END;

  LabelData = RECORD
    loc: INTEGER;		(* location (index) of label in "code" *)
    refs: IntList;		(* list of references (jumps) to this label *)
  END;

VAR
  mu := NEW(MUTEX);
  loc: CARDINAL;			 (* current code location *)
  code := NEW(JunoRT.ByteStream, 1000);	 (* result bytestream buffer *)
  labelSeq := NEW(IntSeq.T).init(sizeHint := 100);

TYPE Z3 = ARRAY [0..3] OF INTEGER;

CONST NoLabel = 0; (* the number of an invalid label *)

PROCEDURE Cmd(
    res: JunoCompileRep.Result;
    scp: JunoScope.T;
    temp_cnt: CARDINAL;
    type: CmdType):
    JunoRT.ByteStream =

  PROCEDURE Pred(
      pred: JunoAST.Formula;
      t, f: CARDINAL;
      tBranch: BOOLEAN) =
  (* Generate code for "pred" at the current location in the code stream.
     Generate a branch to label "t" if "pred" evaluates true, and to label "f"
     if "pred" evaluates false. Only generate a branch to "t" at the *end* of
     the stream if "tBranch" is "TRUE".
  
     The code generated to evaluate a predicate must restore the stack to its
     original state before branching to "t" or "f".
  
     NOTE: We do not need to handle the "JunoAST" cases "False", "And", "Or",
     "Not", "Exists", or "Differs", since those are eliminated by the
     compilation procedure "JunoCompile.Cmd.C2q". *)

    PROCEDURE UnaryPred(p: JunoAST.BIUPred; bc: JunoRT.ByteCode) =
      BEGIN
        Expr(p.e, f);
        PushByte(bc);
        FBranch(f);
	IF tBranch THEN Branch(t) END
      END UnaryPred;

    PROCEDURE Relation(p: JunoAST.Relation; bc: JunoRT.ByteCode) =
      BEGIN
        Expr(p.e1, f);
        IF Utils.AlwaysDefined(p.e2) THEN
          Expr(p.e2, f);
          PushByte(bc);
          FBranch(f);
          IF tBranch THEN Branch(t) END
        ELSE
          VAR popFirst := NewLabel(); BEGIN
            Expr(p.e2, popFirst);
	    PushByte(bc);
	    FBranch(f);
	    Branch(t);
	    SetLabelLoc(popFirst);
	    PushByte(BC.DECSP); PushByte(1);
	    Branch(f)
          END
        END
      END Relation;

    (* Pred *)
    BEGIN
      pred.start := loc;
      TYPECASE pred OF <* NOWARN *>
	JunoAST.True => IF tBranch THEN Branch(t) END
      | JunoAST.GroupedExpr (p) => Pred(p.expr, t, f, tBranch)
      | JunoAST.IsReal (p) => UnaryPred(p, BC.IS_REAL)
      | JunoAST.IsText (p) => UnaryPred(p, BC.IS_TEXT)
      | JunoAST.IsPair (p) => UnaryPred(p, BC.IS_PAIR)
      | JunoAST.IsInt (p)  => UnaryPred(p, BC.IS_INT)
      | JunoAST.Equals (p)  => Relation(p, BC.EQUAL)
      | JunoAST.Less (p)    => Relation(p, BC.LESS)
      | JunoAST.AtMost (p)  => Relation(p, BC.AT_MOST)
      | JunoAST.Cong (p)    => Relation(p, BC.CONG)
      | JunoAST.Para (p)    => Relation(p, BC.PARA)
      | JunoAST.Hor (p)     => Relation(p, BC.HOR)
      | JunoAST.Ver (p)     => Relation(p, BC.VER)
      | JunoAST.Greater (p) =>
          Relation(NEW(JunoAST.Relation, e1 := p.e2, e2 := p.e1), BC.LESS)
      | JunoAST.AtLeast (p) =>
          Relation(NEW(JunoAST.Relation, e1 := p.e2, e2 := p.e1), BC.AT_MOST)
      | JunoAST.Call (c) =>
          (* Push IN's *)
          ExprList(c.ins, f);
          (* Generate CALL instruction *)
          PushByte(BC.CALL);
          PushULong(c.name.index);
          (* Skip past IN's *)
          Repeat(BC.DECSP, c.ins.size);
          (* Branch appropriately *)
          FBranch(f);
          IF tBranch THEN Branch(t) END
      END;
      pred.end := loc
    END Pred;

  PROCEDURE OutInout(outCnt: CARDINAL; inouts: JunoAST.ExprList) =
    BEGIN
      Repeat(BC.INCSP, outCnt);		 (* Leave space for OUT's *)
      QIdList(inouts);			 (* Push INOUT's *)
    END OutInout;

  PROCEDURE AfterCall(inCnt: CARDINAL; inouts: JunoAST.ExprList;
    uJump: BOOLEAN; label: INTEGER := NoLabel) =
  (* Lay down the instructions after a "JunoAST.Call" node has been assembled.
     Namely, write instructions to skip the SP down past the "inCnt" IN
     parameters and to pop the INOUT parameters into "inouts". Then, if
     "uJump" is TRUE, write a "UJUMP" instruction to the offset "label".

     On return from a user-defined function, functional procedure, or
     functional external procedure, the internal condition bit is set as the
     function was defined or not. The UBRANCH instruction tests if the
     condition bit was set. If it was, the instruction is a no-op; otherwise,
     it decrements SP past the single OUT parameter, sets the internal
     "undefined term" address, and branches to its argument "label". *)
    BEGIN
      Repeat(BC.DECSP, inCnt);
      PopQIds(inouts.head, scp);
      IF uJump THEN
        PushByte(BC.UJUMP);
        AddReference(label)
      END
    END AfterCall;

  PROCEDURE AfterProcCall(inCnt: CARDINAL; inouts, outs: JunoAST.ExprList) =
    BEGIN
      Repeat(BC.DECSP, inCnt);
      PopQIds(inouts.head, scp);
      PopQIds(outs.head, scp)
    END AfterProcCall;

  PROCEDURE Apply(outCnt, inoutCnt: CARDINAL;
      ins: JunoAST.ExprList; u: CARDINAL) =
  (* Assuming space has already been left for the OUT parameters and the INOUT
     parameters have been pushed on the stack, write code to assemble an
     "APPLY" expression with "outCnt" OUT parameters, "inoutCnt" INOUT
     parameters, and with IN parameters in "ins". The generated code branches
     to "u" in the event that the APPLY bytecode instruction is undefined or
     if any of the expressions in "ins" is undefined. *)
    BEGIN
      ins.start := loc;
      (* Push APPLY arguments except the first *)
      ExprListTail(ins.head.next, u, level := outCnt + inoutCnt);
      (* Push 1st APPLY (closure) argument *)
      VAR expr := ins.head.expr; BEGIN
        IF Utils.AlwaysDefined(expr) THEN
          Expr(expr, u := NoLabel)
        ELSE
          VAR def, pop := NewLabel(); BEGIN
            Expr(expr, pop);
            Branch(def);
            SetLabelLoc(pop);
            Repeat(BC.DECSP, outCnt + inoutCnt + ins.size - 1);
            Branch(u);
            SetLabelLoc(def)
          END
        END
      END;
      ins.end := loc;
      (* Generate APPLY instruction *)
      PushByte(BC.APPLY);
      PushUShort(outCnt);
      PushUShort(inoutCnt);
      PushUShort(ins.size - 1);
      AddReference(u);
      (* On return from "APPLY", adjust the stack pointer to account for the
         IN parameters in the run-time closure. *)
      PushByte(BC.CLDECSP)
    END Apply;

  PROCEDURE Expr(expr: JunoAST.Expr; u: CARDINAL) =
  (* Generate code at the current location in the code stream to place the
     value of "expr" on the top of the stack. Generate a branch to label "u"
     if any portion of the evaluation is undefined; in that case, the code
     must restore the stack to its original state before executing the
     instruction to branch to "u". *)

    PROCEDURE UnaryFunc(f: JunoAST.BIUFunc; bc: JunoRT.ByteCode) =
      BEGIN
        Expr(f.e, u);
        PushByte(bc);
        IF u # NoLabel THEN
          AddReference(u)
	ELSE
	  (* this case can arise because the negation of a literal
             number is always defined *)
          StuffShort(loc, u);
          INC(loc, 2);
        END
      END UnaryFunc;

    PROCEDURE BinaryFunc(
        f: JunoAST.BIBFunc;
        bc: JunoRT.ByteCode;
        uArg := TRUE) =
      BEGIN
        Expr(f.e1, u);
        IF Utils.AlwaysDefined(f.e2) THEN
          Expr(f.e2, u := NoLabel);
          PushByte(bc);
          IF uArg THEN AddReference(u) END
        ELSE
          VAR popFirst, defined := NewLabel(); BEGIN
            Expr(f.e2, popFirst);
            Branch(defined);
            SetLabelLoc(popFirst);
            PushByte(BC.DECSP); PushByte(1);
            Branch(u);
            SetLabelLoc(defined);
            PushByte(bc);
            IF uArg THEN AddReference(u) END;
          END
        END
      END BinaryFunc;

    (* Expr *)
    BEGIN
      expr.start := loc;
      TYPECASE expr OF <* NOWARN *>
      | JunoAST.Number (e) =>
          PushByte(BC.PUSHNUM);
          PushReal(e.val)
      | JunoAST.Nil =>
          PushByte(BC.PUSHNIL)
      | JunoAST.Text (e) =>
	  PushByte(BC.PUSHG);
	  PushULong(e.index)
      | JunoAST.QId (e) =>
	  <* ASSERT e.index # 0 *>
	  CASE e.type OF <* NOWARN *>
	  | JunoAST.IdType.Local => 
	      PushByte(BC.PUSHL); 
	      PushShort(e.index)
	  | JunoAST.IdType.Const, JunoAST.IdType.Var => 
	      PushByte(BC.PUSHG);
	      PushULong(e.index)
          | JunoAST.IdType.Proc => 
              PushByte(BC.NEWCL);
              PushULong(e.index)
          | JunoAST.IdType.ExtProc => 
              PushByte(BC.NEWEXTCL);
              PushULong(e.index)
	  END
      | JunoAST.GroupedExpr (e) => Expr(e.expr, u)
      | JunoAST.UMinus (e)  => UnaryFunc(e, BC.NEGATE)
      | JunoAST.Floor (e)   => UnaryFunc(e, BC.FLOOR_)
      | JunoAST.Ceiling (e) => UnaryFunc(e, BC.CEILING_)
      | JunoAST.Round (e)   => UnaryFunc(e, BC.ROUND_)
      | JunoAST.Abs (e)     => UnaryFunc(e, BC.ABS_)
      | JunoAST.Sin (e)     => UnaryFunc(e, BC.SIN)
      | JunoAST.Cos (e)     => UnaryFunc(e, BC.COS)
      | JunoAST.Exp (e)     => UnaryFunc(e, BC.EXP)
      | JunoAST.Ln (e)      => UnaryFunc(e, BC.LN)
      | JunoAST.Car (e)     => UnaryFunc(e, BC.CAR)
      | JunoAST.Cdr (e)     => UnaryFunc(e, BC.CDR)
      | JunoAST.Plus (e)    => BinaryFunc(e, BC.ADD) 
      | JunoAST.Minus (e)   => BinaryFunc(e, BC.SUBTRACT)
      | JunoAST.Concat (e)  => BinaryFunc(e, BC.CONCAT)
      | JunoAST.Times (e)   => BinaryFunc(e, BC.MULTIPLY)
      | JunoAST.Divide (e)  => BinaryFunc(e, BC.DIVIDE)
      | JunoAST.Div (e)     => BinaryFunc(e, BC.DIV_)
      | JunoAST.Mod (e)     => BinaryFunc(e, BC.MOD_)
      | JunoAST.Rel (e)     => BinaryFunc(e, BC.REL)
      | JunoAST.Max (e)     => BinaryFunc(e, BC.MAX_)
      | JunoAST.Min (e)     => BinaryFunc(e, BC.MIN_)
      | JunoAST.Atan (e)    => BinaryFunc(e, BC.ATAN)
      | JunoAST.Pair (e)    => BinaryFunc(e, BC.CONS, uArg := FALSE)
      | JunoAST.List (e) =>
          ExprList(e.elts, u);
          PushByte(BC.LIST);
          PushUShort(e.elts.size)
      | JunoAST.Call (c) =>
          IF BuiltInSlots.IsCloseProc(c.name) THEN
            (* CLOSE(...) *)
            ExprList(c.ins, u);
            PushByte(BC.CLOSE);
            PushUShort(c.ins.size - 1);
            AddReference(u);
          ELSIF BuiltInSlots.IsApplyProc(c.name) THEN
            (* APPLY(...) *)
            OutInout(1, c.inouts);
            Apply(1, c.inouts.size, c.ins, u);
            (* After the call to APPLY, the condition bit is irrelevant, since
               calls to user-defined procedures never fail, and since the
               "APPLY" bytecode automatically signals a run-time error in the
               event that a call to an external procedure fails. *)
            AfterCall(c.ins.size - 1, c.inouts, uJump := FALSE)
          ELSE
            (* User-defined function, functional procedure, or functional
               external procedure *)
            OutInout(1, c.inouts);
            ExprList(c.ins, u, level := 1 + c.inouts.size);
            (* Generate CALL instruction *)
            VAR uJump: BOOLEAN; BEGIN
              CASE c.name.type OF <* NOWARN *>
              | JunoAST.IdType.Func =>    PushByte(BC.CALL);    uJump := TRUE
              | JunoAST.IdType.Proc =>    PushByte(BC.CALL);    uJump := FALSE
              | JunoAST.IdType.ExtProc => PushByte(BC.CALLEXT); uJump := FALSE
              END;
              PushULong(c.name.index);
              AfterCall(c.ins.size, c.inouts, uJump := uJump, label := u)
            END
          END
      END;
      expr.end := loc
    END Expr;

  PROCEDURE QIdList(l: JunoAST.ExprList) =
  (* Generates code to push the qualified identifiers in "l" onto the stack.
     Since the evaluation of a QId is never undefined, we need not generate
     code to restore the stack in the event of an undefined term. It is an
     unchecked runtime error for "l" to contain an expression "e" such that
     "NOT Utils.AlwaysDefined(e)". *)
    VAR curr := l.head; BEGIN
      l.start := loc;
      WHILE curr # NIL DO
        Expr(curr.expr, u := NoLabel);
        curr := curr.next
      END;
      l.end := loc
    END QIdList;
  
  PROCEDURE ExprList(l: JunoAST.ExprList; u: CARDINAL; level: CARDINAL := 0) =
  (* Generate code to push the expressions in the list "l" onto the stack,
     branching to label "u" if any of them are undefined. In this case, the
     stack is restored to its original state (before the first expression of
     "l" was evaluated) and an additional "level" elements are then popped
     off the stack before branching to "u". *)
    BEGIN
      l.start := loc;
      ExprListTail(l.head, u, level);
      l.end := loc
    END ExprList;

  PROCEDURE ExprListTail(
      l: JunoAST.ExprLink;
      u: CARDINAL;
      level: CARDINAL) =
  (* Generate code to push the expressions in the list "l" onto the stack,
     branching to label "u" if any are undefined. However, in this case, the
     stack is restored to its original state, and an additional "level"
     elements are popped from the stack before branching to "u".

     We use the global "labelsSeq" as a stack of labels. We push a new label
     value for each expression so we know afterward how much to decrement the
     stack pointer for each expression. This procedure preserves the size of
     "labelsSeq"; we could add code to assert that here, but instead, we
     simply assert at the end of "JunoAssemble.Cmd" the the size of the
     sequence is 0. *) 
    VAR newLabels := FALSE; pop: INTEGER; stackSz := 0; BEGIN
      (* Push expressions *)
      WHILE l # NIL DO
        IF Utils.AlwaysDefined(l.expr) THEN
          pop := NoLabel
        ELSIF level + stackSz = 0 THEN
          pop := u
        ELSE
          pop := NewLabel();
          newLabels := TRUE
        END;
        labelSeq.addhi(pop); INC(stackSz);
        Expr(l.expr, pop);
        l := l.next
      END;

      (* Generate code to pop (if necessary) *)
      IF newLabels THEN
        (* write branches for each new label *)
        VAR end := NewLabel(); BEGIN
          Branch(end);
          WHILE stackSz > 0 DO
            pop := labelSeq.remhi(); DEC(stackSz);
            IF pop # NoLabel AND pop # u THEN
              SetLabelLoc(pop);
              Repeat(BC.DECSP, level + stackSz);
              Branch(u)
            END
          END;
          SetLabelLoc(end)
        END
      ELSE
        (* pop "stackSz" elements from "labelSeq" stack *)
        WHILE stackSz > 0 DO
          pop := labelSeq.remhi();
          DEC(stackSz)
        END
      END
    END ExprListTail;
  
  PROCEDURE Cmd0(
      cmd: JunoAST.Cmd;
      h, g, u: CARDINAL;
      hBranch: BOOLEAN) =
  (* Generate code for "cmd" at the current location in the code stream.
     Generate a branch to label "h" on successful termination, to label
     "g" on a guard failure, and to label "u" on an "undefined term" run-time
     error. Only generate a branch to "h" at the end of the stream if
     "hBranch" is "TRUE". *)
    BEGIN
      cmd.start := loc;
      TYPECASE cmd OF <* NOWARN *>
	JunoAST.Skip =>
	  IF hBranch THEN Branch(h) END
      | JunoAST.Abort =>
	  PushByte(BC.ERROR);
	  PushByte(ORD(JunoRTError.Code.Abort));
	  IF hBranch THEN Branch(h) END
      | JunoAST.Halt =>
	  PushByte(BC.ERROR);
	  PushByte(ORD(JunoRTError.Code.Halt));
	  IF hBranch THEN Branch(h) END
      | JunoAST.Fail =>
	  Branch(g)
      | JunoAST.Assign (c) =>
          ExprList(c.exprs, u);
	  PopQIds(c.vars.head, scp);
	  IF hBranch THEN Branch(h) END
      | JunoAST.If (c) =>
	  VAR l := NewLabel(); BEGIN
	    Cmd0(c.body, h, l, u, hBranch := TRUE);
	    SetLabelLoc(l);
	    PushByte(BC.ERROR);
	    PushByte(ORD(JunoRTError.Code.IfFailure))
	  END
      | JunoAST.Do (c) =>
	  VAR l := NewLabel(); BEGIN
	    SetLabelLoc(l);
	    Cmd0(c.body, l, h, u, hBranch := TRUE)
	  END
      | JunoAST.Seq (c) =>
          Cmd0Seq(c, h, g, u, hBranch)
      | JunoAST.Else (c) =>
	  VAR l := NewLabel(); BEGIN
	    Cmd0(c.c1, h, l, u, hBranch := TRUE);
	    SetLabelLoc(l);
	    Cmd0(c.c2, h, g, u, hBranch)
	  END
      | JunoAST.GroupedCmd (c) =>
	  Cmd0(c.body, h, g, u, hBranch)
      | JunoAST.Flip (c) =>
	  Cmd0(c.body, g, h, u, hBranch := TRUE)
      | JunoAST.Safe (c) =>
	  Cmd0(c.body, h, g, g, hBranch)
      | JunoAST.ProcCall (c) =>
          IF BuiltInSlots.IsApplyProc(c.name) THEN
            (* APPLY(...) *)
            OutInout(c.outs.size, c.inouts);
            Apply(c.outs.size, c.inouts.size, c.ins, u);
            AfterProcCall(c.ins.size - 1, c.inouts, c.outs)
          ELSE
            (* procedure or external procedure command *)
            OutInout(c.outs.size, c.inouts);
            ExprList(c.ins, u, level := c.outs.size + c.inouts.size);
	    (* Generate CALL instruction *)
            CASE c.name.type OF <* NOWARN *>
            | JunoAST.IdType.Proc => PushByte(BC.CALL)
            | JunoAST.IdType.ExtProc => PushByte(BC.CALLEXT)
            END;
            PushULong(c.name.index);
            AfterProcCall(c.ins.size, c.inouts, c.outs)
          END;
          IF hBranch THEN Branch(h) END
      | JunoAST.Query (q) =>
	  <* ASSERT q.vars.size = 0 *>
          (* Query of form: P?() *)
          Pred(q.f, h, g, tBranch := hBranch)
      | JunoAST.ConjQuery (q) =>
	  (* Query of form: P?(v1,...,vn) (represented by arrays) *)
          IF NUMBER(q.conj^) > 0 THEN	 (* skip "(TRUE)?(v1,...,vn)" *)
	    PushConjVars(q.var^);
	    PushConstraints(q.var^, q.conj^);
	    VAR l := NewLabel(); BEGIN
	      TBranch(l);
	      (* Skip over OUT vars if solve failed *)
	      Repeat(BC.DECSP, NUMBER(q.var^));
	      Branch(g);
	      SetLabelLoc(l);
	      (* Update INOUT's in "q.vars" *)
	      PopConjVars(q.var^)
            END
          END;
          IF hBranch THEN Branch(h) END
      END; (* TYPECASE *)
      cmd.end := loc
    END Cmd0;

  PROCEDURE Cmd0Seq(
      seq: JunoAST.Seq;
      h, g, u: CARDINAL;
      hBranch: BOOLEAN) =
  (* Equivalent to:
|
|      VAR l := NewLabel(); BEGIN
|        Cmd0(c.c1, l, g, u, hBranch := FALSE);
|        SetLabelLoc(l)
|      END;
|      Cmd0(c.c2, h, g, u, hBranch)
|
     except that fewer frames are used on the stack if "seq" is a
     long list of semi-colon-separated commands. *)
    VAR curr := seq; BEGIN
      (* Loop down list to generate code *)
      LOOP
        VAR l := NewLabel(); BEGIN
          Cmd0(curr.c1, l, g, u, hBranch := FALSE);
          SetLabelLoc(l);
        END;
        TYPECASE curr.c2 OF JunoAST.Seq (next) =>
          next.start := loc; curr := next
        ELSE EXIT
        END
      END;
      Cmd0(curr.c2, h, g, u, hBranch);

      (* Now, loop a second time to set "end" locations *)
      curr := seq;
      LOOP
        TYPECASE curr.c2 OF JunoAST.Seq (next) =>
          next.end := loc; curr := next
        ELSE EXIT
        END
      END
    END Cmd0Seq;

  (* Cmd *)
  BEGIN
    LOCK mu DO
      loc := 0;
      InitLabelTable();
      Repeat(BC.PUSHM3NIL, temp_cnt);
      VAR h := NewLabel(); err := NewLabel(); BEGIN
        CASE type OF
          CmdType.Pred, CmdType.Func =>
            (* When the bodies of predicates and functions are compiled as
               commands, they become queries that either halt or fail. The
               procedure for a predicate must set the condition bit as the
               command halted or failed, to indicate if the predicate was true
               or not. The procedure for a function must also set the
               condition bit as the command halted or failed, to indicate if
               the function was defined or not. *)
            Cmd0(res.cmd, h, err, NoLabel, hBranch := FALSE);
            SetLabelLoc(h);
            PushByte(BC.C_ON);
            PushByte(BC.RET);
            SetLabelLoc(err);
            PushByte(BC.C_OFF);
            PushByte(BC.RET);
        | CmdType.Proc =>
            (* User-defined procedures are total: they never fail. Hence, we
               don't need to set the condition bit at the end of the
               procedure. *)
            Cmd0(res.cmd, h, NoLabel, err, hBranch := FALSE);
            SetLabelLoc(h);
            PushByte(BC.RET);
            SetLabelLoc(err);
            PushByte(BC.ERROR);
            PushByte(ORD(JunoRTError.Code.UndefTerm))
        END
      END;
      res.cmd.end := loc;
      BackPatchReferences();
      <* ASSERT labelSeq.size() = 0 *>
      RETURN CodeCopy();
    END
  END Cmd;

PROCEDURE PushConjVars(READONLY v: JunoAST.Vars) =
(* Push the "frozen" variables in "v" to the stack; push "unfrozen" variables
   in "v" as "NIL". To decrease the size of the output code, do run-length
   encoding on the "PUSHM3NIL"'s. *)
  VAR i := FIRST(v); nilCnt := 0; BEGIN
    WHILE i <= LAST(v) DO
      IF v[i].frozen THEN
        IF nilCnt > 0 THEN Repeat(BC.PUSHM3NIL, nilCnt); nilCnt := 0 END;
        PushByte(BC.PUSHL);
        PushShort(v[i].index)
      ELSE
        INC(nilCnt)
      END;
      INC(i)
    END;
    IF nilCnt > 0 THEN Repeat(BC.PUSHM3NIL, nilCnt) END
  END PushConjVars;

PROCEDURE PopConjVars(READONLY v: JunoAST.Vars) =
(* Pop the non-temporary variables in "vars" in reverse order; discard
   temporary variables. To decrease the size of the output code, do run-length
   encoding on the "DECSP"'s. *)
  VAR i := LAST(v); tempCnt := 0; BEGIN
    WHILE i >= FIRST(v) DO
      IF v[i].evar THEN
        INC(tempCnt)
      ELSE
        IF tempCnt > 0 THEN Repeat(BC.DECSP, tempCnt); tempCnt := 0 END;
        <* ASSERT v[i].index # 0 *>
        PushByte(BC.POPL);
        PushShort(v[i].index)
      END;
      DEC(i)
    END;
    IF tempCnt > 0 THEN Repeat(BC.DECSP, tempCnt) END
  END PopConjVars;

PROCEDURE PushConstraints(
    READONLY v: JunoAST.Vars;
    READONLY c: JunoAST.Formulas) =
  VAR index := NUMBER(v); argNum := NEW(REF ARRAY OF Z3, NUMBER(c)); BEGIN
    FOR i := FIRST(argNum^) TO LAST(argNum^) DO
      ProcessLeaf1(c[i], argNum[i], v, index)
    END;
    PushByte(BC.SOLVE);
    PushUShort(NUMBER(v));		 (* # of variables (INOUTS) *)
    PushUShort(index - NUMBER(v));	 (* # of constants (INS)    *)
    PushUShort(NUMBER(c));		 (* # of constraints        *)
    FOR i := FIRST(argNum^) TO LAST(argNum^) DO
      ProcessLeaf2(c[i], argNum[i])
    END
  END PushConstraints;

PROCEDURE ProcessLeaf1(
    f: JunoAST.Formula;
    VAR (* OUT *) z3: Z3;
    READONLY inouts: JunoAST.Vars;
    VAR (* INOUT *) index: CARDINAL) =
(* Set "z3" to the canonicalized form of "f". Increment "index" by the number
   of knowns in "f", where an "id" is known if it does not appear in "inouts".
   For each such known, generate code to push its value. *)
  VAR j: CARDINAL := 0;

  PROCEDURE Atom(e: JunoAST.AtomicExpr) =
    BEGIN
      TYPECASE e OF <* NOWARN *>
        JunoAST.LitValue (lit) =>
          z3[j] := index;
          INC(index);
          TYPECASE lit OF <* NOWARN *>
            JunoAST.Text (t) =>   PushByte(BC.PUSHG); PushULong(t.index)
          | JunoAST.Number (n) => PushByte(BC.PUSHNUM); PushReal(n.val)
          | JunoAST.Nil =>        PushByte(BC.PUSHNIL)
          END
      | JunoAST.QId (qid) =>
          CASE qid.type OF <* NOWARN *>
            JunoAST.IdType.Local =>
	      VAR k := Utils.MemVars(qid, inouts); BEGIN
		IF k # -1 THEN	(* INOUT *)
		  z3[j] := k
		ELSE		(* IN Local var *)
		  z3[j] := index;
		  INC(index);
		  PushByte(BC.PUSHL);
		  PushShort(qid.index)
		END
	      END
          | JunoAST.IdType.Var, JunoAST.IdType.Const =>
	      z3[j] := index;
	      INC(index);
	      PushByte(BC.PUSHG);
	      PushULong(qid.index)
          END
      END;
      INC(j)
    END Atom;
  BEGIN
    TYPECASE f OF <* NOWARN *>
      JunoAST.Equals (eq) =>
        Atom(eq.e1);
        TYPECASE eq.e2 OF <* NOWARN *>
          JunoAST.AtomicExpr => Atom(eq.e2)
        | JunoAST.BIUFunc (e) => Atom(e.e)
        | JunoAST.BIBFunc (e) => Atom(e.e1); Atom(e.e2)
        END
    | JunoAST.BIUPred (e) => Atom(e.e)
    END
  END ProcessLeaf1;

PROCEDURE ProcessLeaf2(f: JunoAST.Formula; READONLY z3: Z3) =
  BEGIN
    TYPECASE f OF <* NOWARN *>
      JunoAST.Equals (eq) =>
        TYPECASE eq.e2 OF <* NOWARN *>
        | JunoAST.Pair =>
            PushByte(BC.CONS_C);
            PushUShort(z3[0]);
            PushUShort(z3[1]);
            PushUShort(z3[2]);
        | JunoAST.Plus =>
            PushByte(BC.SUM_C);
            PushUShort(z3[0]);
            PushUShort(z3[1]);
            PushUShort(z3[2]);
        | JunoAST.Times =>
            PushByte(BC.PROD_C);
            PushUShort(z3[0]);
            PushUShort(z3[1]);
            PushUShort(z3[2]);
        | JunoAST.Atan =>
            PushByte(BC.ATAN_C);
            PushUShort(z3[0]);
            PushUShort(z3[1]);
            PushUShort(z3[2]);
        | JunoAST.AtomicExpr =>
            PushByte(BC.EQUAL_C);
            PushUShort(z3[0]);
            PushUShort(z3[1]);
        | JunoAST.Sin =>
            PushByte(BC.SIN_C);
            PushUShort(z3[0]);
            PushUShort(z3[1]);
        | JunoAST.Cos =>
            PushByte(BC.COS_C);
            PushUShort(z3[0]);
            PushUShort(z3[1]);
        | JunoAST.Exp =>
            PushByte(BC.EXP_C);
            PushUShort(z3[0]);
            PushUShort(z3[1]);
        END
    | JunoAST.IsReal =>
        PushByte(BC.REAL_C);
        PushUShort(z3[0]);
    | JunoAST.IsText =>
        PushByte(BC.TEXT_C);
        PushUShort(z3[0]);
    END
  END ProcessLeaf2;

PROCEDURE PopIds(id: JunoAST.IdLink) =
(* Generate code to pop the locals indexed in "ids" in reverse order. *)
  BEGIN
    IF id # NIL THEN
      PopIds(id.next);
      <* ASSERT id.index # 0 *>
      PushByte(BC.POPL);
      PushShort(id.index)
    END
  END PopIds;

PROCEDURE PopQIds(v: JunoAST.ExprLink; scp: JunoScope.T) =
(* The list "v" is a list of QId's. Generate code to pop the values on the
   stack into the locations designated by these QId's in reverse order. It is
   a checked run-time error for the type of any QId in "v" not to be a local
   or global variable. *)
  BEGIN
    IF v # NIL THEN
      PopQIds(v.next, scp);
      VAR qid: JunoAST.QId := v.expr; BEGIN
        <* ASSERT qid.index # 0 *>
        CASE qid.type OF <* NOWARN *>
        | JunoAST.IdType.Local =>
            PushByte(BC.POPL);
            PushShort(qid.index)
        | JunoAST.IdType.Var =>
            PushByte(BC.POPG);
            PushULong(qid.index)
        END
      END
    END
  END PopQIds;

PROCEDURE Branch(lbl: CARDINAL) =
  BEGIN
    PushByte(BC.JUMP);
    AddReference(lbl);
  END Branch;

PROCEDURE TBranch(lbl: CARDINAL) =
  BEGIN
    PushByte(BC.TJUMP);
    AddReference(lbl);
  END TBranch;

PROCEDURE FBranch(lbl: CARDINAL) =
  BEGIN
    PushByte(BC.FJUMP);
    AddReference(lbl);
  END FBranch;

PROCEDURE PushByte(byte: JunoRT.ByteCode) =
  BEGIN
    IF loc > LAST(code^) THEN
      VAR new := NEW(JunoRT.ByteStream, 2 * NUMBER(code^)); BEGIN
        SUBARRAY(new^, 0, NUMBER(code^)) := code^;
        code := new
      END
    END;
    code[loc] := byte;
    INC(loc)
  END PushByte;

<* INLINE *>
PROCEDURE PushUShort(i: CARDINAL) =
  BEGIN WriteVal.UShort(code, loc, i) END PushUShort;

<* INLINE *>
PROCEDURE PushShort(i: INTEGER) =
  BEGIN WriteVal.Short(code, loc, i) END PushShort;

<* INLINE *>
PROCEDURE PushULong(i: CARDINAL) =
  BEGIN WriteVal.ULong(code, loc, i) END PushULong;

<* INLINE *>
PROCEDURE PushReal(r: JunoValue.Real) =
  BEGIN WriteVal.Real(code, loc, r) END PushReal;

PROCEDURE Repeat(instruction: JunoRT.ByteCode; cnt: CARDINAL) =
  BEGIN
    WHILE cnt > 0 DO
      PushByte(instruction);
      WITH num = MIN(cnt, 255) DO
        PushByte(num);
        DEC(cnt, num)
      END
    END
  END Repeat;

<* INLINE *>
PROCEDURE StuffShort(at: CARDINAL; val: INTEGER) =
(* Stuffs the 2-byte value "val" at locations "code[at..at+1]". *)
  BEGIN WriteVal.Short(code, at, val) END StuffShort;

(* Label Procedures ------------------------------------------------------- *)

CONST FirstLabel = 1; (* the number of the first valid label *)

VAR
  last_label: INTEGER; (* the most-recently granted valid label *)
  labels := NEW(REF ARRAY OF LabelData, 500);

<* INLINE *>
PROCEDURE InitLabelTable() =
(* Clears the label table and resets the last label number. *)
  BEGIN last_label := FirstLabel - 1 END InitLabelTable;

PROCEDURE NewLabel(): CARDINAL =
(* Returns a new, unused label. A new label has a "loc" of -1 and an empty
   references list. All valid label numbers are strictly positive. *)
  BEGIN
    INC(last_label);
    IF last_label >= NUMBER(labels^) THEN
      VAR newLabels := NEW(REF ARRAY OF LabelData, 2 * NUMBER(labels^)); BEGIN
        SUBARRAY(newLabels^, 0, NUMBER(labels^)) := labels^;
        labels := newLabels
      END
    END;
    labels[last_label].loc := -1;
    labels[last_label].refs := NIL;
    RETURN last_label
  END NewLabel;

<* INLINE *>
PROCEDURE SetLabelLoc(lbl: CARDINAL) =
(* Assert that label "lbl" should be associated with index "loc" in the output
   "code". It is either a checked or an unchecked run-time error to set the
   label location of a label that was not obtained by a call to NewLabel(). *)
  BEGIN
    <* ASSERT lbl >= FirstLabel *>
    labels[lbl].loc := loc;
  END SetLabelLoc;

<* INLINE *>
PROCEDURE AddReference(lbl: CARDINAL) =
(* Assert that the bytes at locations "code[loc..loc+1]" should eventually be
   back-patched to be relative offsets to label "lbl", and increment "loc"
   past these bytes. *)
  BEGIN
    <* ASSERT lbl >= FirstLabel *>
    WITH data = labels[lbl] DO
      data.refs := NEW(IntList, val := loc, next := data.refs)
    END;
    INC(loc, 2);
  END AddReference;

PROCEDURE BackPatchReferences() =
(* Backpatch the 2-bytes in "code" whose indices are stored in the lists
   "labels[FirstLabel..last_label].refs" to contain relative offsets to the
   corresponding label locations "labels[FirstLabel..last_label].loc". *)
  BEGIN
    FOR lbl := FirstLabel TO last_label DO
      VAR p := labels[lbl].refs; BEGIN
        WHILE p # NIL DO
          StuffShort(p.val, labels[lbl].loc - (p.val + 2));
          p := p.next
        END
      END
    END
  END BackPatchReferences;

PROCEDURE CodeCopy(): JunoRT.ByteStream =
(* Return a copy of "code[0..loc-1]". *)
  VAR res := NEW(JunoRT.ByteStream, loc); BEGIN
    res^ := SUBARRAY(code^, 0, loc);
    RETURN res
  END CodeCopy;

BEGIN END JunoAssemble.
