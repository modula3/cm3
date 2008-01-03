(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 26 15:22:51 PDT 1997 by heydon                   *)

MODULE JunoCompileNF;

IMPORT JunoAST, JunoScope, StackTbl, JunoASTUtils AS Utils;
IMPORT   IndexedNF, JunoUnparse;
FROM JunoCompileErr IMPORT Error, Raise;
IMPORT Wr, Atom, IntRefTbl, RefList, Fmt;
FROM Stdio IMPORT stderr;
FROM Thread IMPORT Alerted;

<* FATAL Wr.Failure, Alerted *>
VAR debug := 0;
(* = 0  => no debugging output
| >= 1  => show predicate/assignment extraction & in-line expansions
| >= 2  => show hint propagation
*)

VAR (* CONST *)
  dummyVar := Atom.FromText("_x");      (* name of newly introduced "E" vars *)

(* ============================= Normalize ================================= *)

PROCEDURE Normalize(p: JunoAST.Formula; tbl: StackTbl.T): JunoAST.NormalForm =
  VAR inf := IndexedNF.New(var_cnt := 100, conj_cnt := 200); BEGIN
    Flatten(p, tbl, inf);
    B1(tbl, inf);
    VAR res := IndexedNF.ToNF(inf); BEGIN
      IndexedNF.Dispose(inf);
      RETURN res
    END
  END Normalize;

PROCEDURE Flatten(
    READONLY p: JunoAST.Formula;
    tbl: StackTbl.T;
    VAR (*INOUT*) res: IndexedNF.T) =
(* "p" is interpretted as a conjunction. Ungroup the conjuncts and store them
   in "res". There is one special case:

   For each conjunct of the form "(E nearvars :: P)", add the existentially
   quantified variables to the list of variables to solve for in "res", and
   add any hints for those variables and the ungrouped body "P" to the
   conjuncts of "res".

   IMPLEMENTATION: To make this more efficient, we may want to allocate the
   "stack" local from an avail list. *)

  VAR
    stack := NEW(REF JunoAST.Formulas, 50);
    sp: CARDINAL := 0;

  PROCEDURE Push(READONLY f: JunoAST.Formula) =
    BEGIN
      IF sp = NUMBER(stack^) THEN
        VAR new := NEW(REF JunoAST.Formulas, 2 * NUMBER(stack^)); BEGIN
          SUBARRAY(new^, 0, NUMBER(stack^)) := stack^;
          stack := new
        END
      END;
      stack[sp] := f;
      INC(sp)
    END Push;

  (* Flatten *)
  VAR pred: JunoAST.Formula; BEGIN
    res.bp := p; res.v_cnt := 0; res.c_cnt := 0;
    Push(p);
    WHILE sp # 0 DO
      DEC(sp);
      pred := Utils.Ungroup(stack[sp]);
      TYPECASE pred OF
        JunoAST.And (and) =>
          Push(and.f2); Push(and.f1)
      | JunoAST.Or (or) =>
	  PROCEDURE IsTrue(f: JunoAST.NormalForm): BOOLEAN =
	    BEGIN
	      RETURN NUMBER(f.var^) = 0 AND NUMBER(f.conj^) = 1
	        AND f.conj[0] = JunoAST.TrueVal
	    END IsTrue;
          VAR f1 := Normalize(or.f1, tbl); f2 := Normalize(or.f2, tbl); BEGIN
            IF IsTrue(f1) OR IsTrue(f2) THEN
              IndexedNF.AddConj(res, JunoAST.TrueVal)
            ELSE
              IndexedNF.AddConj(res, NEW(JunoAST.Or, bp := or, f1:=f1, f2:=f2))
            END
          END
      | JunoAST.Not (not) =>
          IndexedNF.AddConj(res, NEW(JunoAST.Not, bp := not,
            f := Normalize(not.f, tbl)))
      | JunoAST.Exists (e) =>
          VAR uv := e.vars; hints := Utils.ExtractHints(uv); BEGIN
            Push(e.f);
            IF hints # JunoAST.TrueVal THEN Push(hints) END;
            IndexedNF.AddVarList(res, Utils.StripHints(uv));
            (* Set the "evar" bits of the variables just added to "res" *)
            FOR i := res.v_cnt - uv.size TO res.v_cnt - 1 DO
              res.var[i].evar := TRUE
            END
          END
      ELSE
          IndexedNF.AddConj(res, pred)
      END
    END
  END Flatten;

PROCEDURE B1(tbl: StackTbl.T; VAR (*INOUT*) res: IndexedNF.T) =
(* Change "res" into an equivalent formula, all of whose simple formulas are
   normal. This process may require the introduction of new existentially
   quantified variables; the indices for the new variables are taken from
   "tbl".

   Requires all top-level formulas in "res.conj" to be ungrouped. This code is
   still responsible for ungrouping the sub-expressions in equalities. *)
  VAR i: CARDINAL := 0; BEGIN
    WHILE i # res.c_cnt DO
      TYPECASE res.conj[i] OF
        JunoAST.Or, JunoAST.Not =>
          INC(i) (* don't modify compound formulas *)
      | JunoAST.Equals (p) =>                          (* rules C2.30, C2.33 *)
          VAR
            e1 := Utils.Ungroup(p.e1); e2 := Utils.Ungroup(p.e2);
            var: JunoAST.AtomicExpr := NIL; rhs: JunoAST.Expr;
          BEGIN
            TYPECASE e1 OF JunoAST.AtomicExpr (ae) =>      var := ae; rhs := e2
            ELSE TYPECASE e2 OF JunoAST.AtomicExpr (ae) => var := ae; rhs := e1
            ELSE (* SKIP *)
            END END;
            IF var # NIL THEN
              TYPECASE rhs OF                                (* form: x ~= y *)
                JunoAST.AtomicExpr => (* SKIP *)               (* rule C2.38 *)
              ELSE
                VAR flat := Unnest(rhs, tbl); BEGIN
                  IF flat.vars.size # 0 THEN  (* form: v ~= f(...,g(..),...) *)
                    res.conj[i] := NEW(JunoAST.Equals,         (* rule C2.33 *)
                      e1 := var, e2 := flat.t0, near := p.near);
                    IndexedNF.AddVarList(res, flat.vars);
                    FormEqualities(flat.vars, flat.terms, near := p.near);
                    IndexedNF.AddConjList(res, flat.terms)
                  ELSIF var # e1 THEN
                    res.conj[i] := NEW(JunoAST.Equals,   (* swap lhs and rhs *)
                      e1 := var, e2 := rhs, near := p.near)
                  END
                END
              END;
              INC(i)
	    ELSE                                   (* form: f(...) ~= g(...) *)
	      VAR v := DummyVar(tbl); qid := Utils.QIdFromNearVar(v); BEGIN
		(* swap in last element *)
		DEC(res.c_cnt);
		res.conj[i] := res.conj[res.c_cnt];
		(* add two new equalities *)
		IndexedNF.AddVar(res, v);                      (* rule C2.30 *)
		IndexedNF.AddConj(res, NEW(JunoAST.Equals,
		  bp := p, e1 := qid, e2 := e1, near := p.near));
		IndexedNF.AddConj(res, NEW(JunoAST.Equals,
		  bp := p, e1 := qid, e2 := e2, near := p.near))
	      END
	    END (* IF *)
          END (* BEGIN *)
      ELSE                                         (* form: p(...,g(..),...) *)
        VAR flat := Unnest(res.conj[i], tbl); BEGIN            (* rule C2.32 *)
          IF flat.vars.size # 0 THEN
            res.conj[i] := flat.t0;
            IndexedNF.AddVarList(res, flat.vars);
            FormEqualities(flat.vars, flat.terms);
            IndexedNF.AddConjList(res, flat.terms)
          END
        END;
        INC(i)
      END (* TYPECASE *)
    END (* WHILE *)
  END B1;

PROCEDURE DummyVar(tbl: StackTbl.T): JunoAST.NearVarLink =
(* Return an unhinted, E-quantified dummy variable with the next available
   local index according to "tbl". *)
  VAR res := NEW(JunoAST.NearVarLink, id := dummyVar,
    hint := JunoAST.NilExpr, evar := TRUE, index := tbl.next_index);
  BEGIN
    INC(tbl.next_index);
    RETURN res
  END DummyVar;

PROCEDURE FormEqualities(
    v: JunoAST.NearVarList;
    t: JunoAST.ExprList;
    near := FALSE) =
(* Requires "v" and "t" are lists of the same length. Destructively changes
   the list "t" so that the new value for "t_i.expr" is an equality between
   "v_i" and the old value of "t_i.expr". If "near" is "TRUE", then each
   equality is a near equality. *)
  VAR curr_v := v.head; curr_t := t.head; BEGIN
    <* ASSERT v.size = t.size *>
    WHILE curr_t # NIL DO
      curr_t.expr := NEW(JunoAST.Equals, near := near,
        e1 := Utils.QIdFromNearVar(curr_v), e2 := curr_t.expr);
      curr_v := curr_v.next;
      curr_t := curr_t.next
    END
  END FormEqualities;

TYPE
  FlatRec = RECORD
    t0: JunoAST.Expr;
    vars: JunoAST.NearVarList;
    terms: JunoAST.ExprList;
  END;

(* If "fr" is a "FlatRec", then each variable in "fr.vars" is unhinted and has
   its "evar" bit set. *)

PROCEDURE Unnest(t: JunoAST.Expr; tbl: StackTbl.T): FlatRec =
(* The expression "t" is assumed to be of the form "f(e1,e2,...,en)", where
   "f" represents either a built-in or user-defined predicate or function
   (note that "t" is not a parenthesized expression). Each of the argument
   expressions "e_i" is either atomic or non-atomic. The procedure returns a
   "fr: FlatRec" such that "fr.t0" is obtained from "t" by replacing each
   non-atomic argument by a dummy variable, "fr.vars" is a list of these dummy
   variables, and "fr.terms" contains the union of the terms obtained by
   recursively unnesting the original non-atomic "e_i" in "t". *)
  VAR
    last_v := NEW(JunoAST.NearVarLink);	 (* sentinal *)
    curr_t := NEW(JunoAST.ExprLink, expr := t);
    last_t := curr_t;
    f: FlatRec;
  BEGIN
    <* ASSERT NOT ISTYPE(t, JunoAST.AtomicExpr) *>
    f.vars := NEW(JunoAST.NearVarList, head := last_v);
    f.terms := NEW(JunoAST.ExprList, size := 1, head := curr_t);
    (* Loop invariants:
|        1) All terms before "curr_t" have been unnested.
|        2) All expressions on the list "f.terms.head" are ungrouped.
|        3) "last_t" points to the last expression on the list "f.terms.head".
|        4) "last_v" points to the last var on the list "f.vars".
    *)
    WHILE curr_t # NIL DO
      PROCEDURE ProcessArg(e: JunoAST.Expr): JunoAST.Expr =
        BEGIN
          e := Utils.Ungroup(e);
          IF ISTYPE(e, JunoAST.AtomicExpr) THEN RETURN e END;
          VAR v: JunoAST.NearVarLink := DummyVar(tbl); BEGIN
            last_v.next := v;
            last_v := v;
            INC(f.vars.size);
            last_t.next := NEW(JunoAST.ExprLink, expr := e);
            last_t := last_t.next;
            INC(f.terms.size);
            RETURN Utils.QIdFromNearVar(v)
          END
        END ProcessArg;
      BEGIN
        curr_t.expr := Utils.MapArgs(curr_t.expr, ProcessArg);
        curr_t := curr_t.next
      END
    END;
    f.vars.head := f.vars.head.next;	 (* skip sentinal *)
    f.t0 := f.terms.head.expr;
    f.terms.head := f.terms.head.next;
    DEC(f.terms.size);
    RETURN f
  END Unnest;

(* ================================ ToCmd ================================== *)

TYPE
  FormCmd = RECORD
    f: JunoAST.Formula;
    kind: CARDINAL
  END;
  FormCmds = ARRAY OF FormCmd;

(* A "FormCmd" represents a formula to be compiled into a command. If "fc" is
   a "FormCmd", then "fc.kind" is 0 if "fc.f" is to become a test, 1 if "fc.f"
   is to become an assignment to its left variable, and 2 if "fc.f" is to
   become an assignment to its right variable. *)

EXCEPTION ExtraNearConstraints;
(* Raised by Phase2() *)

PROCEDURE ToCmd(
    nf: JunoAST.NormalForm;
    scp: JunoScope.T;
    stack_tbl: StackTbl.T;
    xtra_vars: JunoAST.NearVarList := NIL):
  JunoAST.Cmd RAISES {Error} =
(* IMPLEMENTATION: The implementation works in two phases. Roughly speaking,
   the first phase repeatedly reduces the number of unknowns and in-line
   substitutes non-primitive predicate and function calls. The second phase
   propagates hints without reducing the number of unknowns. In both phases,
   the "frozen" bit of a variable is set whenever an assignment to that
   variable is generated in the result, indicating that the variable has a
   valid value prior to the query.

   The first phase works in two parts. The first part (Phase1A) works by
   extracting predicate calls and equalities all of whose components are
   literals or known variables. The normal simple formulas so extracted are
   converted to queries (with no unknowns) and assignment statements. As
   opposed to the Prolog implementation, this implementation extracts formulas
   to be converted to queries before it extracts formulas to be converted to
   assignments. *)

  VAR
    inf := IndexedNF.New(var_cnt := 100, conj_cnt := 200);
    cmds := NEW(REF FormCmds, 20); (* extracted commands *)
    tbl: IntRefTbl.T;	 (* maps variable index to list of AST's (formulas) *)
    last_near: CARDINAL; (* number of near constraints in "inf.conj[]" *)
    cmd_cnt: CARDINAL;   (* number of commands in "cmds[]" *)

(* IMPLEMENTATION: For efficiency, we may want to allocate the "cmds" from
   an avail list. *)

  (* Invariants:
|
|      I1: 0 <= last_near <= inf.c_cnt
|          0 <= cmd_cnt <= NUMBER(cmd^)
|
|      I2: Variables inf.var[0..inf.v_cnt-1] are unknown.
|
|      I3: Formulas inf.conj[0..last_near-1] are near constraints.
|          Forumlas inf.conj[last_near..inf.c_cnt-1] are non-near constraints.
|
|      I4: Formulas cmd[0..cmd_cnt-1] are formulas that have been extracted to
|          be converted to queries (with no unknowns) and assignments. The
|	   "b3cnt" field of these formulas is 0 if they are to become tests,
|          1 if they are to become assignments to their left variable, and 2
|          if they are to become assignments to their right variable. *)

  PROCEDURE SwapV(VAR (*INOUT*) v1, v2: JunoAST.NearVarLink) =
    VAR temp := v1; BEGIN v1 := v2; v2 := temp END SwapV;

  PROCEDURE SwapF(VAR (*INOUT*) f1, f2: JunoAST.Formula) =
    VAR temp := f1; BEGIN f1 := f2; f2 := temp END SwapF;

  PROCEDURE MakeCmd(VAR (*INOUT*) f: JunoAST.Formula; kind: CARDINAL) =
  (* Move "f" from its location in "inf.conj" to the end of the "cmds" array.
     Grow the "cmds" array if necessary. *)
    BEGIN
      CopyCmd(f, kind);
      DEC(inf.c_cnt);
      f := inf.conj[inf.c_cnt]
    END MakeCmd;

  PROCEDURE CopyCmd(VAR (*INOUT*) f: JunoAST.Formula; kind: CARDINAL) =
  (* Copy "f" from its location in "inf.conj" to the end of the "cmds" array.
     Grow the "cmds" array if necessary. *)
    BEGIN
      IF cmd_cnt > LAST(cmds^) THEN
        VAR new := NEW(REF FormCmds, 2 * NUMBER(cmds^)); BEGIN
          SUBARRAY(new^, 0, NUMBER(cmds^)) := cmds^;
          cmds := new
        END
      END;
      cmds[cmd_cnt] := FormCmd{kind := kind, f := f};
      INC(cmd_cnt);
    END CopyCmd;

  PROCEDURE UpdateB3Cnts(index: INTEGER) =
    VAR ref: REFANY; BEGIN
      IF NOT tbl.get(index, ref) THEN <* ASSERT FALSE *> END;
      VAR l: RefList.T := ref; BEGIN
        WHILE l # NIL DO
          VAR f: JunoAST.Formula := l.head; BEGIN DEC(f.b3cnt) END;
          l := l.tail
        END
      END;
      EVAL tbl.delete(index, ref)
    END UpdateB3Cnts;

  PROCEDURE PermuteNearToPrefix(start: CARDINAL) =
  (* Establish I3, assuming that I3 is already valid for formulas in the
     range [0..start-1]. Namely, the initial assumptions are that:
|
|       Formulas inf.conj[0..last_near-1] are near constraints.
|       Forumlas inf.conj[last_near..start-1] are non-near constraints. *)
    BEGIN
      FOR i := start TO inf.c_cnt - 1 DO
        TYPECASE inf.conj[i] OF JunoAST.Equals (e) =>
          IF e.near THEN
            SwapF(inf.conj[last_near], inf.conj[i]);
            INC(last_near)
          END
        ELSE (* SKIP *)
        END
      END
    END PermuteNearToPrefix;

  PROCEDURE Phase1A() =
  (* Extract queries and assignments from "inf.conj[last_near..inf.c_cnt-1]"
     into "cmd[0..cmd_cnt-1]". Assumes and maintains I1 - I4. Sets "inf.c_cnt"
     to the lowest and "cmd_cnt" to the highest possible values that still
     maintain these invariants. *)
    VAR done := FALSE; debugOutput := FALSE; BEGIN
      IF debug >= 1 THEN
        Wr.PutText(stderr, "\n    Extracting predicates/assignments:\n")
      END;
      WHILE NOT done DO
        done := TRUE;
        (* Extract as many queries as possible *)
        VAR i := last_near; BEGIN
          WHILE i < inf.c_cnt DO
            IF inf.conj[i].b3cnt = 0 THEN
              IF debug >= 1 THEN
                debugOutput := TRUE;
                Wr.PutText(stderr, "      b3cnt = 0, ");
                JunoUnparse.P(stderr, inf.conj[i], indent := 0, debug := TRUE);
                Wr.PutChar(stderr, '\n')
              END;
              MakeCmd(inf.conj[i], kind := 0)
            ELSE
              INC(i)
            END
          END
        END;
        (* Attempt to extract a single assignment *)
        FOR i := last_near TO inf.c_cnt - 1 DO
          IF inf.conj[i].b3cnt = 1 THEN
            TYPECASE inf.conj[i] OF JunoAST.Equals (e) =>
              IF debug >= 1 THEN
                debugOutput := TRUE;
                Wr.PutText(stderr, "      b3cnt = 1, ");
                JunoUnparse.P(stderr, inf.conj[i], indent := 0, debug := TRUE);
                Wr.PutChar(stderr, '\n')
              END;
              VAR
                j: CARDINAL;
                qid := UnknownVar(e, SUBARRAY(inf.var^, 0, inf.v_cnt), j);
              BEGIN
                IF qid # NIL THEN
                  (* update variable array *)
                  DEC(inf.v_cnt);
                  inf.var[j] := inf.var[inf.v_cnt];
                  (* update b3cnt fields of relevant expressions *)
                  UpdateB3Cnts(qid.index);
                  <* ASSERT inf.conj[i].b3cnt = 0 *>
                  (* make the formula a command *)
                  MakeCmd(inf.conj[i], kind := 1 + ORD(qid = e.e2));
                  done := FALSE;
                  (* assignment found; exit FOR loop early *)
                  EXIT
                END
              END
            ELSE (* SKIP *)
            END
          END (* IF *)
        END (* FOR *)
      END;  (* WHILE *)
      IF debug >= 1 THEN
        IF NOT debugOutput THEN Wr.PutText(stderr, "      <none>\n") END;
        Wr.Flush(stderr)
      END
    END Phase1A;

  PROCEDURE Phase1B(): BOOLEAN RAISES {Error} =
  (* In-line expand the first reducible predicate or formula in the array
     "inf.conj[last_near..inf.c_cnt]" and return TRUE. If all these formulas
     are irreducible, return FALSE. Raises "Error" if any of the formulas is
     an invalid application of a function, predicate, or compound formula in a
     constraint. *)
    VAR i := last_near; nf: JunoAST.NormalForm; BEGIN
      WHILE i < inf.c_cnt AND Irreducible(inf.conj[i], nf) DO INC(i) END;
      IF i = inf.c_cnt THEN RETURN FALSE END;
      IF debug >= 1 THEN
        Wr.PutText(stderr, "    In-line substituting:\n");
        JunoUnparse.P(stderr, inf.conj[i], indent := 6, debug := TRUE);
        Wr.PutText(stderr, "\n        with\n");
        JunoUnparse.P(stderr, nf, indent := 6, debug := TRUE);
        Wr.PutChar(stderr, '\n'); Wr.Flush(stderr)
      END;
      VAR old_v_cnt, old_c_cnt: CARDINAL; BEGIN
        (* in-line substitute "inf.conj[i]" *)
        DEC(inf.c_cnt);
        inf.conj[i] := inf.conj[inf.c_cnt];
        old_v_cnt := inf.v_cnt;
        IndexedNF.AddVarArray(inf, nf.var^);
        old_c_cnt := inf.c_cnt;
        IndexedNF.AddConjArray(inf, nf.conj^);

        (* update use table *)
        UpdateUseTbl(tbl,
          SUBARRAY(inf.var^, old_v_cnt, inf.v_cnt - old_v_cnt),
          SUBARRAY(inf.conj^, old_c_cnt, inf.c_cnt - old_c_cnt));

        (* move new near constraints to front *)
        PermuteNearToPrefix(start := old_c_cnt)
      END;
      RETURN TRUE
    END Phase1B;

  PROCEDURE Irreducible(
      VAR (*INOUT*) f: JunoAST.Formula;
      VAR (*OUT*) newF: JunoAST.NormalForm)
    : BOOLEAN RAISES {Error} =
  (* Return "TRUE" if "f" is a primitive solve predicate. In this
     case, the value of "newF" on exit is undefined, and if "f" is an
     equality, its left-hand side is required to be a literal or QId.

     Otherwise, "f" is a predicate or an equality between a variable and a
     function; set "newF" to the result of replacing the predicate or function
     in "f" by its definition and return "FALSE".

     Requires "f" is not a near constraint. 

     Raises "Error" if "f" is an invalid application of a function, predicate,
     or compound formula (i.e., "OR" or "NOT") in a constraint. *)
    BEGIN
      TYPECASE f OF <* NOWARN *>
	JunoAST.LitPred => RETURN TRUE
      | JunoAST.IsReal, JunoAST.IsText => RETURN TRUE
      | JunoAST.IsPair (ip) => newF := ExpandIsPair(ip)
      | JunoAST.Or, JunoAST.Not, JunoAST.IsInt =>
          Raise("You can't write this in a constraint", f)
      | JunoAST.Call (c) =>
	  newF := Substitute(c.normal_form, c.ins, NIL,
	    stack_tbl.next_index - 1, c)
      | JunoAST.Equals (e) =>
          <* ASSERT NOT e.near *>
          <* ASSERT ISTYPE(e.e1, JunoAST.AtomicExpr) *>
          TYPECASE e.e2 OF             (* check for triples *)
            JunoAST.AtomicExpr, JunoAST.Plus, JunoAST.Times, JunoAST.Pair,
            JunoAST.Atan, JunoAST.Sin, JunoAST.Cos, JunoAST.Exp,
            JunoAST.Ln =>
              RETURN TRUE
          | JunoAST.List(list) =>
              newF := ExpandList(e.e1, list)
          ELSE
              newF := Substitute(GetFunc(e.e2), Actuals(e.e2), e.e1,
		stack_tbl.next_index - 1, e)
          END
      | JunoAST.Relation (r) =>
	  newF := Substitute(GetPred(r), Actuals(r), NIL,
	    stack_tbl.next_index - 1, r);
      END;
      RETURN FALSE
    END Irreducible;

  PROCEDURE Substitute(
      nf: JunoAST.NormalForm;
      actuals: JunoAST.ExprList;	 (* IN parameters *)
      last_actual: JunoAST.Expr;	 (* single OUT result parameter *)
      index_delta: CARDINAL;
      bp: JunoAST.Formula):
    JunoAST.NormalForm =
  (* Return a new normal-form formula equivalent to "nf" with all free
     occurrences of the "formals" in "nf" replaced by the corresponding
     "actuals", and with the index of each *local* variable of "nf"
     incremented by "index_delta". The last actual only counts if it is not
     NIL. This procedure has the side-effect of incrementing the global
     "stack_tbl.next_index" by the number of local variables in "nf", namely,
     "NUMBER(nf.var^)". All newly consed nodes have the given "bp". *)
    VAR
      arg_cnt := actuals.size + ORD(last_actual # NIL);
      args := NEW(REF ARRAY OF JunoAST.Expr, arg_cnt);

    PROCEDURE SubstExpr(e: JunoAST.Expr): JunoAST.Expr =
      BEGIN
	TYPECASE Utils.Ungroup(e) OF
	  JunoAST.LitValue => RETURN e
	| JunoAST.QId (q) =>
            IF q.type = JunoAST.IdType.Local THEN
              IF q.index < 0 THEN	 (* "q" is a formal parameter *)
                RETURN args[arg_cnt + q.index]
	      ELSE			 (* "q" is a local variable *)
	        RETURN NEW(JunoAST.QId, bp := q, id0 := q.id0, id1 := q.id1,
	          type := q.type, index := q.index + index_delta)
              END
            ELSE RETURN q		 (* "q" is a const or global var *)
            END
	ELSE
	    RETURN Utils.MapArgs(e, SubstExpr)
	END
      END SubstExpr;

    (* Substitute *)
    VAR subst_inf := IndexedNF.New(var_cnt := 100, conj_cnt := 200); BEGIN
      (* Copy "nf" to global "subst_inf" *)
      IndexedNF.FromNF(nf, subst_inf);
      subst_inf.bp := bp;
      (* Build "args" array with IN parameters last (in order)
	 and single OUT parameter first (if it exists) *)
      VAR curr := actuals.head; i := arg_cnt - actuals.size; BEGIN
        WHILE curr # NIL DO
          <* ASSERT ISTYPE(curr.expr, JunoAST.AtomicExpr) *>
          args[i] := curr.expr; INC(i);
          curr := curr.next
        END;
        IF last_actual # NIL THEN args[0] := last_actual END
      END;
      (* Increase the index of each local variable by "index_delta" *)
      FOR i := 0 TO subst_inf.v_cnt - 1 DO
        WITH v = subst_inf.var[i] DO
          v := NEW(JunoAST.NearVarLink, id := v.id, hint := v.hint,
            evar := v.evar, index := v.index + index_delta)
        END
      END;
      (* Copy and substitute in each conjunct *)
      FOR i := 0 TO subst_inf.c_cnt - 1 DO
        subst_inf.conj[i] := SubstExpr(subst_inf.conj[i])
      END;
      INC(stack_tbl.next_index, subst_inf.v_cnt);
      VAR res := IndexedNF.ToNF(subst_inf); BEGIN
        IndexedNF.Dispose(subst_inf);
        RETURN res
      END
    END Substitute;

  PROCEDURE GetFunc(call: JunoAST.Expr): JunoAST.NormalForm RAISES {Error} =
  (* Return the normal form bound in "scp" to the user-defined or built-in
     function at the root of "call". Requires that "call" is not one of the
     built-in functions understood by the Juno solver or a "JunoAST.List".

     If "call" has type "JunoAST.Call", then this procedure relies on the fact
     that JunoCompile.AnnotateAtoms has annotated "call.normal_form" such
     that: if "call" represents a function call, then "call.normal_form" is
     the normal form for that function's body, and if "call" represents a
     functional procedure call, then "call.normal_form" is NIL. In the latter
     case, "Error" will be raised with an appropriate message.

     Raises "Error" if "call" is illegal in a constraint. *)
    VAR name: JunoAST.QId; BEGIN
      TYPECASE call OF
        JunoAST.UMinus  => name := JunoAST.UMinusName
      | JunoAST.Car     => name := JunoAST.CarName
      | JunoAST.Cdr     => name := JunoAST.CdrName
      | JunoAST.Minus   => name := JunoAST.MinusName
      | JunoAST.Divide  => name := JunoAST.DivideName
      | JunoAST.Rel     => name := JunoAST.RelName
      | JunoAST.Atan, JunoAST.Sin, JunoAST.Cos, JunoAST.Exp, JunoAST.Ln,
        JunoAST.Plus, JunoAST.Times, JunoAST.Pair, JunoAST.List =>
          <* ASSERT FALSE *>
      | JunoAST.Call (c) =>
          IF c.normal_form # NIL
            THEN RETURN c.normal_form
            ELSE Raise("Procedure calls aren't allowed in constraints", c)
          END
      ELSE
	  Raise("You can't write this in a constraint", call)
      END;
      VAR dummy: JunoScope.Entity; f: JunoScope.Func; BEGIN
	f := JunoScope.LookupQId(scp, name, dummy);
	<* ASSERT f # NIL *>		 (* Built-ins must be in defined *)
	RETURN f.normal_form
      END
    END GetFunc;
  
  PROCEDURE GetPred(r: JunoAST.Relation): JunoAST.NormalForm RAISES {Error} =
  (* Return the normal form bound in "scp" to the built-in relation "r".
     Requires that "r" is not the equality relation.
  
     Raises "Error" if "r" is an invalid predicate in a constraint. *)
    VAR name: JunoAST.QId; BEGIN
      TYPECASE r OF
	JunoAST.Cong => name := JunoAST.CongName
      | JunoAST.Para => name := JunoAST.ParaName
      | JunoAST.Hor  => name := JunoAST.HorName
      | JunoAST.Ver  => name := JunoAST.VerName
      | JunoAST.Equals => <* ASSERT FALSE *>
      ELSE
	  Raise("You can't write this in a constraint", r)
      END;
      VAR dummy: JunoScope.Entity; p: JunoScope.Pred; BEGIN
	p := JunoScope.LookupQId(scp, name, dummy);
	<* ASSERT p # NIL *>		 (* built-ins must be in scope *)
	RETURN p.normal_form
      END
    END GetPred;

  PROCEDURE ExpandIsPair(ip: JunoAST.IsPair): JunoAST.NormalForm =
  (* Return a new normal-form formula equivalent to "ip" that does not contain
     a PAIR predicate at the top level. In particular, "PAIR(e)" is tranlated
     into the formula: "(E _x[1], _x[2] :: e = (_x[1], _x[2]))". The indices
     for the newly quantified variables are actually taken from
     "stack_tbl.next_index".*)
    VAR
      vars := NewNearVars(2);
      f := NEW(REF JunoAST.Formulas, 1);
    BEGIN
      f[0] := NEW(JunoAST.Equals, bp := ip, e1 := ip.e,
        e2 := NEW(JunoAST.Pair,
          e1 := Utils.QIdFromNearVar(vars[0]),
          e2 := Utils.QIdFromNearVar(vars[1])));
      RETURN NEW(JunoAST.NormalForm, conj := f, var := vars)
    END ExpandIsPair;

  PROCEDURE ExpandList(
      id: JunoAST.AtomicExpr;
      l: JunoAST.List):
    JunoAST.NormalForm =
  (* Return a new normal-form formula not containing any "JunoAST.List"'s at
     the top level and equivalent to "id = l". For example, if "id" is "a" and
     "l" is "[1,2,3]", then return the formula:
|        (E _x[1], _x[2] ::
|                a = (1, _x[1])
|        AND _x[1] = (2, _x[2])
|        AND _x[2] = (3, NIL))
     The indices for the newly quantified variables are actually taken from
     "stack_tbl.next_index". *)
    VAR
      vars := NewNearVars(l.elts.size - 1);
      f := NEW(REF JunoAST.Formulas, l.elts.size);
      lhs: JunoAST.AtomicExpr := id;
      cdr: JunoAST.NearVarLink;
      currE := l.elts.head;
    BEGIN
      FOR i := FIRST(f^) TO LAST(f^) DO
        IF i < LAST(f^) THEN cdr := vars[i] ELSE cdr := NIL END;
        VAR cdrVal := QIdFromNearVarOrNil(cdr); BEGIN
          f[i] := NEW(JunoAST.Equals, e1 := lhs,
            e2 := NEW(JunoAST.Pair, e1 := currE.expr, e2 := cdrVal));
          lhs := cdrVal
        END;
        currE := currE.next
      END;
      RETURN NEW(JunoAST.NormalForm, conj := f, var := vars)
    END ExpandList;

  PROCEDURE NewNearVars(cnt: CARDINAL): REF JunoAST.Vars =
  (* Return an array of size "cnt" of unhinted E-quantified dummy near vars.
     The dummy variables have the same name (the value of the global
     "dummyVar"), but they are given distinct indices from "stack_tbl". *)
    VAR res := NEW(REF JunoAST.Vars, cnt); BEGIN
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := NEW(JunoAST.NearVarLink, id := dummyVar, evar := TRUE,
          hint := JunoAST.NilExpr, index := stack_tbl.next_index);
        INC(stack_tbl.next_index)
      END;
      RETURN res
    END NewNearVars;

  PROCEDURE Phase2() RAISES { ExtraNearConstraints } =
    (* Extract hint assignments from "inf.conj[0..inf.c_cnt-1]". Requires I1
       - I4. Any near constraints that could be extracted are deleted from
       "inf.conj[0..inf.c_cnt-1]". Raises "ExtraNearConstraints" if there were
       any active near constraints that could not be extracted.

       IMPLEMENTATION: During this phase, the "b3cnt" field of the formulas
       that are active candidates for processing represents the number of
       unhinted (not unknown) variables remaining in that formula. See Allan's
       notes from 9-Nov-92 for more details about this phase. *)
    VAR
      done := FALSE;
      last_unhinted := inf.v_cnt;
      active_near := last_near;
    (* Invariant:
|
|      I2': Variables inf.var[0..last_unhinted-1] are unhinted unknowns.
|           Variables inf.var[last_unhinted..inf.v_cnt-1] are hinted unknowns.
    *)

    PROCEDURE ShowConstraints(msg: TEXT) =
      CONST n = 7; nl = "\n        "; BEGIN
        Wr.PutText(stderr, "\n    " & msg);
        Wr.PutText(stderr, "\n      Unhinted Vars:" & nl);
        IF 0 > last_unhinted - 1 THEN Wr.PutText(stderr, "<none>") END;
        FOR i := 0 TO last_unhinted - 1 DO
          JunoUnparse.P(stderr, Utils.QIdFromNearVar(inf.var[i]),
            indent := 0, debug := TRUE);
          IF i < last_unhinted - 1 THEN
            IF i MOD n = (n - 1)
              THEN Wr.PutText(stderr, nl)
              ELSE Wr.PutText(stderr, ", ")
            END
          END
        END;
        Wr.PutText(stderr, "\n      Hinted Vars:" & nl);
        IF last_unhinted > inf.v_cnt - 1 THEN Wr.PutText(stderr, "<none>") END;
        FOR i := last_unhinted TO inf.v_cnt - 1 DO
          JunoUnparse.P(stderr, Utils.QIdFromNearVar(inf.var[i]),
            indent := 0, debug := TRUE);
          IF i < inf.v_cnt - 1 THEN
            IF (i - last_unhinted) MOD n = (n - 1)
              THEN Wr.PutText(stderr, nl)
              ELSE Wr.PutText(stderr, ", ")
            END
          END
        END;
        Wr.PutText(stderr, "\n      Constraints:" & nl);
        IF 0 > inf.c_cnt - 1 THEN Wr.PutText(stderr, "<none>") END;
        FOR i := 0 TO inf.c_cnt - 1 DO
          Wr.PutText(stderr, "b3cnt = " & Fmt.Int(inf.conj[i].b3cnt) & ", ");
          JunoUnparse.P(stderr, inf.conj[i], indent := 0, debug := TRUE);
          IF i < inf.c_cnt - 1 THEN Wr.PutText(stderr, nl) END
        END;
        Wr.PutChar(stderr, '\n'); Wr.Flush(stderr)
      END ShowConstraints;

    (* Phase2 *)
    BEGIN
      (* Some variables (passed in queries to the compiler) may already be
	 designated as hinted, so move them to the end of the array. *)
      VAR i := 0; BEGIN
        WHILE i < last_unhinted DO
          IF inf.var[i].frozen THEN
            DEC(last_unhinted);
            SwapV(inf.var[i], inf.var[last_unhinted]);
            UpdateB3Cnts(inf.var[last_unhinted].index)
          ELSE
            INC(i)
          END
        END
      END;

      (* Extract hint-initializing assignments (Rules 2 & 4) *)
      IF debug >= 2 THEN
        ShowConstraints("Extracting hint assignments from:")
      END;
      WHILE NOT done DO
        done := TRUE;
        FOR i := 0 TO inf.c_cnt - 1 DO
          IF inf.conj[i].b3cnt = 1 THEN
            TYPECASE inf.conj[i] OF JunoAST.Equals (e) =>
              VAR
                j: CARDINAL;
                qid := UnknownVar(e, SUBARRAY(inf.var^,0,last_unhinted), j);
              BEGIN
                IF qid # NIL THEN
                  (* update variable array *)
                  (* Set "inf.var[j]" to a copy of itself w/ "frozen:=TRUE" *)
                  VAR old := inf.var[j]; BEGIN
                    inf.var[j] := NEW(JunoAST.NearVarLink, id := old.id,
                      evar := old.evar, frozen := TRUE, hint := old.hint,
                      index := old.index, next := old.next)
                  END;
                  DEC(last_unhinted);
                  SwapV(inf.var[j], inf.var[last_unhinted]);
                  (* update b3cnt fields of relevant expressions *)
                  UpdateB3Cnts(qid.index);
                  <* ASSERT inf.conj[i].b3cnt = 0 *>
                  (* copy formula to "cmds" array *)
                  CopyCmd(inf.conj[i], kind := 1 + ORD(qid = e.e2));
                  (* Delete near formula within "inf.conj" *)
                  IF e.near THEN
                    DEC(inf.c_cnt);
                    inf.conj[i] := inf.conj[inf.c_cnt];
                    DEC(active_near)
                  END;
                  done := FALSE;
                  (* assignment found; exit FOR loop early *)
                  EXIT
                END
              END
            ELSE (* SKIP *)
            END
          END
        END
      END; (* WHILE *)

      (* Eliminate near constraints on hinted or known vars (Rule 3) *)
      IF debug >= 2 THEN
        ShowConstraints("Eliminating extra near constraints:")
      END;
      VAR i := 0; near_found: BOOLEAN; BEGIN
        WHILE i < inf.c_cnt DO
          near_found := FALSE;
          IF inf.conj[i].b3cnt = 0 THEN
            TYPECASE inf.conj[i] OF JunoAST.Equals (e) =>
              IF e.near THEN
                near_found := TRUE;
                DEC(inf.c_cnt);
                inf.conj[i] := inf.conj[inf.c_cnt];
                DEC(active_near)
              END
            ELSE (* SKIP *)
            END
          END;
          IF NOT near_found THEN INC(i) END
        END
      END;
      IF debug >= 2 THEN
        ShowConstraints("Done processing hints:")
      END;
      IF active_near > 0 THEN RAISE ExtraNearConstraints END
    END Phase2;

  PROCEDURE PreQueryCmd(READONLY forms: FormCmds): JunoAST.Cmd RAISES {Error} =
  (* Return a sequence of queries and assignments formed from the formulas in
     "forms". The order of the commands in the result matches the order of the
     input formulas. The "kind" field of each formula is used to produce a
     corresponding query (when "b3cnt = 0") or assignment (when "b3cnt = 1" or
     "b3cnt = 2"). The result always ends with "JunoAST.SkipVal". *)
    VAR res: JunoAST.Cmd := JunoAST.SkipVal; curr: JunoAST.Cmd; BEGIN
      FOR i := LAST(forms) TO FIRST(forms) BY -1 DO
	CASE forms[i].kind OF <* NOWARN *>
	  0 =>
	    curr := C2q2(forms[i].f)
	| 1 =>
	    VAR eq := NARROW(forms[i].f, JunoAST.Equals); BEGIN
	      curr := NEW(JunoAST.Assign, bp := forms[i].f,
		vars := Utils.NewQIdList(eq.e1),
		exprs := Utils.NewExprList(eq.e2))
	    END
	| 2 =>
	    VAR eq := NARROW(forms[i].f, JunoAST.Equals); BEGIN
	      curr := NEW(JunoAST.Assign, bp := forms[i].f,
		vars := Utils.NewQIdList(eq.e2),
		exprs := Utils.NewExprList(eq.e1))
	    END
	END;
	IF curr # JunoAST.SkipVal THEN
	  res := NEW(JunoAST.Seq, c1 := curr, c2 := res)
	END
      END;
      RETURN res
    END PreQueryCmd;
  
  PROCEDURE C2q2(pred: JunoAST.Formula): JunoAST.Cmd RAISES {Error} =
  (* This procedure works like "JunoCompile.C2q", but with the following
     exceptions:
|      1) It does not handle "And" and "Exists" predicates, and
|      2) It recursively calls "ToCmd" (instead of "C2q") on the arguments
|         to "Not" and "Or".
  *)
    BEGIN
      TYPECASE Utils.Ungroup(pred) OF
	JunoAST.True  => RETURN JunoAST.SkipVal
      | JunoAST.False => RETURN JunoAST.FailVal
      | JunoAST.Differs (p) =>				       (* rule C2.17 *)
	  RETURN NEW(JunoAST.Flip, body := NEW(JunoAST.Query,
	    f := NEW(JunoAST.Equals, bp := p, e1 := p.e1, e2 := p.e2),
	    vars := JunoAST.EmptyNVList))
      | JunoAST.Not (p) =>
	  RETURN NEW(JunoAST.Flip, bp := p,
	    body := ToCmd(p.f, scp, stack_tbl))
      | JunoAST.Or (p) =>
	  RETURN NEW(JunoAST.Else, bp := p,
	    c1 := ToCmd(p.f1, scp, stack_tbl),
	    c2 := ToCmd(p.f2, scp, stack_tbl))
      ELSE						       (* rule C2.21 *)
	  RETURN NEW(JunoAST.Query, f := pred, vars := JunoAST.EmptyNVList)
      END
    END C2q2;

  PROCEDURE Combine(pre: JunoAST.Cmd; post: IndexedNF.T): JunoAST.Cmd =
  (* Return a sequence of the commands "pre" and the solve query "post".
     Either or both of "pre" and "post" may be "empty". "Pre" is empty if it
     is "JunoAST.SkipVal". "Post" is empty if it has no variables to solve for
     (in which case, it is required not to have any conjuncts to solve for
     either). If one or the other is empty, return the other. If both are
     empty, return "pre" (i.e., "SKIP"). *)
    VAR res: JunoAST.Cmd; BEGIN
      IF post.v_cnt = 0 THEN <* ASSERT post.c_cnt = 0 *> RETURN pre END;
      VAR
        qNF := IndexedNF.ToNF(post);
        qCmd := NEW(JunoAST.ConjQuery, var := qNF.var, conj := qNF.conj);
      BEGIN
        IF pre = JunoAST.SkipVal
          THEN res := qCmd
          ELSE res := NEW(JunoAST.Seq, c1 := pre, c2 := qCmd)
        END
      END;
      RETURN res
    END Combine;

  (* ToCmd *)
  BEGIN
    (* Check the pre-condition *)
    FOR i := 0 TO LAST(nf.var^) DO
      WITH v = nf.var[i] DO
        <* ASSERT v.evar=TRUE AND v.hint=JunoAST.NilExpr AND v.frozen=FALSE *>
      END
    END;
    IF xtra_vars # NIL THEN
      VAR l := xtra_vars.head; BEGIN
    	WHILE l # NIL DO
    	  <* ASSERT l.evar=FALSE AND l.hint=JunoAST.NilExpr *>
    	  l := l.next
    	END
      END
    END;

    (* Create an indexed normal form that includes "vars" *)
    IndexedNF.FromNF(nf, inf);
    IF xtra_vars # NIL THEN IndexedNF.AddVarList(inf, xtra_vars) END;

    (* Create and initialize a new use table *)
    tbl := NEW(IntRefTbl.Default).init(inf.v_cnt);
    UpdateUseTbl(tbl, SUBARRAY(inf.var^, 0, inf.v_cnt), nf.conj^);

    (* Establish I1, I2, I3, and I4 *)
    last_near := 0;
    cmd_cnt := 0;
    PermuteNearToPrefix(start := 0);

    (* Loop until no more substitutions are possible *)
    IF debug >= 1 THEN
      Wr.PutText(stderr, "\n  Simplifying constraint...\n")
    END;
    REPEAT Phase1A() UNTIL NOT Phase1B();

    (* Propagate hints *)
    TRY Phase2() EXCEPT ExtraNearConstraints =>
      Raise("Extra near constraints", nf)
    END;

    (* Construct return result *)
    VAR body := Combine(PreQueryCmd(SUBARRAY(cmds^, 0, cmd_cnt)), inf); BEGIN
      IndexedNF.Dispose(inf);
      RETURN NEW(JunoAST.Safe, bp := nf, body := body)
    END
  END ToCmd;

PROCEDURE UpdateUseTbl(
    VAR (*INOUT*) tbl: IntRefTbl.T;
    READONLY vars: JunoAST.Vars;
    READONLY nsf: JunoAST.Formulas) =
(* Incrementally update "tbl" to contain new entries for the variables "vars".
   For each "v" in the resulting table, let "tbl(v) = l" before the call ("l"
   is the empty list for any "v" in "vars"), and let "tbl(v) = l'" after the
   call. Then "l' = lpre & l", where "lpre" is a list of those formulas in
   "nsf" that contain "v". The formula "f" appears in "lpre" exactly as many
   times as "v" occurs in "f", which number is also added to "f.b3cnt".
   Requires that each "nsf[i]" is a normal simple formula. *)
  VAR curr_nsf: JunoAST.Formula;

  PROCEDURE CntUse(e: JunoAST.Expr): JunoAST.Expr =
  (* For each local variable "v" that occurs in "e" and whose index is
     registered in "tbl", add the formula "curr_nsf" to the list of formulas
     associated with "v" in the table "tbl", and increment the "b3cnt" field
     of "curr_nsf". Returns Modula-3 "NIL" (so it can be passed as an argument
     to "JunoASTUtils.MapArgs"). *)
    BEGIN
      e := Utils.Ungroup(e);
      TYPECASE e OF
        JunoAST.LitValue => (* SKIP *)
      | JunoAST.QId (q) =>
          VAR ref: REFANY; BEGIN
            <* ASSERT q.index # 0 *>
            IF q.type = JunoAST.IdType.Local
              AND tbl.get(q.index, ref) THEN
      	      INC(curr_nsf.b3cnt);
              VAR rl: RefList.T := ref; BEGIN
      	        EVAL tbl.put(q.index, RefList.Cons(curr_nsf, rl))
              END
            END
          END
      | JunoAST.Not (not) =>
          EVAL CntUse(not.f);
      | JunoAST.Or (or) =>
          EVAL CntUse(or.f1);
          EVAL CntUse(or.f2);
      | JunoAST.NormalForm (nf) =>
          FOR i := 0 TO LAST(nf.conj^) DO
            EVAL CntUse(nf.conj[i])
          END
      ELSE
          EVAL Utils.MapArgs(e, CntUse)
      END;
      RETURN NIL
    END CntUse;

  (* UpdateUseTbl *)
  BEGIN
    (* Install a new entry in the table for each variable *)
    FOR i := FIRST(vars) TO LAST(vars) DO
      <* ASSERT vars[i].index # 0 *>
      EVAL tbl.put(vars[i].index, NIL);
    END;

    (* Update the table and use counts for each normal simple formula *)
    FOR i := FIRST(nsf) TO LAST(nsf) DO
      curr_nsf := nsf[i];
      TYPECASE curr_nsf OF JunoAST.Equals (e) =>
        EVAL CntUse(e.e1);
        EVAL CntUse(e.e2)
      ELSE
        EVAL CntUse(curr_nsf)
      END
    END
  END UpdateUseTbl;

(* ========================== MISCELLANEOUS ================================ *)

PROCEDURE Actuals(call: JunoAST.Expr): JunoAST.ExprList =
(* Return the actual arguments to "call" in left-to-right order. *)
  VAR res := NEW(JunoAST.ExprList);
  PROCEDURE ConsArg(e: JunoAST.Expr): JunoAST.Expr =
    BEGIN
      res.head := NEW(JunoAST.ExprLink, expr := e, next := res.head);
      INC(res.size);
      RETURN e
    END ConsArg;
  (* Actuals *)
  BEGIN
    EVAL Utils.MapArgs(call, ConsArg);
    res.head := ReverseExprList(res.head);
    RETURN res;
  END Actuals;

PROCEDURE ReverseExprList(el: JunoAST.ExprLink): JunoAST.ExprLink =
(* Destructively reverse the list "el" in place, and return a pointer to the
   head of the new list. *)
  VAR curr, next: JunoAST.ExprLink; BEGIN
    IF el # NIL THEN
      next := el.next;
      el.next := NIL;
      WHILE next # NIL DO
        curr := next;
        next := curr.next;
        curr.next := el;
        el := curr
      END
    END;
    RETURN el
  END ReverseExprList;

PROCEDURE UnknownVar(
    r: JunoAST.Relation;
    READONLY unknowns: JunoAST.Vars;
    VAR (*OUT*) index: CARDINAL):
    JunoAST.QId =
(* Return "r.e1" or "r.e2" if either is a local variable appearing in
   "unknowns". In this case, set "index" to the index of the identifier in
   "unknowns". If there is no such local variable, the value of
   "index" is undefined; return "NIL". *)

  PROCEDURE CheckExpr(e: JunoAST.Expr; VAR (*OUT*) index: CARDINAL):
      JunoAST.QId =
    VAR j: INTEGER; BEGIN
      TYPECASE e OF JunoAST.QId (q) =>
    	IF q.type = JunoAST.IdType.Local THEN
    	  j := MemVars(q.index, unknowns);
    	  IF j >= 0 THEN index := j; RETURN q END
    	END
      ELSE (* SKIP *)
      END;
      RETURN NIL
    END CheckExpr;

  (* UnknownVar *)
  VAR res: JunoAST.QId; BEGIN
    res := CheckExpr(r.e1, index);
    IF res = NIL THEN res := CheckExpr(r.e2, index) END;
    RETURN res
  END UnknownVar;

PROCEDURE MemVars(index: INTEGER; READONLY vars: JunoAST.Vars): INTEGER =
(* Return the index of the variable in "vars" having index "index", or -1 if
   there is no such variable. *)
  BEGIN
    <* ASSERT index # 0 *>
    FOR i := FIRST(vars) TO LAST(vars) DO
      IF index = vars[i].index THEN RETURN i END
    END;
    RETURN -1
  END MemVars;

PROCEDURE QIdFromNearVarOrNil(v: JunoAST.NearVarLink): JunoAST.AtomicExpr =
(* If "v = NIL", return the Juno value "NIL"; otherwise, return
   "Utils.QIdFromNearVar(v)". *)
  BEGIN
    IF v = NIL
      THEN RETURN JunoAST.NilVal
      ELSE RETURN Utils.QIdFromNearVar(v)
    END
  END QIdFromNearVarOrNil;

BEGIN
END JunoCompileNF.
