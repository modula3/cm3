(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 17:19:24 PST 1997 by heydon                   *)

MODULE JunoASTUtils;

IMPORT BuiltInSlots, JunoAST, JunoValue, Atom;

(* ====================== Id/QId/NearVar Conversions ======================= *)

PROCEDURE QIdFromNearVar(v: JunoAST.NearVarLink): JunoAST.QId =
  BEGIN
    RETURN NEW(JunoAST.QId, id0 := JunoAST.NilId, id1 := v.id,
      type := JunoAST.IdType.Local, index := v.index)
  END QIdFromNearVar;

PROCEDURE QIdFromIds(mod, id: JunoAST.Id): JunoAST.QId =
  BEGIN
    <* ASSERT id # JunoAST.NilId *>
    RETURN NEW(JunoAST.QId, bp := JunoAST.End, id0 := mod, id1 := id)
  END QIdFromIds;

PROCEDURE QIdFromTexts(mod, id: TEXT): JunoAST.QId =
  BEGIN
    RETURN NEW(JunoAST.QId, bp := JunoAST.End,
      id0 := Atom.FromText(mod), id1 := Atom.FromText(id))
  END QIdFromTexts;

PROCEDURE QIdFromId(id: JunoAST.Id): JunoAST.QId =
  BEGIN
    <* ASSERT id # NIL *>
    RETURN NEW(JunoAST.QId, bp := JunoAST.End,
      id0 := JunoAST.NilId, id1 := id)
  END QIdFromId;

PROCEDURE QIdFromText(t: TEXT): JunoAST.QId =
  BEGIN RETURN QIdFromId(Atom.FromText(t)) END QIdFromText;

(* ====================== IdList Conversions =============================== *)

PROCEDURE IdListToNearVarList(l: JunoAST.IdList): JunoAST.NearVarList =
  VAR h := l.head; hres: JunoAST.NearVarLink := NIL; BEGIN
    WHILE h # NIL DO
      hres := NEW(JunoAST.NearVarLink, id := h.id, index := h.index,
        hint := JunoAST.NilExpr, next := hres);
      h := h.next
    END;
    RETURN NEW(JunoAST.NearVarList, bp := l, size := l.size, head := hres)
  END IdListToNearVarList;

PROCEDURE IdListToQIdList(l: JunoAST.IdList): JunoAST.QIdList =
  VAR head, last: JunoAST.ExprLink := NIL; curr := l.head; BEGIN
    WHILE curr # NIL DO
      VAR new := NEW(JunoAST.ExprLink, expr := QIdFromId(curr.id)); BEGIN
        IF head = NIL
          THEN head := new
          ELSE last.next := new
        END;
        last := new
      END;
      curr := curr.next
    END;
    RETURN NEW(JunoAST.QIdList, bp := l, size := l.size, head := head)
  END IdListToQIdList;

(* ==================== Create New 1-Item Lists ============================ *)

PROCEDURE NewExprList(e: JunoAST.Expr; bp: JunoAST.T := NIL): JunoAST.ExprList=
  BEGIN
    RETURN NEW(JunoAST.ExprList, size := 1, bp := bp,
      head := NEW(JunoAST.ExprLink, expr := e))
  END NewExprList;

PROCEDURE NewQIdList(qid: JunoAST.QId; bp: JunoAST.T := NIL): JunoAST.QIdList =
  BEGIN
    RETURN NEW(JunoAST.QIdList, size := 1, bp := bp,
      head := NEW(JunoAST.ExprLink, expr := qid))
  END NewQIdList;

PROCEDURE NewIdList(id: JunoAST.Id; index: INTEGER := 0): JunoAST.IdList =
  BEGIN
    RETURN NEW(JunoAST.IdList, bp := JunoAST.End, size := 1,
      head := NEW(JunoAST.IdLink, id := id, index := index))
  END NewIdList;

(* ======================== Membership Tests =============================== *)

PROCEDURE MemIdList(id: JunoAST.Id; l: JunoAST.IdList): BOOLEAN =
  VAR curr := l.head; BEGIN
    WHILE curr # NIL AND curr.id # id DO curr := curr.next END;
    RETURN curr # NIL
  END MemIdList;

PROCEDURE MemNearVarList(id: JunoAST.Id; l: JunoAST.NearVarList):
  JunoAST.NearVarLink =
  VAR curr := l.head; BEGIN
    WHILE curr # NIL AND curr.id # id DO curr := curr.next END;
    RETURN curr
  END MemNearVarList;

(* ======================= Operations on IdList's ========================== *)

PROCEDURE CopyIdLinks(l: JunoAST.IdLink; VAR (*OUT*) last: JunoAST.IdLink):
  JunoAST.IdLink = 
(* Return a copy of "l", but set "last" to the last "JunoAST.IdLink" of the
   new list. If "l" is "NIL", then "last" is set to "NIL". *)
  VAR res: JunoAST.IdLink := NIL; BEGIN
    last := NIL;
    WHILE l # NIL DO
      VAR new := NEW(JunoAST.IdLink, id := l.id, index := l.index); BEGIN
        IF last = NIL
          THEN res := new
          ELSE last.next := new
        END;
        last := new
      END;
      l := l.next
    END;
    RETURN res;
  END CopyIdLinks;

PROCEDURE CopyIdList(l: JunoAST.IdList): JunoAST.IdList =
  VAR dummy: JunoAST.IdLink; BEGIN
    RETURN NEW(JunoAST.IdList, bp := l, size := l.size,
      head := CopyIdLinks(l.head, dummy))
  END CopyIdList;

PROCEDURE ConcatIdLists(l1, l2: JunoAST.IdList): JunoAST.IdList =
  VAR last, dummy: JunoAST.IdLink; head := CopyIdLinks(l1.head, last); BEGIN
    last.next := CopyIdLinks(l2.head, dummy);
    RETURN NEW(JunoAST.IdList, bp := l1, size := l1.size + l2.size,
      head := head);
  END ConcatIdLists;

(* ===================== Operations on NearVarList's ======================= *)

PROCEDURE NearVarListUnion(l1, l2: JunoAST.NearVarList): JunoAST.NearVarList =
  VAR h1 := l1.head; h2 := l2.head; BEGIN
    WHILE h1 # NIL DO
      h2 := NEW(JunoAST.NearVarLink, id := h1.id, index := h1.index,
        frozen := h1.frozen, hint := h1.hint, evar := h1.evar, next := h2);
      h1 := h1.next
    END;
    RETURN NEW(JunoAST.NearVarList, size := l1.size + l2.size, head := h2)
  END NearVarListUnion;

PROCEDURE CopyLinks(l: JunoAST.NearVarLink): JunoAST.NearVarLink =
(* Return a copy of the list "l". *)
  VAR res, last: JunoAST.NearVarLink := NIL; BEGIN
    WHILE l # NIL DO
      VAR new: JunoAST.NearVarLink; BEGIN
      	new := NEW(JunoAST.NearVarLink, id := l.id, evar := l.evar,
      	  frozen := l.frozen, hint := l.hint, index := l.index);
      	IF last = NIL
      	  THEN res := new
      	  ELSE last.next := new
      	END;
      	last := new
      END;
      l := l.next
    END;
    RETURN res
  END CopyLinks;

PROCEDURE NearVarListCopy(l: JunoAST.NearVarList): JunoAST.NearVarList =
  BEGIN
    RETURN NEW(JunoAST.NearVarList, size := l.size, head := CopyLinks(l.head))
  END NearVarListCopy;

PROCEDURE ExtractHints(vars: JunoAST.NearVarList): JunoAST.Formula =
  VAR
    res: JunoAST.Formula := NIL;
    h_in: JunoAST.NearVarLink := vars.head;
    eq: JunoAST.Equals;
  BEGIN
    <* ASSERT vars.size > 0 *>
    WHILE h_in # NIL DO
      IF h_in.hint # JunoAST.NilExpr THEN
        eq := NEW(JunoAST.Equals, bp := vars, near := NOT h_in.frozen,
           e1 := QIdFromNearVar(h_in), e2 := h_in.hint);
        IF res = NIL
          THEN res := eq
          ELSE res := NEW(JunoAST.And, bp := vars, f1 := eq, f2 := res)
        END
      END;
      h_in := h_in.next
    END;
    IF res = NIL THEN res := JunoAST.TrueVal END;
    RETURN res
  END ExtractHints;

PROCEDURE StripHints(vars: JunoAST.NearVarList): JunoAST.NearVarList =
(* IMPLEMENTATION: The resulting list is "vars" in reverse order. *)
  VAR
    res := NEW(JunoAST.NearVarList, bp := vars, size := vars.size);
    curr := vars.head;
  BEGIN
    WHILE curr # NIL DO
      res.head := NEW(JunoAST.NearVarLink, id := curr.id, index := curr.index,
        hint := JunoAST.NilExpr, next := res.head);
      curr := curr.next
    END;
    RETURN res
  END StripHints;

(* ============================= MapArgs =================================== *)

PROCEDURE MapArgs(expr: JunoAST.Expr; p: Mappee): JunoAST.Expr =
  BEGIN
    TYPECASE expr OF <* NOWARN *>
      JunoAST.Call (e) =>
        RETURN NEW(JunoAST.Call, bp := e, inouts := e.inouts,
          inout_parens := e.inout_parens, name := e.name,
          ins := NEW(JunoAST.ExprList, bp := e.ins, size := e.ins.size,
            head := MapExprList(e.ins.head, p)),
          normal_form := e.normal_form);
    | JunoAST.LitPred => RETURN expr
    | JunoAST.BIUPred (e) =>
        VAR res: JunoAST.BIUPred; BEGIN
          TYPECASE e OF <* NOWARN *>
            JunoAST.IsReal => res := NEW(JunoAST.IsReal)
          | JunoAST.IsText => res := NEW(JunoAST.IsText)
          | JunoAST.IsPair => res := NEW(JunoAST.IsPair)
          | JunoAST.IsInt  => res := NEW(JunoAST.IsInt)
          END;
          res.bp := e; res.e := p(e.e);
          RETURN res
        END
    | JunoAST.Relation (e) =>
        VAR res: JunoAST.Relation; BEGIN
          TYPECASE e OF <* NOWARN *>
            JunoAST.Equals (eq) => res := NEW(JunoAST.Equals, near := eq.near)
          | JunoAST.Differs	=> res := NEW(JunoAST.Differs)
          | JunoAST.Less	=> res := NEW(JunoAST.Less)
          | JunoAST.Greater	=> res := NEW(JunoAST.Greater)
          | JunoAST.AtMost	=> res := NEW(JunoAST.AtMost)
          | JunoAST.AtLeast	=> res := NEW(JunoAST.AtLeast)
          | JunoAST.Cong	=> res := NEW(JunoAST.Cong)
          | JunoAST.Para	=> res := NEW(JunoAST.Para)
          | JunoAST.Hor		=> res := NEW(JunoAST.Hor)
          | JunoAST.Ver		=> res := NEW(JunoAST.Ver)
          END;
          res.bp := e; res.e1 := p(e.e1); res.e2 := p(e.e2);
          RETURN res
        END
    | JunoAST.BIUFunc (e) =>
        VAR res: JunoAST.BIUFunc; BEGIN
          TYPECASE e OF <* NOWARN *>
            JunoAST.UMinus  => res := NEW(JunoAST.UMinus)
          | JunoAST.Floor   => res := NEW(JunoAST.Floor)
          | JunoAST.Ceiling => res := NEW(JunoAST.Ceiling)
          | JunoAST.Round   => res := NEW(JunoAST.Round)
          | JunoAST.Abs     => res := NEW(JunoAST.Abs)
          | JunoAST.Sin     => res := NEW(JunoAST.Sin)
          | JunoAST.Cos     => res := NEW(JunoAST.Cos)
          | JunoAST.Exp     => res := NEW(JunoAST.Exp)
          | JunoAST.Ln      => res := NEW(JunoAST.Ln)
          | JunoAST.Car     => res := NEW(JunoAST.Car)
          | JunoAST.Cdr     => res := NEW(JunoAST.Cdr)
          END;
          res.bp := e; res.e := p(e.e);
          RETURN res
        END
    | JunoAST.BIBFunc (e) =>
        VAR res: JunoAST.BIBFunc; BEGIN
          TYPECASE e OF <* NOWARN *>
            JunoAST.Plus   => res := NEW(JunoAST.Plus)
          | JunoAST.Minus  => res := NEW(JunoAST.Minus)
          | JunoAST.Concat => res := NEW(JunoAST.Concat)
          | JunoAST.Times  => res := NEW(JunoAST.Times)
          | JunoAST.Divide => res := NEW(JunoAST.Divide)
          | JunoAST.Div    => res := NEW(JunoAST.Div)
          | JunoAST.Mod    => res := NEW(JunoAST.Mod)
          | JunoAST.Pair   => res := NEW(JunoAST.Pair)
          | JunoAST.Rel    => res := NEW(JunoAST.Rel)
          | JunoAST.Max    => res := NEW(JunoAST.Max)
          | JunoAST.Min    => res := NEW(JunoAST.Min)
          | JunoAST.Atan   => res := NEW(JunoAST.Atan)
          END;
          res.bp := e; res.e1 := p(e.e1); res.e2 := p(e.e2);
          RETURN res
        END
    | JunoAST.List (e) =>
        RETURN NEW(JunoAST.List, bp := e, elts := NEW(JunoAST.ExprList,
          size := e.elts.size, head := MapExprList(e.elts.head, p)))
    END
  END MapArgs;

PROCEDURE MapExprList(el: JunoAST.ExprLink; p: Mappee): JunoAST.ExprLink =
(* Return the list of expressions obtained by mapping "p" over each element of
   "el". *)
  BEGIN
    IF el = NIL THEN
      RETURN NIL
    ELSE
      RETURN NEW(JunoAST.ExprLink, expr := p(el.expr),
        next := MapExprList(el.next, p))
    END
  END MapExprList;

(* ======================= Operations on JunoAST.Vars ====================== *)

PROCEDURE MemVars(qid: JunoAST.QId; READONLY vars: JunoAST.Vars): INTEGER =
  BEGIN
    FOR i := FIRST(vars) TO LAST(vars) DO
      <* ASSERT vars[i].index # 0 *>
      IF vars[i].index = qid.index THEN
        <* ASSERT vars[i].id = qid.id1 *>
        RETURN i
      END
    END;
    RETURN -1
  END MemVars;

(* ==================== Create New Special-Purpose AST's =================== *)

PROCEDURE NewNumber(x: JunoValue.Real): JunoAST.Expr =
  BEGIN
    IF x < 0.0
      THEN RETURN NEW(JunoAST.UMinus, e := NEW(JunoAST.Number, val := ABS(x)))
      ELSE RETURN NEW(JunoAST.Number, val := x)
    END
  END NewNumber;

PROCEDURE NewPoint(x, y: JunoValue.Real): JunoAST.Pair =
  BEGIN
    RETURN NEW(JunoAST.Pair,
      e1 := NewNumber(x),
      e2 := NewNumber(y))
  END NewPoint;

PROCEDURE NewASTFromValue(v: JunoValue.T): JunoAST.T =
  BEGIN
    IF JunoValue.IsList(v)
      THEN RETURN NewASTList(v)
      ELSE RETURN NewASTFromValue2(v)
    END
  END NewASTFromValue;

PROCEDURE NewASTFromValue2(v: JunoValue.T): JunoAST.T =
  BEGIN
    TYPECASE v OF <* NOWARN *>
      JunoValue.Null => RETURN JunoAST.NilVal
    | TEXT (t) => RETURN NEW(JunoAST.Text, val := t)
    | REF JunoValue.Real (r) => RETURN NEW(JunoAST.Number, val := r^)
    | REF JunoValue.Pair (p) => RETURN NEW(JunoAST.Pair,
        e1 := NewASTFromValue(p.car), e2 := NewASTFromValue(p.cdr))
    END
  END NewASTFromValue2;

PROCEDURE NewASTList(v: JunoValue.T): JunoAST.T =
(* Requires "v" to be a non-empty list value (i.e. "JunoValue.IsList(v)"). *)
  VAR
    res := NEW(JunoAST.List, elts := NEW(JunoAST.ExprList));
    curr, new: JunoAST.ExprLink := NIL;
  BEGIN
    <* ASSERT v # JunoValue.Nil *>
    WITH list = res.elts DO
      WHILE v # JunoValue.Nil DO
        TYPECASE v OF <* NOWARN *> REF JunoValue.Pair (p) =>
          new := NEW(JunoAST.ExprLink, expr := NewASTFromValue(p.car));
          IF curr = NIL
            THEN list.head := new
            ELSE curr.next := new
          END;
          curr := new;
          INC(list.size);
          v := p.cdr
        END
      END
    END;
    RETURN res
  END NewASTList;

PROCEDURE NewAssign(v: JunoAST.QId; e: JunoAST.Expr): JunoAST.Assign =
  BEGIN
    RETURN NEW(JunoAST.Assign, vars := NewQIdList(v), exprs := NewExprList(e))
  END NewAssign;

(* ============================ Miscellaneous ============================== *)

PROCEDURE Ungroup(ast: JunoAST.T): JunoAST.T =
  BEGIN
    LOOP
      TYPECASE ast OF
        JunoAST.GroupedCmd (c) =>  ast := c.body
      | JunoAST.GroupedExpr (e) => ast := e.expr
      ELSE RETURN ast
      END
    END
  END Ungroup;

PROCEDURE EqualQIds(qid1, qid2: JunoAST.QId): BOOLEAN =
  BEGIN RETURN qid1.id0 = qid2.id0 AND qid1.id1 = qid2.id1 END EqualQIds;

PROCEDURE FirstProcCall(cmd: JunoAST.Cmd; qid: JunoAST.QId): JunoAST.ProcCall =
  VAR res: JunoAST.ProcCall := NIL; BEGIN
    TYPECASE cmd OF
      NULL => (*SKIP*)
    | JunoAST.ProcCall (pc) =>
        IF EqualQIds(pc.name, qid) THEN res := pc END
    ELSE
      VAR it := cmd.iterator(); c: JunoAST.T; BEGIN
        WHILE res = NIL AND it.next((*OUT*) c) DO
          TYPECASE c OF JunoAST.Cmd (cmd0) =>
            res := FirstProcCall(cmd0, qid)
          ELSE (*SKIP*)
          END
        END
      END
    END;
    RETURN res
  END FirstProcCall;

PROCEDURE AlwaysDefined(e: JunoAST.Expr): BOOLEAN =
(* Note: the definition of this procedure is very important to the correct
   functioning of the compiler and assembler. *)
  BEGIN
    TYPECASE e OF
      JunoAST.LitValue, JunoAST.QId => RETURN TRUE
    | JunoAST.GroupedExpr (g) => RETURN AlwaysDefined(g.expr)
    | JunoAST.List (l) => RETURN ExprsDefined(l.elts)
    | JunoAST.Pair (p) =>
        RETURN AlwaysDefined(p.e1) AND AlwaysDefined(p.e2)
    | JunoAST.UMinus (m) => RETURN ISTYPE(m.e,JunoAST.Number)
    | JunoAST.Call (c) =>
        CASE c.name.type OF <* NOWARN *>
          JunoAST.IdType.ExtProc =>
            (* An external procedure call is always defined so long as its
               arguments are defined; this is because the EXTCALL bytecode
               immediately signals a run-time error if the procedure failed on
               its arguments. *)
            RETURN ExprsDefined(c.ins)
        | JunoAST.IdType.Proc =>
            (* A user-defined procedure call (except for the special "APPLY"
               and "CLOSE" built-in user-defined procedures) is always defined
               so long as its arguments are defined. *)
            RETURN NOT BuiltInSlots.IsApplySlot(c.name.index)
               AND NOT BuiltInSlots.IsCloseSlot(c.name.index)
               AND ExprsDefined(c.ins)
        | JunoAST.IdType.Func, JunoAST.IdType.None =>
            (* User-defined function calls can always be undefined, regardless
               of their args. *)
            RETURN FALSE
        END
    ELSE (* SKIP *)
    END;
    RETURN FALSE
  END AlwaysDefined;

PROCEDURE ExprsDefined(el: JunoAST.ExprList): BOOLEAN =
(* Return TRUE iff "AlwaysDefined(e)" for every "e" in the list "el". *)
  VAR curr := el.head; BEGIN
    WHILE curr # NIL DO
      IF NOT AlwaysDefined(curr.expr) THEN RETURN FALSE END;
      curr := curr.next
    END;
    RETURN TRUE
  END ExprsDefined;

BEGIN
END JunoASTUtils.
