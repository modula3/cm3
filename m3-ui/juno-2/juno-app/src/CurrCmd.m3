(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 26 13:38:54 PST 1997 by heydon                   *)
(*      modified on Tue May 30 12:01:27 PST 1995 by gnelson                  *)
(*      modified on Sat Aug 22 22:40:59 PDT 1992 by myers                    *)
<*PRAGMA LL*>

MODULE CurrCmd;

IMPORT JunoBuild, JunoPt;
IMPORT JunoAST, JunoASTUtils, JunoChkBNF, JunoCompileErr, JunoScope;
IMPORT JunoRT, JunoValue AS Value, JunoRTError;
IMPORT Atom, AtomRefTbl, Fmt;

CONST InitCacheSize = 30;

REVEAL
  T = TPublic BRANDED "CurrCmd.T" OBJECT
    ast: JunoAST.Cmd;
    scp: JunoScope.T;
    pointCache, oldPointCache: AtomRefTbl.Default;
    othersCache, oldOthersCache: AtomRefTbl.Default;
    changedVal: REF ARRAY OF BOOLEAN;
  END;

(* A "CurrCmd.T" contains "ast", an AST representing a current command. This
   field should never be "NIL".

   "pointCache" maps names of variables representing points to their
   current positions, represented as "Value.T"'s. The "othersCache" is
   similar, but maps to all values that are not pairs of real numbers.
   The "old..." caches are used to tell when one of the cached values has
   changed, to avoid unnecessary compilations of the current command.

   The "changedVal" array is set by the "FillCache" procedure, and used by the
   "UpdateNearVars" procedure. If there are "n" variables in the current
   command, then "changedVal[i]" for "0 <= i < n" is true iff the current
   value of the "i"th variable is different from the cached value, or if there
   is no value cached for the "i"th variable. *)

VAR R2DotPlus := JunoASTUtils.QIdFromTexts("R2", "Plus");

(* ----------------- Creation / Replacement / Accessors -------------------- *)

PROCEDURE New(ast: JunoAST.Cmd; scp: JunoScope.T): T =
  VAR res: T; BEGIN
    <* ASSERT ast # NIL *>
    res := NEW(T, ast := ast,
      pointCache := NEW(AtomRefTbl.Default).init(sizeHint := InitCacheSize),
      oldPointCache := NEW(AtomRefTbl.Default).init(sizeHint := InitCacheSize),
      othersCache := NEW(AtomRefTbl.Default).init(sizeHint := InitCacheSize),
      oldOthersCache := NEW(AtomRefTbl.Default).init(sizeHint:=InitCacheSize),
      changedVal := NEW(REF ARRAY OF BOOLEAN, InitCacheSize));
    IF scp = NIL THEN scp := JunoScope.New(NIL) END;
    res.scp := scp;
    RETURN res
  END New;

PROCEDURE GetAST(cc: T): JunoAST.Cmd =
  BEGIN RETURN cc.ast END GetAST;

PROCEDURE ChangeAST(cc: T; ast: JunoAST.Cmd) =
  BEGIN
    <* ASSERT ast # NIL *>
    cc.ast := ast;
    cc.codeValid := FALSE;
    ClearCaches(cc)
  END ChangeAST;

PROCEDURE GetVariables(ast: JunoAST.Cmd): JunoAST.NearVarList =
  BEGIN
    TYPECASE ast OF JunoAST.Proj (p) =>
      RETURN p.vars
    ELSE RETURN JunoAST.EmptyNVList
    END
  END GetVariables;

PROCEDURE GetVariable(ast: JunoAST.T; name: JunoAST.Id): JunoAST.NearVarLink =
  VAR vars := GetVariables(ast); BEGIN
    VAR v: JunoAST.NearVarLink := vars.head; BEGIN
      WHILE v # NIL DO
        IF v.id = name THEN RETURN v END;
        v := v.next
      END
    END;
    RETURN NIL
  END GetVariable;

PROCEDURE GetConstraint(ast: JunoAST.Cmd): JunoAST.Formula =
  BEGIN
    TYPECASE ast OF JunoAST.Proj (p) =>
      TYPECASE p.body OF JunoAST.Guard (g) =>
        RETURN g.grd
      ELSE (* SKIP *)
      END
    ELSE (* SKIP *)
    END;
    RETURN JunoAST.TrueVal
  END GetConstraint;

PROCEDURE GetCmd(ast: JunoAST.Cmd): JunoAST.Cmd =
  BEGIN
    TYPECASE ast OF JunoAST.Proj (p) =>
      TYPECASE p.body OF JunoAST.Guard (g) =>
        RETURN g.body
      ELSE RETURN p.body
      END
    ELSE RETURN ast
    END
  END GetCmd;

(* --------------------- Scope-Related Operations -------------------------- *)

PROCEDURE GetScope(cc: T): JunoScope.T =
  BEGIN RETURN cc.scp END GetScope;

PROCEDURE SetScope(cc: T; scp: JunoScope.T) =
  BEGIN cc.scp := scp END SetScope;

PROCEDURE NewDeclName(cc: T; prefix: TEXT; tryEmptySuffix := FALSE): TEXT =
  VAR res := prefix; i := 0; BEGIN
    WHILE
      (i = 0 AND NOT tryEmptySuffix) OR
      JunoScope.Lookup(cc.scp, Atom.FromText(res), localOnly := TRUE) # NIL
    DO
      res := prefix & Fmt.Int(i);
      INC(i)
    END;
    RETURN res
  END NewDeclName;

(* --------------------------- Modification -------------------------------- *)

PROCEDURE Skipify(ast: JunoAST.Cmd): JunoAST.Cmd =
  BEGIN
    TYPECASE ast OF
      JunoAST.Proj (p) =>
        RETURN NEW(JunoAST.Proj, bp := p,
          vars := p.vars, body := Skipify(p.body))
    | JunoAST.Guard (g) =>
        RETURN NEW(JunoAST.Guard, bp := g,
          grd := g.grd, body := Skipify(g.body))
    ELSE RETURN JunoAST.SkipVal
    END
  END Skipify;

PROCEDURE AddVariable(
    cc: T;
    v: JunoAST.Id;
    loc: JunoPt.T;
    near: JunoAST.Expr;
    frozen := FALSE) =
  VAR
    new := NEW(JunoAST.NearVarLink, id := v,
      hint := near, frozen := frozen);
  BEGIN
    <* ASSERT near # NIL *>
    TYPECASE cc.ast OF <* NOWARN *>
      JunoAST.Proj (p) =>
        VAR v2 := p.vars.head; BEGIN
          WHILE v2.next # NIL DO
            <* ASSERT v2.id # v *>
            v2 := v2.next
          END;
          v2.next := new;
          p.vars.size := p.vars.size + 1
        END
    | JunoAST.Cmd (c) =>
        cc.ast := NEW(JunoAST.Proj, bp := JunoAST.End,
          vars := NEW(JunoAST.NearVarList, bp := JunoAST.End,
            size := 1, head := new),
          body := c)
    END;
    EVAL cc.pointCache.put(v, Value.NewPoint(loc.x, loc.y));
    cc.codeValid := FALSE
  END AddVariable;

PROCEDURE AddConstraint(cc: T; con: JunoAST.Constraint) =
  BEGIN
    TYPECASE cc.ast OF <* NOWARN *>
      JunoAST.Proj (p) => AddConstraint1(p.body, con)
    | JunoAST.Cmd => AddConstraint1(cc.ast, con)
    END;
    cc.codeValid := FALSE
  END AddConstraint;

PROCEDURE AddConstraint1(
    VAR (*INOUT*) ast: JunoAST.Cmd;
    con: JunoAST.Constraint) =
  BEGIN
    TYPECASE ast OF <* NOWARN *>
      JunoAST.Guard (g) => AddConstraint2(g.grd, con)
    | JunoAST.Cmd (c) =>
        ast := NEW(JunoAST.Guard, bp := JunoAST.End, grd := con, body := c)
    END
  END AddConstraint1;

PROCEDURE AddConstraint2(
    VAR (*INOUT*) f: JunoAST.Formula;
    con: JunoAST.Constraint) =
  BEGIN
    TYPECASE f OF
      JunoAST.Or (or) => AddConstraint2(or.f2, con)
    ELSE f := NEW(JunoAST.And, bp := JunoAST.End, f1 := f, f2 := con)
    END
  END AddConstraint2;

PROCEDURE AddCommand(cc: T; cmd: JunoAST.Cmd) =
  BEGIN
    TYPECASE cc.ast OF <* NOWARN *>
      JunoAST.Proj (p) => AddCommand1(p.body, cmd)
    | JunoAST.Cmd => AddCommand2(cc.ast, cmd)
    END;
    cc.codeValid := FALSE
  END AddCommand;

PROCEDURE AddCommand1(VAR (*INOUT*) c1: JunoAST.Cmd; c2: JunoAST.Cmd) =
  BEGIN
    TYPECASE c1 OF <* NOWARN *>
      JunoAST.Guard (g) => AddCommand2(g.body, c2)
    | JunoAST.Cmd => AddCommand2(c1, c2)
    END
  END AddCommand1;

PROCEDURE AddCommand2(VAR (*INOUT*) c1: JunoAST.Cmd; c2: JunoAST.Cmd) =
  BEGIN
    TYPECASE c1 OF <* NOWARN *>
      JunoAST.Skip => c1 := c2
    | JunoAST.Cmd =>
        c1 := NEW(JunoAST.Seq, bp := JunoAST.End, c1 := c1, c2 := c2)
    END
  END AddCommand2;

PROCEDURE RemCommand(cc: T): BOOLEAN =
  VAR changed: BOOLEAN; BEGIN
    TYPECASE cc.ast OF <* NOWARN *>
      JunoAST.Skip => changed := FALSE
    | JunoAST.Proj (p) => changed := RemCommand1(p.body)
    | JunoAST.Cmd => changed := RemCommand2(cc.ast)
    END;
    cc.codeValid := cc.codeValid AND NOT changed;
    RETURN changed
  END RemCommand;

PROCEDURE RemCommand1(VAR (*INOUT*) cmd: JunoAST.Cmd): BOOLEAN =
  BEGIN
    TYPECASE cmd OF <* NOWARN *>
      JunoAST.Guard (g) => RETURN RemCommand2(g.body)
    | JunoAST.Cmd => RETURN RemCommand2(cmd)
    END
  END RemCommand1;

PROCEDURE RemCommand2(VAR (*INOUT*) ast: JunoAST.Cmd): BOOLEAN =
  VAR res: BOOLEAN; BEGIN
    TYPECASE ast OF
      JunoAST.Skip => res := FALSE;
    | JunoAST.Seq (seq) =>
        res := RemCommand2(seq.c2);
        IF seq.c2 = JunoAST.SkipVal THEN ast := seq.c1; res := TRUE END
    | JunoAST.Cmd => ast := JunoAST.SkipVal; res := TRUE
    END;
    RETURN res
  END RemCommand2;

PROCEDURE DoRel(cc: T; c, a, b: JunoAST.Id) =
  VAR v := GetVariable(cc.ast, c); BEGIN
    VAR aa, bb: JunoAST.Id; BEGIN
      IF IsRelHint(v.hint, aa, bb) AND a = aa AND b = bb THEN
        v.hint := PointValue(cc, c);
        cc.codeValid := FALSE;
        RETURN
      END
    END;
    (* Make the hint of the form "c ~= (x, y) REL (a, b)" *)
    VAR ax, ay, bx, by: Real; BEGIN
      EVAL PointLocation(cc, a, ax, ay);
      EVAL PointLocation(cc, b, bx, by);
      v.hint := MkRelHint(cc, c, a, ax, ay, b, bx, by, v.hint);
      cc.codeValid := FALSE
    END
  END DoRel;

PROCEDURE IsRelHint(hint: JunoAST.Expr;
  VAR (*OUT*) a, b: JunoAST.Id): BOOLEAN =
(* If "hint" is of the form (num, num) REL (p,q), for "Id"'s "p" and "q",
   set "a,b := p,q" and return "TRUE"; else return "FALSE". *)
  BEGIN
    IF hint = NIL THEN RETURN FALSE END;
    TYPECASE hint OF JunoAST.Rel (rel) =>
      TYPECASE rel.e2 OF JunoAST.Pair (p) =>
        TYPECASE p.e1 OF JunoAST.QId (p1) =>
	  TYPECASE p.e2 OF JunoAST.QId (p2) =>
            IF p1.id0 = JunoAST.NilId AND
               p2.id0 = JunoAST.NilId AND
               IsNumericPoint(rel.e1) 
            THEN
              a := p1.id1;
              b := p2.id1;
              RETURN TRUE
            END
	  ELSE (* SKIP *)
	  END
        ELSE (* SKIP *)
        END
      ELSE (* SKIP *)
      END
    ELSE (* SKIP *)
    END;
    RETURN FALSE
  END IsRelHint;

PROCEDURE IsRel1Hint(hint: JunoAST.Expr; VAR (*OUT*) a: JunoAST.Id): BOOLEAN =
(* If "hint" is of the form "R2.Plus(p, (num, num))", for "Id" "p",
   set "a := p" and return "TRUE"; else return "FALSE". *)
  BEGIN
    TYPECASE hint OF 
      NULL => (*SKIP*)
    | JunoAST.Call (call) =>
      IF call.ins.size = 2 THEN
        TYPECASE call.ins.head.expr OF 
          JunoAST.QId (arg0) =>
            IF arg0.id0 = JunoAST.NilId AND
               JunoASTUtils.EqualQIds(call.name, R2DotPlus) AND
               IsNumericPoint(call.ins.head.next.expr)
            THEN
              a := arg0.id1;
              RETURN TRUE
            END
        ELSE (* SKIP *)
        END
      END
    ELSE (* SKIP *)
    END;
    RETURN FALSE
  END IsRel1Hint;

PROCEDURE DoRel1(cc: T; c, a: JunoAST.Id) =
  VAR v := GetVariable(cc.ast, c); BEGIN
    VAR aa: JunoAST.Id; BEGIN
      IF IsRel1Hint(v.hint, aa) AND a = aa THEN
        v.hint := PointValue(cc, c);
        cc.codeValid := FALSE;
        RETURN
      END
    END;
    (* Make the hint of the form "c ~= R2.Plus(a, (x, y))" *)
    VAR ax, ay: Real; BEGIN
      EVAL PointLocation(cc, a, ax, ay);
      v.hint := MkRel1Hint(cc, c, a, ax, ay, v.hint);
      cc.codeValid := FALSE
    END
  END DoRel1;

PROCEDURE MkRelHint(cc: T; c: JunoAST.Id; 
  a: JunoAST.Id; ax, ay: Real;
  b: JunoAST.Id; bx, by: Real;
  else: JunoAST.Expr): JunoAST.Expr =
(* Return "c"'s position relative to "a" and "b", or return "else" if
   this is not possible. *)
  VAR x, y, cx, cy: Real; BEGIN
    IF NOT PointLocation(cc, c, cx, cy) THEN RETURN else END;
    IF NOT JunoPt.RelVal(cx, cy, ax, ay, bx, by, x, y) THEN RETURN else END;
    RETURN
      NEW(JunoAST.Rel, bp := JunoAST.End,
	e1 := JunoASTUtils.NewPoint(x, y),
	e2 := NEW(JunoAST.Pair, bp := JunoAST.End,
	  e1 := JunoASTUtils.QIdFromIds(JunoAST.NilId, a),
	  e2 := JunoASTUtils.QIdFromIds(JunoAST.NilId, b)))  
  END MkRelHint;

PROCEDURE MkRel1Hint(cc: T; c: JunoAST.Id; 
  a: JunoAST.Id; ax, ay: Real; else: JunoAST.Expr): JunoAST.Expr =
(* Return "c"'s position relative to "a". *)
  VAR x, y, cx, cy: Real; BEGIN
    IF NOT PointLocation(cc, c, cx, cy) THEN RETURN else END;
    x := cx - ax;
    y := cy - ay;
    RETURN
      NEW(JunoAST.Call, bp := JunoAST.End,
	name := R2DotPlus,
	inouts := JunoAST.EmptyExprList,
	ins := NEW(JunoAST.ExprList, bp := JunoAST.End, size := 2, head :=
	  NEW(JunoAST.ExprLink,
	    expr := JunoASTUtils.QIdFromId(a),
	    next := NEW(JunoAST.ExprLink,
              expr := JunoASTUtils.NewPoint(x, y)))))
  END MkRel1Hint;

(* ------------------------ Operations on Points --------------------------- *)

PROCEDURE PointLocation(cc: T; a: JunoAST.Id; VAR (*OUT*) h, v: Real):
    BOOLEAN =
(* Set "h" and "v" to the coordinates of the point named "a" in
   "cc.pointCache" and return TRUE; return FALSE if there is no
   such point in the cache. *)
  VAR vl: Value.T; BEGIN
    IF cc.pointCache.get(a, vl) THEN
      TYPECASE vl OF <* NOWARN *>
        REF Value.Pair (p) =>
          h := NARROW(p.car, REF Real)^;
          v := NARROW(p.cdr, REF Real)^;
          RETURN TRUE
      END
    ELSE
      RETURN FALSE
    END
  END PointLocation;

PROCEDURE PointValue(cc: T; a: JunoAST.Id): JunoAST.Pair =
(* Requires that the point named "a" is in "cc.pointCache". Returns an AST
   corresponding to the point's value in the cache. *)
  VAR x, y: Real; inCache := PointLocation(cc, a, (*OUT*) x, (*OUT*) y); BEGIN
    <* ASSERT inCache *>
    RETURN JunoASTUtils.NewPoint(x, y)
  END PointValue;

PROCEDURE FreezePoint(cc: T; a: JunoAST.Id) =
  VAR v := GetVariable(cc.ast, a); BEGIN
    <* ASSERT v # NIL *>
    v.frozen := NOT v.frozen;
    cc.codeValid := FALSE
  END FreezePoint;

PROCEDURE IsFrozen(cc: T; a: JunoAST.Id): BOOLEAN =
  VAR v := GetVariable(cc.ast, a); BEGIN
    IF v = NIL
      THEN RETURN FALSE
      ELSE RETURN v.frozen
    END
  END IsFrozen;

PROCEDURE DiffNumericPair(p: JunoAST.Pair; x, y: Real): BOOLEAN =
(* Return TRUE iff "p" is a pair of constant numbers, but differs from the
   pair "(x, y)". *)
  VAR px, py: Real; BEGIN
    RETURN IsNumber(p.e1, px) AND IsNumber(p.e2, py) AND (x # px OR y # py)
  END DiffNumericPair;

PROCEDURE IsNumericPoint(e: JunoAST.Expr): BOOLEAN =
(* Return TRUE iff "e" is a pair of real numbers. *)
  BEGIN
    TYPECASE e OF
      NULL => (*SKIP*)
    | JunoAST.Pair (p) =>
        VAR dummy: Real; BEGIN
          RETURN IsNumber(p.e1, dummy) AND IsNumber(p.e2, dummy)
        END
    ELSE (*SKIP*)
    END;
    RETURN FALSE
  END IsNumericPoint;

PROCEDURE IsNumber(e: JunoAST.Expr; VAR (*OUT*) v: Real): BOOLEAN =
(* Return TRUE iff "e" a numeric constant of the form "<number>" or
   "- <number>", and if so, set "v" to its value. *)
  BEGIN
    TYPECASE e OF
      JunoAST.Number (n) =>
        v := n.val; RETURN TRUE
    | JunoAST.UMinus (u) =>
        VAR res := IsNumber(u.e, v); BEGIN
          IF res THEN v := -v END;
          RETURN res
        END
    ELSE (* SKIP *)
    END;
    RETURN FALSE
  END IsNumber;

PROCEDURE MovePoint(cc: T; a: JunoAST.Id; x, y: Real) =
  VAR v: JunoAST.NearVarLink := GetVariable(cc.ast, a); BEGIN
    <* ASSERT v # NIL AND v.hint # JunoAST.NilExpr *>
    VAR newV := Value.NewPoint(x, y); oldV: Value.T; BEGIN
      (* only do work if the point does not already have the given value *)
      IF NOT (cc.pointCache.get(v.id, oldV) AND Value.Equal(newV, oldV)) THEN
        EVAL cc.pointCache.put(v.id, newV);
        EVAL MovePoint1(cc, v, x, y);
        cc.codeValid := FALSE
      END
    END
  END MovePoint;

PROCEDURE MovePoint1(
  cc: T; v: JunoAST.NearVarLink; x, y: Real; changed := TRUE): BOOLEAN =
(* Update "v.hint" to have the value "(x, y)", preserving the current
   structure of the hint. For the current hint to be changed, it must be
   either a simple pair, a literal value, an expression of the form
   "(x,y) REL (a, b)", where "x" and "y" are numbers, or an expression of the
   form "R2.Plus(a, (x,y))". In the latter two cases, the values for the
   points "a" and "b" are read from "cc.pointCache", and the pair "(x,y)" is
   updated. If none of these cases applies, the hint is unchanged. Returns
   TRUE iff the hint "v.hint" was changed.

   If "changed" is "FALSE" and the hint is absolute, then the hint is not
   changed and "FALSE" is returned. *)
  VAR res := FALSE; aa, bb: JunoAST.Id; BEGIN
    <* ASSERT v.hint # NIL AND v.hint # JunoAST.NilExpr *>
    TYPECASE v.hint OF JunoAST.Pair (p) =>
      (* The hint is of the form "(x, y)" *)
      IF changed AND DiffNumericPair(p, x, y) THEN
        res := TRUE;
        p.e1 := JunoASTUtils.NewNumber(x);
        p.e2 := JunoASTUtils.NewNumber(y)
      END
    ELSE
      IF IsLiteral(v.hint) THEN
        (* update hint if it is a literal of some sort *)
        res := TRUE;
        v.hint := JunoASTUtils.NewPoint(x, y)
      ELSIF IsRelHint(v.hint, aa, bb) THEN
        (* The hint is of the form "(num, num) REL (aa, bb)" *)
        VAR ax, ay, bx, by, x2, y2: Real; BEGIN
          IF PointLocation(cc, aa, ax, ay) AND
      	     PointLocation(cc, bb, bx, by) AND
      	     JunoPt.RelVal(x, y, ax, ay, bx, by, x2, y2) 
      	  THEN
            VAR r: JunoAST.Rel := v.hint; p1: JunoAST.Pair := r.e1; BEGIN
              IF DiffNumericPair(p1, x2, y2) THEN
                res := TRUE;
                r.e1 := JunoASTUtils.NewPoint(x2, y2)
              END
            END
          END
        END
      ELSIF IsRel1Hint(v.hint, aa) THEN
        (* The hint is of the form "R2.Plus(aa, (num, num))" *)
        VAR ax, ay, x2, y2: Real; BEGIN
          IF PointLocation(cc, aa, ax, ay) THEN
            x2 := x - ax; y2 := y - ay;
            VAR
              call: JunoAST.Call := v.hint;
              p1: JunoAST.Pair := call.ins.head.next.expr;
            BEGIN
              IF DiffNumericPair(p1, x2, y2) THEN
                res := TRUE;
                call.ins.head.next.expr := JunoASTUtils.NewPoint(x2, y2)
              END
            END
          END
        END
      END
    END;
    RETURN res
  END MovePoint1;

PROCEDURE IsLiteral(expr: JunoAST.Expr): BOOLEAN =
(* Return TRUE iff "expr" is a literal value, namely, a (possibly negated)
   number, a text, the value Juno "NIL", a list of literals, or a
   parenthesized literal. *)
  BEGIN
    IF expr = JunoAST.NilExpr THEN RETURN TRUE END;
    TYPECASE expr OF
      JunoAST.Number, JunoAST.Text, JunoAST.Nil => RETURN TRUE
    | JunoAST.UMinus (minus) => RETURN IsLiteral(minus.e)
    | JunoAST.List (l) =>
        VAR res := TRUE; curr := l.elts.head; BEGIN
          WHILE curr # NIL AND res DO
            res := res AND IsLiteral(curr.expr);
            curr := curr.next
          END;
          RETURN res
        END
    | JunoAST.GroupedExpr (g) => RETURN IsLiteral(g.expr)
    ELSE RETURN FALSE
    END
  END IsLiteral;

PROCEDURE ForAllPoints(cc: T; p: PointProc) =
  VAR it := cc.pointCache.iterate(); a: Atom.T; v: Value.T; BEGIN
    WHILE it.next(a, v) DO
      VAR
        pair := NARROW(v, REF Value.Pair);
        x := NARROW(pair.car, REF Real);
        y := NARROW(pair.cdr, REF Real);
      BEGIN
        p(a, JunoPt.T{x^, y^})
      END
    END;
  END ForAllPoints;

(* ------------------------ Folding Operations ----------------------------- *)

PROCEDURE WrapProcHeader(
    name: JunoAST.Id;
    ins: JunoAST.IdList;
    body: JunoAST.Cmd)
  : JunoAST.ProcDecl =
(* Return a procedure declaration named "name" with IN parameters "ins" and
   body "body". If "body" is not total, it is first bracketed by "IF...FI". *)
  BEGIN
    TRY JunoChkBNF.TotalCmd(body) EXCEPT JunoCompileErr.Error =>
      body := NEW(JunoAST.If, body := body, bp := JunoAST.End)
    END;
    RETURN NEW(JunoAST.ProcDecl, bp := JunoAST.End,
      header := NEW(JunoAST.ProcHeader, name := name, ins := ins,
        outs := JunoAST.EmptyIdList, inouts := JunoAST.EmptyIdList),
      body := body)
  END WrapProcHeader;

PROCEDURE FoldNoArgs(cc: T; name: JunoAST.Id): JunoAST.ProcDecl =
  BEGIN
    RETURN WrapProcHeader(name, JunoAST.EmptyIdList, cc.ast)
  END FoldNoArgs;

PROCEDURE GetFoldArgs(cc: T): JunoAST.IdList =
  VAR
    res := NEW(JunoAST.IdList, bp := JunoAST.End);
    vars := JunoASTUtils.NearVarListCopy(GetVariables(cc.ast));
    nv := vars.head;
    last: JunoAST.IdLink := NIL;
  BEGIN
    WHILE nv # NIL DO
      IF nv.hint = JunoAST.NilExpr OR IsNumericPoint(nv.hint) THEN
        (* add to end of "params" list *)
        VAR new := NEW(JunoAST.IdLink, id := nv.id); BEGIN
          IF last = NIL
            THEN res.head := new
            ELSE last.next := new
          END;
          last := new
        END;
        INC(res.size)
      END;
      nv := nv.next
    END;
    RETURN res;
  END GetFoldArgs;

PROCEDURE FoldPred(
    cc: T;
    name: JunoAST.Id;
    params: JunoAST.IdList;
    locals: JunoAST.NearVarList): JunoAST.PredDecl =
(* Fold the current command "cc" into a predicate declaration, and return it.
   "Name" is the predicate's name, "Params" are its arguments, and "locals"
   are its (existentially quantified) local variables. The body of the
   predicate is the constraint section of the current command "cc". *)
  VAR body := GetConstraint(cc.ast); BEGIN
    IF locals.size > 0 THEN
      body := NEW(JunoAST.Exists, bp := JunoAST.End, vars := locals, f := body)
    END;
    RETURN NEW(JunoAST.PredDecl, bp := JunoAST.End,
      header := NEW(JunoAST.PredHeader, name := name, ins := params),
      body := body)
  END FoldPred;

PROCEDURE FoldProc(
    cc: T;
    name: JunoAST.Id;
    params: JunoAST.IdList;
    locals: JunoAST.NearVarList): JunoAST.ProcDecl =
(* Fold the current command "cc" into a procedure declaration, and return it.
   "Name" is the procedure's name, "params" are its (IN) parameters, and
   "locals" are its (projected) local variables. The procedure's body is
   formed from the constraint section and command section of the current
   command "cc". *)
  VAR grd := GetConstraint(cc.ast); body := GetCmd(cc.ast); BEGIN
    IF grd # JunoAST.TrueVal THEN
      body := NEW(JunoAST.Guard, bp := JunoAST.End,
        grd := grd, body := body)
    END;
    IF locals.size > 0 THEN
      body := NEW(JunoAST.Proj, bp := JunoAST.End,
        vars := locals, body := body)
    END;
    RETURN WrapProcHeader(name, params, body)
  END FoldProc;

TYPE RelToArray = ARRAY [0..1] OF RECORD id: JunoAST.Id; x,y: Real END;

(* If "rt: RelToArray", then "rt" holds the names and (point) values of up to
   two parameters relative to which the other variable's hints will be
   computed. *)

PROCEDURE SetUpRelTo(
    cc: T; 
    args: JunoAST.IdList; 
    VAR (*OUT*) rt: RelToArray;
    VAR (*OUT*) i: INTEGER) =
(* Find up to "NUMBER(rt)" elements of "args" whose values are points, store
   their names and values in "rt", and set "i" to the number found. *)
  VAR l := args.head; BEGIN
    i := 0;
    WHILE i < NUMBER(rt) AND l # NIL DO
      IF PointLocation(cc, l.id, rt[i].x, rt[i].y) THEN
        rt[i].id := l.id;
        INC(i)
      END;
      l := l.next
    END
  END SetUpRelTo;

PROCEDURE AugmentRelTo(
  cc: T;
  nv: JunoAST.NearVarLink;
  ignore: JunoAST.IdList;
  VAR (*INOUT*) rt: RelToArray;
  VAR (*INOUT*) i: INTEGER) =
(* If "i=1" and there is some point in "nv" not named in the list "ignore"
   whose hint is "REL1" to "rt[0]", add that point to "rt". *)
  BEGIN
    WHILE i = 1 AND nv # NIL DO
      IF NOT JunoASTUtils.MemIdList(nv.id, ignore) THEN
      	VAR aa: JunoAST.Id; BEGIN
      	  IF IsRel1Hint(nv.hint, aa) AND aa = rt[0].id THEN
      	    rt[i].id := nv.id;
      	    EVAL PointLocation(cc, nv.id, rt[i].x, rt[i].y);
      	    INC(i)
      	  END
        END
      END;
      nv := nv.next
    END
  END AugmentRelTo;

PROCEDURE CheckFoldArgs(ids: JunoAST.IdList; locals: JunoAST.NearVarList)
    RAISES {BadFoldArg} =
(* Check that each variable in "ids" occurs in "locals". Raise "BadFoldArg"
   with the name of the variable if there is one that doesn't appear in
   "locals". This procedure is implemented by brute-force; it requires time
   on the order of "ids.size * locals.size". *)
  VAR curr := ids.head; BEGIN
    WHILE curr # NIL DO
      IF JunoASTUtils.MemNearVarList(curr.id, locals) = NIL THEN
        RAISE BadFoldArg(curr.id)
      END;
      curr := curr.next
    END
  END CheckFoldArgs;
    
PROCEDURE MkLocals(cc: T; args: JunoAST.IdList;
  args2: JunoAST.IdList := NIL): JunoAST.NearVarList
  RAISES {BadFoldArg} =
(* Return the list of local variables for the procedure or predicate resulting
   from folding the current command "cc" with arguments "args". The variables
   in the resulting list will have hints relative to the "args" if possible.
   Any variables named in "args2" will not be incorporated into the result
   either, but these variables are not used as a basis for relativizing the
   hints of the result variables. *)
  VAR
    rt: RelToArray;
    rtCount: INTEGER;
    res := NEW(JunoAST.NearVarList, bp := JunoAST.End);
    locals := JunoASTUtils.NearVarListCopy(GetVariables(cc.ast));
    nv: JunoAST.NearVarLink := locals.head;
    last: JunoAST.NearVarLink := NIL;
  BEGIN
    CheckFoldArgs(args, locals);
    IF args2 = NIL
      THEN args2 := JunoAST.EmptyIdList
      ELSE CheckFoldArgs(args2, locals)
    END;
    SetUpRelTo(cc, args, (*OUT*) rt, (*OUT*) rtCount);
    AugmentRelTo(cc, nv, args2, (*INOUT*) rt, (*INOUT*) rtCount);
    WHILE nv # NIL DO
      IF NOT JunoASTUtils.MemIdList(nv.id, args) AND
         NOT JunoASTUtils.MemIdList(nv.id, args2) THEN
        IF IsNumericPoint(nv.hint) THEN
          (* make nv's hint relative to rt *)
          CASE rtCount OF <* NOWARN *>
            0 => (* SKIP *)
          | 1 => nv.hint :=
              MkRel1Hint(cc, nv.id, rt[0].id, rt[0].x, rt[0].y, nv.hint)
          | 2 => nv.hint := 
              MkRelHint(cc, nv.id, rt[0].id, rt[0].x, 
                rt[0].y, rt[1].id, rt[1].x, rt[1].y, nv.hint)
          END
        END;
        (* add nv to the end of res *)
        IF last = NIL
          THEN res.head := nv
          ELSE last.next := nv
        END;
        last := nv;
        INC(res.size)
      END;
      nv := nv.next
    END;
    IF last # NIL THEN last.next := NIL END;
    RETURN res
  END MkLocals;

PROCEDURE FoldByHeader(cc: T; hdr: JunoAST.PredHeader; kind: FoldKind):
  JunoAST.Decl RAISES {BadFoldArg} =
  BEGIN
    IF kind = FoldKind.ProcNoArgs THEN 
      RETURN FoldNoArgs(cc, hdr.name)
    END;
    <* ASSERT hdr.ins # NIL *>
    VAR locals := MkLocals(cc, hdr.ins); BEGIN
      CASE kind OF <* NOWARN *>
    	FoldKind.Pred => RETURN FoldPred(cc, hdr.name, hdr.ins, locals)
      | FoldKind.Proc => RETURN FoldProc(cc, hdr.name, hdr.ins, locals)
      END
    END
  END FoldByHeader;

PROCEDURE FoldByName(cc: T; name: JunoAST.Id; kind := FoldKind.Proc):
  JunoAST.Decl =
  BEGIN
    CASE kind OF
    | FoldKind.ProcNoArgs => RETURN FoldNoArgs(cc, name)
    | FoldKind.Pred, FoldKind.Proc =>
        VAR
          hdr := NEW(JunoAST.PredHeader, bp := JunoAST.End,
            name := name, ins := GetFoldArgs(cc));
        <* FATAL BadFoldArg *> BEGIN
          RETURN FoldByHeader(cc, hdr, kind)
        END
    END
  END FoldByName;

PROCEDURE FoldAnim(cc: T; hdr: JunoAST.PredHeader;
    sliderPts: JunoAST.IdList): JunoAST.ProcDecl
    RAISES {BadFoldArg} =
  VAR
    newHdr := NEW(JunoAST.PredHeader, bp := hdr, name := hdr.name,
      ins := JunoASTUtils.ConcatIdLists(hdr.ins, sliderPts));
    locals := MkLocals(cc, hdr.ins, sliderPts);
  BEGIN
    RETURN FoldProc(cc, newHdr.name, newHdr.ins, locals)
  END FoldAnim;

PROCEDURE AppendSuffix(cc: T; id: JunoAST.Id; suffix: TEXT): JunoAST.Id =
(* Return the name resulting from concatenating "id" with "suffix". If this
   name is already defined in the current scope, append additional digits as
   necessary to produce an unused name. *)
  VAR resTxt := Atom.ToText(id) & suffix; BEGIN
    resTxt := NewDeclName(cc, resTxt, tryEmptySuffix := TRUE);
    RETURN Atom.FromText(resTxt);
  END AppendSuffix;

VAR (* READONLY after init *)
  DurId := Atom.FromText("dur");
  TId := Atom.FromText("t");
  DurQId := JunoASTUtils.QIdFromId(DurId);
  TQId := JunoASTUtils.QIdFromId(TId);
  DurIdList := JunoASTUtils.NewIdList(DurId);
  TIdList := JunoASTUtils.NewIdList(TId);
  ZeroVal := NEW(JunoAST.Number, bp := JunoAST.End, val := 0.0);
  OneVal := NEW(JunoAST.Number, bp := JunoAST.End, val := 1.0);

PROCEDURE FoldAnimFrame(cc: T; hdr: JunoAST.PredHeader;
  sliderPts: JunoAST.IdList): JunoAST.ProcDecl =
(* Construct a declaration of the form:

|  PROC <hdr.name>Frame(<hdr.ins>, dur, t) IS
|    IF
|      VAR
|    	 <pt[0].nm> = <pt[0].value>,
|    	 <pt[2].nm> = <pt[2].value>,
|    	 <pt[1].nm> = (t/dur, 0) REL (<pt[0].nm>, <pt[2].nm>)
|      IN
|    	 <hdr.name>(<hdr.ins>, <sliderPts>)
|      END
|    FI

   where <pt[i]> denotes the ith variable in the "sliderPts" argument,
   the "nm" field is the name of the point, and the "value" field is the
   value of that variable in the current command point cache. *)
  VAR
    pHeader := NEW(JunoAST.ProcHeader, bp := JunoAST.End,
      name := AppendSuffix(cc, hdr.name, "Frame"),
      ins := JunoASTUtils.ConcatIdLists(hdr.ins,
        JunoASTUtils.ConcatIdLists(DurIdList, TIdList)),
      outs := JunoAST.EmptyIdList, inouts := JunoAST.EmptyIdList);
    s0Name := sliderPts.head.id;
    s0QName := JunoASTUtils.QIdFromId(s0Name);
    s1Name := sliderPts.head.next.id;
    s2Name := sliderPts.head.next.next.id;
    s2QName := JunoASTUtils.QIdFromId(s2Name);
    (* initialize local variables in reverse order *)
    div := NEW(JunoAST.Divide, bp := JunoAST.End, e1 := TQId, e2 := DurQId);
    s1Hint := NEW(JunoAST.Rel, bp := JunoAST.End,
      e2 := NEW(JunoAST.Pair, bp := JunoAST.End, e1 := s0QName, e2 := s2QName),
      e1 := NEW(JunoAST.Pair, bp := JunoAST.End, e1 := div, e2 := ZeroVal));
    s1Var := NEW(JunoAST.NearVarLink, id := s1Name,
      frozen := TRUE, hint := s1Hint);
    s2Var := NEW(JunoAST.NearVarLink, id := s2Name,
      frozen := TRUE, hint := PointValue(cc, s2Name), next := s1Var);
    s0Var := NEW(JunoAST.NearVarLink, id := s0Name,
      frozen := TRUE, hint := PointValue(cc, s0Name), next := s2Var);
    nearVars := NEW(JunoAST.NearVarList, bp := JunoAST.End,
      size := 3, head := s0Var);
    call := NEW(JunoAST.ProcCall, bp := JunoAST.End,
      name := JunoASTUtils.QIdFromId(hdr.name),
      ins := JunoASTUtils.IdListToQIdList(
        JunoASTUtils.ConcatIdLists(hdr.ins, sliderPts)),
      outs := JunoAST.EmptyQIdList, inouts := JunoAST.EmptyExprList);
    proj := NEW(JunoAST.Proj, bp := JunoAST.End,
      vars := nearVars, body := call);
    ifCmd := NEW(JunoAST.If, bp := JunoAST.End, body := proj);
  BEGIN
    RETURN NEW(JunoAST.ProcDecl, bp := JunoAST.End,
      header := pHeader, body := ifCmd)
  END FoldAnimFrame;

PROCEDURE FoldAnimCreator(cc: T; hdr: JunoAST.PredHeader;
  frameNm: JunoAST.Id): JunoAST.ProcDecl =
(* Return a procedure declaration of the form:

|  PROC an := <hdr.name>Anim(<hdr.ins>, dur) IS
|    an := (CLOSE(<frameNm>, <hdr.ins>, dur), dur)
|  END
*)
  VAR
    anId := Atom.FromText("an");
    inArgs := JunoASTUtils.ConcatIdLists(hdr.ins, DurIdList);
    pHeader := NEW(JunoAST.ProcHeader, bp := JunoAST.End,
      name := AppendSuffix(cc, hdr.name, "Anim"), ins := inArgs,
      outs := JunoASTUtils.NewIdList(anId), inouts := JunoAST.EmptyIdList);
    closeArgs := JunoASTUtils.IdListToQIdList(
      JunoASTUtils.ConcatIdLists(JunoASTUtils.NewIdList(frameNm), inArgs));
    close := NEW(JunoAST.Call, bp := JunoAST.End,
      name := JunoASTUtils.QIdFromText("CLOSE"),
      ins := closeArgs, inouts := JunoAST.EmptyExprList);
    animPair := NEW(JunoAST.Pair, bp := JunoAST.End,
      e1 := close, e2 := DurQId);
    assign := NEW(JunoAST.Assign, bp := JunoAST.End,
      vars := JunoASTUtils.NewQIdList(JunoASTUtils.QIdFromId(anId)),
      exprs := JunoASTUtils.NewExprList(animPair, bp := JunoAST.End));
  BEGIN
    RETURN NEW(JunoAST.ProcDecl, bp := JunoAST.End,
      header := pHeader, body := assign)
  END FoldAnimCreator;

PROCEDURE SelectVar(cc: T; nm: JunoAST.Id): JunoAST.NearVarLink =
(* Return a new "NearVarLink" for the variable named "nm" whose hint is
   determined from the current value of "nm" in "cc"'s point cache. The hint
   is frozen or not according to whether it is frozen or hinted in "cc". The
   fields other than "id", "frozen", and "hint" of the result are defaulted. *)
  BEGIN
    RETURN NEW(JunoAST.NearVarLink, id := nm,
      frozen := IsFrozen(cc, nm), hint := PointValue(cc, nm))
  END SelectVar;

PROCEDURE SelectVars(cc: T; vars: JunoAST.IdList): JunoAST.NearVarList =
(* Return a "NearVarList" containing the variables "vars" (in the same order),
   hinted to values determined from the current values of those variables in
   "cc"'s point cache, and with the hint being frozen or not according to
   whether it is frozen or hinted in the current command "cc". *)
  VAR head, last: JunoAST.NearVarLink := NIL; curr := vars.head; BEGIN
    WHILE curr # NIL DO
      VAR new := SelectVar(cc, curr.id); BEGIN
      	IF head = NIL
      	  THEN head := new
      	  ELSE last.next := new
      	END;
        last := new
      END;
      curr := curr.next
    END;
    RETURN NEW(JunoAST.NearVarList, bp := JunoAST.End,
      size := vars.size, head := head)
  END SelectVars;

PROCEDURE PrependVar(id: JunoAST.Id; hint: JunoAST.Expr;
  l: JunoAST.NearVarList; frozen := TRUE): JunoAST.NearVarList =
(* Destructively prepend a "NearVarLink" of the form "<id> ~= <hint>" to the
   "NearVarList" "l". The "frozen" parameter determines whether "~" or "="
   is used. *)
  VAR link := NEW(JunoAST.NearVarLink, id := id, frozen := frozen,
    hint := hint, next := l.head);
  BEGIN
    INC(l.size);
    l.head := link;
    RETURN l
  END PrependVar;
    
PROCEDURE FoldAnimCmd(cc: T; args: JunoAST.IdList; animProcNm: JunoAST.Id):
    JunoAST.Cmd =
(* Produce an animation command of the form:
|
|    VAR
|      dur = 1,
|      <near-vars>
|    IN
|      Slider.SetVisibility(Slider.Invisible);
|      Anim.Play(<animProcNm>(<args>, dur))
|    END

   The <near-vars> are formed from both "cc" and "args".
*)
  VAR
    nearVars := PrependVar(DurId, OneVal, SelectVars(cc, args));
    animCall := NEW(JunoAST.Call, bp := JunoAST.End,
      name := JunoASTUtils.QIdFromId(animProcNm),
      ins := JunoASTUtils.IdListToQIdList(
        JunoASTUtils.ConcatIdLists(args, DurIdList)),
      inouts := JunoAST.EmptyExprList);
    sliderCall := NEW(JunoAST.ProcCall, bp := JunoAST.End,
      name := JunoASTUtils.QIdFromTexts("Slider", "SetVisibility"),
      ins := JunoASTUtils.NewExprList(
        JunoASTUtils.QIdFromTexts("Slider", "Invisible")),
      outs := JunoAST.EmptyQIdList, inouts := JunoAST.EmptyExprList);
    playCall := NEW(JunoAST.ProcCall, bp := JunoAST.End,
      name := JunoASTUtils.QIdFromTexts("Anim", "Play"),
      ins := JunoASTUtils.NewExprList(animCall),
      outs := JunoAST.EmptyQIdList, inouts := JunoAST.EmptyExprList);
    semi := NEW(JunoAST.Seq, bp := JunoAST.End,
      c1 := sliderCall, c2 := playCall);
  BEGIN
    RETURN NEW(JunoAST.Proj, bp := JunoAST.End,
      vars := nearVars, body := semi)
  END FoldAnimCmd;

(* ------------------------- Running / Updating ---------------------------- *)

PROCEDURE Run(cc: T; skipify: BOOLEAN): BOOLEAN
    RAISES {CompileError, RuntimeError} =
  BEGIN
    (* compile the current command if necessary *)
    TRY
      IF NOT cc.codeValid OR skipify # cc.skipify THEN
        VAR ast := cc.ast; BEGIN
          IF skipify THEN ast := Skipify(ast) END;
          cc.slot := JunoBuild.CurrCmd(ast, cc.scp);
          cc.codeValid := TRUE;
          cc.skipify := skipify
        END
      END
    EXCEPT
      JunoCompileErr.Error (err) =>
        RAISE CompileError(err.msg)
    END;
    (* run it *)
    RETURN Run2(cc)
  END Run;

PROCEDURE Run2(cc: T): BOOLEAN RAISES {RuntimeError} =
  VAR
    res: BOOLEAN;
    runRes := JunoRT.ExecFromSlot(cc.slot, reset := TRUE);
  BEGIN
    (* We expect the command to halt *)
    IF runRes.trapCode = JunoRT.TrapCode.Error THEN
      IF runRes.errorCode # JunoRTError.Code.Halt THEN
        JunoRT.ResetMachine();
        RAISE RuntimeError(RTError{JunoRT.TrapMessage(runRes), runRes})
      END
    ELSE
      RAISE RuntimeError(RTError{JunoRT.TrapMessage(runRes), runRes})
    END;

    (* update hints from stack frame *)
    res := UpdateHints(cc);

    (* Finish running the command until completion (this will be a no-op *)
    runRes := JunoRT.Exec();
    <* ASSERT runRes.trapCode = JunoRT.TrapCode.NormalHalt *>
    RETURN res
  END Run2;

PROCEDURE ClearCaches(cc: T) =
  BEGIN
    EVAL cc.pointCache.init(sizeHint := InitCacheSize);
    EVAL cc.othersCache.init(sizeHint := InitCacheSize)
  END ClearCaches;

PROCEDURE UpdateHints(cc: T): BOOLEAN =
  PROCEDURE SwapTables(VAR t1, t2: AtomRefTbl.Default) =
    VAR t := t1; BEGIN t1 := t2; t2 := t END SwapTables;
  VAR res := FALSE; BEGIN
    SwapTables(cc.oldPointCache, cc.pointCache);
    SwapTables(cc.oldOthersCache, cc.othersCache);
    ClearCaches(cc);
    TYPECASE cc.ast OF
      NULL => (* SKIP *)
    | JunoAST.Proj (proj) =>
        FillCache(cc, proj.vars);
        res := UpdateNearVars(cc, proj.vars)
    ELSE (* SKIP *)
    END;
    RETURN res
  END UpdateHints;

PROCEDURE FillCache(cc: T; vars: JunoAST.NearVarList) =
(* Read the values for the points declared in "vars" from
   the run-time stack, and store those values that are points in
   "cc.pointCache" and those that are not in "cc.othersCache". The
   "changedVal" array is set to indicate which values have changed. If
   "cc.ast" is not a projection, then both caches are made empty, and
   "changedVal" is unchanged. *)
  BEGIN
    <* ASSERT cc.ast # NIL AND ISTYPE(cc.ast, JunoAST.Proj) *>
    VAR nv := vars.head; frame := JunoRT.BaseFrame().up(); i := 0; BEGIN
      IF vars.size > NUMBER(cc.changedVal^) THEN
        cc.changedVal := NEW(REF ARRAY OF BOOLEAN,
          MAX(vars.size, 2 * NUMBER(cc.changedVal^)))
      END;
      WHILE nv # NIL DO
        <* ASSERT nv.index > 0 *>
        VAR v := frame.getLocal(nv.index); oldV: Value.T; BEGIN
          WITH changed = cc.changedVal[i] DO
            IF v # NIL THEN
              (* The frame contains a value for "nv" *)
              IF IsPointValue(v) THEN
            	changed := NOT (cc.oldPointCache.get(nv.id, oldV)
            	  AND Value.Equal(v, oldV));
            	EVAL cc.pointCache.put(nv.id, v)
              ELSE
            	changed := NOT (cc.oldOthersCache.get(nv.id, oldV)
            	  AND Value.Equal(v, oldV));
            	EVAL cc.othersCache.put(nv.id, v)
              END
            ELSE
              (* There is no value in the frame, so the value is being changed
                 only if it exists in one of the old caches. *)
              IF cc.oldPointCache.get(nv.id, oldV)
                 OR cc.oldOthersCache.get(nv.id, oldV) THEN
                changed := TRUE
              END
            END
          END
        END;
        nv := nv.next;
        INC(i)
      END
    END
  END FillCache;

PROCEDURE IsPointValue(v: Value.T): BOOLEAN =
(* Return TRUE iff "v" is the value of a 2D point, that is, a pair of two
   numbers. *)
  BEGIN
    TYPECASE v OF REF Value.Pair (p) =>
      TYPECASE p.car OF REF Value.Real =>
        TYPECASE p.cdr OF REF Value.Real =>
          RETURN TRUE
        ELSE (* SKIP *)
        END
      ELSE (* SKIP *)
      END
    ELSE (* SKIP *)
    END;
    RETURN FALSE
  END IsPointValue;

PROCEDURE UpdateNearVars(cc: T; vars: JunoAST.NearVarList): BOOLEAN =
(* Update the hints in "vars" from "cc.pointCache" and "cc.othersCache", and
   mark the compiled code as being out-of-date iff some hint changed. Returns
   "TRUE" iff some hint changed.

   Requires that all values "v" in "cc.pointCache" satisfy "IsPointValue(v)",
   and that "cc.changedVal" accurately reflects which values need updating. *)
  VAR nv := vars.head; v: Value.T; changedHint := FALSE; i := 0; BEGIN
    <* ASSERT NUMBER(cc.changedVal^) >= vars.size *>
    WHILE nv # NIL DO
      IF cc.pointCache.get(nv.id, v) THEN
        TYPECASE v OF <* NOWARN *> REF Value.Pair (p) =>
          VAR
            x := NARROW(p.car, REF Real);
            y := NARROW(p.cdr, REF Real);
          BEGIN
            IF nv.hint = JunoAST.NilExpr THEN
              (* the value didn't previously have a hint; now it does *)
              changedHint := TRUE;
              nv.hint := JunoASTUtils.NewPoint(x^, y^)
            ELSE
              (* change the existing hint for the point *)
              changedHint := MovePoint1(cc, nv, x^, y^,
                changed := cc.changedVal[i]) OR changedHint
            END;
          END
        END
      ELSIF cc.changedVal[i] THEN
        changedHint := TRUE;
        IF cc.othersCache.get(nv.id, v) THEN
          (* only update the hint for a non-point if it already has
             a literal hint *)
          IF nv.hint # JunoAST.NilExpr AND IsLiteral(nv.hint) THEN
            nv.hint := JunoASTUtils.NewASTFromValue(v)
          END
        ELSE
          (* If not in either cache, then what has changed is that the
             variable no longer has a hint. *)
          nv.hint := JunoAST.NilExpr
        END
      END;
      INC(i);
      nv := nv.next
    END;
    cc.codeValid := cc.codeValid AND NOT changedHint;
    RETURN changedHint
  END UpdateNearVars;

BEGIN
END CurrCmd.
