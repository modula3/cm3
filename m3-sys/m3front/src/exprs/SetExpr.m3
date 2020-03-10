(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SetExpr.m3                                            *)
(* Last modified on Fri Feb 24 16:47:18 PST 1995 by kalsow     *)
(*      modified on Thu May 20 08:20:18 PDT 1993 by muller     *)

MODULE SetExpr;
(* For set constructors. *)

IMPORT Text, Fmt;

IMPORT M3, CG, Expr, ExprRep, Type, Error, IntegerExpr, EnumExpr;
IMPORT RangeExpr, KeywordExpr, SetType, AssignStmt, CheckExpr;
IMPORT M3ID, Target, TInt, TWord, Bool, M3Buf, Module;

TYPE
  Node = REF RECORD
    next : Node;
    min  : INTEGER;
    max  : INTEGER;
  END;

TYPE
  P = Expr.T OBJECT
        tipe        : Type.T;
        args        : Expr.List;
        tree        : Node;
        others      : Expr.List;
        nOthers     : INTEGER;
        tmp         : CG.Var;
        globalOffset: INTEGER;
        RTErrorMsg  : TEXT := NIL;
        minI, maxI  : INTEGER; (* Of the set's element type. *)
        minT, maxT  : Target.Int;
        RTErrorCode := CG.RuntimeError.Unknown;
        mapped      : BOOLEAN;
        is_const    : BOOLEAN;
        broken      : BOOLEAN;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := Evaluate;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := IsZeroes;
        genFPLiteral := GenFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
        use          := Use;
      END;

TYPE
  VisitState = RECORD
    a, b       : Node;    (* private to the iterator *)
    amin, amax : INTEGER; (* " *)
    bmin, bmax : INTEGER; (* " *)
    min, max   : INTEGER; (* resulting range *)
    inA, inB   : BOOLEAN; (* location of resulting range *)
  END;

VAR (*CONST*)
  Grain : CARDINAL;  (* allocation grain for set values *)
  full  : Target.Int;
  left  : ARRAY [0..64] OF Target.Int;
  right : ARRAY [0..64] OF Target.Int;

(*EXPORTED:*)
PROCEDURE New (type: Type.T;  args: Expr.List): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.type    := type;
    p.repType := type;
    p.tipe    := type;
    p.args    := args;
    p.mapped  := FALSE;
    p.broken  := FALSE;
    p.tree    := NIL;
    p.others  := NIL;
    p.nOthers := -1;
    p.tmp     := NIL;
    p.is_const := TRUE;
    p.checked := FALSE;
    RETURN p;
  END New;

PROCEDURE NewFromTree (p: P;  node: Node): Expr.T =
  VAR c: P;
  BEGIN
    c := NEW (P);
    c.origin  := p.origin;
    c.type    := p.type;
    c.repType := p.repType;
    c.checked := p.checked;
    c.tipe    := p.tipe;
    c.args    := p.args;
    c.mapped  := TRUE;
    c.broken  := FALSE;
    c.tree    := NormalizeTree (node);
    c.others  := NIL;
    c.nOthers := -1;
    c.is_const := TRUE;
    RETURN c;
  END NewFromTree;

(*EXPORTED:*)
PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    RETURN (TYPECODE (e) = TYPECODE (P));
  END Is;

(*EXPORTED:*)
PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN =
  VAR p, q: P;  le, eq, ge: BOOLEAN := TRUE;  s: VisitState;
  BEGIN
    IF NOT CheckPair (a, b, p, q) THEN RETURN FALSE END;
    SetupVisit (s, p.tree, q.tree);
    WHILE Visit (s) DO
      IF (s.min <= s.max) THEN
        (* we got a non-empty range *)
        IF (s.inA) AND (NOT s.inB) THEN
          eq := FALSE;  le := FALSE;
        ELSIF (s.inB) AND (NOT s.inA) THEN
          eq := FALSE;  ge := FALSE;
        END;
      END;
    END;
    IF    (le AND NOT eq) THEN sign :=  -1
    ELSIF (ge AND NOT eq) THEN sign := 1
    ELSIF (eq)            THEN sign := 0
    ELSE                       sign :=  -99;
    END;
    RETURN TRUE;
  END Compare;

(*EXPORTED:*)
PROCEDURE Union (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR p, q: P;  n, x: Node;
  BEGIN
    IF NOT CheckPair (a, b, p, q) THEN RETURN FALSE END;
    n := NIL;
    x := p.tree;
    WHILE (x # NIL) DO
      n := AddNode (n, x.min, x.max);
      x := x.next;
    END;
    x := q.tree;
    WHILE (x # NIL) DO
      n := AddNode (n, x.min, x.max);
      x := x.next;
    END;
    c := NewFromTree (p, n);
    RETURN TRUE;
  END Union;

(*EXPORTED:*)
PROCEDURE Intersection (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR p, q: P;  n: Node;  s: VisitState;
  BEGIN
    IF NOT CheckPair (a, b, p, q) THEN RETURN FALSE END;
    n := NIL;
    SetupVisit (s, p.tree, q.tree);
    WHILE Visit (s) DO
      IF (s.min <= s.max) AND (s.inA) AND (s.inB) THEN
        n := AddNode (n, s.min, s.max);
      END;
    END;
    c := NewFromTree (p, n);
    RETURN TRUE;
  END Intersection;

(*
(* Not implemented: *)
(*EXPORTED:*)
PROCEDURE Negate (set: Expr.T; VAR result: Expr.T): BOOLEAN =
  VAR constSet: P;
  BEGIN
    result := NIL;
    IF NOT BuildMap (set, constSet) THEN RETURN FALSE END;
    n := NIL;
    x := p.tree;
    WHILE x # NIL DO
    END;
  END Negate;
*)

(*EXPORTED:*)
PROCEDURE Difference (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR p, q: P;  n: Node;  s: VisitState;
  BEGIN
    IF NOT CheckPair (a, b, p, q) THEN RETURN FALSE END;
    n := NIL;
    SetupVisit (s, p.tree, q.tree);
    WHILE Visit (s) DO
      IF (s.min <= s.max) AND (s.inA) AND (NOT s.inB) THEN
        n := AddNode (n, s.min, s.max);
      END;
    END;
    c := NewFromTree (p, n);
    RETURN TRUE;
  END Difference;

(*EXPORTED:*)
PROCEDURE SymDifference (a, b: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR p, q: P;  n: Node;  s: VisitState;
  BEGIN
    IF NOT CheckPair (a, b, p, q) THEN RETURN FALSE END;
    n := NIL;
    SetupVisit (s, p.tree, q.tree);
    WHILE Visit (s) DO
      IF (s.min <= s.max) AND (s.inA # s.inB) THEN
        n := AddNode (n, s.min, s.max);
      END;
    END;
    c := NewFromTree (p, n);
    RETURN TRUE;
  END SymDifference;

(*EXPORTED:*)
PROCEDURE Include (set, elt: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR p: P;  i: INTEGER;  n, x: Node;
  BEGIN
    IF NOT ConstElt (elt, i) THEN RETURN FALSE END;
    IF NOT BuildMap (set, p) THEN RETURN FALSE END;
    n := AddNode (NIL, i, i);
    x := p.tree;
    WHILE (x # NIL) DO
      n := AddNode (n, x.min, x.max);
      x := x.next;
    END;
    c := NewFromTree (p, n);
    RETURN TRUE;
  END Include;

(*EXPORTED:*)
PROCEDURE Exclude (set, elt: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR p: P;  i: INTEGER;  n, x: Node;
  BEGIN
    IF NOT ConstElt (elt, i) THEN RETURN FALSE END;
    IF NOT BuildMap (set, p) THEN RETURN FALSE END;
    n := NIL;
    x := p.tree;
    WHILE (x # NIL) DO
      IF (x.min <= i) AND (i <= x.max) THEN
        n := AddNode (n, x.min, i - 1);
        n := AddNode (n, i + 1, x.max);
      ELSE
        n := AddNode (n, x.min, x.max);
      END;
      x := x.next;
    END;
    c := NewFromTree (p, n);
    RETURN TRUE;
  END Exclude;

(*EXPORTED:*)
PROCEDURE Member (set, elt: Expr.T;  VAR c: Expr.T): BOOLEAN =
  VAR p: P;  i: INTEGER;  x: Node;
  BEGIN
    IF NOT ConstElt (elt, i) THEN RETURN FALSE END;
    IF NOT BuildMap (set, p) THEN RETURN FALSE END;
    x := p.tree;
    WHILE (x # NIL) DO
      IF (x.min <= i) AND (i <= x.max) THEN
        c := Bool.Map [TRUE];
        RETURN TRUE;
      END;
      x := x.next;
    END;
    c := Bool.Map [FALSE];
    RETURN TRUE;
  END Member;

PROCEDURE ConstElt (elt: Expr.T;  VAR i: INTEGER): BOOLEAN =
  VAR t: Type.T;  int: Target.Int;
  BEGIN
    elt := Expr.ConstValue (elt);
    IF (elt = NIL) THEN RETURN FALSE END;
    RETURN (IntegerExpr.Split (elt, int, t) OR EnumExpr.Split (elt, int, t))
       AND TInt.ToInt (int, i);
  END ConstElt;

PROCEDURE CheckPair (a, b: Expr.T;  VAR p, q: P): BOOLEAN =
  BEGIN
    RETURN BuildMap (a, p)
       AND BuildMap (b, q)
       AND Type.IsEqual (p.tipe, q.tipe, NIL);
  END CheckPair;

PROCEDURE SetupVisit (VAR s: VisitState;  x, y: Node) =
  BEGIN
    s.a := x;
    s.b := y;
    IF (x # NIL) THEN s.amin := x.min;  s.amax := x.max END;
    IF (y # NIL) THEN s.bmin := y.min;  s.bmax := y.max END;
  END SetupVisit;

PROCEDURE Visit (VAR s: VisitState): BOOLEAN =
  BEGIN
    IF (s.a = NIL) AND (s.b = NIL) THEN
      (* both lists are empty *)
      RETURN FALSE;
    ELSIF (s.a = NIL) THEN
      (* A list is empty *)
      s.min := s.bmin;  s.max := s.bmax;  s.inA := FALSE;  s.inB := TRUE;
      s.bmin := s.bmax + 1;
    ELSIF (s.b = NIL) THEN
      (* B list is empty *)
      s.min := s.amin;  s.max := s.amax;  s.inA := TRUE;  s.inB := FALSE;
      s.amin := s.amax + 1;
    ELSE (* both lists are non-empty *)
      IF (s.amin < s.bmin) THEN
        s.min := s.amin;    s.inA := TRUE;  s.inB := FALSE;
        IF (s.amax < s.bmin) THEN
          s.max := s.amax; s.amin := s.amax + 1;
        ELSE (* s.amax >= s.bmin *)
          s.max := s.bmin - 1;
          s.amin := s.bmin;  s.bmin := s.amin;
        END;
      ELSIF (s.amin = s.bmin) THEN
        s.min := s.amin;  s.inA := TRUE;  s.inB := TRUE;
        IF (s.bmax < s.amax) THEN
          s.max := s.bmax;  s.amin := s.bmax + 1;
        ELSE (* s.amax <= s.bmax *)
          s.max := s.amax;  s.amin := s.amax + 1;
        END;
        s.bmin := s.amin;
      ELSE (* s.amin > s.bmin *)
        s.min := s.bmin;  s.inA := FALSE;   s.inB := TRUE;
        IF (s.bmax < s.amin) THEN
          s.max := s.bmax;  s.bmin := s.bmax + 1;
        ELSE (* s.amin <= s.bmax *)
          s.max := s.amin - 1;
          s.amin := s.amin;  s.bmin := s.amin;
        END;
      END;
    END;
    IF (s.amax < s.amin) AND (s.a # NIL) THEN
      s.a := s.a.next;
      IF (s.a # NIL) THEN s.amin := s.a.min;  s.amax := s.a.max  END;
    END;
    IF (s.bmax < s.bmin) AND (s.b # NIL) THEN
      s.b := s.b.next;
      IF (s.b # NIL) THEN s.bmin := s.b.min;  s.bmax := s.b.max  END;
    END;
    RETURN TRUE;
  END Visit;

PROCEDURE BuildMap (e: Expr.T;  VAR p: P): BOOLEAN =
  VAR
    t, range: Type.T;
    elt, eMin, eMax: Expr.T;
    from, to, min, max: Target.Int;
    iFrom, iTo: INTEGER;
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(x) => p := x;
    ELSE      RETURN FALSE;
    END;
    IF (p.mapped) THEN RETURN (p.others = NIL) END;
    p.mapped := TRUE;
    IF NOT SetType.Split (p.tipe, range) THEN RETURN FALSE END;
    EVAL Type.GetBounds (range, min, max);
    (* IF TInt.LT (max, min) THEN RETURN FALSE END; ?? -- 8/2/93 WKK -- *)
    FOR i := 0 TO LAST (p.args^) DO
      elt := Expr.ConstValue (p.args[i]);
      IF (elt = NIL) THEN (* not a constant *)
        p.is_const := FALSE;
        AddOther (p, p.args[i]);
      ELSIF IntegerExpr.Split (elt, from, t)
        OR EnumExpr.Split (elt, from, t) THEN
        IF TInt.LT (from, min) OR TInt.LT (max, from) THEN
          (* CheckRT will have emitted a warning.  Omit the value. *)
        ELSIF TInt.ToInt (from, iFrom) THEN
          p.tree := AddNode (p.tree, iFrom, iFrom);
        ELSE (* treat values outside host INTEGER as non-constants *)
          AddOther (p, elt);
        END;
      ELSIF (RangeExpr.Split (elt, eMin, eMax)) THEN
        eMin := Expr.ConstValue (eMin);
        eMax := Expr.ConstValue (eMax);
        IF (eMin # NIL) AND (eMax # NIL)
          AND (IntegerExpr.Split (eMin, from, t)
               OR EnumExpr.Split (eMin, from, t))
          AND (IntegerExpr.Split (eMax, to, t)
               OR EnumExpr.Split (eMax, to, t))
        THEN
          IF TInt.LT (from, min) OR TInt.LT (max, from) THEN
            (* CheckRT will have emitted a warning.  Omit the value. *)
          ELSIF TInt.ToInt (from, iFrom) AND TInt.ToInt (to, iTo) THEN
            p.tree := AddNode (p.tree, iFrom, iTo);
          ELSE (* since the values are so big, treat them as non-constants *)
            AddOther (p, elt);
          END;
        ELSE (* not a constant range *)
          AddOther (p, elt);
        END;
      ELSE
        <* ASSERT FALSE *>
      END;
    END (*FOR*);
    p.tree := NormalizeTree (p.tree);
    RETURN (p.others = NIL);
  END BuildMap;

PROCEDURE AddOther (p: P;  elt: Expr.T) =
  BEGIN
    IF (p.others = NIL) THEN
      p.others := NEW (Expr.List, NUMBER (p.args^));
      p.nOthers := 0;
    END;
    p.others[p.nOthers] := elt;
    INC (p.nOthers);
  END AddOther;

PROCEDURE AddNode (n: Node;  min, max: INTEGER): Node =
  VAR x: Node;
  BEGIN
    IF (max < min) THEN RETURN n END;
    x := n;
    LOOP
      IF (x = NIL) THEN
        x := NEW (Node);  x.next := n;  x.min := min;  x.max := max;
        RETURN x;
      END;
      IF ((x.min <= min) AND (min <= x.max))
      OR ((x.min <= max) AND (max <= x.min)) THEN
        IF (min < x.min) THEN x.min := min END;
        IF (x.max < max) THEN x.max := max END;
        RETURN n;
      END;
      x := x.next;
    END;
  END AddNode;

PROCEDURE NormalizeTree (n: Node): Node =
  VAR x1, x2, x3: Node;  done: BOOLEAN;
  BEGIN
    IF (n = NIL) THEN RETURN NIL END;

    (* destructively sort the input list *)
    done := FALSE;
    WHILE (NOT done) DO
      done := TRUE;
      x1 := n.next;  x2 := n;  x3 := NIL;
      WHILE (x1 # NIL) DO
        IF (x1.min < x2.min) THEN
          (* swap x1 and x2 *)
          x2.next := x1.next;
          x1.next := x2;
          IF (x3 = NIL) THEN n := x1 ELSE x3.next := x1 END;
          x2 := x1;
          x1 := x2.next;
          done := FALSE;
        END;
        x3 := x2;  x2 := x1;  x1 := x1.next;
      END;
    END;

    (* merge adjacent nodes *)
    x1 := n.next;  x2 := n;
    WHILE (x1 # NIL) DO
      IF (x2.min <= x1.min) AND (x1.min <= x2.max) THEN
        IF (x2.max < x1.max) THEN x2.max := x1.max END;
        x1 := x1.next;
        x2.next := x1;
      ELSE
        x2 := x1;  x1 := x1.next;
      END;
    END;

    RETURN n;
  END NormalizeTree;

PROCEDURE MergeRTError (p: P; Code: CG.RuntimeError; Msg: TEXT) =
  BEGIN
    IF p.RTErrorMsg = NIL THEN
      p.RTErrorCode := Code;
      p.RTErrorMsg := Msg;
    END;
  END MergeRTError;

(* Externally dispatched-to: *)
PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR
    eltType          : Type.T;
    minExpr, maxExpr : Expr.T;
    argExpr, value   : Expr.T;
    key              : M3ID.T;
    RTErrorMsg       : TEXT;
    RTErrorCode      : CG.RuntimeError;
    buf: ARRAY [0..TInt.Size] OF CHAR;
(*  argType          : Type.T;
    minT, maxT       : Target.Int;
    minE, maxE       : Target.Int;
*)
  BEGIN
    p.tipe := Type.Check (p.tipe);
    p.type := p.tipe;
    p.repType := p.tipe;
    FOR i := 0 TO LAST (p.args^) DO Expr.TypeCheck (p.args[i], cs) END;
    IF NOT SetType.Split (p.tipe, eltType) THEN
      (* How can this happen? ConsExpr wouldn't have created this node, if not. *)
      Error.Msg ("Set constructor must specify a set type (2.6.8).");
      p.broken := TRUE;
      p.checked := TRUE;
      RETURN;
    END;
    EVAL Type.GetBounds (eltType, p.minT,p.maxT);
    IF NOT TInt.ToInt (p.minT, p.minI)
        OR NOT TInt.ToInt (p.maxT, p.maxI) THEN
      Error.Msg
        ("CM3 restriction: set domain (" 
         & Text.FromChars (SUBARRAY(buf, 0, TInt.ToChars(p.minT, buf))) & ".."
         & Text.FromChars (SUBARRAY(buf, 0, TInt.ToChars(p.maxT, buf)))
         & ") too large for host INTEGER ("
         & Fmt.Int (FIRST(INTEGER)) & ".." & Fmt.Int (FIRST(INTEGER)) & ").");
      p.minI := FIRST (INTEGER);
      <*ASSERT TInt.FromInt (p.minI, p.minT) *>
      p.maxI := LAST (INTEGER);
      <*ASSERT TInt.FromInt (p.maxI, p.maxT) *>
    END;
    FOR i := 0 TO LAST (p.args^) DO
      argExpr := p.args[i];

      IF KeywordExpr.Split (argExpr, key, value) THEN
        Error.Msg ("keyword values not allowed in set constructors");
        argExpr := value;
        p.args[i] := value;
      END;

      IF RangeExpr.Split (argExpr, minExpr, maxExpr) THEN
        IF NOT Type.IsAssignable (eltType, Expr.TypeOf (minExpr)) THEN
          Error.Msg
            ("Range min is not assignable to set constructor's element type "
             & "(2.6.8).");
          p.broken := TRUE;
        ELSE
          AssignStmt.CheckRT
            (eltType, minExpr, cs, (*VAR*) RTErrorCode, (*VAR*) RTErrorMsg);
          MergeRTError (p, RTErrorCode, RTErrorMsg);
        END;
        IF NOT Type.IsAssignable (eltType, Expr.TypeOf (maxExpr)) THEN
          Error.Msg
            ("Range max is not assignable to set constructor's element type "
             & "(2.6.8).");
          p.broken := TRUE;
        ELSE
          AssignStmt.CheckRT
            (eltType, maxExpr, cs, (*VAR*) RTErrorCode, (*VAR*) RTErrorMsg);
          MergeRTError (p, RTErrorCode, RTErrorMsg);
        END;
      ELSE (* argExpr is a single set element value *)
        IF NOT Type.IsAssignable (eltType, Expr.TypeOf (argExpr)) THEN
          Error.Msg
            ("Expression is not assignable to set constructor's element type "
             & "(2.6.8).");
          p.broken := TRUE;
        ELSE
          AssignStmt.CheckRT
            (eltType, argExpr, cs,  RTErrorCode, (*VAR*) RTErrorMsg);
          MergeRTError (p, RTErrorCode, RTErrorMsg);
        END;
        minExpr := argExpr;
        maxExpr := argExpr;
      END;
      p.checked := TRUE;
(*
      argType := Expr.TypeOf (argExpr);
      minExpr := Expr.ConstValue (minExpr);
      maxExpr := Expr.ConstValue (maxExpr);
      IF (minExpr # NIL) AND (maxExpr # NIL)
        AND (IntegerExpr.Split (minExpr, minE, argType)
             OR EnumExpr.Split (minExpr, minE, argType))
        AND (IntegerExpr.Split (maxExpr, maxE, argType)
             OR EnumExpr.Split (maxExpr, maxE, argType))
        THEN
        IF TInt.LT (minE, minT) OR TInt.LT (maxT, maxE) THEN
          Error.Msg ("illegal set value");
        END;
      END;
*)
    END (*FOR*);
  END Check;

(* Externally dispatched-to: *)
PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  VAR b: P;  ax, bx: Expr.T;
  BEGIN
    TYPECASE e OF
    | NULL  => RETURN FALSE;
    | P(bb) => b := bb;
    ELSE       RETURN FALSE;
    END;

    IF (NOT Type.IsEqual (a.tipe, b.tipe, x))
      OR ((a.args = NIL) # (b.args = NIL))
      OR ((a.args # NIL) AND (NUMBER (a.args^) # NUMBER (b.args^))) THEN
      RETURN FALSE;
    END;

    FOR i := 0 TO LAST (a.args^) DO
      ax := Expr.ConstValue (a.args[i]);
      IF (ax = NIL) THEN ax := a.args[i] END;
      bx := Expr.ConstValue (b.args[i]);
      IF (bx = NIL) THEN bx := b.args[i] END;
      IF NOT Expr.IsEqual (ax, bx, x) THEN RETURN FALSE END;
    END;

    RETURN TRUE;
  END EqCheck;

(* Externally dispatched-to: *)
PROCEDURE NeedsAddress (<*UNUSED*> p: P) =
  BEGIN
    (* yep, all sets get memory addresses *)
  END NeedsAddress;

(* Externally dispatched-to: *)
PROCEDURE Prep (p: P) =
  VAR info: Type.Info;
  BEGIN
    IF p.broken THEN RETURN; END;
    EVAL BuildMap (p, p);  (* evaluate the constants *)
    (* prep the non-constant elements *)
    FOR i := 0 TO p.nOthers-1 DO
      Expr.Prep (p.others[i]);
    END;

    EVAL Type.CheckInfo (p.tipe, info);
    IF (info.size > Target.Integer.size) AND (p.nOthers > 0) THEN
      (* large non-constant sets almost always turn into procedure
         calls in the code generator, so we might as well make life
         simpler there... *)
      p.tmp := PrepBig (p, info);
    END;
  END Prep;

(* Externally dispatched-to: *)
PROCEDURE Compile (p: P) =
  VAR info: Type.Info;
  BEGIN
    IF p.broken THEN RETURN; END;
    EVAL Type.CheckInfo (p.tipe, info);
    IF info.size <= Target.Word.size THEN
      (* Set fits w/in one target word. *)
      CompileSmall (p, info);
    ELSIF p.nOthers <= 0 THEN
      (* Multi-word set, but all elements are constant and within host INTEGER range. *)
      p.globalOffset := Module.Allocate (info.size, info.alignment, TRUE, "*set*");
      GenLiteral (p, p.globalOffset, p.tipe, TRUE);
      CG.Load_addr_of (Module.GlobalData (TRUE), p.globalOffset, info.alignment);
    ELSE
      (* Multi-word set with elements non-constant and/or outside host INTEGER
         range. We built it at Prep time. *)
      CG.Load_addr_of_temp (p.tmp, 0, Target.Integer.align);
      p.tmp := NIL;
    END;
  END Compile;

PROCEDURE PrepBig (p: P;  VAR info: Type.Info): CG.Var =
(* PRE: others can contain both non-constant exprs and constant values outside
        the range of host INTEGER. *)
(* Leave result in p.tmp. *)
  VAR
    loWordNo, hiWordNo       : INTEGER;
    loBitNo, hiBitNo       : INTEGER;
    minExpr      : Expr.T;
    maxExpr      : Expr.T;
    e            : Expr.T;
    t1           : CG.Var;
    nWords       : INTEGER;
    curWordNo      : INTEGER;
    curMask      : Target.Int;
    n            : Node;
    tmp          : Target.Int;
  BEGIN
    nWords := info.size DIV Grain;
    t1 := CG.Declare_temp (nWords * Grain, Target.Word.align,
                           CG.Type.Struct, in_memory := TRUE);

    (* generate the constant words *)
    n := p.tree;
    curWordNo := 0;
    curMask := TInt.Zero;
    WHILE (n # NIL) DO
      loWordNo := (n.min - p.minI) DIV Grain;
      loBitNo := (n.min - p.minI) MOD Grain;
      hiWordNo := (n.max - p.minI) DIV Grain;
      hiBitNo := (n.max - p.minI) MOD Grain;
      IF (loWordNo # curWordNo) THEN
        EmitAssign (t1, curWordNo, curMask);
        FOR i := curWordNo+1 TO loWordNo-1 DO  EmitAssign (t1, i, TInt.Zero) END;
        curWordNo := loWordNo;
        curMask := TInt.Zero;
      END;
      IF (loWordNo # hiWordNo) THEN
        TWord.Or (curMask, left [loBitNo], tmp);
        EmitAssign (t1, loWordNo, tmp);
        FOR i := loWordNo + 1 TO hiWordNo - 1 DO  EmitAssign (t1, i, full)  END;
        curWordNo := hiWordNo;
        curMask := right [hiBitNo];
      ELSE (* x = y *)
        TWord.And (left [loBitNo], right[hiBitNo], tmp);
        TWord.Or (curMask, tmp, curMask);
      END;
      n := n.next;
    END; (* while *)

    (* write the last mask *)
    EmitAssign (t1, curWordNo, curMask);

    (* write zeros for the remainder of the set *)
    FOR i := curWordNo+1 TO nWords-1 DO EmitAssign (t1, i, TInt.Zero) END;

    (* finally, add the non-constant elements *)
    FOR i := 0 TO p.nOthers-1 DO
      e := p.others[i];
      IF RangeExpr.Split (e, minExpr, maxExpr) THEN
        CG.Load_addr_of (t1, 0, Target.Integer.align);
        CG.ForceStacked ();
        GenElement (minExpr, p.minT, p.maxT);
        GenElement (maxExpr, p.minT, p.maxT);
        CG.Set_range (info.size);
        (* ^For info.size > Target.Word.size, works on in-memory operands. *)
      ELSE (* single value *)
        CG.Load_addr_of (t1, 0, Target.Integer.align);
        GenElement (e, p.minT, p.maxT);
        CG.Set_singleton (info.size);
        (* ^For info.size > Target.Word.size, works on in-memory operands. *)
      END;
    END;

    RETURN t1;
  END PrepBig;

PROCEDURE EmitAssign (set: CG.Var;  wordNo: INTEGER;
                      READONLY value: Target.Int) =
  VAR tmp: Target.Int;
  BEGIN
    EVAL TInt.Extend (value, Target.Integer.bytes, tmp);
    CG.Load_integer (Target.Integer.cg_type, tmp);
    CG.Store_int (Target.Integer.cg_type, set, wordNo * Grain);
    <* ASSERT Grain = Target.Integer.size *>
  END EmitAssign;

PROCEDURE GenElement (e: Expr.T;  READONLY minT, maxT: Target.Int) =
  VAR cg_type := Type.CGType (Type.Base (Expr.TypeOf (e)));
  BEGIN
    CheckExpr.EmitChecks (e, minT, maxT, CG.RuntimeError.ValueOutOfRange);
    IF NOT TInt.EQ (minT, TInt.Zero) THEN
      CG.Load_integer (cg_type, minT);
      CG.Subtract (cg_type);
    END;
    CG.Loophole (cg_type, Target.Integer.cg_type);
  END GenElement;

PROCEDURE CompileSmall (p: P;  VAR info: Type.Info) =
(* PRE: Entire set fits in a target word. *)
(* PRE: p.others contains no constants with values outside of host INTEGER. *)
  VAR
    loBitNo, hiBitNo: INTEGER;
    minExpr      : Expr.T;
    maxExpr      : Expr.T;
    e            : Expr.T;
    curMask      : Target.Int;
    n            : Node;
    tmpT          : Target.Int;
  BEGIN
    Type.Compile (p.tipe);
    <*ASSERT info.size <= Target.Word.size *>

    (* generate the constant words *)
    n := p.tree;
    curMask := TInt.Zero;
    WHILE (n # NIL) DO
      loBitNo := (n.min - p.minI);
      hiBitNo := (n.max - p.minI);
      TWord.And (left [loBitNo], right[hiBitNo], tmpT);
      TWord.Or (curMask, tmpT, curMask);
      n := n.next;
    END (*WHILE*);

    (* push the mask *)
    CG.Load_integer (Target.Word.cg_type, curMask);
    CG.ForceStacked ();

    (* finally, add the non-constant elements *)
    FOR i := 0 TO p.nOthers-1 DO
      e := p.others[i];
      IF RangeExpr.Split (e, minExpr, maxExpr) THEN
        GenElement (minExpr, p.minT, p.maxT);
        GenElement (maxExpr, p.minT, p.maxT);
        CG.Set_range (info.size)
        (* ^For info.size <= Target.Word.size, works on on-stack operands. *)
      ELSE (* single value *)
        GenElement (e, p.minT, p.maxT);
        CG.Set_singleton (info.size);
        (* ^For info.size <= Target.Word.size, works on on-stack operands. *)
      END;
    END;
  END CompileSmall;

(* Externally dispatched-to: *)
PROCEDURE Evaluate (e: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    EVAL BuildMap (e, p);
    IF p.is_const
      THEN RETURN e;
      ELSE RETURN NIL;
    END;
  END Evaluate;

(* Externally dispatched-to: *)
PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN (p.args = NIL) OR (NUMBER (p.args^) <= 0);
  END IsZeroes;

(* Externally dispatched-to: *)
PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "SET<");
    FOR i := 0 TO LAST (p.args^) DO
      IF (i > 0) THEN M3Buf.PutChar (buf, ',') END;
      Expr.GenFPLiteral (p.args[i], buf);
    END;
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

(* Externally dispatched-to: *)
PROCEDURE GenLiteral
  (p: P;  offset: INTEGER; type: Type.T; is_const: BOOLEAN) =
  VAR
    loWordNo, hiWordNo : INTEGER;
    loBitNo, hiBitNo   : INTEGER;
    curWordNo          : INTEGER;
    n                  : Node;
    curMask            : Target.Int;
    tmpT               : Target.Int;
    info               : Type.Info;
 BEGIN
    EVAL BuildMap (p, p);
    <* ASSERT p.others = NIL *>

    curMask := TInt.Zero;
    n := p.tree;
    EVAL Type.CheckInfo (type, info);
    IF info.size <= Target.Integer.size THEN
      (* Set fits in a target word, possibly less. *)
      WHILE (n # NIL) DO
        loBitNo := (n.min - p.minI);
        hiBitNo := (n.max - p.minI);
        <* ASSERT hiBitNo < Target.Word.size *>
        TWord.And (left [loBitNo], right[hiBitNo], tmpT);
        TWord.Or (curMask, tmpT, curMask);
        n := n.next;
      END (*WHILE*);
      CG.Init_int (offset, info.size, curMask, is_const);
    ELSE (* Multi-word set.  Whole words only. *)
      curWordNo := 0;
      WHILE (n # NIL) DO
        loWordNo := (n.min - p.minI) DIV Grain;
        loBitNo := (n.min - p.minI) MOD Grain;
        hiWordNo := (n.max - p.minI) DIV Grain;
        hiBitNo := (n.max - p.minI) MOD Grain;
        IF (loWordNo # curWordNo) THEN
          (* write the mask we've accumulated *)
          IF NOT TInt.EQ (curMask, TInt.Zero) THEN
            EVAL TInt.Extend (curMask, Target.Integer.bytes, tmpT);
            CG.Init_int (offset + curWordNo*Target.Integer.pack,
                          Target.Integer.size, tmpT, is_const);
          END;
          curWordNo := loWordNo;
          curMask := TInt.Zero;
        END;
        IF (loWordNo # hiWordNo) THEN
          (* write the full words [loWordNo..hiWordNo-1] *)
          TWord.Or (curMask, left [loBitNo], tmpT);
          EVAL TInt.Extend (tmpT, Target.Integer.bytes, tmpT);
          CG.Init_int (offset + loWordNo * Target.Integer.pack, Target.Integer.size,
                       tmpT, is_const);
          FOR i := loWordNo + 1 TO hiWordNo - 1 DO
            EVAL TInt.Extend (full, Target.Integer.bytes, tmpT);
            CG.Init_int (offset + i * Target.Integer.pack, Target.Integer.size,
                         tmpT, is_const);
          END;
          curWordNo := hiWordNo;
          curMask := right [hiBitNo];
        ELSE
          TWord.And (left [loBitNo], right[hiBitNo], tmpT);
          TWord.Or (curMask, tmpT, curMask);
        END;
        n := n.next;
      END (*WHILE*);

      (* write the last mask *)
      IF NOT TInt.EQ (curMask, TInt.Zero) THEN
        EVAL TInt.Extend (curMask, Target.Integer.bytes, tmpT);
        CG.Init_int (offset + curWordNo * Target.Integer.pack,
                     Target.Integer.size, tmpT, is_const);
      END;
    END;
  END GenLiteral;

(*EXPORTED:*)
PROCEDURE Init () =
  BEGIN
    Grain := Target.Word.size;
    TWord.Not (TInt.Zero, full);
    TWord.And (full, Target.Word.max, full);
    FOR i := 0 TO Grain - 1 DO
      TWord.Shift (full, i + 1 - Grain, right [i]);
      TWord.And (right[i], Target.Word.max, right[i]);
      TWord.Shift (full, i, left [i]);
      TWord.And (left[i], Target.Word.max, left[i]);
    END;
  END Init;

PROCEDURE SetConstrExpr (expr: Expr.T): P =
(* Look through a NamedExpr and then a ConsExpr, for a SetExpr.T.  NIL if not. *)

  VAR strippedExpr: Expr.T;
  BEGIN
    strippedExpr := Expr.StripNamedCons (expr);
    TYPECASE strippedExpr OF
    | NULL => RETURN NIL;
    | P (setExpr) => RETURN setExpr;
    ELSE RETURN NIL;
    END;
  END SetConstrExpr;

(*EXPORTED:*)
PROCEDURE CheckRT
  (expr: Expr.T; VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT) =
  VAR constrExpr: P;
  BEGIN
    constrExpr := SetConstrExpr (expr);
    TYPECASE constrExpr OF
    | NULL =>
    | P(p) =>
      <* ASSERT p.checked *>
      Code := p.RTErrorCode;
      Msg := p.RTErrorMsg;
      RETURN;
    END;
    Msg := NIL;
    Code := CG.RuntimeError.Unknown;
  END CheckRT;

(* Externally dispatched-to: *)
PROCEDURE Use (p: P): BOOLEAN =
  BEGIN
    <* ASSERT p.checked *>
    IF AssignStmt.DoGenRTAbort (p.RTErrorCode) AND Evaluate (p) # NIL THEN
      CG.Comment
        (p.globalOffset, TRUE, "Use of set constructor with bad element: ",
         p.RTErrorMsg);
      CG.Abort (p.RTErrorCode);
      RETURN FALSE;
    ELSE RETURN TRUE;
    END;
  END Use;

BEGIN
END SetExpr.
