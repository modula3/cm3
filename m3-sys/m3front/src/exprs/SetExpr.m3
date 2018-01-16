(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SetExpr.m3                                            *)
(* Last modified on Fri Feb 24 16:47:18 PST 1995 by kalsow     *)
(*      modified on Thu May 20 08:20:18 PDT 1993 by muller     *)

MODULE SetExpr;
(* For set constructors. *) 

IMPORT M3, CG, Expr, ExprRep, Type, Error, IntegerExpr, EnumExpr;
IMPORT RangeExpr, KeywordExpr, SetType, AssignStmt, CheckExpr;
IMPORT M3ID, Target, TInt, TWord, Bool, M3Buf, Module, Text;

TYPE
  Node = REF RECORD
    next : Node;
    min  : INTEGER;
    max  : INTEGER;
  END;

TYPE
  P = Expr.T OBJECT
        tipe    : Type.T;
        args    : Expr.List;
        mapped  : BOOLEAN;
        tree    : Node;
        others  : Expr.List;
        nOthers : INTEGER;
        tmp     : CG.Var;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := Fold;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := IsZeroes;
        genFPLiteral := GenFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
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

PROCEDURE New (type: Type.T;  args: Expr.List): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.type    := type;
    p.tipe    := type;
    p.args    := args;
    p.mapped  := FALSE;
    p.tree    := NIL;
    p.others  := NIL;
    p.nOthers := -1;
    p.tmp     := NIL;
    RETURN p;
  END New;

PROCEDURE NewFromTree (p: P;  node: Node): Expr.T =
  VAR c: P;
  BEGIN
    c := NEW (P);
    c.origin  := p.origin;
    c.type    := p.type;
    c.checked := p.checked;
    c.tipe    := p.tipe;
    c.args    := p.args;
    c.mapped  := TRUE;
    c.tree    := NormalizeTree (node);
    c.others  := NIL;
    p.nOthers := -1;
    RETURN c;
  END NewFromTree;

PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    RETURN (TYPECODE (e) = TYPECODE (P));
  END Is;

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
      IF (elt = NIL) THEN
        (* not a constant *)
        AddOther (p, p.args[i]);
      ELSIF IntegerExpr.Split (elt, from, t)
        OR EnumExpr.Split (elt, from, t) THEN
        IF TInt.LT (from, min) OR TInt.LT (max, from) THEN
          Error.Warn (2, "set element out of range");
          AddOther (p, elt);
        ELSIF TInt.ToInt (from, iFrom) THEN
          p.tree := AddNode (p.tree, iFrom, iFrom);
        ELSE (* treat big values as non-constants *)
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
            Error.Warn (2, "set element out of range");
            AddOther (p, elt);
          ELSIF TInt.ToInt (from, iFrom) AND TInt.ToInt (to, iTo) THEN
            p.tree := AddNode (p.tree, iFrom, iTo);
          ELSE (* since the values are so big, treat them as non-constants *)
            AddOther (p, elt);
          END;
        ELSE (* not a constant range *)
          AddOther (p, elt);
        END;
      ELSE
        Error.Warn (2, "set element is not an ordinal");
        AddOther (p, elt);
      END;
    END;
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

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR
    t, range   : Type.T;
    minT, maxT : Target.Int;
    minE, maxE : Target.Int;
    min, max   : Expr.T;
    e, value   : Expr.T;
    key        : M3ID.T;
  BEGIN
    p.tipe := Type.Check (p.tipe);
    FOR i := 0 TO LAST (p.args^) DO Expr.TypeCheck (p.args[i], cs) END;
    p.type := p.tipe;
    IF NOT SetType.Split (p.tipe, range) THEN
      Error.Msg ("set constructor must specify a set type");
      RETURN;
    END;
    EVAL Type.GetBounds (range, minT, maxT);
    FOR i := 0 TO LAST (p.args^) DO
      e := p.args[i];
      t := Expr.TypeOf (e);

      IF KeywordExpr.Split (e, key, value) THEN
        Error.Msg ("keyword values not allowed in set constructors");
        e := value;
        p.args[i] := value;
      END;

      IF RangeExpr.Split (e, min, max) THEN
        AssignStmt.Check (range, min, cs);
        AssignStmt.Check (range, max, cs);
        min := Expr.ConstValue (min);
        max := Expr.ConstValue (max);
      ELSE (* single value *)
        AssignStmt.Check (range, e, cs);
        min := Expr.ConstValue (e);
        max := min;
      END;

      IF (min # NIL) AND (max # NIL)
        AND (IntegerExpr.Split (min, minE, t)
             OR EnumExpr.Split (min, minE, t))
        AND (IntegerExpr.Split (max, maxE, t)
             OR EnumExpr.Split (max, maxE, t))
        THEN
        IF TInt.LT (minE, minT) OR TInt.LT (maxT, maxE) THEN
          Error.Msg ("illegal set value");
        END;
      END;

    END;
  END Check;

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

PROCEDURE NeedsAddress (<*UNUSED*> p: P) =
  BEGIN
    (* yep, all sets get memory addresses *)
  END NeedsAddress;

PROCEDURE Prep (p: P) =
  VAR info: Type.Info;
  BEGIN
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
      p.tmp := CompileBig (p, info);
    END;
  END Prep;

PROCEDURE Compile (p: P) =
  VAR info: Type.Info;  offset: INTEGER;
  BEGIN
    EVAL Type.CheckInfo (p.tipe, info);
    IF (info.size <= Target.Integer.size) THEN
      CompileSmall (p, info);
    ELSIF (p.nOthers <= 0) THEN
      (* large, constant set *)
      offset := Module.Allocate (info.size, info.alignment, TRUE, "*set*");
      GenLiteral (p, offset, p.tipe, TRUE);
      CG.Load_addr_of (Module.GlobalData (TRUE), offset, info.alignment);
    ELSE
      CG.Load_addr_of_temp (p.tmp, 0, Target.Integer.align);
      p.tmp := NIL;
    END;
  END Compile;

PROCEDURE CompileBig (p: P;  VAR info: Type.Info): CG.Var =
  VAR
    range        : Type.T;
    w1, w2       : INTEGER;
    b1, b2       : INTEGER;
    minT, maxT   : INTEGER;
    min_T, max_T : Target.Int;
    min, max     : Expr.T;
    e            : Expr.T;
    t1           : CG.Var;
    nWords       : INTEGER;
    curWord      : INTEGER;
    curMask      : Target.Int;
    n            : Node;
    b            : BOOLEAN;
    tmp          : Target.Int;
    buf: ARRAY [0..TInt.Size] OF CHAR;
  BEGIN
    b := SetType.Split (p.tipe, range); <* ASSERT b *>
    EVAL Type.GetBounds (range, min_T, max_T);
    IF NOT TInt.ToInt (min_T, minT)
        OR NOT TInt.ToInt (max_T, maxT) THEN
      Error.Msg ("set domain too large (" &
        Text.FromChars (SUBARRAY(buf, 0, TInt.ToChars(min_T, buf))) & ".." &
        Text.FromChars (SUBARRAY(buf, 0, TInt.ToChars(max_T, buf))) & ")");
      minT := FIRST (INTEGER);
      maxT := LAST (INTEGER);
    END;

    nWords := info.size DIV Grain;
    t1 := CG.Declare_temp (nWords * Grain, Target.Integer.align,
                           CG.Type.Struct, in_memory := TRUE);

    (* generate the constant words *)
    n := p.tree;  curWord := 0;   curMask := TInt.Zero;
    WHILE (n # NIL) DO
      w1 := (n.min - minT) DIV Grain; 
      b1 := (n.min - minT) MOD Grain;
      w2 := (n.max - minT) DIV Grain;
      b2 := (n.max - minT) MOD Grain;
      IF (w1 # curWord) THEN
        EmitAssign (t1, curWord, curMask);
        FOR i := curWord+1 TO w1-1 DO  EmitAssign (t1, i, TInt.Zero) END;
        curWord := w1;
        curMask := TInt.Zero;
      END;
      IF (w1 # w2) THEN
        TWord.Or (curMask, left [b1], tmp);
        EmitAssign (t1, w1, tmp);
        FOR i := w1 + 1 TO w2 - 1 DO  EmitAssign (t1, i, full)  END;
        curWord := w2;
        curMask := right [b2];
      ELSE (* x = y *)
        TWord.And (left [b1], right[b2], tmp);
        TWord.Or (curMask, tmp, curMask);
      END;
      n := n.next;
    END; (* while *)

    (* write the last mask *)
    EmitAssign (t1, curWord, curMask);

    (* write zeros for the remainder of the set *)
    FOR i := curWord+1 TO nWords-1 DO EmitAssign (t1, i, TInt.Zero) END;

    (* finally, add the non-constant elements *)
    FOR i := 0 TO p.nOthers-1 DO
      e := p.others[i];
      IF RangeExpr.Split (e, min, max) THEN
        CG.Load_addr_of (t1, 0, Target.Integer.align);
        CG.Force ();
        GenElement (min, min_T, max_T);
        GenElement (max, min_T, max_T);
        CG.Set_range (info.size);
      ELSE (* single value *)
        CG.Load_addr_of (t1, 0, Target.Integer.align);
        GenElement (e, min_T, max_T);
        CG.Set_singleton (info.size);
      END;
    END;

    RETURN t1;
  END CompileBig;

PROCEDURE EmitAssign (set: CG.Var;  index: INTEGER; 
                      READONLY value: Target.Int) =
  VAR tmp: Target.Int;
  BEGIN
    EVAL TInt.Extend (value, Target.Integer.bytes, tmp);
    CG.Load_integer (Target.Integer.cg_type, tmp);
    CG.Store_int (Target.Integer.cg_type, set, index * Grain);
    <* ASSERT Grain = Target.Integer.size *>
  END EmitAssign;

PROCEDURE GenElement (e: Expr.T;  READONLY min, max: Target.Int) =
  VAR cg_type := Type.CGType (Type.Base (Expr.TypeOf (e)));
  BEGIN
    CheckExpr.EmitChecks (e, min, max, CG.RuntimeError.ValueOutOfRange);
    IF NOT TInt.EQ (min, TInt.Zero) THEN
      CG.Load_integer (cg_type, min);
      CG.Subtract (cg_type);
    END;
    CG.Loophole (cg_type, Target.Integer.cg_type);
  END GenElement;

PROCEDURE CompileSmall (p: P;  VAR info: Type.Info) =
  VAR
    range        : Type.T;
    b1, b2       : INTEGER;
    minT, maxT   : INTEGER;
    min_T, max_T : Target.Int;
    min, max     : Expr.T;
    e            : Expr.T;
    nWords       : INTEGER;
    curMask      : Target.Int;
    n            : Node;
    b            : BOOLEAN;
    tmp          : Target.Int;
    buf: ARRAY [0..TInt.Size] OF CHAR;
  BEGIN
    Type.Compile (p.tipe);
    nWords  := info.size DIV Grain;
    <*ASSERT info.size <= Target.Integer.size *>
    <*ASSERT nWords <= 1 *>

    b := SetType.Split (p.tipe, range); <* ASSERT b *>
    EVAL Type.GetBounds (range, min_T, max_T);
    IF NOT TInt.ToInt (min_T, minT)
        OR NOT TInt.ToInt (max_T, maxT) THEN
      Error.Msg ("set domain too large (" &
        Text.FromChars (SUBARRAY(buf, 0, TInt.ToChars(min_T, buf))) & ".." &
        Text.FromChars (SUBARRAY(buf, 0, TInt.ToChars(max_T, buf))) & ")");
      minT := FIRST (INTEGER);
      maxT := LAST (INTEGER);
    END;

    (* generate the constant words *)
    n := p.tree;  curMask := TInt.Zero;
    WHILE (n # NIL) DO
      b1 := (n.min - minT);
      b2 := (n.max - minT);
      TWord.And (left [b1], right[b2], tmp);
      TWord.Or (curMask, tmp, curMask);
      n := n.next;
    END; (* while *)

    (* push the mask *)
    CG.Load_integer (Target.Integer.cg_type, curMask);
    CG.Force ();

    (* finally, add the non-constant elements *)
    FOR i := 0 TO p.nOthers-1 DO
      e := p.others[i];
      IF RangeExpr.Split (e, min, max) THEN
        GenElement (min, min_T, max_T);
        GenElement (max, min_T, max_T);
        CG.Set_range (info.size);
      ELSE (* single value *)
        GenElement (e, min_T, max_T);
        CG.Set_singleton (info.size);
      END;
    END;
  END CompileSmall;

PROCEDURE Fold (e: Expr.T): Expr.T =
  VAR p: P;
  BEGIN
    IF BuildMap (e, p)
      THEN RETURN e;
      ELSE RETURN NIL;
    END;
  END Fold;

PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN (p.args = NIL) OR (NUMBER (p.args^) <= 0);
  END IsZeroes;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "SET<");
    FOR i := 0 TO LAST (p.args^) DO
      IF (i > 0) THEN M3Buf.PutChar (buf, ',') END;
      Expr.GenFPLiteral (p.args[i], buf);
    END;
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  <*UNUSED*> type: Type.T;  is_const: BOOLEAN) =
  VAR
    range        : Type.T;
    min_T, max_T : Target.Int;
    minT, maxT   : INTEGER;
    w1, w2       : INTEGER;
    b1, b2       : INTEGER;
    curWord      : INTEGER;
    curMask      : Target.Int;
    n            : Node;
    b            : BOOLEAN;
    tmp          : Target.Int;
  BEGIN
    b := SetType.Split (p.tipe, range);  <*ASSERT b*>
    EVAL Type.GetBounds (range, min_T, max_T);
    IF NOT TInt.ToInt (min_T, minT)
      OR NOT TInt.ToInt (max_T, maxT) THEN
      Error.Msg ("set constant's domain is too large");
      minT := FIRST (INTEGER);
      maxT := LAST (INTEGER);
    END;

    EVAL BuildMap (p, p);
    <* ASSERT p.others = NIL *>

    n := p.tree;  curWord := 0;   curMask := TInt.Zero;
    WHILE (n # NIL) DO
      w1 := (n.min - minT) DIV Grain;
      b1 := (n.min - minT) MOD Grain;
      w2 := (n.max - minT) DIV Grain;
      b2 := (n.max - minT) MOD Grain;
      IF (w1 # curWord) THEN
        (* write the mask we've accumulated *)
        IF NOT TInt.EQ (curMask, TInt.Zero) THEN
          EVAL TInt.Extend (curMask, Target.Integer.bytes, tmp);
          CG.Init_int (offset + curWord*Target.Integer.pack,
                        Target.Integer.size, tmp, is_const);
        END;
        curWord := w1;
        curMask := TInt.Zero;
      END;
      IF (w1 # w2) THEN
        (* write the full words [w1..w2-1] *)
        TWord.Or (curMask, left [b1], tmp);
        EVAL TInt.Extend (tmp, Target.Integer.bytes, tmp);
        CG.Init_int (offset + w1 * Target.Integer.pack, Target.Integer.size,
                     tmp, is_const);
        FOR i := w1 + 1 TO w2 - 1 DO
          EVAL TInt.Extend (full, Target.Integer.bytes, tmp);
          CG.Init_int (offset + i * Target.Integer.pack, Target.Integer.size,
                       tmp, is_const);
        END;
        curWord := w2;
        curMask := right [b2];
      ELSE
        TWord.And (left [b1], right[b2], tmp);
        TWord.Or (curMask, tmp, curMask);
      END;
      n := n.next;
    END;

    (* write the last mask *)
    IF NOT TInt.EQ (curMask, TInt.Zero) THEN
      EVAL TInt.Extend (curMask, Target.Integer.bytes, tmp);
      CG.Init_int (offset + curWord * Target.Integer.pack,
                   Target.Integer.size, tmp, is_const);
    END;
  END GenLiteral;

PROCEDURE Init () =
  BEGIN
    Grain := Target.Integer.size;
    TWord.Not (TInt.Zero, full);
    TWord.And (full, Target.Word.max, full);
    FOR i := 0 TO Grain - 1 DO
      TWord.Shift (full, i + 1 - Grain, right [i]);
      TWord.And (right[i], Target.Word.max, right[i]);
      TWord.Shift (full, i, left [i]);
      TWord.And (left[i], Target.Word.max, left[i]);
    END;
  END Init;

BEGIN
END SetExpr.
