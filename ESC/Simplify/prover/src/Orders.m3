(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyight 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Thu Nov 29 12:31:11 PST 2001 by saxe   
        modified on Thu Sep  5 12:18:41 PDT 1996 by detlefs
*)

MODULE Orders;

IMPORT AF, OrdNode, PredSx, Enode, ContextPrivate, Prover,
       MatchingRule, Clause;
IMPORT OrdNodePODiGraph;
IMPORT POEdgeType, Atom, Word, Wr, Stdio, Fmt, Sx, FPrint,
       TextWr, TextRd;
IMPORT AtomRefTbl, RefList, RefRefTbl, AtomSetList;

IMPORT Thread;
<*FATAL Thread.Alerted, Wr.Failure *>
TYPE
  Ord = OBJECT
    g: OrdNodePODiGraph.T;
    gt, ge: Atom.T;
  END (* OBJECT *);

VAR
  symTbl: AtomRefTbl.T;
  ords: RefList.T; (* OF Ord *)

TYPE
  DclAF = AF.T BRANDED OBJECT
    gt, ge: Atom.T;
   OVERRIDES
    assert := DclAssert;
    toSx := DclToSx;
    fingerprint := DclFP;
  END (* OBJECT *);

PROCEDURE DclRelLit(gtSym, geSym: Atom.T): AF.Lit RAISES { Prover.Error } =
  BEGIN
    IF gtSym = geSym THEN 
      RAISE Prover.Error("ORDER of duplicate symbols")
    ELSE
      VAR af := NEW(DclAF, gt := gtSym, ge := geSym).init(); BEGIN
        RETURN NEW(AF.Lit, af := af)
      END (* BEGIN *)
    END (* IF *)
  END DclRelLit;

PROCEDURE DclAssert(self: DclAF; lit: AF.Lit): BOOLEAN =
  VAR ra: REFANY; ord: Ord; BEGIN
    IF lit.sense THEN
      IF symTbl.get(self.gt, ra) THEN
	ord := ra;
	<*ASSERT ord.ge = self.ge *> 
      ELSIF symTbl.get(self.ge, ra) THEN
	ord := ra;
	<*ASSERT ord.gt = self.gt *> 
      ELSE
	VAR g := NEW(OrdNodePODiGraph.T).init(POEdgeType.csr, TRUE);
	    ord := NEW(Ord, g := g, gt := self.gt, ge := self.ge);
	    b: BOOLEAN;
	PROCEDURE DoOrds(e: Enode.T; data: REFANY): BOOLEAN =
	  VAR res: ARRAY [0..1] OF Enode.T; resRest: RefList.T; BEGIN
	    Enode.Args(e, res, resRest);
	    IF resRest # NIL OR res[0] = NIL OR res[1] = NIL THEN
	      RETURN FALSE
	    ELSE
	      ContextPrivate.Propagate(
		  NewEdgeLit(data, res[0], res[1])); <*NOWARN*>
	      RETURN TRUE
	    END (* IF *)
	  END DoOrds;
	BEGIN
	  EVAL symTbl.put(self.gt, ord);
	  EVAL symTbl.put(self.ge, ord);
          ord.g.push();
	  ords := RefList.Cons(ord, ords);
	  UndoStackPush(UndoType.NewOrd);
	  b := Enode.MapOverTruePreds(self.gt, DoOrds, self.gt); <*ASSERT b*>
	  b := Enode.MapOverTruePreds(self.ge, DoOrds, self.ge); <*ASSERT b*>
          (* Propagate a matching rule for 
             "(FORALL (x) (EQ (GE x x) TRUE))".
          *)
          VAR x := Atom.FromText("x");
              vars := RefList.List1(x);
              pat := RefList.List3(self.ge, MatchingRule.pv[0],
                                   MatchingRule.pv[0]);
              pats := RefList.List1(RefList.List1(pat));
              templ := RefList.List3(PredSx.eqSym, pat, Enode.trueSym);
              mr := NEW(MatchingRule.T).init(vars, pats, templ,
                                             unit := TRUE, clausal := TRUE);
              notTempl :=
                  PredSx.Not(
                      RefList.List3(
                          PredSx.forallSym, vars,
                          RefList.List3(PredSx.eqSym,
                                        RefList.List3(self.ge, x, x),
                                        Enode.trueSym)));
          BEGIN
            ContextPrivate.Propagate(
                NEW(AF.Lit, af := NEW(MatchingRule.AtomF).init(
                                                       pos := mr, neg := notTempl)))
          END (* BEGIN *)
	END (* BEGIN *)
      END (* IF *);
      RETURN TRUE
    ELSE
      VAR twr := NEW(TextWr.T).init();
          gt := Atom.ToText(self.gt);
          ge := Atom.ToText(self.ge);
      BEGIN
        Wr.PutText(twr, "(NOT (AND ");

        Wr.PutText(twr, "(FORALL (x) (NOT (EQ (");
        Wr.PutText(twr, gt); Wr.PutText(twr, " x x) |@true|)))");

        Wr.PutText(twr, "(FORALL (x y) (NOT (AND (EQ (");
        Wr.PutText(twr, gt); Wr.PutText(twr, " x y) |@true|) (EQ (");
        Wr.PutText(twr, gt); Wr.PutText(twr, " y x) |@true|))))");

        Wr.PutText(twr, "(FORALL (x y z) (IMPLIES (AND (EQ (");
        Wr.PutText(twr, gt); Wr.PutText(twr, " x y) |@true|) (EQ (");
        Wr.PutText(twr, gt); Wr.PutText(twr, " y z) |@true|)) (EQ (");
        Wr.PutText(twr, gt); Wr.PutText(twr, " x z) |@true|)))");

        Wr.PutText(twr, "(FORALL (x) (EQ (");
        Wr.PutText(twr, ge); Wr.PutText(twr, " x x) |@true|))");

        Wr.PutText(twr, "(FORALL (x y) (IMPLIES (AND (EQ (");
        Wr.PutText(twr, ge); Wr.PutText(twr, " x y) |@true|) (EQ (");
        Wr.PutText(twr, ge); Wr.PutText(twr, " y x) |@true|)) (EQ x y)))");

        Wr.PutText(twr, "(FORALL (x y z) (IMPLIES (AND (EQ (");
        Wr.PutText(twr, ge); Wr.PutText(twr, " x y) |@true|) (EQ (");
        Wr.PutText(twr, ge); Wr.PutText(twr, " y z) |@true|)) (EQ (");
        Wr.PutText(twr, ge); Wr.PutText(twr, " x z) |@true|)))");

        Wr.PutText(twr, "(FORALL (x y) (IMPLIES (EQ (");
        Wr.PutText(twr, gt); Wr.PutText(twr, " x y) |@true|) (EQ (");
        Wr.PutText(twr, ge); Wr.PutText(twr, " x y) |@true|)))");

        Wr.PutText(twr, "(FORALL (x y) (IMPLIES (EQ (");
        Wr.PutText(twr, ge); Wr.PutText(twr, " x y) |@true|) (OR (EQ (");
        Wr.PutText(twr, gt); Wr.PutText(twr, " x y) |@true|) (EQ x y))))");

        VAR sx := Sx.Read(TextRd.New(TextWr.ToText(twr))); BEGIN <*NOWARN*>
          ContextPrivate.Propagate(Clause.CNF(sx)); <*NOWARN*>
          RETURN TRUE
        END (* BEGIN *)
      END (* BEGIN *)
    END (* IF *)
  END DclAssert;

PROCEDURE DclToSx(dcl: DclAF; <*UNUSED*> normForm: BOOLEAN): Sx.T =
  BEGIN RETURN RefList.List3(Atom.FromText("ORDER"), dcl.gt, dcl.ge)
  END DclToSx;

VAR (*CONST*) orderFP: FPrint.T;

PROCEDURE DclFP(dcl: DclAF): FPrint.T =
  BEGIN
    RETURN FPrint.Combine(
               orderFP,
               FPrint.Combine(
                   FPrint.FromText(Atom.ToText(dcl.gt)),
                   FPrint.FromText(Atom.ToText(dcl.ge))))
  END DclFP;

PROCEDURE IsRel(sym: Atom.T): BOOLEAN =
  VAR ra: REFANY; BEGIN
    RETURN symTbl.get(sym, ra)
  END IsRel;

TYPE
  Edge = AF.T BRANDED OBJECT
    fsym: Atom.T;
    on1, on2: OrdNode.T;
   OVERRIDES
    assert := EdgeAssert;
    toSx := EdgeToSx;
  END (* OBJECT *);

  Eq = AF.T BRANDED OBJECT
    ord: Ord;
    on1, on2: OrdNode.T;
   OVERRIDES
    assert := EqAssert;
    toSx := EqToSx;
  END (* OBJECT *);

  UndoType = { Mark, NewOrd, NewOrdNode };
  UndoRec = RECORD type: UndoType; on: OrdNode.T END (* RECORD *);
  UndoStack = REF ARRAY OF UndoRec;

  EdgeTbl = RefRefTbl.Default OBJECT
   OVERRIDES
    keyEqual := EdgeEqual;
    keyHash := EdgeHash;
  END (* OBJECT *);
VAR
  edgeTbl: EdgeTbl;

PROCEDURE EdgeEqual(<*UNUSED*> tbl: EdgeTbl;
                    READONLY edge1RA, edge2RA: REFANY): BOOLEAN =
  VAR edge1: Edge := edge1RA; edge2: Edge := edge2RA; BEGIN
    RETURN edge1.fsym = edge2.fsym AND edge1.on1.e = edge2.on1.e
           AND edge1.on2.e = edge2.on2.e
  END EdgeEqual;

PROCEDURE EdgeHash(<*UNUSED*> tbl: EdgeTbl; READONLY edgeRA: REFANY): Word.T =
  VAR edge: Edge := edgeRA; BEGIN
    RETURN Word.Plus(Atom.Hash(edge.fsym),
                     Word.Plus(edge.on1.e.getId(), edge.on2.e.getId()))
  END EdgeHash;

PROCEDURE NewEdgeLit(fsym: Atom.T; e1, e2: Enode.T;
                     sense := TRUE): AF.Lit RAISES { Prover.Error } =
  VAR ordRA: REFANY; ord: Ord; BEGIN
    IF NOT symTbl.get(fsym, ordRA) THEN
      RAISE Prover.Error("Symbol '" & Atom.ToText(fsym) &
            "' is not a known order symbol")
    ELSE
      ord := ordRA;
      VAR on1, on2: OrdNode.T;
          res: Edge; ra: REFANY;
      BEGIN
        IF e1.getMisc(FALSE) # NIL AND e1.getMisc(FALSE).ordNode # NIL THEN
          on1 := e1.getMisc(FALSE).ordNode
        ELSE
          e1 := Enode.Root(e1);
          IF e1.getMisc(FALSE) # NIL AND e1.getMisc(FALSE).ordNode # NIL THEN
            on1 := e1.getMisc(FALSE).ordNode
          ELSE
            on1 := NEW(OrdNode.T, e := e1);
            e1.getMisc().ordNode := on1;
            UndoStackPush(UndoType.NewOrdNode, on1)
          END (* IF *)
        END (* IF *);
        IF e2.getMisc(FALSE) # NIL AND e2.getMisc(FALSE).ordNode # NIL THEN
          on2 := e2.getMisc(FALSE).ordNode
        ELSE
          e2 := Enode.Root(e2);
          IF e2.getMisc(FALSE) # NIL AND e2.getMisc(FALSE).ordNode # NIL THEN
            on2 := e2.getMisc(FALSE).ordNode
          ELSE
            on2 := NEW(OrdNode.T, e := e2);
            e2.getMisc().ordNode := on2;
            UndoStackPush(UndoType.NewOrdNode, on2)
          END (* IF *)
        END (* IF *);
        res := NEW(Edge, fsym := fsym, on1 := on1, on2 := on2); 
        IF edgeTbl.get(res, ra) THEN
          VAR edge: Edge := ra; BEGIN
            RETURN NEW(AF.Lit, af := edge, sense := sense)
          END (* BEGIN *)
        ELSE
          IF on1.ords = NIL THEN
            on1.ords := NEW(AtomSetList.T).init()
          END (* IF *);
          EVAL on1.ords.insert(ord.ge);
          IF on2.ords = NIL THEN
            on2.ords := NEW(AtomSetList.T).init()
          END (* IF *);
          EVAL on2.ords.insert(ord.ge);
          RETURN NEW(AF.Lit, af := res.init(), sense := sense)
        END (* IF *)
      END
    END (* IF *)
  END NewEdgeLit;

PROCEDURE EdgeAssert(self: Edge; lit: AF.Lit): BOOLEAN =
  BEGIN
    IF lit.sense THEN
      VAR ordRA: REFANY; ord: Ord; b: BOOLEAN;
      PROCEDURE PropNewEdge(on1: OrdNode.T; ev: POEdgeType.T; on2: OrdNode.T) =
        VAR e1 := Enode.Root(on1.e); e2 := Enode.Root(on2.e); BEGIN
          IF Prover.propagateTrace THEN
            Wr.PutText(Stdio.stdout,
              ";   Orders.EdgeAssert.PropNewEdge called\n");
            Wr.Flush(Stdio.stdout)
          END;
          IF ev = POEdgeType.T.EQ THEN
            IF e1 # e2 THEN
              ContextPrivate.Propagate(Enode.NewEq(e1, e2))
            END (* IF *)
          ELSE
            VAR sym: Atom.T; BEGIN
              CASE ev OF <*NOWARN*>
              | POEdgeType.T.GT => sym := ord.gt
              | POEdgeType.T.GE => sym := ord.ge
              END (* CASE *);
              VAR args := Enode.Cons(e1, Enode.Cons(e2, Enode.enil));
                  e := Enode.Cons(Enode.FromSym(sym), args);
              BEGIN
                ContextPrivate.Propagate(Enode.NewEq(e, Enode.eTrue));
                IF sym = ord.gt THEN
                  VAR eGE := Enode.Cons(Enode.FromSym(ord.ge), args); BEGIN
                    ContextPrivate.Propagate(Enode.NewEq(eGE, Enode.eTrue))
                  END (* BEGIN *)
                END (* IF *)
              END (* BEGIN *)
            END (* BEGIN *)
          END (* IF *)
        END PropNewEdge;
      BEGIN
	b := symTbl.get(self.fsym, ordRA); <*ASSERT b*>
	ord := ordRA;
        VAR oldEV, newEV: POEdgeType.T; BEGIN
          IF self.fsym = ord.gt THEN
            IF Enode.Root(self.on1.e) = Enode.Root(self.on2.e) THEN
              RETURN FALSE
            END (* IF *);
            newEV := POEdgeType.T.GT
          ELSE 
            <*ASSERT self.fsym = ord.ge *>
            newEV := POEdgeType.T.GE
          END (* IF *);
          IF ord.g.getEdge(self.on1, self.on2, oldEV) THEN
            newEV := POEdgeType.csr.plus(newEV, oldEV);
            IF newEV = POEdgeType.csr.bottom THEN RETURN FALSE END (* IF *)
          ELSE
            oldEV := POEdgeType.csr.plusIdent
          END (* IF *);
          IF newEV = oldEV THEN
            RETURN TRUE
          ELSE
            IF newEV = POEdgeType.T.GT THEN
              VAR eGE := Enode.Cons(Enode.FromSym(ord.ge),
                                    Enode.Cons(self.on1.e,
                                               Enode.Cons(self.on2.e,
                                                          Enode.enil)));
              BEGIN
                IF Prover.propagateTrace THEN
                  Wr.PutText(Stdio.stdout,
                    ";   Orders.EdgeAssert:  " &
                    "Calling ContextPrivate.Propagate directly\n");
                  Wr.Flush(Stdio.stdout)
                END;
                ContextPrivate.Propagate(Enode.NewEq(eGE, Enode.eTrue))
              END (* BEGIN *)
            END (* IF *);
            RETURN ord.g.addEdgeAndClose(self.on1, newEV, self.on2,
                                         addNodes := TRUE,
                                         edgeChange := PropNewEdge)
          END (* IF *)
        END (* BEGIN *)
      END (* BEGIN *)
    ELSE
      VAR e := Enode.Cons(Enode.FromSym(self.fsym),
                          Enode.Cons(self.on1.e,
                                     Enode.Cons(self.on2.e, Enode.enil)));
      BEGIN
          IF Prover.propagateTrace THEN
            Wr.PutText(Stdio.stdout,
              ";   Orders.EdgeAssert:  " &
              "Calling ContextPrivate.Propagate directly\n");
            Wr.Flush(Stdio.stdout)
          END;
        ContextPrivate.Propagate(Enode.NewEq(e, Enode.eTrue, sense := FALSE));
        RETURN TRUE
      END (* BEGIN *)
    END (* IF *)    
  END EdgeAssert;

PROCEDURE EqAssert(self: Eq; lit: AF.Lit): BOOLEAN =
  BEGIN
    IF lit.sense THEN
      VAR ord := self.ord;
      PROCEDURE PropNewEdge(on1: OrdNode.T; ev: POEdgeType.T; on2: OrdNode.T) =
        VAR e1 := on1.e; e2 := on2.e; BEGIN
          IF Prover.propagateTrace THEN
            Wr.PutText(Stdio.stdout,
              "; Orders.EqAssert.PropNewEdge called\n");
            Wr.Flush(Stdio.stdout)
          END;
          IF ev = POEdgeType.T.EQ THEN
            ContextPrivate.Propagate(Enode.NewEq(e1, e2))
          ELSE
            VAR sym: Atom.T; BEGIN
              CASE ev OF <*NOWARN*>
              | POEdgeType.T.GT => sym := ord.gt
              | POEdgeType.T.GE => sym := ord.ge
              END (* CASE *);
              VAR args := Enode.Cons(e1, Enode.Cons(e2, Enode.enil));
                  e := Enode.Cons(Enode.FromSym(sym), args);
              BEGIN
                ContextPrivate.Propagate(Enode.NewEq(e, Enode.eTrue));
                IF sym = ord.gt THEN
                  VAR eGE := Enode.Cons(Enode.FromSym(ord.ge), args); BEGIN
                    ContextPrivate.Propagate(Enode.NewEq(eGE, Enode.eTrue))
                  END (* BEGIN *)
                END (* IF *)
              END (* BEGIN *)
            END (* BEGIN *)
          END (* IF *)
        END PropNewEdge;
(*
      PROCEDURE PropNewEdge(on1: OrdNode.T; ev: POEdgeType.T; on2: OrdNode.T) =
        VAR e1 := on1.e; e2 := on2.e; BEGIN
          IF ev = POEdgeType.T.EQ THEN
            ContextPrivate.Propagate(Enode.NewEq(e1, e2))
          ELSE
            VAR sym: Atom.T; BEGIN
              IF ev = POEdgeType.T.GT THEN
                sym := ord.gt
              ELSE
                <*ASSERT ev = POEdgeType.T.GE *>
                sym := ord.ge
              END (* IF *);
              VAR e := Enode.Cons(Enode.FromSym(sym),
                                  Enode.Cons(e1, Enode.Cons(e2, Enode.enil)));
              BEGIN
                ContextPrivate.Propagate(Enode.NewEq(e, Enode.eTrue))
              END (* BEGIN *)
            END (* BEGIN *)
          END (* IF *)
        END PropNewEdge;
*)
      BEGIN
        RETURN ord.g.addEdgeAndClose(self.on1, POEdgeType.T.EQ, self.on2,
                                     addNodes := TRUE, edgeChange := PropNewEdge)

      END (* BEGIN *)      
    ELSE
      IF Prover.propagateTrace THEN
        Wr.PutText(Stdio.stdout,
          ";   Orders.EqAssert:  Calling ContextPrivate.Propagate directly\n");
        Wr.Flush(Stdio.stdout)
      END;
      ContextPrivate.Propagate(
          Enode.NewEq(self.on1.e, self.on2.e, sense := FALSE));
      RETURN TRUE
    END (* IF *)
  END EqAssert;

PROCEDURE PropEqs(on1, on2: OrdNode.T) =
  VAR intersect := on1.ords.intersection(on2.ords);
      iter := intersect.iterate(); sym: Atom.T;
  BEGIN
    BEGIN
      WHILE iter.next(sym) DO
        VAR ord: REFANY; b: BOOLEAN; BEGIN
          b := symTbl.get(sym, ord); <*ASSERT b*>
          ContextPrivate.Propagate(
              NEW(AF.Lit,
                  af := NEW(Eq, ord := ord, on1 := on1, on2 := on2).init()));
          (* It seems necessary to propagate ordering equalities in both
             directions: *)
          ContextPrivate.Propagate(
              NEW(AF.Lit,
                  af := NEW(Eq, ord := ord, on1 := on2, on2 := on1).init()))
        END (* BEGIN *)
      END (* WHILE *)
    END (* BEGIN *)
  END PropEqs;

VAR undoSP: CARDINAL;
    undoStack: UndoStack;

PROCEDURE UndoStackPush(type: UndoType; on: OrdNode.T := NIL) =
  BEGIN
    IF undoSP = NUMBER(undoStack^) THEN
      VAR new := NEW(REF ARRAY OF UndoRec, 2*undoSP); BEGIN
        SUBARRAY(new^, 0, undoSP) := undoStack^;
        undoStack := new
      END (* BEGIN *);
    END (* IF *);
    undoStack[undoSP] := UndoRec{type := type, on := on};
    INC(undoSP)
  END UndoStackPush;

PROCEDURE Push() =
  VAR ords2 := ords; BEGIN
    WHILE ords2 # NIL DO
      VAR ord: Ord := ords2.head; BEGIN ord.g.push() END (* BEGIN *);
      ords2 := ords2.tail
    END (* WHILE *);
    UndoStackPush(UndoType.Mark)
  END Push;

PROCEDURE Pop() =
  BEGIN
    VAR ords2 := ords; BEGIN
      WHILE ords2 # NIL DO
        VAR ord: Ord := ords2.head; BEGIN ord.g.pop() END (* BEGIN *);
        ords2 := ords2.tail
      END (* WHILE *)
    END (* BEGIN *);
    LOOP
      IF undoSP < NUMBER(undoStack^) THEN
        undoStack[undoSP].on := NIL
      END (* IF *);
      DEC(undoSP);
      WITH top = undoStack[undoSP] DO
        CASE top.type OF
        | UndoType.Mark =>
            EXIT
        | UndoType.NewOrd =>
            VAR ord: Ord := ords.head; ra: REFANY; BEGIN
              EVAL symTbl.delete(ord.gt, ra);
              EVAL symTbl.delete(ord.ge, ra)
            END (* BEGIN *);
            ords := ords.tail
        | UndoType.NewOrdNode =>
            VAR m := top.on.e.getMisc(FALSE); BEGIN
              (* This test is necessary because the creation of the
                 misc record might be undone before this. *)
              IF m # NIL THEN m.ordNode := NIL END (* IF *)
            END (* BEGIN *);
            top.on.e := NIL
        END (* CASE *)
      END (* WITH *)
    END (* LOOP *);
    <*ASSERT symTbl.size() = RefList.Length(ords)*2 *>
  END Pop;

PROCEDURE Top(): RefList.T =
  VAR ords2 := ords; res: RefList.T := NIL; BEGIN
    WHILE ords2 # NIL DO
      TopForOrd(ords2.head, res);
      ords2 := ords2.tail
    END (* WHILE *);
    RETURN res
  END Top;

CONST EdgeValVal = ARRAY POEdgeType.T OF [0..2]{2, 1, 0, 0, 0};

PROCEDURE TopForOrd(ord: Ord; VAR res: RefList.T) =
  PROCEDURE DoEdge(tail: OrdNode.T; ev: POEdgeType.T; head: OrdNode.T) =
    VAR tailSuccs := ord.g.getSuccList(tail); <*NOWARN*>
        evVal := EdgeValVal[ev];
        keep := TRUE;
    BEGIN
      VAR p := tailSuccs; BEGIN
	WHILE p # NIL DO
	  VAR edge: OrdNodePODiGraph.Edge := p.head; BEGIN
	    edge.to.value.mark := EdgeValVal[edge.value]
	  END (* BEGIN *);
	  p := p.tail
	END (* WHILE *)
      END (* BEGIN *);
      VAR headPreds := ord.g.getPredList(head); <*NOWARN*>
          p := headPreds;
      BEGIN
        WHILE p # NIL AND keep DO
          VAR edge: OrdNodePODiGraph.Edge := p.head; BEGIN
            keep := edge.from.value.mark * EdgeValVal[edge.value] < evVal
          END (* BEGIN *);
          p := p.tail
        END (* WHILE *)
      END (* BEGIN *);
      VAR p := tailSuccs; BEGIN
	WHILE p # NIL DO
	  VAR edge: OrdNodePODiGraph.Edge := p.head; BEGIN
	    edge.to.value.mark := 0
	  END (* BEGIN *);
	  p := p.tail
	END (* WHILE *)
      END (* BEGIN *);
      IF keep THEN
        VAR sym: Atom.T; BEGIN
          CASE ev OF <*NOWARN*>
          | POEdgeType.T.GT => sym := ord.gt
          | POEdgeType.T.GE => sym := ord.ge
          ELSE (* skip *)
          END (* CASE *);
          res := RefList.Cons(RefList.List3(
                                  PredSx.eqSym,
                                  RefList.List3(sym,
                                                Enode.ToSx(tail.e),
                                                Enode.ToSx(head.e)),
                                  Enode.trueSym),
                              res)
        END (* BEGIN *)
      END (* IF *)
    END DoEdge;
  BEGIN
    ord.g.mapOverEdges(DoEdge) <*NOWARN*>
  END TopForOrd;

PROCEDURE Init() =
  BEGIN
    edgeTbl := NEW(EdgeTbl).init();
    undoSP := 0;
    undoStack := NEW(UndoStack, 100);
    VAR ords2 := ords; BEGIN
      WHILE ords2 # NIL DO
        VAR ord: Ord := ords2.head; BEGIN
          ord.g := NEW(OrdNodePODiGraph.T).init(POEdgeType.csr, TRUE)
        END (* BEGIN *);
        ords2 := ords2.tail
      END (* WHILE *)
    END (* BEGIN *)
  END Init;

PROCEDURE Init0() =
  BEGIN
    symTbl := NEW(AtomRefTbl.Default).init();
    ords := NIL
  END Init0;

PROCEDURE EdgeToSx(self: Edge; normForm: BOOLEAN): REFANY =
  BEGIN
    IF normForm THEN
      RETURN RefList.List3(self.fsym,
                           Enode.ToSx(self.on1.e),
                           Enode.ToSx(self.on2.e))
    ELSE
      RETURN RefList.List3(self.fsym,
                           Enode.DbgToSx(self.on1.e),
                           Enode.DbgToSx(self.on2.e))
    END (* IF *)
  END EdgeToSx;

PROCEDURE EqToSx(self: Eq; normForm: BOOLEAN): REFANY =
  BEGIN 
    IF normForm THEN
      RETURN RefList.List3(RefList.List2(self.ord.gt, PredSx.eqSym),
                           Enode.ToSx(self.on1.e),
                           Enode.ToSx(self.on2.e))
    ELSE
      RETURN RefList.List3(RefList.List2(self.ord.gt, PredSx.eqSym),
                           Enode.DbgToSx(self.on1.e),
                           Enode.DbgToSx(self.on2.e))
    END (* IF *)
  END EqToSx;

(* Printing a graph. *)
PROCEDURE PrintOrdNode(wr: Wr.T; on: OrdNode.T; width: CARDINAL) =
  BEGIN
    Wr.PutText(wr, Fmt.Pad(Fmt.Int(on.e.getId()), width))
  END PrintOrdNode;

PROCEDURE PrintPOEdge(wr: Wr.T; <*UNUSED*> edgeExists: BOOLEAN; 
                      ev: POEdgeType.T; width: CARDINAL) =
  VAR t: TEXT; BEGIN
    CASE ev OF <*NOWARN*>
    | POEdgeType.T.GT => t := ">"
    | POEdgeType.T.GE => t := ">="
    | POEdgeType.T.EQ => t := "="
    | POEdgeType.T.Absent => t := ""
    END (* CASE *);
    Wr.PutText(wr, Fmt.Pad(t, width))
  END PrintPOEdge;

PROCEDURE PrintAll() =
  VAR ords2 := ords;  BEGIN
    Wr.PutText(Stdio.stdout, "\n");
    WHILE ords2 # NIL DO
      VAR ord: Ord := ords2.head; BEGIN
        PrintGraph(ord.g);
        Wr.PutText(Stdio.stdout, "\n");
        Wr.Flush(Stdio.stdout);
      END (* BEGIN *);
      ords2 := ords2.tail
    END (* WHILE *)
  END PrintAll;

PROCEDURE PrintGraph(g: OrdNodePODiGraph.T) =
  BEGIN
    g.printAsMatrix(Stdio.stdout, PrintOrdNode, PrintPOEdge, 2, 4,
                    POEdgeType.T.Absent);
    Wr.Flush(Stdio.stdout)
  END PrintGraph;

BEGIN
  PredSx.Init();
  orderFP := FPrint.FromText(Atom.ToText(PredSx.orderSym));
END Orders.
