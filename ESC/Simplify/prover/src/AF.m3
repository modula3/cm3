(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1992 DEC *)
(* Last modified on Thu Nov  2 17:31:56 PST 2000 by saxe    *)
(*      modified on Fri Oct 23 15:14:14 PDT 1998 by gnelson *)
(*      modified on Tue Oct 29 16:56:27 PST 1996 by detlefs *)

MODULE AF;

IMPORT PredSx, Enode, PropVar, RefListMisc, LabelName, Prover;
IMPORT Word, FPrint, Wr, Sx, Integer, Atom, Stdio, Fmt;
IMPORT RefList, RefListSort, AFTabUndoRec, AFTabUndoRecSeq;
IMPORT Simplex;
IMPORT Context; (* for scanGeneration *)

(* For debugging *)
IMPORT Thread, ProxyProp;
<*FATAL Wr.Failure, Thread.Alerted, Sx.PrintError*>

REVEAL
  T = Public BRANDED OBJECT
   OVERRIDES
    init := TInit;
    get := Get;
    set := Set;
    equalInContext := DefEqual;
    hashInContext := DefHash;
    
  END (* OBJECT *);

VAR
  afId: CARDINAL;

PROCEDURE TInit(self: T): T =
  BEGIN
    self.id := afId;
    INC(afId);
    self.tval := TruthVal.Unknown;
    RETURN self
  END TInit;

PROCEDURE Get(self: T): TruthVal =
  BEGIN RETURN self.tval END Get;

PROCEDURE Set(self: T; val, asserted: BOOLEAN) =
  VAR prev := self.tval; BEGIN
    IF val THEN
      <*ASSERT NOT (prev IN TVFalse)*>
      IF asserted THEN
        self.tval := TruthVal.TrueAsserted
      ELSIF prev = TruthVal.Unknown THEN
        self.tval := TruthVal.True
      END (* IF *)
    ELSE
      <*ASSERT NOT (prev IN TVTrue)*>
      IF asserted THEN
        self.tval := TruthVal.FalseAsserted
      ELSIF prev = TruthVal.Unknown THEN
        self.tval := TruthVal.False
      END (* IF *)
    END (* IF *);
    IF prev # self.tval THEN
      undoStack.addhi(AFTabUndoRec.T{self, prev})
    END (* IF *)
  END Set;

CONST TVNot = ARRAY TruthVal OF TruthVal{
  TruthVal.TrueAsserted, TruthVal.True, TruthVal.Unknown, TruthVal.False, TruthVal.FalseAsserted
}; 

PROCEDURE Status(lit: Lit): TruthVal =
  VAR res := lit.af.get(); BEGIN
    INC(stats.totLitStatus);
    CASE res OF
    | TruthVal.Unknown =>
        IF NOT Prover.noEnodeStatus THEN
	  TYPECASE lit.af OF
	  | Enode.Equality(eq) =>
	      res := Enode.Status(eq);
	      CASE res OF
	      | TruthVal.Unknown => (* SKIP *)
	      ELSE
		  INC(stats.litStatusCheap);
		  lit.af.set(res = TruthVal.TrueAsserted, TRUE)
	      END (* CASE *)
	  ELSE
	  END (* TYPECASE *)
        END;
        IF Prover.intStatus THEN
	  TYPECASE lit.af OF
	  | Simplex.GEInequality(gei) =>
	      res := Simplex.GEStatus(gei);
	      CASE res OF
	      | TruthVal.Unknown => (* SKIP *)
	      ELSE
		  INC(stats.litStatusCheap);
		  lit.af.set(res = TruthVal.TrueAsserted, TRUE)
	      END (* CASE *)
	  ELSE
	  END (* TYPECASE *)
        END (* IF *)
    ELSE
        INC(stats.litStatusFree)
    END (* CASE *);
    IF lit.sense THEN RETURN res
    ELSE RETURN TVNot[res]
    END (* IF *)
  END Status;


PROCEDURE DeepStatus(lit: Lit; lo, hi: TruthVal): TruthVal =
  VAR res := AFDeepStatus(lit.af); BEGIN
    IF NOT lit.sense THEN res := TVNot[res] END;
    IF res >= hi THEN RETURN hi END;
    IF res <= lo THEN RETURN lo END;
    RETURN res;
  END DeepStatus;

PROCEDURE AFDeepStatus (af: T): TruthVal =
  BEGIN
    IF af.scanGeneration = Context.scanGeneration OR
         af.tval # TruthVal.Unknown THEN
      RETURN af.tval;
    END;
    af.scanGeneration := Context.scanGeneration;
    TYPECASE af OF
    | Enode.Equality (eq) =>
        IF NOT Prover.noEnodeStatus THEN
          VAR res := Enode.Status(eq); BEGIN
            CASE res OF
            | TruthVal.Unknown => (* SKIP *)
            ELSE
              INC(stats.litStatusCheap);
              af.set(res = TruthVal.TrueAsserted, TRUE)
            END (* CASE *)
          END (* VAR *)
        END; (* IF *)
        RETURN af.tval;
    | ProxyProp.T (p) =>
        VAR
          ll := p.lits;
          minSoFar := TruthVal.True; BEGIN
          WHILE ll # NIL DO
            VAR s := DeepStatus(ll.head, TruthVal.False, TruthVal.True); BEGIN
              minSoFar := MIN(s, minSoFar);
              IF minSoFar = TruthVal.False THEN
                af.set(FALSE, FALSE);
                RETURN TruthVal.False;
              END; (* IF*)
            END; (* VAR *)
          ll := ll.tail;
          END; (* WHILE *)
          IF minSoFar = TruthVal.True THEN
            af.set(TRUE, FALSE)
          END; (* IF *)
          RETURN af.tval;
        END; (* VAR *)
      ELSE
        RETURN af.tval   
    END; (* TYPECASE *)
  END AFDeepStatus;
  


PROCEDURE DefEqual(af1, af2: T): BOOLEAN =
  BEGIN RETURN af1.id = af2.id END DefEqual;

PROCEDURE DefHash(af: T): Word.T =
  BEGIN RETURN af.id END DefHash;

VAR
  undoStack: AFTabUndoRecSeq.T;
  
PROCEDURE Init() =
  BEGIN
    PropVar.Init();
    undoStack := NEW(AFTabUndoRecSeq.T).init(100);
    afId := 0;
    trueAF := PropVar.New(Atom.FromText("TRUE"));
    trueAF.set(TRUE, TRUE);
    trueLit := NEW(Lit, af := trueAF);
    falseLit := NEW(Lit, af := trueAF, sense := FALSE);
    stats := StatRec{}
  END Init;

PROCEDURE Push() =
  BEGIN
    undoStack.addhi(AFTabUndoRec.T{NIL, TruthVal.Unknown}) (* Mark *)
  END Push;

PROCEDURE Pop() =
  BEGIN
    LOOP
      VAR rec := undoStack.remhi(); BEGIN
        IF rec.af = NIL THEN
          RETURN
        ELSE
          rec.af.tval := rec.tv
        END (* IF *)
      END (* BEGIN *)
    END (* LOOP *)
  END Pop;

PROCEDURE Not(lit: Lit): Lit =
  BEGIN
    IF lit.af = trueAF AND lit.sense AND lit.lbls = NIL THEN
      RETURN falseLit
    ELSIF lit.af = trueAF AND NOT lit.sense AND lit.lbls = NIL THEN
      RETURN trueLit
    ELSE RETURN NEW(Lit, af := lit.af,
                    lbls := lit.lbls,
                    sense := NOT lit.sense)
    END (* IF *)
  END Not;

PROCEDURE LitCopy(lit: Lit): Lit =
  BEGIN
    RETURN NEW(Lit, af := lit.af, lbls := lit.lbls, sense := lit.sense)
  END LitCopy;

PROCEDURE LitAddLabel(lit: Lit; lblName: LabelName.T; sense: BOOLEAN) =
  BEGIN
    lit.lbls := RefList.Cons(
                    NEW(Label, sense := sense, name := lblName),
                    lit.lbls);
  END LitAddLabel;

PROCEDURE LitEquiv(lit1, lit2: Lit): BOOLEAN =
  BEGIN
    RETURN lit1.af.id = lit2.af.id AND lit1.sense = lit2.sense
  END LitEquiv;

PROCEDURE LitEqual(lit1, lit2: Lit): BOOLEAN =
  PROCEDURE LabelEqualLocal(READONLY l1, l2: REFANY): BOOLEAN =
    BEGIN RETURN LabelEqual(l1, l2)
    END LabelEqualLocal;
  BEGIN
    RETURN lit1.af = lit2.af AND lit1.sense = lit2.sense AND
           RefListMisc.SetEquiv(lit1.lbls, lit2.lbls, LabelEqualLocal)
  END LitEqual;

PROCEDURE LabelEqual(READONLY lbl1, lbl2: Label): BOOLEAN =
  BEGIN
    RETURN lbl1.sense = lbl2.sense AND LabelName.Equal(lbl1.name, lbl2.name)
  END LabelEqual;

PROCEDURE LitToSx(lit: Lit; normForm := FALSE): PredSx.T =
  VAR res := lit.af.toSx(normForm); BEGIN
    IF NOT lit.sense THEN
      res := RefList.List2(PredSx.notSym, res)
    END (* IF *);
    RETURN res
  END LitToSx;

PROCEDURE PrintLit(wr: Wr.T; lit: Lit; normForm := FALSE) =
  BEGIN
    Sx.Print(wr, LitToSx(lit, normForm));
    Wr.Flush(wr)
  END PrintLit;

PROCEDURE PrintLit2(wr: Wr.T; lit: Lit; sense := TRUE) =
  VAR n := 0; lbls := lit.lbls; BEGIN
    WHILE lbls # NIL DO
      VAR lbl: Label := lbls.head; BEGIN
        IF sense = lit.sense = lbl.sense THEN
          Wr.PutText(wr, "(LBLPOS ")
        ELSE
          Wr.PutText(wr, "(LBLNEG ")
        END (* IF *);
        LabelName.Print(wr, lbl.name);
        Wr.PutText(wr, " ")
      END (* BEGIN *);
      lbls := lbls.tail; INC(n)
    END (* WHILE *);
    TYPECASE lit.af OF
    | ProxyProp.T(pp) =>
        IF sense = lit.sense THEN Wr.PutText(wr, "(AND\n");
        ELSE Wr.PutText(wr, "(OR\n");
        END (* IF *);
        VAR lits := pp.lits; BEGIN
          WHILE lits # NIL DO
            PrintLit2(wr, lits.head, sense = lit.sense);
            lits := lits.tail;
            IF lits # NIL THEN Wr.PutText(wr, "\n") END (* IF *)
          END (* WHILE *)
        END (* BEGIN *);
        Wr.PutText(wr, ")")
    ELSE
        VAR res := lit.af.toSx(FALSE); BEGIN
          IF sense # lit.sense THEN res := PredSx.Not(res) END (* IF *);
          Sx.Print(wr, res)
        END (* BEGIN *)
    END (* TYPECASE *);
    WHILE n > 0 DO Wr.PutText(wr, ")"); DEC(n) END (* WHILE *)
  END PrintLit2;
          
PROCEDURE LitFP(lit: Lit): FPrint.T =
  VAR res := lit.af.fingerprint();
      tc: INTEGER := TYPECODE(lit.af);
  BEGIN
    IF NOT lit.sense THEN tc := -tc END (* IF *);
    RETURN FPrint.Combine(res, FPrint.FromInt(tc))
  END LitFP;

PROCEDURE LitListCopy(lits: RefList.T): RefList.T =
  VAR res: RefList.T := NIL; BEGIN
    WHILE lits # NIL DO
      res := RefList.Cons(LitCopy(lits.head), res); lits := lits.tail
    END (* WHILE *);
    RETURN res
  END LitListCopy;

PROCEDURE LitListSortD(lits: RefList.T): RefList.T =
  BEGIN RETURN RefListSort.SortD(lits, LitCompare) END LitListSortD;

PROCEDURE LitCompare(l1ra, l2ra: REFANY): [-1..1] =
  VAR l1: Lit := l1ra; l2: Lit := l2ra;
      id1 := l1.af.id; id2 := l2.af.id;
  BEGIN
    IF NOT l1.sense THEN id1 := -id1 END (* IF *);
    IF NOT l2.sense THEN id2 := -id2 END (* IF *);
    RETURN Integer.Compare(id1, id2)
  END LitCompare;

TYPE
  StatRec = RECORD 
    totLitStatus,
    litStatusFree,
    litStatusCheap := 0
  END (* RECORD *);

VAR stats: StatRec;

PROCEDURE Stats() =
  BEGIN
    Wr.PutText(Stdio.stdout, "\nChecked status of " &
      Fmt.Int(stats.totLitStatus) & " literals:\n");
    Wr.PutText(Stdio.stdout, "  " &
      Fmt.Int(stats.litStatusFree) &
      " were known immediately, " & Fmt.Int(stats.litStatusCheap) &
      " via egraph check.\n\n");
  END Stats;

BEGIN
END AF.
