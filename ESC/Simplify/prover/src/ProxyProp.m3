(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Sep  8 00:21:38 PDT 1999 by saxe                     *)
(*      modified on Fri Nov  1 14:14:02 PST 1996 by detlefs                  *)

MODULE ProxyProp;

IMPORT AF, Clause, ClausePrivate, ContextPrivate, RefListMisc,
       Prover, LabelName;
IMPORT Wr, Word, Fmt, FPrint, Stdio, Atom, Sx, Env;
IMPORT RefRefTbl, RefSeq, RefList;

(* For debugging *)
IMPORT Thread;
<*FATAL Wr.Failure, Thread.Alerted *>

REVEAL
  T = Public BRANDED OBJECT
    clause: Clause.T := NIL;
    fp := FPrint.Zero;
    root: T;
   METHODS
    init(): T := TInit;
   OVERRIDES
    assert := Assert;
    toSx := ToSx;
    format := Format;
    fingerprint := FP;
  END (* OBJECT *);

TYPE
  ProxyTbl = RefRefTbl.Default OBJECT
   OVERRIDES
    keyHash := LitListSetHash;
    keyEqual := LitListSetEquiv;
  END (* OBJECT *);
  (* A "ProxyTbl" maps lists of "Literal"s (considered as sets) to
     "ProxyPropVar"'s representing their assertion in the positive sense. *)
VAR
  proxyTbl: ProxyTbl;
  undoStack: RefSeq.T;
  flattenHead := NEW(RefList.T, head := NIL, tail := NIL);
     
VAR proxyHits, proxyAttempts: INTEGER;

PROCEDURE Init() =
  BEGIN
    undoStack := NEW(RefSeq.T).init(100);
    proxyTbl := NEW(ProxyTbl).init(100);
    proxyHits := 0; proxyAttempts := 0;
    leftToRightConjs := Env.Get("PROVER_LR_CONJS") # NIL;
    IF leftToRightConjs THEN
      Prover.envVars := Prover.envVars & "PROVER_LR_CONJS\n"
    END (* IF *)
  END Init;

(* The "proxyTbl" maps lists of literals to equivalence classes of
   "T"'s.  All elements of an equivalence class have equivalent
   literal lists, where lists are equivalent if they are equivalent
   as sets, ignoring labels when considering literal equality.  Thus,
   the "T"'s in a class may differ only in the labels of their
   literals.  All "T"'s in an equivalence class have the same "root",
   which is a member of the equivalence class, and have the same "id"
   as the root.
*)


PROCEDURE Push() =
  BEGIN undoStack.addhi(NIL)
  END Push;

PROCEDURE Pop() =
  BEGIN
    LOOP
      VAR lits: RefList.T := undoStack.remhi(); ra: REFANY; BEGIN
        IF lits = NIL THEN RETURN
        ELSE 
          VAR elemsRA: REFANY; elems: RefList.T; b: BOOLEAN; BEGIN
            b := proxyTbl.get(lits, elemsRA); <*ASSERT b*>
            elems := elemsRA; elems := elems.tail;
            IF elems = NIL THEN
              EVAL proxyTbl.delete(lits, ra)
            ELSE
              EVAL proxyTbl.put(lits, elems)
            END (* IF *)
          END (* BEGIN *)
        END (* IF *)
      END (* BEGIN *)
    END (* LOOP *)
  END Pop;

PROCEDURE TInit(self: T): T =
  BEGIN 
    EVAL AF.T.init(self);
    self.root := self;
    RETURN self
  END TInit;

VAR flatten := Env.Get("PROVER_NO_PROXY_FLATTEN") = NIL;

PROCEDURE New(lits: RefList.T): T =
  VAR resRA: REFANY; ppv: T; BEGIN
    INC(proxyAttempts);
    IF flatten THEN
      lits := FlattenLitList(lits)
    END (* IF *);
    IF proxyTbl.get(lits, resRA) THEN
      INC(proxyHits);
      VAR elems: RefList.T := resRA;
          elems2 := elems;
          found := FALSE;
      BEGIN
        WHILE elems2 # NIL AND NOT found DO
          VAR elem: T := elems2.head;
              eLits := elem.lits;
          PROCEDURE LitEqualLocal(READONLY l1, l2: REFANY): BOOLEAN =
            BEGIN RETURN AF.LitEqual(l1, l2)
            END LitEqualLocal;
          BEGIN
            (* The common case is two lists of length 2, pairwise
               equal. *)
            IF (eLits # NIL AND eLits.tail # NIL AND eLits.tail.tail = NIL
                AND lits # NIL AND lits.tail # NIL AND lits.tail.tail = NIL
                AND AF.LitEqual(eLits.head, lits.head)
                AND AF.LitEqual(eLits.tail.head, lits.tail.head)) OR
               RefListMisc.SetEquiv(eLits, lits, LitEqualLocal) THEN
              ppv := elem; found := TRUE
            END (* IF *)
          END (* BEGIN *);
          elems2 := elems2.tail
        END (* WHILE *);
        IF NOT found THEN
          (* Otherwise, add the new element to the class. *)
          ppv := NEW(T, lits := lits);
          ppv.root := NARROW(elems.head, T).root;
          ppv.id := ppv.root.id;
          EVAL proxyTbl.put(lits, RefList.Cons(ppv, elems))
        END (* IF *)
      END (* BEGIN *)
    ELSE
      ppv := NEW(T, lits := lits).init();
      EVAL proxyTbl.put(lits, RefList.List1(ppv));
      undoStack.addhi(lits);
    END (* IF *);
    RETURN ppv
  END New;

PROCEDURE NewLit(lits: RefList.T; sense: BOOLEAN): AF.Lit =
  BEGIN RETURN NEW(AF.Lit, af := New(lits), sense := sense)
  END NewLit;

PROCEDURE FlattenLitList(lits: RefList.T): RefList.T =
  VAR nlits: RefList.T := NIL;
      flattenLast := flattenHead;
      tlits := lits;
  BEGIN
    IF Prover.internDebug THEN
      Wr.PutText(Stdio.stdout, "ProxyProp.FlattenLitList called ...\n");
      Wr.Flush(Stdio.stdout);
    END;
    WHILE tlits # NIL DO
      VAR lit: AF.Lit := tlits.head;
          tlitTail := tlits.tail;
      BEGIN
        tlits.tail := NIL;
        TYPECASE lit.af OF
        | T(ppv) =>
            IF lit.sense THEN
              IF lit.lbls = NIL THEN
(*
                nlits := RefList.Append(nlits, ppv.lits)
*)
                VAR tlits2 := ppv.lits;
                BEGIN
                  WHILE tlits2 # NIL DO
                    flattenLast.tail :=
                      NEW(RefList.T, head := tlits2.head, tail := NIL);
                    flattenLast := flattenLast.tail;
                    tlits2 := tlits2.tail
                  END
                END
              ELSE
                VAR (* nlits2: RefList.T := NIL; *)
                    tlits2 := ppv.lits;
                    split := LabelName.splitCntr;
                    newPosNames: RefList.T := NIL;
                    newNegNames: RefList.T := NIL;
                    lbls := lit.lbls;
                    n := RefList.Length(ppv.lits);
                    i := 1;
                BEGIN
                  INC(LabelName.splitCntr);
                  WHILE lbls # NIL DO
                    VAR lbl: AF.Label := lbls.head; BEGIN
                      IF lbl.sense THEN
                        newPosNames := 
                            RefList.Cons(
                                LabelName.MkOr(lbl.name, split),
                                newPosNames)
                      ELSE
                        newNegNames := 
                            RefList.Cons(
                                LabelName.MkOr(lbl.name, split),
                                newNegNames)
                      END (* IF *)
                    END (* BEGIN *);
                    lbls := lbls.tail
                  END (* WHILE *);
                  WHILE tlits2 # NIL DO
                    VAR tlit: AF.Lit := AF.LitCopy(tlits2.head);
                        tposNames := newPosNames;
                        tnegNames := newNegNames;
                    BEGIN
                      WHILE tposNames # NIL DO
                        VAR tname: LabelName.T := tposNames.head; BEGIN
                          tlit.lbls :=
                              RefList.Cons(
                                  NEW(AF.Label,
                                      sense := tlit.sense,
                                      name := LabelName.MkAnd(tname, i, n)),
                                  tlit.lbls)
                        END (* BEGIN *);
                        tposNames := tposNames.tail
                      END (* WHILE *);
                      WHILE tnegNames # NIL DO
                        tlit.lbls := RefList.Cons(
                                         NEW(AF.Label,
                                             sense := NOT tlit.sense,
                                             name := tnegNames.head),
                                         tlit.lbls);
                        tnegNames := tnegNames.tail
                      END (* WHILE *);
(*
                      nlits2 := RefList.Cons(tlit, nlits2)
*)
                      flattenLast.tail :=
                        NEW(RefList.T, head := tlit, tail := NIL);
                      flattenLast := flattenLast.tail;
                    END (* BEGIN *);
                    tlits2 := tlits2.tail; INC(i)
                  END (* WHILE *);
(*
                  nlits := RefList.Append(nlits, RefList.ReverseD(nlits2))
*)                END (* BEGIN *)
              END (* IF *)
            ELSE
(*
              nlits := RefList.Append(nlits, tlits)
*)
              flattenLast.tail := tlits;
              flattenLast := tlits;
            END (* IF *)
        ELSE
(*
            nlits := RefList.Append(nlits, tlits)
*)
            flattenLast.tail := tlits;
            flattenLast := tlits;
        END (* TYPECASE *);
        tlits := tlitTail
      END (* BEGIN *)
    END (* WHILE *);
    IF Prover.internDebug THEN
      Wr.PutText(Stdio.stdout, "ProxyProp.FlattenLitList ... returning\n");
      Wr.Flush(Stdio.stdout);
    END;
(*
    RETURN nlits
*)
    nlits := flattenHead.tail;
    flattenHead.tail := NIL;
    RETURN nlits
  END FlattenLitList;

PROCEDURE And(l1, l2: AF.Lit): AF.Lit =
  BEGIN
    IF l1.af = AF.trueAF THEN
      IF l1.sense THEN RETURN AddLbls(l2, l1.lbls, TRUE)
      ELSE RETURN l1
      END (* IF *)
    ELSIF l2.af = AF.trueAF THEN
      IF l2.sense THEN RETURN AddLbls(l1, l2.lbls, TRUE)
      ELSE RETURN l2
      END (* IF *)
    ELSE
      RETURN NewLit(RefList.List2(l1, l2), TRUE)
    END (* IF *)
  END And;

PROCEDURE Or(l1, l2: AF.Lit): AF.Lit =
  BEGIN
    IF l1.af = AF.trueAF THEN
      IF l1.sense THEN RETURN l1
      ELSE RETURN AddLbls(l2, l1.lbls, FALSE)
      END (* IF *)
    ELSIF l2.af = AF.trueAF THEN
      IF l2.sense THEN RETURN l2
      ELSE RETURN AddLbls(l1, l2.lbls, FALSE)
      END (* IF *)
    ELSE
      RETURN NewLit(RefList.List2(AF.Not(l1), AF.Not(l2)), FALSE)
    END (* IF *)
  END Or;

(* Adds all the labels in "lbls" whose sense agrees with "sense" to
   the label list of "l", giving them the sense of "l". *)
PROCEDURE AddLbls(l: AF.Lit; lbls: RefList.T (* OF AF.Label *);
                  sense: BOOLEAN): AF.Lit =
  VAR res: AF.Lit := NIL; BEGIN
    WHILE lbls # NIL DO
      VAR lbl: AF.Label := lbls.head; BEGIN
	IF lbl.sense = sense THEN
	  IF res = NIL THEN res := AF.LitCopy(l) END (* IF *);
	  AF.LitAddLabel(res, lbl.name, l.sense)
	END (* IF *)
      END (* BEGIN *);
      lbls := lbls.tail
    END (* WHILE *);
    IF res = NIL THEN res := l END (* IF *);
    RETURN res
  END AddLbls;

VAR leftToRightConjs: BOOLEAN;

PROCEDURE Assert(ppv: T; lit: AF.Lit): BOOLEAN =
  BEGIN
    IF lit.sense THEN
      VAR lits := ppv.lits; BEGIN
        IF leftToRightConjs THEN
          L2RProp(lit, lits)
        ELSE
	  WHILE lits # NIL DO
	    VAR nlit := AF.LitCopy(lits.head); BEGIN
	      nlit.clause := lit.clause;
	      IF lit.rightMost AND lits.tail = NIL THEN
		nlit.rightMost := TRUE
	      END (* IF *);
	      ContextPrivate.Propagate(nlit)
	    END (* BEGIN *);
	    lits := lits.tail
	  END (* WHILE *)
        END (* IF *)
      END (* BEGIN *)
    ELSE
      IF ppv.clause = NIL THEN
        VAR lits := ppv.lits; lits2: RefList.T := NIL; BEGIN
          WHILE lits # NIL DO
            lits2 := RefList.Cons(AF.Not(lits.head), lits2);
            lits := lits.tail
          END (* WHILE *);
          lits2 := RefList.ReverseD(lits2);
          ppv.clause := NEW(Clause.T, lits := lits2).init();
          IF lit.rightMost THEN
            ppv.clause.score := RightMostScore;
            VAR lits2 := ppv.clause.lits; BEGIN
              WHILE lits2 # NIL DO
                NARROW(lits2.head, AF.Lit).rightMost := TRUE;
                lits2 := lits2.tail
              END (* WHILE *)
            END (* BEGIN *)
          END (* IF *);
        END (* BEGIN *)
      END (* IF *);
      ppv.clause.parent := lit.clause;
      ContextPrivate.AddClause(ppv.clause)
    END (* IF *);
    RETURN TRUE
  END Assert;

PROCEDURE L2RProp(lit: AF.Lit; lits: AF.LitList) =
  BEGIN
    IF lits # NIL THEN
      L2RProp(lit, lits.tail);
      VAR nlit := AF.LitCopy(lits.head); BEGIN
        nlit.clause := lit.clause;
        IF lit.rightMost AND lits.tail = NIL THEN
          nlit.rightMost := TRUE
        END (* IF *);
        ContextPrivate.Propagate(nlit)
      END (* BEGIN *)
    END (* IF *)
  END L2RProp;

VAR (*CONST*) proxySym := Atom.FromText("PROXYPROP");

PROCEDURE ToSx(pv: T; <*UNUSED*> normForm: BOOLEAN): REFANY =
  BEGIN RETURN RefList.List2(proxySym, Sx.FromInt(pv.id))
  END ToSx;

PROCEDURE LitListSetHash(<*UNUSED*> pt: ProxyTbl;
                      READONLY litsRA: REFANY): Word.T =
  VAR res := 0; lits: RefList.T := litsRA; BEGIN
    WHILE lits # NIL DO
      VAR lit: AF.Lit := lits.head;
          id := lit.af.id;
      BEGIN
        IF NOT lit.sense THEN id := -id END (* IF *);
        res := Word.Xor(res, id)
      END (* BEGIN *);
      lits := lits.tail
    END (* WHILE *);
    RETURN res
  END LitListSetHash;

PROCEDURE LitListSetEquiv(<*UNUSED*> pt: ProxyTbl;
                          READONLY lits1RA, lits2RA: REFANY): BOOLEAN =
  VAR lits1: AF.LitList := lits1RA;
      lits2: AF.LitList := lits2RA;
  PROCEDURE LitEquivLocal(READONLY l1, l2: REFANY): BOOLEAN =
    BEGIN RETURN AF.LitEquiv(l1, l2)
    END LitEquivLocal;
  BEGIN
    (* The common case is two lists of length 2, pairwise equivalent. *)
    RETURN
      (lits1 # NIL AND lits1.tail # NIL AND lits1.tail.tail = NIL
       AND lits2 # NIL AND lits2.tail # NIL AND lits2.tail.tail = NIL
       AND AF.LitEquiv(lits1.head, lits2.head)
       AND AF.LitEquiv(lits1.tail.head, lits2.tail.head)) OR
       RefListMisc.SetEquiv(lits1, lits2, LitEquivLocal)
  END LitListSetEquiv;

PROCEDURE Format(ppv: T; wr: Wr.T; sense: BOOLEAN; level: CARDINAL) =
  VAR lits := ppv.lits; BEGIN
    IndentTo(wr, level);
    IF sense THEN
      Wr.PutText(wr, "(AND [")
    ELSE
      Wr.PutText(wr, "(OR [")
    END (* IF *);
    Wr.PutText(wr, Fmt.Int(ppv.id) & "]\n");
    WHILE lits # NIL DO
      VAR lit: AF.Lit := lits.head; BEGIN
        IF NOT sense THEN lit := AF.Not(lit) END (* IF *);
        TYPECASE lit.af OF
        | T(ppv2) =>
            ppv2.format(wr, lit.sense, level+1)
        ELSE
            IndentTo(wr, level+1);
            AF.PrintLit(wr, lit);
            Wr.PutText(wr, "\n")
        END (* TYPECASE *)
      END (* BEGIN *);
      lits := lits.tail
    END (* WHILE *);
    IndentTo(wr, level); Wr.PutText(wr, ")\n")
  END Format;

CONST Indent = 2;

PROCEDURE IndentTo(wr: Wr.T; level: CARDINAL) =
  BEGIN
    FOR i := 1 TO level*Indent DO Wr.PutChar(wr, ' ') END (* FOR *)
  END IndentTo;

PROCEDURE FP(ppv: T): FPrint.T =
  BEGIN 
    IF ppv.fp = FPrint.Zero THEN
      ppv.fp := FPWork(ppv)
    END (* IF *);
    RETURN ppv.fp
  END FP;

PROCEDURE FPWork(ppv: T): FPrint.T =
  VAR res := FPrint.Zero;
      lits := ppv.lits;
  BEGIN
    WHILE lits # NIL DO
      res := FPrint.Combine(res, AF.LitFP(lits.head));
      lits := lits.tail
    END (* WHILE *);
    RETURN res
  END FPWork;

PROCEDURE Stats() =
  BEGIN
    Wr.PutText(Stdio.stdout,
               "Got " & Fmt.Int(proxyHits) & " hits out of " &
               Fmt.Int(proxyAttempts) & " in the proxy table.\n")
  END Stats;

PROCEDURE ConjOfNonProxies(p: T): BOOLEAN =
  VAR res := TRUE; lits := p.lits; BEGIN
    WHILE res AND lits # NIL DO
      VAR lit: AF.Lit := lits.head; BEGIN
        TYPECASE lit.af OF
        | T(p2) =>
            res := lit.sense AND ConjOfNonProxies(p2)
        ELSE
        END (* TYPECASE *);
        lits := lits.tail
      END (* BEGIN *)
    END (* WHILE *);
    RETURN res
  END ConjOfNonProxies;

PROCEDURE CNFSize(p: T; sense: BOOLEAN; VAR (*OUT*) c, w: CARDINAL) =
  BEGIN
    IF sense THEN
      VAR lits := p.lits; BEGIN
        c := 0; w := 0;
        WHILE lits # NIL DO
          VAR lit: AF.Lit := lits.head; BEGIN
	    TYPECASE lit.af OF
	    | T(p2) =>
		VAR nc, nw: CARDINAL; BEGIN
		  CNFSize(p2, lit.sense, nc, nw);
		  c := c + nc; w := MAX(w, nw)
		END (* BEGIN *)
	    ELSE
                INC(c); w := MAX(w, 1)
	    END (* TYPECASE *)
          END (* BEGIN *);
          lits := lits.tail
        END (* WHILE *)
      END (* BEGIN *)
    ELSE
      VAR lits := p.lits; BEGIN
        c := 1; w := 0;
        WHILE lits # NIL DO
          VAR lit: AF.Lit := lits.head; BEGIN
	    TYPECASE lit.af OF
	    | T(p2) =>
		VAR nc, nw: CARDINAL; BEGIN
		  CNFSize(p2, NOT lit.sense, nc, nw);
                  (* The MAX below protects against overflow *)
		  c := MAX(c * nc, c); w := w + nw
		END (* BEGIN *)
	    ELSE
		w := w + 1
	    END (* TYPECASE *)
          END (* BEGIN *);
          lits := lits.tail
        END (* WHILE *)
      END (* BEGIN *)
    END (* IF *)
  END CNFSize;


PROCEDURE CNF(p: T; sense: BOOLEAN): RefList.T =
  BEGIN
    IF sense THEN
      VAR res: RefList.T := NIL; lits := p.lits; BEGIN
        WHILE lits # NIL DO
          VAR lit: AF.Lit := lits.head; BEGIN
	    TYPECASE lit.af OF
	    | T(p2) =>
                res := RefList.AppendD(CNF(p2, lit.sense), res)
	    ELSE
		res := RefList.Cons(RefList.List1(lit), res)
	    END (* TYPECASE *);
	    lits := lits.tail
          END (* BEGIN *)
        END (* WHILE *);
        RETURN res
      END (* BEGIN *)
    ELSE
      VAR res := RefList.List1(NIL); lits := p.lits; BEGIN
        WHILE lits # NIL AND res # NIL DO
          VAR lit: AF.Lit := lits.head; BEGIN
	    TYPECASE lit.af OF
	    | T(p2) =>
                res := CNFOr(res, CNF(p2, NOT lit.sense));
	    ELSE
                res := CNFOrLit(res, AF.Not(lit))
	    END (* TYPECASE *)
          END (* BEGIN *);
          lits := lits.tail
        END (* WHILE *);
        RETURN res
      END (* BEGIN *)
    END (* IF *)
  END CNF;

(* Requires "cnf1 # NIL".  Destructively modifies "cnf1". *)
PROCEDURE CNFOr(cnf1, cnf2: RefList.T): RefList.T =
  VAR res: RefList.T := NIL; BEGIN
    IF cnf2 = NIL THEN
      RETURN NIL
    ELSIF cnf2.tail = NIL AND cnf1.tail = NIL THEN
      cnf1.head := RefList.AppendD(cnf1.head, cnf2.head);
      RETURN cnf1
    ELSE
      WHILE cnf1 # NIL DO
	VAR tcnf2 := cnf2; BEGIN
	  WHILE tcnf2 # NIL DO
	    res := RefList.Cons(CNFAppendClauses(cnf1.head, tcnf2.head),
				res);
	    tcnf2 := tcnf2.tail
	  END (* WHILE *)
	END (* BEGIN *);
	cnf1 := cnf1.tail
      END (* WHILE *)
    END (* IF *);
    RETURN res
  END CNFOr;

(* Destructively modifies "cnf1". *)
PROCEDURE CNFOrLit(cnf1: RefList.T; lit: AF.Lit): RefList.T =
  VAR res := cnf1; BEGIN
    WHILE cnf1 # NIL DO
      cnf1.head := RefList.Cons(AF.LitCopy(lit), cnf1.head);
      cnf1 := cnf1.tail
    END (* WHILE *);
    RETURN res
  END CNFOrLit;


PROCEDURE CNFAppendClauses(cl1, cl2: RefList.T): RefList.T =
  BEGIN
    RETURN RefList.AppendD(AF.LitListCopy(cl1), AF.LitListCopy(cl2))
  END CNFAppendClauses;

BEGIN
END ProxyProp.
