(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun  4 12:14:37 PDT 2002 by saxe                         *)
(*      modified on Fri Nov  1 12:00:48 PST 1996 by detlefs                      *)
(*      modified on Thu Mar 21 18:31:12 1996 by gnelson                      *)

UNSAFE MODULE Clause EXPORTS Clause, ClausePrivate;
(* UNSAFE only because of importation of RTHeapRep. *)

IMPORT Context, Enode, PredSx, Simplex, AF,
       MatchingRule, Sx, Prover, PropVar, ProxyProp, LabelName, Orders,
       PredDefs;
IMPORT PredSxList;
IMPORT Atom, Env, Integer, Word, TextWr, FPrint, SxPrint;
       (* RTHeapStats, RTAllocator *)
IMPORT RefList, RefListSort, AtomRefTbl, AtomSet, AtomSetList, AtomSetDef;

(* For debugging. *)
IMPORT Wr, Stdio, Thread, Fmt;
<*FATAL Wr.Failure, Thread.Alerted, Sx.PrintError, Prover.Error*>

REVEAL
  T = ClauseRep BRANDED OBJECT
   OVERRIDES
    init := ClauseInit;
  END (* OBJECT *);

(* If "self" is a "T", "self.pred" and "self.succ" are the predecessor
   and successor nodes in a doubly linked list of "T"s used by
   "Clause.DNF".  If "self.pred" and "self.succ" are NIL, then "self"
   has either been deleted from the list, or is the entire list.  Each
   clause may participate in the definition of some {\it proxy
   propositional variable}; the "nextDef" field, if non-NIL, leads to
   the next clause in a list of all clauses that define the same
   variable. "lits" is a list of the literals in the clause.  If
   "self" was created by the instantiation of some non-unit matching
   rule, "self.mr" is that rule.  "self.score" is incremented every
   time "self" is the last clause case split upon before a
   contradication is detected; "score" is used in a heuristic for
   determining which clauses should be split upon first.  If a clause
   was added to the DNF clause list "l" by assertion of a
   "ProxyPropVar", "parent" is the clause that contained the literal
   containing that "ProxyPropVar".

   If "self" is a "T", "self.init()" ensures that for all literals "l"
   in "self.lits", "l.clause = self".
*)

PROCEDURE CNF(sx: REFANY;
              READONLY sub: MatchingRule.Substitution := MatchingRule.EmptySub;
              rightMost := FALSE): AF.Lit =
  BEGIN
    VAR lit := CNFWork(sx, sub); BEGIN
      lit.rightMost := rightMost;
      RETURN lit
    END (* BEGIN *)
  END CNF;

<*UNUSED*>
PROCEDURE LitPrint(lit: AF.Lit) =
  BEGIN
    TYPECASE lit.af OF
    | ProxyProp.T(ppv) =>
        ppv.format(Stdio.stdout, lit.sense, 0)
    ELSE
        AF.PrintLit(Stdio.stdout, lit)
    END (* TYPECASE *);
    Wr.Flush(Stdio.stdout)
  END LitPrint;

PROCEDURE CNFWork(sx: REFANY; READONLY sub: MatchingRule.Substitution): AF.Lit =
  (* "sx" is required to be a "<formula>".  Return a "AF.Lit"
     equivalent to "sx".
  *)
  BEGIN
    IF Prover.internDebug THEN
      Wr.PutText(Stdio.stdout, "Clause.CNFWork called on: ");
      SxPrint.Print(Stdio.stdout, sx, sub, 4, 4);
      Wr.PutText(Stdio.stdout, "\n");
      Wr.Flush(Stdio.stdout); 
    END (* IF *);
    TYPECASE sx OF
    | Atom.T =>
        RETURN SxToLiteral(sx, sub)
    | RefList.T(l) =>
        VAR head := l.head; BEGIN
          IF head = PredSx.labelSym OR head = PredSx.lblPosSym OR 
            head = PredSx.lblNegSym THEN
            VAR lit := CNFWork(l.tail.tail.head, sub); BEGIN
              RETURN LabelWrap(head, l.tail.head, lit, sub)
            END (* BEGIN *)
          ELSIF head = PredSx.orderSym THEN
            RETURN Orders.DclRelLit(l.tail.head, l.tail.tail.head)
          ELSIF head = PredSx.notSym THEN
            RETURN AF.Not(CNFWork(RefList.Nth(l, 1), sub))
          ELSIF head = PredSx.impliesSym THEN
            RETURN CNFWork(RefList.List3(PredSx.orSym,
                                         PredSx.Not(l.tail.head),
                                         l.tail.tail.head),
                           sub)
          ELSIF head = PredSx.expliesSym THEN
            RETURN CNFWork(RefList.List3(PredSx.orSym,
                                         PredSx.Not(l.tail.tail.head),
                                         l.tail.head),
                           sub)
          ELSIF head = PredSx.iffSym THEN
            VAR x := CNFWork(RefList.Nth(l, 1), sub);
                y := CNFWork(RefList.Nth(l, 2), sub);
            BEGIN
              IF x.af = AF.trueAF THEN
                IF x.sense THEN RETURN y
                ELSE RETURN AF.Not(y)
                END (* IF *)
              ELSIF y.af = AF.trueAF THEN
                IF y.sense THEN RETURN x
                ELSE RETURN AF.Not(x)
                END (* IF *)
              ELSE
                VAR l1 := ProxyProp.NewLit(
                              RefList.List2(x, AF.Not(y)), FALSE);
                    l2 := ProxyProp.NewLit(
                              RefList.List2(y, AF.Not(x)), FALSE);
                BEGIN
                  RETURN ProxyProp.NewLit(RefList.List2(l1, l2), TRUE)
                END (* BEGIN *)
              END (* IF *)
            END (* BEGIN *)
          ELSIF head = PredSx.proofSym THEN
            VAR steps := l.tail;
                conjArgs: RefList.T := NIL;
                antecedent: RefList.T := NIL;
            BEGIN
              WHILE steps.tail # NIL DO
                antecedent := RefList.Cons(steps.head, antecedent);
                conjArgs := RefList.Cons(
                                RefList.List3(
                                    PredSx.impliesSym,
                                    RefList.Cons(PredSx.andSym,
                                                 antecedent),
                                    steps.tail.head),
                                conjArgs);
                steps := steps.tail
              END (* WHILE *);
              RETURN CNFWork(RefList.Cons(PredSx.andSym, conjArgs), sub)
            END (* BEGIN *)
          ELSIF head = PredSx.orSym THEN
            IF RefList.Length(l) = 1 THEN
              RETURN AF.falseLit;
            ELSIF RefList.Length(l) = 2 THEN
              RETURN CNFWork(RefList.Nth(l, 1), sub);
            ELSE
              VAR lits := FormulasToLits(l.tail, sub, FALSE);
              BEGIN
                IF lits = NIL THEN
                  RETURN AF.falseLit
                ELSIF lits.tail = NIL THEN
                  RETURN AF.Not(lits.head)
                ELSE
                  RETURN ProxyProp.NewLit(lits, FALSE)
                END
              END (* BEGIN *)
(*
              VAR x := CNFWork(RefList.Nth(l, 1), sub);
                  y := CNFWork(RefList.Cons(PredSx.orSym, l.tail.tail), sub);
              BEGIN
                RETURN ProxyProp.Or(x, y)
              END (* BEGIN *)
*)
            END (* IF *)
          ELSIF head = PredSx.andSym THEN
            IF RefList.Length(l) = 1 THEN
              RETURN AF.trueLit;
            ELSIF RefList.Length(l) = 2 THEN
              RETURN CNFWork(RefList.Nth(l, 1), sub);
            ELSE
              VAR lits := FormulasToLits(l.tail, sub, TRUE);
              BEGIN
                IF lits = NIL THEN
                  RETURN AF.trueLit
                ELSIF lits.tail = NIL THEN
                  RETURN lits.head
                ELSE
                  RETURN ProxyProp.NewLit(lits, TRUE)
                END
              END (* BEGIN *)
(*
              VAR x := CNFWork(RefList.Nth(l, 1), sub);
                  y := CNFWork(RefList.Cons(PredSx.andSym, l.tail.tail), sub);
              BEGIN
                RETURN ProxyProp.And(x, y)
              END (* BEGIN *)
*)
            END (* IF *)
          ELSE
            RETURN SxToLiteral(sx, sub)
          END (* IF *)
        END (* BEGIN *)
    | AF.Lit(lit) =>
        RETURN lit
    ELSE
        RAISE Prover.Error("Sx of unrecognized type.")
    END (* TYPECASE *)
  END CNFWork;


PROCEDURE FormulasToLits(
            READONLY formulas: RefList.T;
            READONLY sub: MatchingRule.Substitution;
            sense: BOOLEAN): RefList.T =

  VAR rest: RefList.T; 
      trueLits: RefList.T := NIL;
      otherLits: RefList.T := NIL;
      otherLitsLast: RefList.T := NIL;
      lit: AF.Lit;
  BEGIN
    rest := formulas;
    WHILE rest # NIL DO
      lit := CNFWork(rest.head, sub);
      rest := rest.tail;
      IF NOT sense THEN lit := AF.Not(lit)END;
      IF lit.af = AF.trueAF THEN
        IF lit.sense THEN
          IF lit.lbls # NIL THEN
            trueLits := NEW(RefList.T, head := lit, tail := trueLits);
          END
        ELSE
          RETURN NEW(RefList.T, head := lit, tail := NIL);
        END
      ELSE
        IF otherLits = NIL THEN
          otherLits := NEW(RefList.T, head := lit, tail := NIL);
          otherLitsLast := otherLits
        ELSE
          otherLitsLast.tail := NEW(RefList.T, head := lit, tail := NIL);
          otherLitsLast := otherLitsLast.tail
        END
      END
    END;
    IF trueLits # NIL AND otherLits = NIL THEN
      otherLits := trueLits;
      trueLits := trueLits.tail;
      otherLits.tail := NIL
    END;
    WHILE trueLits # NIL DO
      lit := trueLits.head;
      otherLits.head := ProxyProp.AddLbls(otherLits.head, lit.lbls, TRUE);
      trueLits := trueLits.tail
    END;
    RETURN otherLits
  END FormulasToLits;

(*
PROCEDURE FormulasToLits(
            READONLY formulas: RefList.T;
            READONLY sub: MatchingRule.Substitution;
            sense: BOOLEAN): RefList.T 
            RAISES { Prover.Error } =
  VAR first, resthead: AF.Lit; rest: RefList.T;
  BEGIN
    IF formulas = NIL THEN RETURN RefList.Cons(AF.trueLit, NIL) END;
    first := CNFWork(formulas.head, sub);
    IF NOT sense THEN first := AF.Not(first) END;
    IF first.af = AF.trueAF AND NOT first.sense THEN
      RETURN RefList.Cons(first, NIL)
    END;
    rest := FormulasToLits(formulas.tail, sub, sense);
    resthead := rest.head;
    IF first.af = AF.trueAF THEN
      RETURN RefList.Cons(
        ProxyProp.AddLbls(rest.head, first.lbls, TRUE),
        rest.tail)
    ELSIF resthead.af = AF.trueAF THEN
      IF resthead.sense THEN
        RETURN RefList.Cons(
          ProxyProp.AddLbls(first, resthead.lbls, TRUE),
          rest.tail)
      ELSE
        RETURN rest
      END
    ELSE
      IF Prover.internDebug AND
         AF.Status(first) # AF.TruthVal.Unknown THEN
        Wr.PutText(Stdio.stdout,
                   "Clause.FormulasToLits: Missed optimization\n");
        Wr.Flush(Stdio.stdout)
      END;
      RETURN RefList.Cons(first, rest)
    END
  END FormulasToLits;
*)

PROCEDURE LabelWrap(head: Atom.T; lbl: REFANY; lit: AF.Lit;
                    READONLY sub: MatchingRule.Substitution): AF.Lit =
  BEGIN
    IF Prover.noLabels THEN RETURN lit END (* IF *);
    VAR lblName := ParseLabelName(lbl, sub);
        sense := head = PredSx.lblPosSym;
    BEGIN
      IF lit.af = AF.trueAF THEN
        IF lit.sense AND NOT sense THEN
          RETURN AF.trueLit
        ELSIF NOT lit.sense AND sense THEN
          RETURN AF.falseLit
        ELSE
          lit := AF.LitCopy(lit)
        END (* IF *)
      END (* IF *);
      AF.LitAddLabel(lit, lblName, lit.sense = sense);
      RETURN lit
    END (* BEGIN *)
  END LabelWrap;

PROCEDURE ParseLabelName(
    ln: REFANY; READONLY sub: MatchingRule.Substitution): LabelName.T =
  BEGIN
    IF Prover.internDebug THEN
      Wr.PutText(Stdio.stdout,
        "Clause.ParseLabelName: called on ");
      SxPrint.Print(Stdio.stdout, ln);
      Wr.PutText(Stdio.stdout, "\n");
      Wr.Flush(Stdio.stdout);
    END;
    TYPECASE ln OF <*NOWARN*>
    | Atom.T(at) => RETURN LabelName.MkAtom(at)
    | RefList.T(rl) =>
        IF rl.head = PredSx.lblNameQuantSym THEN
          <*ASSERT RefList.Length(rl) = 3 *>
          VAR args: RefList.T := rl.tail.tail.head;
              newArgs: RefList.T := NIL;
          BEGIN
            WHILE args # NIL DO
              TYPECASE args.head OF <*NOWARN*>
              | MatchingRule.PatVar(pv) =>
                  newArgs := RefList.Cons(sub[pv^], newArgs)
              | Enode.T =>
                  newArgs := RefList.Cons(args.head, newArgs)
              END (* TYPECASE *);
              args := args.tail
            END (* WHILE *);
            RETURN LabelName.MkQuant(ParseLabelName(rl.tail.head, sub),
                                     RefList.ReverseD(newArgs))
          END (* BEGIN *)
        ELSIF rl.head = PredSx.lblNameAndSym THEN
          <*ASSERT RefList.Length(rl) = 4 *>
          VAR ln2 := ParseLabelName(rl.tail.head, sub);
              ri: REF INTEGER := rl.tail.tail.head;
              rn: REF INTEGER := rl.tail.tail.tail.head;
          BEGIN
            RETURN LabelName.MkAnd(ln2, ri^, rn^)
          END (* BEGIN *)
        ELSIF rl.head = PredSx.lblNameOrSym THEN
          <*ASSERT RefList.Length(rl) = 3 *>
          VAR ln2 := ParseLabelName(rl.tail.head, sub);
              runiq: REF INTEGER := rl.tail.tail.head;
          BEGIN
            RETURN LabelName.MkOr(ln2, runiq^)
          END (* BEGIN *)
        ELSE
          <*ASSERT FALSE*>
        END (* IF *)
    END (* TYPECASE *)
  END ParseLabelName;


PROCEDURE SxToLiteral(
    sx: REFANY;
    READONLY sub: MatchingRule.Substitution): AF.Lit =
  BEGIN
    IF Prover.internDebug THEN
      Wr.PutText(Stdio.stdout, "Clause.SxToLiteral called on: ");
      SxPrint.Print(Stdio.stdout, sx, sub);
      Wr.PutText(Stdio.stdout, "\n");
      Wr.Flush(Stdio.stdout); 
    END (* IF *);
    TYPECASE sx OF
    | Atom.T(sym) =>
        IF sym = PredSx.trueSym THEN
          RETURN AF.trueLit
        ELSIF sym = PredSx.falseSym THEN
          RETURN AF.falseLit
        ELSE
          RETURN NEW(AF.Lit, af := PropVar.New(sym));
        END (* IF *)
    | RefList.T(l) =>
        <*ASSERT RefList.Length(l) > 0 *>
        VAR head := l.head; BEGIN
          IF head = PredSx.labelSym OR head = PredSx.lblPosSym OR 
            head = PredSx.lblNegSym THEN
            VAR lit := SxToLiteral(l.tail.tail.head, sub); BEGIN
              RETURN LabelWrap(head, l.tail.head, lit, sub)
            END (* BEGIN *)
          ELSIF head = PredSx.notSym THEN
            RETURN AF.Not(SxToLiteral(l.tail.head, sub))
          ELSIF head = PredSx.eqSym THEN
            RETURN Enode.NewEq(Intern(RefList.Nth(l, 1), sub),
                               Intern(RefList.Nth(l, 2), sub),
                               sense := TRUE)
          ELSIF head = PredSx.diffSym THEN
            RETURN Enode.NewEq(Intern(RefList.Nth(l, 1), sub),
                               Intern(RefList.Nth(l, 2), sub),
                               sense := FALSE)
          ELSIF head = PredSx.distClassSym THEN
            VAR trms: RefList.T := NIL; args := l.tail; BEGIN
              WHILE args # NIL DO
                trms := RefList.Cons(Intern(args.head, sub), trms);
                args := args.tail
              END (* WHILE *);
              RETURN Enode.NewDist(trms, TRUE)
            END (* BEGIN *)
          ELSIF head = PredSx.ltSym THEN
            RETURN Simplex.NewGT(Intern(RefList.Nth(l, 2), sub),
                                 Intern(RefList.Nth(l, 1), sub))
          ELSIF head = PredSx.gtSym THEN
            RETURN Simplex.NewGT(Intern(RefList.Nth(l, 1), sub),
                                 Intern(RefList.Nth(l, 2), sub))
          ELSIF head = PredSx.leSym THEN
            RETURN Simplex.NewGE(Intern(RefList.Nth(l, 2), sub),
                                 Intern(RefList.Nth(l, 1), sub))
          ELSIF head = PredSx.geSym THEN
            RETURN Simplex.NewGE(Intern(RefList.Nth(l, 1), sub),
                                 Intern(RefList.Nth(l, 2), sub))
          ELSIF head = PredSx.forallSym THEN
            RETURN LitForUniversalQuant(MatchingRule.OurApplySubst(l, sub))
          ELSIF head = PredSx.existsSym THEN
            VAR qvars := RefList.Nth(l, 1);
                body := RefList.Nth(l, 2);
            BEGIN
              RETURN AF.Not(
                         LitForUniversalQuant(
                             MatchingRule.OurApplySubst(
                                 RefList.List3(PredSx.forallSym,
                                               qvars,
                                               PredSx.Not(body)),
                                 sub)))
            END (* BEGIN *)
          ELSIF ISTYPE(l.head, Atom.T) AND PredDefs.IsPredSym(l.head) THEN
            RETURN Enode.NewEq(Intern(l, sub), Enode.eTrue)
          ELSIF ISTYPE(l.head, Enode.T) AND Enode.IsPredSym(l.head) THEN
            RETURN Enode.NewEq(Intern(l, sub), Enode.eTrue)
          ELSIF l.head = PredSx.selectSym THEN
            TYPECASE l.tail.head OF <*NOWARN*>
            | RefList.T(pm) =>
                IF NOT PredDefs.IsPredMapSym(pm.head) THEN
                  RAISE Prover.Error("'select' used as predicate symbol.")
                END (* IF *);
                RETURN Enode.NewEq(Intern(l, sub), Enode.eTrue)
            END (* TYPECASE *)
          ELSE
            IF ISTYPE(head, Atom.T) THEN
              RAISE Prover.Error(
                        "Atom '" & Atom.ToText(head) &
                        "' is not an operator for a literal.")
            ELSE
              RAISE Prover.Error("List head of unrecognized type in literal.")
            END (* IF *)
          END (* IF *)
        END (* BEGIN *)
    | Enode.T(e) =>
        IF Enode.IsPredTerm(e) THEN
          RETURN Enode.NewEq(e, Enode.eTrue)
        ELSE
          RAISE Prover.Error("Non-pred enode is not a literal.")
        END (* IF *)
    ELSE
        RAISE Prover.Error("Sx of unrecognized type.")
    END (* TYPECASE *)
  END SxToLiteral;


(* REMOVE
PROCEDURE SxToConjunction(sx: REFANY): AF.Lit RAISES { Prover.Error } =
  BEGIN
    TYPECASE sx OF
    | RefList.T(rl) =>
        IF rl.head = PredSx.andSym THEN
          VAR conjs: RefList.T := NIL; tl := rl.tail; BEGIN
            WHILE tl # NIL DO
              conjs := RefList.Cons(SxToConjunction(tl.head), conjs);
              tl := tl.tail
            END (* WHILE *);
            RETURN ProxyProp.NewLit(conjs, TRUE)
          END (* BEGIN *)
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    RETURN SxToLiteral(sx)
  END SxToConjunction;
*)

PROCEDURE SxToClause(
      sx: REFANY;
      READONLY sub: MatchingRule.Substitution := MatchingRule.EmptySub): T =
  BEGIN
    IF Prover.internDebug THEN
      Wr.PutText(Stdio.stdout, "Clause.SxToClause called on: ");
      SxPrint.Print(Stdio.stdout, sx, sub);
      Wr.PutText(Stdio.stdout, "\n");
      Wr.Flush(Stdio.stdout); 
    END (* IF *);
    TYPECASE sx OF <*NOWARN*>
    | RefList.T(rl) =>
        VAR lits: RefList.T := NIL; BEGIN
          IF rl.head = PredSx.orSym THEN
            VAR disjuncts := rl.tail; valid := FALSE;
                valLit: AF.Lit := NIL; 
            BEGIN
              WHILE disjuncts # NIL AND NOT valid DO
                VAR newLit := SxToLiteral(disjuncts.head, sub); <*NOWARN*>
                BEGIN
                  IF newLit.af = AF.trueAF AND newLit.sense THEN
                    valid := TRUE; valLit := newLit
                  ELSIF newLit.af # AF.trueAF THEN
                    lits := RefList.Cons(newLit, lits)
                  END (* IF *)
                END (* BEGIN *);
                disjuncts := disjuncts.tail
              END (* WHILE *);
              IF valid THEN
                lits := RefList.List1(valLit)
              ELSE
                lits := RefList.ReverseD(lits)
              END (* IF *)
            END (* BEGIN *);
          ELSE
            lits := RefList.List1(SxToLiteral(rl, sub)) <*NOWARN*>
          END (* IF *);
          RETURN NEW(T, lits := lits).init()
        END (* BEGIN *)
    END (* TYPECASE *)
  END SxToClause;

VAR patDebug := Env.Get("PROVER_PAT_DEBUG") # NIL;

(* Requires "sx" be a univerally quantified formula.  Returns a
   literal with equivalent meaning. *)
PROCEDURE LitForUniversalQuant(sx: PredSx.T): AF.Lit
    RAISES { Prover.Error } =
  BEGIN
    RETURN LitForUniversalWork(PredSx.ProcessLabels(PredSx.Desugar(sx), TRUE),
                               sx, NIL, TRUE,
                               NEW(AtomSetList.T).init(),
                               NEW(AtomRefTbl.Default).init(),
                               FALSE)
  END LitForUniversalQuant;


(* Returns a "AF.Lit" equivalent to "(IFF sense (FORALL vars sx))".
   If "triedQuantSimp" is false, may attempt quantifier simplification. *) 
PROCEDURE LitForUniversalWork(sx, origSx: REFANY;
                              upats: RefList.T;
                              sense: BOOLEAN;
                              vars: AtomSet.T;
                              sub: AtomRefTbl.T;
                              triedQuantSimp := FALSE): AF.Lit
    RAISES { Prover.Error } =
  VAR isClause: BOOLEAN; BEGIN
    IF patDebug THEN
      Wr.PutText(Stdio.stdout, "Starting Clause.LitForUniversalWork.\n");
      Wr.PutText(Stdio.stdout, "  sx = ");
      Wr.Flush(Stdio.stdout);
      SxPrint.Print(Stdio.stdout, sx, MatchingRule.EmptySub);
      Wr.PutText(Stdio.stdout, "\n  origSx = ");
      Wr.Flush(Stdio.stdout);
      SxPrint.Print(Stdio.stdout, origSx, MatchingRule.EmptySub);
      Wr.PutText(Stdio.stdout, "\n");
      Wr.Flush(Stdio.stdout);
    END;
    TYPECASE sx OF
    | RefList.T(rl) =>
        IF rl.head = PredSx.forallSym AND sense OR
          rl.head = PredSx.existsSym AND NOT sense THEN
          VAR vs: RefList.T := rl.tail.head;
              subUpats: RefList.T := PredSx.QuantPatSpec(rl);
          BEGIN
            WHILE vs # NIL DO
              EVAL vars.insert(vs.head); vs := vs.tail
            END (* WHILE *);
            IF upats # NIL THEN
              IF subUpats.head # upats.head THEN
                RAISE Prover.Error("Inconsistent pattern specs.")
              ELSE
                subUpats := RefList.AppendD(upats, subUpats.tail)
              END (* IF *)
            END (* IF *);
            RETURN LitForUniversalWork(PredSx.QuantBody(rl), origSx,
                                       subUpats, sense,
                                       vars, sub, triedQuantSimp)
          END (* BEGIN *)
        ELSIF rl.head = PredSx.existsSym AND sense OR
          rl.head = PredSx.forallSym AND NOT sense THEN
          VAR vs: RefList.T := rl.tail.head;
              uVars := AtomSetToRefList(vars);
          BEGIN
            WHILE vs # NIL DO
              VAR eVar: Atom.T := vs.head; BEGIN
                EVAL sub.put(eVar, SkolemAppl(eVar, uVars))
              END (* BEGIN *);
              vs := vs.tail
            END (* WHILE *);
            RETURN LitForUniversalWork(PredSx.QuantBody(rl), origSx, upats,
                                       sense, vars, sub, triedQuantSimp)
          END (* BEGIN *)
        ELSIF rl.head = PredSx.notSym THEN
          RETURN LitForUniversalWork(rl.tail.head, origSx,
                                     upats, NOT sense, vars, sub,
                                     triedQuantSimp)
        ELSIF rl.head = PredSx.impliesSym OR
              rl.head = PredSx.expliesSym OR
              rl.head = PredSx.iffSym THEN
          <*ASSERT FALSE*>
        ELSIF rl.head = PredSx.andSym AND sense OR
          rl.head = PredSx.orSym AND NOT sense THEN
          (* Distribute FORALL over AND *)
          VAR conjs := rl.tail; res: RefList.T := NIL;
              varsRL := AtomSetToRefList(vars);
          BEGIN
            WHILE conjs # NIL DO
              res := RefList.Cons(
                         LitForUniversalWork(
                             conjs.head,
                             RefList.List3(PredSx.forallSym,
                                           varsRL,
                                           conjs.head),
                             upats, sense, vars.copy(), SubCopy(sub),
                             triedQuantSimp),
                         res);
              conjs := conjs.tail
            END (* WHILE *);
            RETURN CNFWork(RefList.Cons(PredSx.andSym, res),
                           MatchingRule.EmptySub)
          END (* BEGIN *)
        ELSIF rl.head = PredSx.orSym AND sense OR
          rl.head = PredSx.andSym AND NOT sense THEN
          sx := Skolemize(sx, sense, vars.copy(), SubCopy(sub));
          sense := TRUE;
          VAR conjs, maxWidth: CARDINAL; BEGIN
            PredSx.CNFSize(sx, conjs, maxWidth);
            IF conjs * maxWidth <= Prover.maxTrueCNF THEN
              sx := PredSx.CNF(sx);   <*NOWARN*>
              TYPECASE sx OF
              | RefList.T(rl2) =>
                  IF rl2.head = PredSx.andSym THEN
                    RETURN LitForUniversalWork(sx, NIL, upats, TRUE, vars,
                                               sub, triedQuantSimp)
                  END (* IF *)
              ELSE
              END (* TYPECASE *);
              isClause := TRUE
            ELSE
              isClause := PredSx.FormIsInCNF(sx)
            END (* IF *);
          END (* BEGIN *)
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    (* Otherwise... *)
    (* Substitute for any already-skolemized variables. *)
    sx := PredSx.Sub(sx, sub);
    (* Eliminate quantified variables that don't appear in the formula. *)
    vars := vars.intersectionD(PredSx.FreeVars(sx));
    (* If var set is empty, return the result of CNF. *)
    IF vars.isEmpty() THEN
      RETURN CNFWork(NegIf(sx, sense), MatchingRule.EmptySub)
    END (* IF *);
    VAR tmpl: MatchingRule.Template;
        isUnit := PredSx.FormIsLit(sx);
        pats: RefList.T;
        varsRL: RefList.T := NIL;
        promote, plunge: BOOLEAN;
        nPVs := 0;
    BEGIN
      VAR pvSub := NEW(AtomRefTbl.Default).init();
          iter := vars.iterate(); v: Atom.T;
      BEGIN
        WHILE iter.next(v) DO
          varsRL := RefList.Cons(v, varsRL);
          EVAL pvSub.put(v, MatchingRule.pv[nPVs]);
          INC(nPVs)
        END (* WHILE *);
        PredSx.ComposeSubD(sub, pvSub)
      END (* BEGIN *);
      varsRL := RefList.ReverseD(varsRL);
      IF patDebug THEN
        Wr.PutText(Stdio.stdout, "\n Looking for a matching rule:\n  ");
        Wr.Flush(Stdio.stdout);
        VAR quant := RefList.List3(PredSx.forallSym,
                                   AtomSetToRefList(vars),
                                   NegIf(MatchingRule.PatToPrintableSx(sx),
                                         sense));
        BEGIN
          SxPrint.Print(Stdio.stdout, quant);
          Wr.PutText(Stdio.stdout, "\n");
          Wr.Flush(Stdio.stdout)
        END (* BEGIN *)
      END (* IF *);
      tmpl := NegIf(PredSx.Sub(sx, sub), sense);
      upats := PredSx.Sub(upats, sub);
      pats := FindPatterns(tmpl, upats, nPVs, promote, plunge);
      IF pats = NIL AND NOT triedQuantSimp THEN
        sx := PredSx.MkQuant(PredSx.forallSym, AtomSetToRefList(vars),
                             NIL, PredSx.NegIf(sx, sense));
        sx := PredSx.SimplifyQuants(sx); <*NOWARN*>
        RETURN LitForUniversalWork(sx, sx, NIL, TRUE,
                                   NEW(AtomSetList.T).init(),
                                   NEW(AtomRefTbl.Default).init(),
                                   triedQuantSimp := TRUE)
      END (* IF *);
      IF pats = NIL THEN
        IF Prover.allowSinglePatVarPat THEN
          VAR mpat: RefList.T := NIL; BEGIN
            FOR i := 0 TO vars.size()-1 DO
              mpat := RefList.Cons(MatchingRule.pv[i], mpat)
            END (* FOR *);
            pats := RefList.List1(mpat)
          END (* BEGIN *)
        ELSE
          IF Prover.triggerlessWarnCountdown > 0 THEN
            Wr.PutText(Stdio.stdout,
             "Warning: triggerless quantifier body");
            IF Prover.triggerlessWarnCountdown = 1 THEN
              Wr.PutText(Stdio.stdout,
                " (will not warn of more for this command)")
            END;
            Wr.PutText(Stdio.stdout, "\n\n");
            Wr.Flush(Stdio.stdout);            
            SxPrint.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(tmpl));
            Wr.PutText(Stdio.stdout,
              "\n\nwith " & Fmt.Int(sub.size()) & " pattern variable");
            IF sub.size() # 1 THEN Wr.PutText(Stdio.stdout, "s") END;
            Wr.PutText(Stdio.stdout,
              ", found while processing the formula\n\n");
            Wr.Flush(Stdio.stdout);            
            SxPrint.Print(Stdio.stdout, origSx);
            Wr.PutText(Stdio.stdout, "\n\n");
            Wr.Flush(Stdio.stdout);            
            DEC(Prover.triggerlessWarnCountdown);
          END (* BEGIN *)
        END (* IF *)
      END (* IF *);
      IF patDebug THEN
        Wr.PutText(Stdio.stdout, "\n  Parsed a matching rule:\n    (");
        Wr.Flush(Stdio.stdout);
        VAR p := pats; BEGIN
          WHILE p # NIL DO
            SxPrint.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(p.head));
            IF p.tail = NIL THEN
              Wr.PutText(Stdio.stdout, ") -->\n      ");
            ELSE
              Wr.PutText(Stdio.stdout, "\n     ");
            END (* IF *);
            p := p.tail
          END (* WHILE *);
          SxPrint.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(tmpl));
          Wr.PutText(Stdio.stdout, "\n");
          Wr.Flush(Stdio.stdout)
        END (* BEGIN *)
      END (* IF *);
      <*ASSERT origSx # NIL *>
      VAR pos := NEW(MatchingRule.T).init(varsRL, pats, tmpl,
                                          isUnit, isClause,
                                          immedPromote := promote,
                                          plungeHint := plunge);
      BEGIN
        RETURN NEW(AF.Lit,
                   af := NEW(MatchingRule.AtomF).init(
                                              pos := pos, 
                                              neg := PredSx.Not(origSx)))
      END (* BEGIN *)
    END (* BEGIN *)
  END LitForUniversalWork;

(* Result is "sense IFF sub(skolem(tmpl))". *)
PROCEDURE Skolemize(tmpl: MatchingRule.Template;
                    sense: BOOLEAN;
                    vars: AtomSet.T;
                    sub: AtomRefTbl.T): MatchingRule.Template =
  BEGIN
    TYPECASE tmpl OF
    | NULL => RETURN NIL
    | Atom.T(at) => 
        VAR ra: REFANY; BEGIN
          IF sub.get(at, ra) THEN RETURN ra
          ELSE RETURN at
          END (* IF *)
        END (* BEGIN *)
    | RefList.T(rl) =>
        IF rl.head = PredSx.notSym THEN
          RETURN Skolemize(rl.tail.head, NOT sense, vars, sub)
        ELSIF rl.head = PredSx.andSym AND sense OR
              rl.head = PredSx.orSym AND NOT sense THEN
          VAR res: PredSx.T := PredSx.trueSym; conjs := rl.tail; BEGIN
            WHILE conjs # NIL DO
              res := PredSx.And(
                         res,
                         Skolemize(conjs.head, sense, vars, sub));
              conjs := conjs.tail
            END (* WHILE *);
            RETURN res
          END (* BEGIN *)
        ELSIF rl.head = PredSx.orSym AND sense OR
              rl.head = PredSx.andSym AND NOT sense THEN
          VAR res: PredSx.T := PredSx.falseSym; disjs := rl.tail; BEGIN
            WHILE disjs # NIL DO
              res := PredSx.Or(
                         res,
                         Skolemize(disjs.head, sense, vars, sub));
              disjs := disjs.tail
            END (* WHILE *);
            RETURN res
          END (* BEGIN *)
        ELSIF rl.head = PredSx.impliesSym THEN
          RETURN Skolemize(PredSx.Or(PredSx.Not(rl.tail.head),
                                               rl.tail.tail.head),
                                     sense, vars, sub)
            
        ELSIF rl.head = PredSx.expliesSym THEN
          RETURN Skolemize(PredSx.Or(PredSx.Not(rl.tail.tail.head),
                                               rl.tail.head),
                                     sense, vars, sub)
        ELSIF rl.head = PredSx.iffSym THEN
          VAR res := RefList.List3(PredSx.iffSym,
                                   Skolemize(rl.tail.head,
                                                      sense, vars, sub),
                                   Skolemize(rl.tail.tail.head,
                                                      sense, vars, sub));
          BEGIN
            IF NOT sense THEN res := PredSx.Not(res) END (* IF *);
            RETURN res
          END (* BEGIN *)
        ELSIF rl.head = PredSx.forallSym AND sense OR
              rl.head = PredSx.existsSym AND NOT sense THEN
          VAR vl: RefList.T := rl.tail.head;
              patSpec := PredSx.QuantPatSpec(rl);
              body := PredSx.QuantBody(rl);
              vars2 := vars.copy();
              subUndos: RefList.T := NIL;
          BEGIN
            VAR vl2 := vl; BEGIN
              WHILE vl2 # NIL DO
                VAR v: Atom.T := vl2.head; ra: REFANY; BEGIN
                  EVAL vars2.insert(vl2.head);
                  IF sub.get(v, ra) THEN
                    subUndos := RefList.Cons(RefList.List2(v, ra), subUndos);
                    EVAL sub.delete(v, ra)
                  END (* IF *)
                END (* BEGIN *);
                vl2 := vl2.tail
              END (* WHILE *)
            END (* BEGIN *);
            patSpec := PredSx.Sub(patSpec, sub);
            VAR res := PredSx.MkQuant(PredSx.forallSym, rl.tail.head,
                                      patSpec,
                                      Skolemize(
                                          body, sense, vars2, sub));
            BEGIN
              WHILE subUndos # NIL DO
                VAR undo: RefList.T := subUndos.head; BEGIN
                  EVAL sub.put(undo.head, undo.tail.head)
                END (* BEGIN *);
                subUndos := subUndos.tail
              END (* WHILE *);
              RETURN res
            END (* BEGIN *)
          END (* BEGIN *)
        ELSIF rl.head = PredSx.existsSym AND sense OR
              rl.head = PredSx.forallSym AND NOT sense THEN
          VAR vl: RefList.T := rl.tail.head;
              uVars: RefList.T := NIL;
              body := PredSx.QuantBody(rl);
              subUndos: RefList.T := NIL;
          BEGIN
            VAR iter := vars.iterate(); v: Atom.T; BEGIN
              WHILE iter.next(v) DO
                uVars := RefList.Cons(PredSx.Sub(v, sub), uVars)
              END (* WHILE *)
            END (* BEGIN *);
            WHILE vl # NIL DO
              VAR exQvar: Atom.T := vl.head; ra: REFANY; BEGIN
                IF NOT sub.get(exQvar, ra) THEN ra := NIL END (* IF *);
                subUndos := RefList.Cons(RefList.List2(exQvar, ra), subUndos);
                EVAL sub.put(exQvar, SkolemAppl(exQvar, uVars))
              END (* BEGIN *);
              vl := vl.tail
            END (* WHILE *);
            VAR res := Skolemize(body, sense, vars, sub); BEGIN
              WHILE subUndos # NIL DO
                VAR undo: RefList.T := subUndos.head; ra: REFANY; BEGIN
                  IF undo.tail.head = NIL THEN
                    EVAL sub.delete(undo.head, ra) 
                  ELSE
                    EVAL sub.put(undo.head, undo.tail.head)
                  END (* IF *)
                END (* BEGIN *);
                subUndos := subUndos.tail
              END (* WHILE *);
              RETURN res
            END (* BEGIN *)
          END (* BEGIN *)
        ELSIF rl.head = PredSx.labelSym OR rl.head = PredSx.lblPosSym OR 
              rl.head = PredSx.lblNegSym THEN
          IF sense THEN
            RETURN RefList.List3(rl.head, rl.tail.head,
                                 Skolemize(rl.tail.tail.head, sense, vars, sub))
          ELSIF rl.head = PredSx.labelSym OR rl.head = PredSx.lblNegSym THEN
            RETURN RefList.List3(PredSx.lblPosSym, rl.tail.head,
                                 Skolemize(rl.tail.tail.head, sense, vars, sub))
          ELSE
            RETURN RefList.List3(PredSx.lblNegSym, rl.tail.head,
                                 Skolemize(rl.tail.tail.head, sense, vars, sub))
          END (* IF *)
        ELSE
          (* Atomic formula. *)
          RETURN NegIf(PredSx.Sub(rl, sub), sense)
        END (* IF *)
    ELSE
        RETURN tmpl
    END (* TYPECASE *)
  END Skolemize;

PROCEDURE NegIf(sx: PredSx.T; sense: BOOLEAN): PredSx.T =
  BEGIN
    IF sense THEN RETURN sx
    ELSE RETURN PredSx.Not(sx)
    END (* IF *)
  END NegIf;

PROCEDURE AtomSetToRefList(vars: AtomSet.T): RefList.T =
  VAR res: RefList.T := NIL;
      iter := vars.iterate(); v: Atom.T;
  BEGIN
    WHILE iter.next(v) DO res := RefList.Cons(v, res) END (* WHILE *);
    RETURN res
  END AtomSetToRefList;

PROCEDURE SubCopy(sub: AtomRefTbl.T): AtomRefTbl.T =
  VAR res := NEW(AtomRefTbl.Default).init();
      iter := sub.iterate(); v: Atom.T; ra: REFANY;
  BEGIN
    WHILE iter.next(v, ra) DO EVAL res.put(v, ra) END (* WHILE *);
    RETURN res
  END SubCopy;

PROCEDURE SkolemAppl(eVar: Atom.T; uVars: RefList.T): PredSx.T =
  VAR i := Context.rs.inc(eVar); skolemFunc: Atom.T; BEGIN
    skolemFunc := Atom.FromText(
                      Atom.ToText(eVar) & PredSx.SkolemSep & Fmt.Int(i));
    RETURN RefList.Cons(skolemFunc, uVars)
  END SkolemAppl;

PROCEDURE FlattenOrs(p: PredSx.T; sense: BOOLEAN): PredSx.T =
  BEGIN
    TYPECASE p OF
    | RefList.T(rl) =>
        IF rl.head = PredSx.notSym THEN
          RETURN FlattenOrs(rl.tail.head, NOT sense)
        ELSIF rl.head = PredSx.orSym AND sense OR
          rl.head = PredSx.andSym AND NOT sense THEN
          VAR disjs := rl.tail; res: PredSx.T := PredSx.falseSym; BEGIN
            WHILE disjs # NIL DO
              res := PredSx.Or(FlattenOrs(disjs.head, sense), res);
              disjs := disjs.tail
            END (* WHILE *);
            RETURN res
          END (* BEGIN *)
        ELSIF rl.head = PredSx.impliesSym AND sense THEN
          RETURN FlattenOrs(PredSx.Or(PredSx.Not(rl.tail.head),
                                      rl.tail.tail.head),
                            sense)
        ELSIF rl.head = PredSx.expliesSym AND sense THEN
          RETURN FlattenOrs(PredSx.Or(PredSx.Not(rl.tail.tail.head),
                                      rl.tail.head),
                            sense)
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    (* Otherwise... *)
    RETURN NegIf(p, sense)
  END FlattenOrs;

PROCEDURE FlattenAnds(p: PredSx.T; sense: BOOLEAN): PredSx.T =
  BEGIN
    TYPECASE p OF
    | RefList.T(rl) =>
        IF rl.head = PredSx.notSym THEN
          RETURN FlattenAnds(rl.tail.head, NOT sense)
        ELSIF rl.head = PredSx.andSym AND sense OR
          rl.head = PredSx.orSym AND NOT sense THEN
          VAR conjs := rl.tail; res: PredSx.T := PredSx.trueSym; BEGIN
            WHILE conjs # NIL DO
              res := PredSx.And(FlattenAnds(conjs.head, sense), res);
              conjs := conjs.tail
            END (* WHILE *);
            RETURN res
          END (* BEGIN *)
        ELSIF rl.head = PredSx.iffSym THEN
          VAR lhs := rl.tail.head; rhs := rl.tail.tail.head; BEGIN
            IF NOT sense THEN lhs := PredSx.Not(lhs) END (* IF *);
            RETURN FlattenAnds(
                       PredSx.And(PredSx.Or(PredSx.Not(lhs), rhs),
                                  PredSx.Or(PredSx.Not(rhs), lhs)),
                       TRUE)
          END (* BEGIN *)
        END (* IF *)
    ELSE
    END (* IF *);
    (* Otherwise... *)
    RETURN NegIf(p, sense)
  END FlattenAnds;

PROCEDURE DistributeAnds(ands, lits: RefList.T): PredSx.T =
  BEGIN
    IF ands = NIL THEN
      VAR res: PredSx.T := PredSx.falseSym; BEGIN
        WHILE lits # NIL DO
          res := PredSx.Or(lits.head, res);
          lits := lits.tail
        END (* WHILE *);
        RETURN res
      END (* BEGIN *)
    ELSE
      VAR and1: RefList.T := ands.head; 
          conjs1 := and1.tail;
          andsRest := DistributeAnds(ands.tail, lits);
          conjsRest: RefList.T;
          res: PredSx.T := PredSx.trueSym;
      BEGIN
        TYPECASE andsRest OF
        | RefList.T(rl) =>
            IF rl.head = PredSx.andSym THEN conjsRest := rl.tail
            ELSE conjsRest := RefList.List1(andsRest)
            END (* IF *)
        ELSE conjsRest := RefList.List1(andsRest)
        END (* TYPECASE *);
        <*ASSERT and1.head = PredSx.andSym*>
        WHILE conjs1 # NIL DO
          VAR cr := conjsRest; BEGIN
            WHILE cr # NIL DO
              res := PredSx.And(PredSx.Or(FlattenOrs(conjs1.head, TRUE),
                                          cr.head),
                                res);
              cr := cr.tail
            END (* WHILE *)
          END (* BEGIN *);
          conjs1 := conjs1.tail
        END (* WHILE *);
        RETURN res
      END (* BEGIN *)
    END (* IF *)
  END DistributeAnds;

PROCEDURE CopySx(sx: PredSx.T): PredSx.T =
  BEGIN
    TYPECASE sx OF
    | NULL => RETURN sx
    | RefList.T(rl) => RETURN RefList.Cons(CopySx(rl.head), CopySx(rl.tail))
    ELSE RETURN sx
    END (* TYPECASE *)
  END CopySx;

TYPE
  PartialPatPair = OBJECT
    pvs: MatchingRule.PatVarNumSet;
    pat: MatchingRule.Pattern;
  END (* OBJECT *);

PROCEDURE PartialPatCompare(ppp1RA, ppp2RA: REFANY): [-1..1] =
  VAR ppp1: PartialPatPair := ppp1RA;
      ppp2: PartialPatPair := ppp2RA;
      n1, n2 := 0;
  BEGIN
    FOR i := 0 TO MatchingRule.MaxPatVars-1 DO
      IF i IN ppp1.pvs THEN INC(n1) END (* IF *);
      IF i IN ppp2.pvs THEN INC(n2) END (* IF *)
    END (* FOR *);
    VAR res := Integer.Compare(n2, n1); BEGIN
      IF res # 0 THEN RETURN res
      ELSE
        (* Prefer smaller patterns... *)
        VAR p2Sz := PatSize(ppp2.pat); p1Sz := PatSize(ppp1.pat); BEGIN
          RETURN Integer.Compare(p1Sz, p2Sz)
        END (* BEGIN *)
      END (* IF *)
    END (* BEGIN *)
  END PartialPatCompare;

PROCEDURE PatSize(pat: REFANY): CARDINAL =
  BEGIN
    TYPECASE pat OF
    | NULL => RETURN 0
    | RefList.T(rl) => RETURN PatSize(rl.head) + PatSize(rl.tail)
    ELSE RETURN 1
    END (* TYPECASE *)
  END PatSize;

    
VAR (*CONST*) PredSyms: AtomSet.T;
(* The set of all the predicate predicate operators in PredSx. *)
  
PROCEDURE FindPatterns(template, upatSpec: RefList.T;
                       hiPatVar: INTEGER;
                       VAR (*OUT*) promote: BOOLEAN;
                       VAR (*OUT*) plunge: BOOLEAN): RefList.T
  RAISES { Prover.Error } =
  (* Returns a list of all minimal subterms in "template" that contain
     all pattern variables whose numbers are less than "hiPatVar". *) 
  VAR full, empty := MatchingRule.PatVarNumSet{};
      partialPats: RefList.T := NIL;
      nonPats: RefList.T := NIL;
  PROCEDURE FindPatternsWork(sx: REFANY;
                             VAR (*OUT*) pvs: MatchingRule.PatVarNumSet;
                             topLevelTerm: BOOLEAN): RefList.T =
    VAR pats: RefList.T := NIL; BEGIN
      pvs := MatchingRule.PatVarNumSet{};
      TYPECASE sx OF
      | MatchingRule.PatVar(ri) =>
          pvs := pvs + MatchingRule.PatVarNumSet{ri^}; RETURN NIL
      | RefList.T(rl) =>
          IF rl.head = PredSx.labelSym OR rl.head = PredSx.lblNegSym OR
            rl.head = PredSx.lblPosSym THEN
            RETURN FindPatternsWork(rl.tail.tail.head, pvs, topLevelTerm)
          ELSIF PredSyms.member(rl.head) THEN
            VAR dummy: MatchingRule.PatVarNumSet; args := rl.tail; BEGIN
              WHILE args # NIL DO
                pats := RefList.AppendD(
                            pats,
                            FindPatternsWork(args.head, dummy,
                                             topLevelTerm := TRUE));
                args := args.tail
              END (* WHILE *)
            END (* BEGIN *)
          ELSIF rl.head = PredSx.forallSym OR rl.head = PredSx.existsSym THEN
            RETURN NIL
          ELSE
            VAR cpvs: MatchingRule.PatVarNumSet;
                subPats: RefList.T; subTermPat := FALSE;
            BEGIN
              rl := rl.tail;
              WHILE rl # NIL DO
                subPats := FindPatternsWork(rl.head, cpvs,
                                            topLevelTerm := FALSE);
                IF cpvs = full AND subPats # NIL THEN
                  subTermPat := TRUE
                END (* IF *);
                pats := RefList.AppendD(subPats, pats);
                pvs := pvs + cpvs;
                rl := rl.tail
              END (* WHILE *);
              IF NOT subTermPat THEN
                IF pvs = full THEN
                  pats := RefList.Cons(RefList.List1(sx), pats);
IF patDebug THEN
  Wr.PutText(Stdio.stdout,"...FindPatternsWork: Found a pattern: ");
  Wr.Flush(Stdio.stdout);
  SxPrint.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(sx));
  Wr.PutText(Stdio.stdout, "\n");
  Wr.Flush(Stdio.stdout)
END
                ELSIF pvs # empty AND topLevelTerm THEN
                  partialPats := RefList.Cons(
                                     NEW(PartialPatPair,
                                         pvs := pvs, pat := sx),
                                     partialPats)
                END (* IF *)
              END (* IF *)
            END (* BEGIN *)
          END (* IF *)
      ELSE
        RETURN NIL
      END (* TYPECASE *);



      
IF patDebug THEN
  Wr.PutText(Stdio.stdout,"...FindPatternsWork: Found patterns: ");
  Wr.Flush(Stdio.stdout);
  SxPrint.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(pats));
  Wr.PutText(Stdio.stdout, "\n");
  Wr.Flush(Stdio.stdout)
END;
      RETURN pats
    END FindPatternsWork;
  VAR pvs: MatchingRule.PatVarNumSet; BEGIN
    promote := FALSE;
    plunge := FALSE;
    FOR i := 0 TO hiPatVar-1 DO
      full := full + MatchingRule.PatVarNumSet{i}
    END (* FOR *);
    IF upatSpec # NIL THEN
      IF upatSpec.head = PredSx.patsSym THEN
        VAR upats := upatSpec.tail; res: RefList.T := NIL; vpat: Sx.T; BEGIN
          WHILE upats # NIL DO
            IF upats.head = PredSx.promoteSym THEN
              promote := TRUE
            ELSIF upats.head = PredSx.plungeSym THEN
              plunge := TRUE
            ELSIF ValidPat(upats.head, full, vpat) THEN
              res := RefList.Cons(vpat, res)
            ELSE
              RAISE Prover.Error("User pattern is not valid.")
            END (* IF *);
            upats := upats.tail
          END (* WHILE *);
          IF res # NIL THEN RETURN res END (* IF *)
        END (* BEGIN *)
      ELSE
        <*ASSERT upatSpec.head = PredSx.noPatsSym *>
        nonPats := upatSpec.tail
      END (* IF *)
    END (* IF *);
    VAR pats := FindPatternsWork(template, pvs, topLevelTerm := TRUE);
        isUnit := PredSx.FormIsLit(template);
        patl := pats;
        res: RefList.T := NIL;
    BEGIN
IF patDebug THEN
  Wr.PutText(Stdio.stdout,"Clause.FindPatterns: Patterns so far ");
  Wr.Flush(Stdio.stdout);
  SxPrint.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(pats));
  Wr.PutText(Stdio.stdout, "\n");
  Wr.PutText(Stdio.stdout,"Clause.FindPatterns: Partial patterns so far: ");
  Wr.Flush(Stdio.stdout);
  SxPrint.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(partialPats));
  Wr.PutText(Stdio.stdout, "\n");
  Wr.Flush(Stdio.stdout)
END;

      (* Now eliminate any patterns that match larger patterns; such
         pairs of patterns would cause a one-rule matching cycle.  Also
         eliminate any patterns the user forbid from being patterns. *)
      IF pats # NIL THEN
        WHILE patl # NIL DO
          VAR patlHead: RefList.T := patl.head; BEGIN
            IF NOT PredSxList.Member(res, patlHead) AND
              NOT PredSxList.Member(nonPats, patlHead.head) AND
              NOT ((isUnit OR NOT Prover.allowNUMatchCycle) AND
              MatchesLargerPattern(patlHead, template, hiPatVar)) THEN
              res := RefList.Cons(patl.head, res)
            END (* IF *)
          END (* BEGIN *);
          patl := patl.tail
        END (* WHILE *);
        IF res = NIL THEN
          patl := pats;
          WHILE patl # NIL DO
            VAR s: PatSubst; BEGIN
              IF MatchesLargerPatternWork(patl.head, template, template,
                                          hiPatVar, s) THEN
                VAR pat2 := PatSubstApply(patl.head, s); BEGIN
                  IF PVs(pat2) = full THEN
                    res := RefList.Cons(patl.head, res)
                  END (* IF *)
                END (* BEGIN *)
              END (* IF *)
            END (* BEGIN *);
            patl := patl.tail
          END (* WHILE *)
        END (* IF *)
      ELSE
        (* If found no single-term pattern, try for largest multi-pattern. *)
        IF res = NIL THEN
          (* First sort the partial patterns in decreasing order of number
             pattern variables found. *)
          VAR pp2: RefList.T := NIL; BEGIN
            WHILE partialPats # NIL DO
              VAR ppat: PartialPatPair := partialPats.head; BEGIN
                IF NOT PredSxList.Member(nonPats, ppat.pat) THEN
                  pp2 := RefList.Cons(partialPats.head, pp2)
                END (* IF *)
              END (* BEGIN *);
              partialPats := partialPats.tail
            END (* WHILE *);
            partialPats := pp2;
          END (* BEGIN *);
          partialPats := RefListSort.SortD(partialPats, PartialPatCompare);
          VAR pvs := empty; mpat: RefList.T := NIL; BEGIN
            WHILE partialPats # NIL AND pvs # full DO 
              VAR ppp: PartialPatPair := partialPats.head; BEGIN
                IF NOT ppp.pvs <= pvs THEN
                  pvs := pvs + ppp.pvs;
                  mpat := RefList.Cons(ppp.pat, mpat)
                END (* IF *)
              END (* BEGIN *);
              partialPats := partialPats.tail
            END (* WHILE *);
            IF pvs = full THEN
              res := RefList.List1(mpat)
            END (* IF *)
          END (* BEGIN *)
        END (* IF *)
      END (* IF *);
      (* Special case: (FORALL (a, b, others) (a = b v rest)) =>
                         ((NEQ a b)) rest *)
      (*
      FindNEQPattern(template, full, res);
      *)
IF patDebug THEN
  Wr.PutText(Stdio.stdout,"Clause.FindPatterns: returning ");
  Wr.Flush(Stdio.stdout);
  SxPrint.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(res));
  Wr.PutText(Stdio.stdout, "\n");
  Wr.Flush(Stdio.stdout)
END;
      RETURN res
    END (* BEGIN *);
  END FindPatterns;

PROCEDURE ValidPat(pat: Sx.T;
                   full: MatchingRule.PatVarNumSet;
                   VAR (*OUT*) vpat: Sx.T): BOOLEAN =
  BEGIN
    TYPECASE pat OF
    | RefList.T(rl) =>
        IF rl.head = PredSx.mpatSym THEN
          vpat := rl.tail;
          RETURN MatchingRule.PatVars(rl.tail) = full
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    vpat := RefList.List1(pat);
    RETURN MatchingRule.PatVars(pat) = full
  END ValidPat;

<*UNUSED*>
PROCEDURE FindNEQPattern(template: RefList.T; full: MatchingRule.PatVarNumSet;
                         VAR res: RefList.T) =
  PROCEDURE IsEQLit(lit: REFANY; sense := TRUE): RefList.T =
    BEGIN
      TYPECASE lit OF
      | RefList.T(rl) =>
          IF rl.head = PredSx.eqSym AND sense OR 
            rl.head = PredSx.diffSym AND NOT sense THEN
            <*ASSERT RefList.Length(rl) = 3*>
            IF ISTYPE(rl.tail.head, MatchingRule.PatVar) AND
              ISTYPE(rl.tail.tail.head, MatchingRule.PatVar) AND
              rl.tail.tail.head # rl.tail.head THEN
              RETURN RefList.Cons(PredSx.eqSym, rl.tail)
            END (* IF *)
          ELSIF rl.head = PredSx.notSym THEN
            RETURN IsEQLit(rl.tail.head, NOT sense)
          END (* IF *)
      ELSE
      END (* TYPECASE *);
      RETURN NIL
    END IsEQLit;
  BEGIN
    IF template.head = PredSx.orSym THEN
      VAR lits := template.tail; BEGIN
        WHILE lits # NIL DO
          VAR eqLit := IsEQLit(lits.head); BEGIN
            IF eqLit # NIL THEN
              VAR litPVs := PVs(eqLit);
                  neqPat := RefList.Cons(PredSx.diffSym, eqLit.tail);
                  res2 := res;
              BEGIN
                WHILE res2 # NIL DO
                  VAR pat: RefList.T := res2.head; BEGIN
                    IF pat.tail = NIL THEN
                      (* Singleton; make multipattern. *)
                      res := RefList.Cons(RefList.List2(pat.head, neqPat), res)
                    ELSE
                      (* multipattern; make n multipatterns. *)
                      VAR patTerms := pat; BEGIN
                        WHILE patTerms # NIL DO
                          IF litPVs + PVs(patTerms.head) = full THEN
                            res := RefList.Cons(RefList.List2(patTerms.head, 
                                                              neqPat),
                                                res)
                          END (* IF *);
                          patTerms := patTerms.tail
                        END (* WHILE *)
                      END (* BEGIN *)
                    END (* IF *)
                    (*
                    res := RefList.Cons(RefList.Append(pat,
                                                       RefList.List1(neqPat)),
                                        res)
                    *)
                  END (* BEGIN *);
                  res2 := res2.tail
                END (* WHILE *);
                RETURN
                END (* BEGIN *)
            END (* IF *)
          END (* BEGIN *);
          lits := lits.tail
        END (* WHILE *)
      END (* BEGIN *)
    END (* IF *)
  END FindNEQPattern;

PROCEDURE PVs(p: REFANY): MatchingRule.PatVarNumSet =
  BEGIN
    TYPECASE p OF
    | NULL => RETURN MatchingRule.PatVarNumSet{}
    | MatchingRule.PatVar(pv) => RETURN MatchingRule.PatVarNumSet{pv^}
    | RefList.T(rl) => RETURN PVs(rl.head) + PVs(rl.tail)
    ELSE RETURN MatchingRule.PatVarNumSet{}
    END (* TYPECASE *)
  END PVs;

PROCEDURE MatchesLargerPattern(pat: MatchingRule.Pattern;
                               t: MatchingRule.Template;
                               hiPatVar: CARDINAL): BOOLEAN =
  VAR s: PatSubst; BEGIN
    FOR i := 0 TO LAST(s) DO s[i] := NIL END (* FOR *);
    RETURN MatchesLargerPatternWork(pat, t, t, hiPatVar, s)
  END MatchesLargerPattern;

PROCEDURE MatchesLargerPatternWork(
    pat: MatchingRule.Pattern;
    origT: MatchingRule.Template;
    t: MatchingRule.Template;
    hiPatVar: CARDINAL;
    VAR s: PatSubst): BOOLEAN =
  VAR empty := MatchingRule.PatVarNumSet{};
      set: ARRAY [0..MatchingRule.MaxPatVars-1] OF BOOLEAN;
  BEGIN
    IF t # NIL THEN
      FOR i := 0 TO hiPatVar-1 DO set[i] := s[i] # NIL END (* FOR *);
      IF PatMatchWork(pat, t, s) THEN
        FOR i := 0 TO hiPatVar-1 DO
          IF NOT (ISTYPE(s[i], MatchingRule.PatVar) OR
                  MatchingRule.PatVars(s[i]) = empty) THEN
            (* We make one exception: we allow recursive definitions
               over the integers that define a function "f(n)" in terms
               of "f(n-c)", as long as the template restricts "n" to be
               greater than or equal to some constant. *)
            IF TRUE OR
              NOT RecursiveIntFunc(origT, MatchingRule.pv[i], s[i]) THEN
              RETURN TRUE
            END (* IF *)
          END (* IF *)
        END (* FOR *) 
      END (* IF *);
      FOR i := 0 TO hiPatVar-1 DO
        IF NOT set[i] THEN s[i] := NIL END (* IF *)
      END (* FOR *)
    END (* IF *);
    (* If we didn't return TRUE, try the subterms of "t." *)
    TYPECASE t OF
    | NULL => 
        RETURN FALSE
    | RefList.T(rl) =>
        RETURN rl.head # PredSx.forallSym AND rl.head # PredSx.existsSym AND
               (MatchesLargerPatternWork(pat, origT, rl.head, hiPatVar, s) OR
                MatchesLargerPatternWork(pat, origT, rl.tail, hiPatVar, s))
    ELSE
        RETURN FALSE
    END (* TYPECASE *)
  END MatchesLargerPatternWork;

PROCEDURE RecursiveIntFunc(t: MatchingRule.Template;
                           pv: MatchingRule.PatVar;
                           match: REFANY): BOOLEAN =
  PROCEDURE PVBoundedBelowLit(lit: REFANY; pv: MatchingRule.PatVar;
                              sense := FALSE): BOOLEAN =
    BEGIN
      TYPECASE lit OF
      | RefList.T(rl) =>
          IF rl.head = PredSx.notSym THEN
            RETURN PVBoundedBelowLit(rl.tail, pv, NOT sense)
          ELSIF rl.head = PredSx.ltSym OR rl.head = PredSx.leSym OR
                rl.head = PredSx.gtSym OR rl.head = PredSx.geSym THEN
            VAR varFirst: BOOLEAN; BEGIN
              varFirst := (rl.head = PredSx.gtSym OR rl.head = PredSx.geSym);
              IF NOT sense THEN varFirst := NOT varFirst END (* IF *);
              IF varFirst THEN
                RETURN rl.tail.head = pv AND
                       ISTYPE(rl.tail.tail.head, REF INTEGER)
              ELSE                   
                RETURN ISTYPE(rl.tail.head, REF INTEGER) AND
                       rl.tail.tail.head = pv
              END (* IF *)
            END (* BEGIN *)
          ELSE
            RETURN FALSE
          END (* IF *)
      ELSE
          RETURN FALSE
      END (* TYPECASE *)
    END PVBoundedBelowLit;
  PROCEDURE PVBoundedBelow(clause: MatchingRule.Template;
                           pv: MatchingRule.PatVar): BOOLEAN =
    BEGIN
      TYPECASE clause OF
      | RefList.T(rl) =>
          IF rl.head = PredSx.orSym THEN
            VAR lits := rl.tail; BEGIN
              WHILE lits # NIL DO
                IF PVBoundedBelowLit(lits.head, pv) THEN
                  RETURN TRUE
                END (* IF *);
                lits := lits.tail
              END (* WHILE *);
              RETURN FALSE
            END (* BEGIN *)
          ELSE
            RETURN PVBoundedBelowLit(t, pv)
          END (* IF *)
      ELSE
          RETURN FALSE
      END (* TYPECASE *)
    END PVBoundedBelow;
  BEGIN
    TYPECASE match OF
    | RefList.T(rl) =>
        IF rl.head = PredSx.minusSym THEN
          IF rl.tail.head = pv AND ISTYPE(rl.tail.tail.head, REF INTEGER)
            OR rl.tail.tail.head = pv AND ISTYPE(rl.tail.head, REF INTEGER) THEN
            RETURN PVBoundedBelow(t, pv)
          END (* IF *)
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    RETURN FALSE
  END RecursiveIntFunc;

TYPE PatSubst = ARRAY [0..MatchingRule.MaxPatVars-1] OF REFANY;

(* The arguments "pat" and "sx" are"MatchingRule.Template"'s; that is,
   S-expressions augmented with "MatchingRule.PatVar"s.  If "pat" matches
   "sx", returns "TRUE" and sets "res" to the substitution that
   produces the match.  (Applying a "PatSubst" "ps" to a
   "MatchingRule.Template" means replacing "PatVar" $n$ with "ps"[$n].)
   Otherwise, returns FALSE.
*)
PROCEDURE PatMatch(pat, sx: MatchingRule.Template;
                   VAR (*OUT*) res: PatSubst): BOOLEAN =
  BEGIN
    FOR i := 0 TO LAST(res) DO res[i] := NIL END (* FOR *);
    RETURN PatMatchWork(pat, sx, res)
  END PatMatch;

PROCEDURE PatMatchWork(pat, sx: MatchingRule.Template;
                       VAR res: PatSubst;
                       patMatch := TRUE): BOOLEAN =
  BEGIN
    TYPECASE pat OF
    | NULL =>
        RETURN sx = NIL
    | MatchingRule.PatVar(pv) =>
        IF patMatch THEN
          IF res[pv^] # NIL THEN
            RETURN PatMatchWork(res[pv^], sx, res, patMatch := FALSE)
          ELSE
            res[pv^] := sx;
            RETURN TRUE
          END (* IF *)
        ELSE
          RETURN pv = sx
        END (* IF *)
    | Atom.T(patAt) =>
        RETURN patAt = sx
    | RefList.T(patRl) =>
        TYPECASE sx OF
        | RefList.T(sxRl) =>
            RETURN
              PatMatchWork(patRl.head, sxRl.head, res, patMatch) AND
              PatMatchWork(patRl.tail, sxRl.tail, res, patMatch)
        ELSE
            RETURN FALSE
        END (* TYPECASE *)
    ELSE
        RETURN pat = sx
    END (* TYPECASE *)
  END PatMatchWork;

PROCEDURE PatSubstApply(pat: MatchingRule.Template;
                        READONLY sub: PatSubst): MatchingRule.Template =
  BEGIN
    TYPECASE pat OF
    | NULL => RETURN NIL
    | RefList.T(rl) =>
        RETURN RefList.Cons(PatSubstApply(rl.head, sub),
                            PatSubstApply(rl.tail, sub))
    | MatchingRule.PatVar(pv) => RETURN sub[pv^]
    ELSE
        RETURN pat
    END (* TYPECASE *)
  END PatSubstApply;

PROCEDURE Covers(p1, p2: MatchingRule.Pattern): BOOLEAN =
  VAR pvs1 := MatchingRule.PatVars(p1); sub: PatSubst; BEGIN
    IF PatMatch(p1, p2, sub) THEN
      VAR i := 0; ok := TRUE; seen := MatchingRule.PatVarNumSet{}; BEGIN
        WHILE i < NUMBER(sub) AND ok DO
          TYPECASE sub[i] OF
          | NULL => (*SKIP*)
          | MatchingRule.PatVar(pv) =>
              IF pv^ # i THEN
                IF pv^ IN pvs1 OR pv^ IN seen THEN
                  ok := FALSE
                ELSE
                  seen := seen + MatchingRule.PatVarNumSet{pv^}
                END (* IF *)
              END (* IF *)
          ELSE
              ok := FALSE
          END (* TYPECASE *);
          INC(i)
        END (* WHILE *);
        RETURN ok
      END (* BEGIN *)
    ELSE
      RETURN FALSE
    END (* IF *)
  END Covers;

PROCEDURE Intern(
    sx: REFANY;
    READONLY sub: MatchingRule.Substitution :=
                       MatchingRule.EmptySub): Enode.T =
  BEGIN 
    RETURN InternWork(sx, sub, isFullTerm := TRUE)
  END Intern;

(* Requires "isFullTerm" to be TRUE iff "sx" represents an entire term. *)
PROCEDURE InternWork(
    sx: REFANY;
    READONLY sub: MatchingRule.Substitution;
    isFullTerm: BOOLEAN):  Enode.T =
  VAR res: Enode.T; BEGIN
    TYPECASE sx OF <*NOWARN*>
    | NULL =>
        res := Enode.enil
    | MatchingRule.PatVar(pv) =>
        RETURN sub[pv^]
    | Enode.T =>
        RETURN sx
    | Atom.T(sym) =>
        res := Enode.FromSym(sym, NOT isFullTerm)
    | RefList.T(l) =>
        IF isFullTerm THEN
          TYPECASE l.head OF
          | Atom.T, Enode.FSym =>
              VAR op: Enode.FSym; BEGIN
                TYPECASE l.head OF <*NOWARN*>
                | Atom.T(at) =>
                    op := Enode.FromSym(at, fsym := TRUE)
                | Enode.FSym(fs) => 
                    op := fs
                END (* TYPECASE *);
                IF op = Enode.plusFS OR op = Enode.minusFS OR 
                  op = Enode.timesFS THEN
                  VAR arg1, arg2: Enode.T; b := FALSE; BEGIN
                    IF op = Enode.minusFS AND l.tail.tail = NIL THEN
                      arg1 := Enode.FromInt(0);
                      arg2 := InternWork(l.tail.head, sub, TRUE);
                    ELSE
                      arg1 := InternWork(l.tail.head, sub, TRUE);
                      IF (op = Enode.plusFS OR op = Enode.timesFS) AND
                        l.tail.tail.tail # NIL THEN
                        arg2 := InternWork(RefList.Cons(l.head, l.tail.tail),
                                           sub, TRUE)
                      ELSE
                        arg2 := InternWork(l.tail.tail.head, sub, TRUE)
                      END (* IF *)
                    END (* IF *);

                    res := Enode.Cons(op, Enode.Cons(arg1,
                                                     Enode.Cons(arg2, 
                                                                Enode.enil)));
                    IF NOT Prover.lazySimplexIntern THEN
                      IF res.getMisc(FALSE) = NIL OR
                        res.getMisc(FALSE).unknown = NIL THEN
                        IF op = Enode.plusFS THEN
                          b := Simplex.IsSum(res, arg1, arg2)
                        ELSIF op = Enode.minusFS THEN
                          b := Simplex.IsDiff(res, arg1, arg2)
                        ELSIF op = Enode.timesFS THEN
                          b := Simplex.IsProd(res, arg1, arg2)
                        END (* IF *);
                        <*ASSERT b*>
                      END (* IF *)
                    END (* IF *)

                  END (* BEGIN *)
                ELSE
                  res := Enode.Cons(op, InternWork(l.tail, sub, FALSE))
                END (* IF *)
              END (* BEGIN *)
          ELSE
              res := Enode.Cons(InternWork(l.head, sub, NOT isFullTerm),
                                InternWork(l.tail, sub, FALSE))
          END (* TYPECASE *)
        ELSE
          res := Enode.Cons(InternWork(l.head, sub, NOT isFullTerm),
                            InternWork(l.tail, sub, FALSE))
        END (* IF *)
    | REF INTEGER(ri) =>
        res := Enode.FromInt(ri^)
    | REF LONGREAL(rlr) =>
        res := Enode.FromLongReal(rlr^);
    END (* TYPECASE *);
    RETURN res
  END InternWork;

PROCEDURE PrintList(wr: Wr.T; l: T; printRule := FALSE; normForm := FALSE) =
  (* Prints the clauses of "l" on standard output. *)
  VAR tw: TextWr.T; BEGIN
    Wr.PutText(
        wr, "---------- " & Fmt.Int(ListLength(l)) & " clauses -----------\n");
    IF normForm THEN tw := NEW(TextWr.T) END (* IF *);
    WHILE l # NIL DO
      IF normForm THEN tw := tw.init() END (* IF *);
      VAR p := l.lits; VAR clauseHash: Word.T := 0; BEGIN
        Wr.PutText(wr, "\n" & Fmt.Real(l.score) & " (");
        WHILE p # NIL DO
          IF normForm THEN
            VAR lit: AF.Lit := p.head;
                litSx := AF.LitToSx(p.head, normForm);
                hash := PredSx.Hash(litSx);
            BEGIN
              clauseHash := Word.Xor(clauseHash, hash);
              Wr.PutText(tw, " (#" & Fmt.Int(hash MOD 10000));
              IF doNUClauseUIDs THEN
                Wr.PutText(tw, "[");
                IF NOT lit.sense THEN Wr.PutText(tw, "N") END (* IF *);
                Wr.PutText(tw, Fmt.Int(lit.af.id) & "]")
              END (* IF *);
              Wr.PutText(tw, " ");
              Sx.Print(tw, litSx);
              Wr.PutText(tw, ")")
            END (* BEGIN *)
          ELSE
            AF.PrintLit(wr, p.head, normForm)
          END (* IF *);
          p := p.tail;
          IF NOT normForm AND p # NIL THEN Wr.PutChar(wr, ' ') END (* IF *)
        END (* WHILE *);
        IF normForm THEN
          Wr.PutText(wr, "#" & Fmt.Int(clauseHash MOD 10000));
          IF doNUClauseUIDs THEN
            Wr.PutText(wr, "[" & Fmt.Int(l.uid) & "]")
          END (* IF *);
          Wr.PutText(wr, "{" & Fmt.Real(l.score) & "}");
          Wr.PutText(wr, " " & TextWr.ToText(tw))
        END (* IF *);
        Wr.PutText(wr, " )\n")
      END (* BEGIN *);
      IF printRule AND l.mr # NIL THEN
        Wr.PutText(wr, "    [from rule ");
        Sx.Print(wr, l.mr.toSx());
        Wr.PutText(wr, "]\n")
      END (* IF *);
      l := l.succ
    END (* WHILE *);
    Wr.PutText(wr, "------------------------------\n");
    Wr.Flush(wr)
  END PrintList;

PROCEDURE ToSx(cl: T; normForm := FALSE): Sx.T =
  VAR res: RefList.T := NIL; lits := cl.lits; BEGIN
    IF normForm THEN Enode.ComputeSxSizes() END (* IF *);
    WHILE lits # NIL DO
      res := RefList.Cons(AF.LitToSx(lits.head, normForm), res);
      lits := lits.tail
    END (* WHILE *);
    RETURN RefList.ReverseD(res)
  END ToSx;

PROCEDURE FingerP(lits: AF.LitList): FPrint.T =
  VAR res := FPrint.Zero; BEGIN
    WHILE lits # NIL DO
      res := FPrint.Combine(res, AF.LitFP(lits.head));
      lits := lits.tail
    END (* WHILE *);
    RETURN res
  END FingerP;
    
PROCEDURE ListLength(c: T): CARDINAL =
  VAR i := 0; BEGIN
    WHILE c # NIL DO INC(i); c := c.succ END (* WHILE *);
    RETURN i
  END ListLength;

PROCEDURE ListAppendD(cl1, cl2: T): T =
  (* Returns the head of the list that result from (destructively)
     appending the clause list headed by "cl2" to the end of the
     clause list headed by "cl1". *)
  VAR cl1a := cl1; BEGIN
    IF cl1a = NIL THEN RETURN cl2 END (* IF *);
    WHILE cl1a.succ # NIL DO cl1a := cl1a.succ END (* WHILE *);
    cl1a.succ := cl2;
    IF cl2 # NIL THEN cl2.pred := cl1a END (* IF *);
    RETURN cl1
  END ListAppendD;

PROCEDURE RefListCopy(rl: RefList.T): RefList.T =
  BEGIN
    IF rl = NIL THEN
      RETURN NIL
    ELSE
      RETURN RefList.Cons(rl.head, RefListCopy(rl.tail))
    END (* IF *)
  END RefListCopy;

VAR clauseUID := 0;

PROCEDURE UnsetProps(lits: RefList.T) =
  BEGIN
    WHILE lits # NIL DO
      VAR head: AF.Lit := lits.head; BEGIN
        head.af.props := head.af.props - AF.PropSet{0,1}
      END;
      lits := lits.tail
    END
  END UnsetProps;

PROCEDURE ClauseInit(cl: T): T =
    (* Remove duplicate literals (using semantic equivalence).  This
       may prevent the assertion of some labels that might otherwise
       have been asserted. *)
  VAR lits := cl.lits; BEGIN
    IF lits = NIL THEN
      lits := RefList.List1(AF.falseLit);
      cl.lits := lits
    END (* IF *);
    (* remove duplicates from "lits". *)
    WHILE lits.tail # NIL DO
      VAR head: AF.Lit := lits.head; BEGIN
        IF head.lbls = NIL THEN
          head.af.props := head.af.props + AF.PropSet{0};
          IF head.sense THEN
            head.af.props := head.af.props + AF.PropSet{1}
          END (* IF *)
        END (* IF *);
        head.clause := cl
      END (* BEGIN *);
      VAR nexthead: AF.Lit := lits.tail.head; BEGIN
        (* Make sure not to delete any labelled literals. *)
        IF NOT 0 IN nexthead.af.props OR nexthead.lbls # NIL THEN
          lits := lits.tail
        ELSIF nexthead.sense = (1 IN nexthead.af.props) THEN
          lits.tail := lits.tail.tail
        ELSE
          UnsetProps(cl.lits);
          cl.lits.head := AF.trueLit;
          cl.lits.tail := NIL;
          lits := cl.lits
        END (* IF *)
      END (* BEGIN *)
    END (* WHILE *);
    UnsetProps(cl.lits);
    cl.fp := FingerP(cl.lits);
    cl.uid := clauseUID; 
    INC(clauseUID);
    RETURN cl
  END ClauseInit;

PROCEDURE DBGPrint(wr: Wr.T; cl: T) =
  VAR i := 0; BEGIN
    <*ASSERT cl # NIL *>
    Wr.PutText(wr, "(OR ;; clause uid " & Fmt.Int(cl.uid) &
      "; score is " & Fmt.Real(cl.score));
    Wr.Flush(wr);
    IF cl.mr # NIL THEN
      Wr.PutText(wr, "; from rule #" & Fmt.Int(cl.mr.id));
      Wr.Flush(wr);
    END (* IF *);
    CASE cl.promoted OF
    | PromoteState.Not =>
    | PromoteState.Promoted => Wr.PutText(wr, "; promoted")
    | PromoteState.Immed => Wr.PutText(wr, "; immed promote")
    END (* TYPECASE *);
    Wr.PutText(wr, ".\n");
    Wr.Flush(wr);
    VAR lits := cl.lits; BEGIN
      WHILE lits # NIL DO
        Wr.PutText(wr, ";; literal number " & Fmt.Int(i) & "\n");
        Wr.Flush(wr);
        AF.PrintLit2(wr, lits.head);
        lits := lits.tail;
        IF lits # NIL THEN
          Wr.PutText(wr, "\n    ");
          Wr.Flush(wr);
        END (* IF *);
        INC(i)
      END (* WHILE *)
    END (* BEGIN *);
    Wr.PutText(wr, ")\n");
    Wr.Flush(wr)
  END DBGPrint;

VAR
  doNUClauseUIDs: BOOLEAN;

PROCEDURE Init() =
  BEGIN
    PredSx.Init();
    PredSyms := NEW(AtomSetDef.T).fromArray(
                                   ARRAY OF Atom.T{
                                                PredSx.eqSym,
                                                PredSx.diffSym,
                                                PredSx.andSym,
                                                PredSx.orSym,
                                                PredSx.notSym,
                                                PredSx.impliesSym,
                                                PredSx.expliesSym,
                                                PredSx.ltSym,
                                                PredSx.gtSym,
                                                PredSx.leSym,
                                                PredSx.geSym});
    doNUClauseUIDs := Env.Get("PROVER_PRINT_NU_NO_UIDS") = NIL;
  END Init;

BEGIN
END Clause.
