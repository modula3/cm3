(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May  9 10:58:32 PDT 2002 by saxe                     *)
(*      modified on Wed Oct 23 14:35:10 PDT 1996 by detlefs                  *)

UNSAFE MODULE PredSx;

(* UNSAFE because of LOOPHOLE in Hash. *)

IMPORT PredSxList;
IMPORT Atom, Fmt, Integer, Word, LabelName;
IMPORT RefList, RefListSort, AtomSet, AtomRefTbl, AtomIntTbl,
       AtomSetList, AtomSetDef, AtomSeq;

(* Debugging *)
IMPORT Wr, Stdio, Sx, Thread;
IMPORT SxPrint, Prover;
IMPORT MatchingRule; (* for EmptySub for tracing *)
<*FATAL Thread.Alerted, Wr.Failure, Sx.PrintError*>


PROCEDURE CNF(form: T; neg := FALSE): T RAISES { Error } =
  VAR cnf := CNFWork(form, neg); BEGIN
    TYPECASE cnf OF
    | RefList.T(rl) =>
        IF rl.head = andSym THEN
          VAR disjs := rl.tail; res: RefList.T := NIL; BEGIN
            WHILE disjs # NIL DO
              VAR disj := CanonicalizeClause(disjs.head); BEGIN
                IF disj # trueSym THEN
                  res := RefList.Cons(disj, res)
                END (* IF *);
              END (* BEGIN *);
              disjs := disjs.tail
            END (* WHILE *);
            IF res = NIL THEN RETURN trueSym
            ELSIF res.tail = NIL THEN RETURN res.head
            ELSE
              res := RemoveComplementaryDisjuncts(res);
              IF res.tail = NIL THEN
                RETURN res.head
              ELSE
                RETURN RefList.Cons(andSym, res)
              END (* IF *)
            END (* IF *)
          END (* BEGIN *)
        ELSE
          RETURN rl
        END (* IF *)
    ELSE
        RETURN cnf
    END (* TYPECASE *)
  END CNF;

PROCEDURE ProcessLabels(form: T; push: BOOLEAN;
                        vars: RefList.T := NIL): T =
  BEGIN RETURN ProcessLabelsWork(form, push, vars # NIL, vars, NIL, NIL, TRUE)
  END ProcessLabels;

PROCEDURE ProcessLabelsWork(form: T;
                            push, ignoreUniv: BOOLEAN; vars: RefList.T;
                            posLbls, negLbls: RefList.T; sense: BOOLEAN): T =
  BEGIN
    TYPECASE form OF
    | RefList.T(rl) =>
        IF rl.head = labelSym OR rl.head = lblNegSym THEN
          VAR ln := rl.tail.head; p := rl.tail.tail.head; BEGIN
            ln := LblQuant(ln, vars);
            IF push THEN
              RETURN ProcessLabelsWork(p, push, ignoreUniv, vars,
                                       posLbls, RefList.Cons(ln, negLbls),
                                       sense)
            ELSE
              RETURN RefList.List3(lblNegSym, ln,
                                   ProcessLabelsWork(
                                       p, push, ignoreUniv, vars,
                                       NIL, NIL, sense))
            END (* IF *)
          END (* BEGIN *)
        ELSIF rl.head = lblPosSym THEN
          VAR ln := rl.tail.head; p := rl.tail.tail.head; BEGIN
            ln := LblQuant(ln, vars);
            IF push THEN
              RETURN ProcessLabelsWork(p, push, ignoreUniv, vars,
                                       RefList.Cons(ln, posLbls), negLbls,
                                       sense)
            ELSE
              RETURN RefList.List3(lblPosSym, ln,
                                   ProcessLabelsWork(
                                       p, push, ignoreUniv, vars,
                                       NIL, NIL, sense))
            END (* IF *)
          END (* BEGIN *)
        ELSIF rl.head = forallSym AND sense OR
              rl.head = existsSym AND NOT sense THEN
          VAR vl: RefList.T := rl.tail.head;
              patSpec := QuantPatSpec(rl);
              body := QuantBody(rl);
          BEGIN
            IF NOT ignoreUniv THEN
              vars := RefList.Append(vl, vars)
            END (* IF *);
            RETURN AddLabels(
                       MkQuant(rl.head, vl, patSpec,
                               ProcessLabelsWork(body, push, TRUE,
                                                 vars, NIL, NIL, sense)),
                       posLbls, negLbls)
          END (* BEGIN *)
        ELSIF rl.head = forallSym AND NOT sense OR
              rl.head = existsSym AND sense THEN
          VAR vl: RefList.T := rl.tail.head;
              patSpec := QuantPatSpec(rl);
              body := QuantBody(rl);
          BEGIN
            RETURN MkQuant(rl.head, vl, patSpec,
                           ProcessLabelsWork(body, push, ignoreUniv, vars,
                                             posLbls, negLbls, sense))
          END (* BEGIN *)
        ELSIF rl.head = notSym THEN
          RETURN Not(ProcessLabelsWork(rl.tail.head, push, ignoreUniv, vars,
                                       negLbls, posLbls, NOT sense))
        ELSIF rl.head = andSym THEN
          IF push THEN
	    VAR args := rl.tail;
		n := Sx.FromInt(RefList.Length(args));
		split := Sx.FromInt(LabelName.splitCntr);
		i := 1;
		newArgs: RefList.T := NIL;
		newPos: RefList.T := NIL;
	    BEGIN
	      INC(LabelName.splitCntr);
	      WHILE posLbls # NIL DO
		VAR ln := posLbls.head; BEGIN
		  newPos := RefList.Cons(RefList.List3(lblNameOrSym, ln, split),
					 newPos)
		END (* BEGIN *);
		posLbls := posLbls.tail
	      END (* WHILE *);
	      WHILE args # NIL DO
		VAR newNewPos: RefList.T := NIL;
		    tmpNewPos := newPos;
		BEGIN
		  WHILE tmpNewPos # NIL DO
		    newNewPos := RefList.Cons(
				     RefList.Cons(
				       lblNameAndSym,
				       RefList.List3(tmpNewPos.head,
						     Sx.FromInt(i), n)),
				     newNewPos);
		    tmpNewPos := tmpNewPos.tail
		  END (* WHILE *);
		  newArgs := RefList.Cons(
				 ProcessLabelsWork(args.head, push, ignoreUniv,
                                                   vars,
                                                   newNewPos, negLbls, sense),
				 newArgs);
		END (* BEGIN *);
		args := args.tail; INC(i)
	      END (* WHILE *);
	      RETURN RefList.Cons(andSym, RefList.ReverseD(newArgs))
	    END (* BEGIN *)
          ELSE
            RETURN ProcessLabelArgs(rl, push, ignoreUniv,
                                    vars, posLbls, negLbls, sense)
          END (* IF *)                             
        ELSIF rl.head = orSym THEN
          IF push THEN
	    VAR args := rl.tail;
		n := Sx.FromInt(RefList.Length(args));
		split := Sx.FromInt(LabelName.splitCntr);
		i := 1;
		newArgs: RefList.T := NIL;
		newNeg: RefList.T := NIL;
	    BEGIN
	      INC(LabelName.splitCntr);
	      WHILE negLbls # NIL DO
		VAR ln := negLbls.head; BEGIN
		  newNeg := RefList.Cons(
                                RefList.List3(lblNameOrSym, ln, split),
                                newNeg)
		END (* BEGIN *);
		negLbls := negLbls.tail
	      END (* WHILE *);
	      WHILE args # NIL DO
		VAR newNewNeg: RefList.T := NIL;
		    tmpNewNeg := newNeg;
		BEGIN
		  WHILE tmpNewNeg # NIL DO
		    newNewNeg := RefList.Cons(
				     RefList.Cons(
				       lblNameAndSym,
				       RefList.List3(tmpNewNeg.head,
						     Sx.FromInt(i), n)),
				     newNewNeg);
		    tmpNewNeg := tmpNewNeg.tail
		  END (* WHILE *);
		  newArgs := RefList.Cons(
				 ProcessLabelsWork(args.head, push, ignoreUniv,
                                                   vars,
                                                   posLbls, newNewNeg, sense),
				 newArgs);
		END (* BEGIN *);
		args := args.tail; INC(i)
	      END (* WHILE *);
	      RETURN RefList.Cons(orSym, RefList.ReverseD(newArgs))
	    END (* BEGIN *)
          ELSE
            RETURN ProcessLabelArgs(rl, push, ignoreUniv,
                                    vars, posLbls, negLbls, sense)
          END (* IF *)
        ELSE
          RETURN AddLabels(form, posLbls, negLbls)
        END (* IF *)
    ELSE
        RETURN AddLabels(form, posLbls, negLbls)
    END (* TYPECASE *)
  END ProcessLabelsWork;

PROCEDURE ProcessLabelArgs(rl: RefList.T; push, ignoreUniv: BOOLEAN;
                           vars: RefList.T;
                           posLbls, negLbls: RefList.T; sense: BOOLEAN): T =
  VAR newArgs: RefList.T := NIL; args := rl.tail; BEGIN
    WHILE args # NIL DO
      newArgs := RefList.Cons(
                     ProcessLabelsWork(args.head, push, ignoreUniv, vars,
                                       posLbls, negLbls, sense),
                     newArgs);
      args := args.tail
    END (* WHILE *);
    RETURN RefList.Cons(rl.head, RefList.ReverseD(newArgs))
  END ProcessLabelArgs;

PROCEDURE AddLabels(p: T; posLbls, negLbls: RefList.T): T =
  BEGIN
    WHILE negLbls # NIL DO
      p := RefList.List3(lblNegSym, negLbls.head, p);
      negLbls := negLbls.tail
    END (* WHILE *);
    WHILE posLbls # NIL DO
      p := RefList.List3(lblPosSym, posLbls.head, p);
      posLbls := posLbls.tail
    END (* WHILE *);
    RETURN p
  END AddLabels;

PROCEDURE LblQuant(ln: REFANY; vars: RefList.T): REFANY =
  BEGIN
    IF vars = NIL THEN RETURN ln
    ELSE RETURN RefList.List3(lblNameQuantSym, ln, vars)
    END (* IF *)
  END LblQuant;

PROCEDURE Desugar(form: T): T =
  BEGIN
    TYPECASE form OF
    | NULL => RETURN form
    | RefList.T(rl) =>
        IF rl.head = impliesSym THEN
          <*ASSERT RefList.Length(rl) = 3*>
          RETURN RefList.List3(orSym, Not(Desugar(rl.tail.head)),
                               Desugar(rl.tail.tail.head))
        ELSIF rl.head = expliesSym THEN
          <*ASSERT RefList.Length(rl) = 3*>
          RETURN RefList.List3(orSym, Not(Desugar(rl.tail.tail.head)),
                               Desugar(rl.tail.head))
        ELSIF rl.head = iffSym THEN
          <*ASSERT RefList.Length(rl) = 3*>
          VAR x := Desugar(rl.tail.head);
              y := Desugar(rl.tail.tail.head);
          BEGIN
            RETURN RefList.List3(
                       andSym,
                       RefList.List3(orSym, Not(x), y),
                       RefList.List3(orSym, Not(y), x))
          END (* BEGIN *)
        ELSE
          RETURN RefList.Cons(Desugar(rl.head), Desugar(rl.tail))
        END (* IF *)
    ELSE
        RETURN form
    END (* TYPECASE *)
  END Desugar;
          

VAR compDisjDebug := FALSE;

(* Assumes "disjs" is a list of canonicalized clauses.  Returns a
   list of clauses whose conjunction is equivalent to the conjunction
   of the clauses in "disjs", in which the following simplications
   have been performed:

   First, we remove any clause subsumed by another; if there is a pair

| (OR a1 ... an), (OR a1 ... an .. an+m)

   we delete the second clause.

   Next, for each pair of clauses that can be put in the forms

| (OR a1 a2 ... an ... an+m), (OR (NOT a1) a2 ... an)

   simplify the first clause to

| (OR a2 ... an ... an+m)


   (If "a1" is true, the first clause is true, and the second clause
   implies the rewritten clause.  If "a1" is false, the rewritten
   clause is equivalent to the first clause.

   Next for each pair of clauses that is exactly equivalent except for
   one complemented literal

| (OR a1 a2 ... an), (OR (NOT a1) a2 ... an)

   rewrite them to the single clause

| (OR a2 ... an)

   NB: destructively modifies "disjs". *)
PROCEDURE RemoveComplementaryDisjuncts(disjs: RefList.T): RefList.T =
  BEGIN
    IF compDisjDebug THEN
      Wr.PutText(Stdio.stdout, "Removing complementary disjuncts from:\n\n");
      VAR disjs2 := disjs; BEGIN
        WHILE disjs2 # NIL DO
          Sx.Print(Stdio.stdout, disjs2.head); Wr.PutText(Stdio.stdout, "\n");
          disjs2 := disjs2.tail
        END (* WHILE *);
      END (* BEGIN *);
      Wr.PutText(Stdio.stdout, "\n");
      Wr.Flush(Stdio.stdout)
    END (* IF *);
    (* Now remove complementary disjuncts. *)
    VAR mod := TRUE; l: RefList.T; BEGIN
      WHILE mod DO
        mod := FALSE; l := disjs;
        WHILE l # NIL AND NOT mod DO
          VAR rest := disjs; prevRest: RefList.T := NIL; BEGIN
            WHILE rest # NIL AND NOT mod DO
              IF rest # l THEN
		VAR equalLength: BOOLEAN;
		    res := ComplementaryDisjuncts(l.head, rest.head, equalLength);
		BEGIN
		  IF res # NIL THEN
		    IF equalLength THEN
		      l.head := res;
		      IF prevRest = NIL THEN
			disjs := rest.tail
		      ELSE
			prevRest.tail := rest.tail
		      END (* IF *);
		      IF compDisjDebug THEN
			Wr.PutText(
			    Stdio.stdout,
			    "After combining a complementary clause pair:\n\n")
		      END (* IF *)
		    ELSE
		      rest.head := res;
		      IF compDisjDebug THEN
			Wr.PutText(Stdio.stdout, "After retracting a clause:\n\n")
		      END (* IF *);
		    END (* IF *);
		    mod := TRUE;
		    IF compDisjDebug THEN
		      VAR disjs2 := disjs; BEGIN
			WHILE disjs2 # NIL DO
			  Sx.Print(Stdio.stdout, disjs2.head);
			  Wr.PutText(Stdio.stdout, "\n");
			  disjs2 := disjs2.tail
			END (* WHILE *);
		      END (* BEGIN *);
		      Wr.PutText(Stdio.stdout, "\n");
		      Wr.Flush(Stdio.stdout)
		    END (* IF *)
		  ELSIF Subsumes(l.head, rest.head) THEN
		    IF prevRest = NIL THEN
		      disjs := rest.tail
		    ELSE
		      prevRest.tail := rest.tail
		    END (* IF *);
		    mod := TRUE;
		    IF compDisjDebug THEN
		      Wr.PutText(Stdio.stdout,
				 "After removing subsumed clause:\n\n");
		      VAR disjs2 := disjs; BEGIN
			WHILE disjs2 # NIL DO
			  Sx.Print(Stdio.stdout, disjs2.head);
			  Wr.PutText(Stdio.stdout, "\n");
			  disjs2 := disjs2.tail
			END (* WHILE *);
		      END (* BEGIN *);
		      Wr.PutText(Stdio.stdout, "\n");
		      Wr.Flush(Stdio.stdout)
		    END (* IF *)
		  END (* IF *)
                END (* BEGIN *)
              END (* IF *);
              prevRest := rest; rest := rest.tail
            END (* WHILE *)
          END (* BEGIN *);
          l := l.tail
        END (* WHILE *)
      END (* WHILE *)
    END (* BEGIN *);
    RETURN disjs
  END RemoveComplementaryDisjuncts;


(* Returns "TRUE" iff "c1" subsumes "c2"; that is "c1 => c2".  "c1"
   and "c2" must be in canonical form. *)
PROCEDURE Subsumes(c1, c2: T): BOOLEAN =
  BEGIN
    TYPECASE c1 OF <*NOWARN*>
    | Atom.T(at1) =>
        TYPECASE c2 OF <*NOWARN*>
        | Atom.T(at2) =>
            RETURN at1 = at2
        | RefList.T(rl2) =>
            RETURN rl2.head = orSym AND PredSxList.Member(rl2.tail, at1)
        END (* TYPECASE *)
    | RefList.T(rl1) =>
        IF rl1.head = orSym THEN
          TYPECASE c2 OF <*NOWARN*>
          | Atom.T => RETURN FALSE
          | RefList.T(rl2) =>
              IF rl2.head = orSym THEN
                VAR lits1 := rl1.tail; lits2 := rl2.tail; BEGIN
                  WHILE lits1 # NIL DO
                    WHILE lits2 # NIL AND NOT Equal(lits1.head, lits2.head) DO
                      lits2 := lits2.tail
                    END (* WHILE *);
                    IF lits2 = NIL THEN RETURN FALSE END (* IF *);
                    lits1 := lits1.tail
                  END (* WHILE *);
                  RETURN TRUE
                END (* BEGIN *)
              END (* IF *)
          END (* TYPECASE *);
          RETURN FALSE
        ELSE
          TYPECASE c2 OF <*NOWARN*>
          | RefList.T(rl2) =>
              IF rl2.head = orSym THEN
                RETURN PredSxList.Member(rl2.tail, rl1)
              ELSE
                RETURN Equal(rl1, rl2)
              END (* IF *)
          ELSE
              RETURN FALSE
          END (* TYPECASE *)
        END (* IF *)
    END (* TYPECASE *)
  END Subsumes;

VAR l1Def := RefList.List1(NIL);  (* NB: The use of these globals assumes *)
    l2Def := RefList.List1(NIL);  (* a single-threaded program. *)


(* "d1" and "d2" must be clauses in canonical form.
   If "d1" and "d2" can be written in the forms

| (OR (NOT a1) a2 ... an), (OR a1 a2 ... an ... an+m)
  
   returns the clause "(OR a2 ... an+m)".  (Note that this may be a
   singleton clause, in which case the return value is simply a
   literal, or an empty clause, in which case "falseSym" is returned.)
   "equalLength is set to TRUE iff "m = 0".
   If "d1" and "d2" are not complementary in this way, returns "NIL". *) 
PROCEDURE ComplementaryDisjuncts(d1, d2: T;
                                 VAR (*OUT*) equalLength: BOOLEAN): T =
  VAR lits1, lits2: RefList.T := NIL; BEGIN
    equalLength := TRUE;
    TYPECASE d1 OF <*NOWARN*>
    | Atom.T(at) =>
        l1Def.head := at; lits1 := l1Def
    | RefList.T(rl) =>
        IF rl.head = orSym THEN lits1 := rl.tail
        ELSE l1Def.head := rl; lits1 := l1Def
        END (* IF *)
    END (* TYPECASE *);
    TYPECASE d2 OF <*NOWARN*>
    | Atom.T(at) =>
        l2Def.head := at; lits2 := l2Def
    | RefList.T(rl) =>
        IF rl.head = orSym THEN lits2 := rl.tail
        ELSE l2Def.head := rl; lits2 := l2Def
        END (* IF *)
    END (* TYPECASE *);
    VAR l1 := lits1; l2 := lits2;
        badLitSeen := FALSE; badLit: T := NIL; notBadLit: T := NIL; 
    BEGIN
      WHILE l2 # NIL DO
	IF l1 # NIL AND Equal(l1.head, l2.head) THEN
	  l1 := l1.tail; l2 := l2.tail
	ELSIF l1 # NIL AND badLit = NIL THEN
	  badLit := l1.head; notBadLit := Not(badLit); l1 := l1.tail;
	ELSIF badLit # NIL AND Equal(badLit, l2.head) THEN
	  badLit := NIL; l2 := l2.tail
	ELSIF badLit # NIL AND Equal(notBadLit, l2.head) THEN
	  badLitSeen := TRUE; l2 := l2.tail
	ELSE
	  equalLength := FALSE; l2 := l2.tail
	END (* IF *)
      END (* WHILE *);
      IF l1 = NIL AND badLitSeen THEN
	<*ASSERT notBadLit # NIL *>
	VAR res: RefList.T := NIL; BEGIN
	  l2 := lits2;
	  WHILE l2 # NIL DO
	    IF NOT Equal(notBadLit, l2.head) THEN
	      res := RefList.Cons(l2.head, res)
	    END (* IF *);
	    l2 := l2.tail
	  END (* WHILE *);
	  IF res = NIL THEN RETURN falseSym
	  ELSIF res.tail = NIL THEN RETURN res.head
	  ELSE RETURN RefList.Cons(orSym, RefList.ReverseD(res))
	  END (* IF *)
	END (* BEGIN *)
      ELSE
	RETURN NIL
      END (* IF *)
    END (* BEGIN *)
  END ComplementaryDisjuncts;

PROCEDURE CanonicalizeClause(cl: T): T =
  BEGIN
    TYPECASE cl OF
    | RefList.T(clRL) =>
        IF clRL.head = orSym THEN
	  VAR lits1 := clRL.tail; res: T := NIL; BEGIN
	    WHILE lits1 # NIL AND res # trueSym DO
	      VAR lits2 := lits1.tail; skip := FALSE; BEGIN
		WHILE lits2 # NIL AND NOT skip AND res # trueSym DO
		  IF Equal(lits1.head, Not(lits2.head)) THEN
		    res := trueSym
		  ELSIF Equal(lits1.head, lits2.head) THEN
		    skip := TRUE
		  END (* IF *);
		  lits2 := lits2.tail
		END (* WHILE *);
		IF NOT skip AND res # trueSym THEN
		  res := RefList.Cons(lits1.head, res)
		END (* IF *);
	      END (* BEGIN *);
	      lits1 := lits1.tail
	    END (* WHILE *);
	    TYPECASE res OF <*NOWARN*>
	    | NULL => <*ASSERT FALSE*>
	    | Atom.T => RETURN res
	    | RefList.T(resRL) =>
		IF resRL.tail = NIL THEN
		  RETURN resRL.head
		ELSE
		  resRL := RefListSort.SortD(resRL, Compare);
		  RETURN RefList.Cons(orSym, resRL)
		END (* IF *)
	    END (* TYPECASE *)
          END (* BEGIN *)
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    (* Otherwise *)
    RETURN cl
  END CanonicalizeClause;

PROCEDURE CNFWork(form: T; neg: BOOLEAN): T RAISES { Error } =
  PROCEDURE DistOr(disj, c2: T): T =
    (* Assumes "disj" is a disjunction of literals, and "c1" is in conjunctive
       normal form.  Returns a formula in conjunctive normal form equivalent
       to "(OR disj c2)". *)
    BEGIN
      TYPECASE c2 OF
      | RefList.T(rl) =>
          IF rl.head = andSym THEN
            VAR c2Disj := rl.tail; res: T := trueSym; BEGIN
              WHILE c2Disj # NIL DO
                res := And(res, Or(disj, c2Disj.head));
                c2Disj := c2Disj.tail
              END (* WHILE *);
              <*ASSERT NOT ISTYPE(res, RefList.T) OR
                       NARROW(res, RefList.T).tail # NIL *>
              RETURN res
            END (* BEGIN *)
          END (* IF *)
      ELSE
      END (* TYPECASE *);
      RETURN Or(disj, c2)
    END DistOr;
  PROCEDURE DistOrs(c1, c2: T): T =
    (* Assumes "c1" and "c2" are in conjunctive normal form.  Returns a
       formula in conjunctive normal form equivalent to "c1 OR c2". *)
    BEGIN
      TYPECASE c1 OF
      | RefList.T(rl) =>
          IF rl.head = andSym THEN
            VAR c1Disj := rl.tail; res: T := trueSym; BEGIN
              WHILE c1Disj # NIL DO
                res := And(res, DistOr(c1Disj.head, c2));
                c1Disj := c1Disj.tail
              END (* WHILE *);
              RETURN res
            END (* BEGIN *)
          END (* IF *)
      ELSE
      END (* TYPECASE *);
      RETURN DistOr(c1, c2)
    END DistOrs;
  BEGIN
    TYPECASE form OF
    | NULL => RETURN NIL
    | RefList.T(rl) =>
        IF rl.head = notSym THEN
          RETURN CNFWork(rl.tail.head, NOT neg)
        ELSIF rl.head = impliesSym THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error("IMPLIES requires two arguments.")
          END (* IF *);
          RETURN CNFWork(RefList.List3(orSym, Not(rl.tail.head),
                                       rl.tail.tail.head),
                         neg)
        ELSIF rl.head = expliesSym THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error("EXPLIES requires two arguments.")
          END (* IF *);
          RETURN CNFWork(RefList.List3(orSym, Not(rl.tail.tail.head),
                                       rl.tail.head),
                         neg)
        ELSIF rl.head = iffSym THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error("IFF requires two arguments.")
          END (* IF *);
          RETURN CNFWork(RefList.List3(
                             andSym,
			     RefList.List3(orSym, Not(rl.tail.head),
					   rl.tail.tail.head),
			     RefList.List3(orSym,
					   rl.tail.head,
					   Not(rl.tail.tail.head))),
			 neg)
        ELSIF (rl.head = andSym AND NOT neg) OR (rl.head = orSym AND neg) THEN
          IF rl.tail = NIL THEN RETURN trueSym
          ELSE RETURN And(CNFWork(rl.tail.head, neg), 
                          CNFWork(RefList.Cons(rl.head, rl.tail.tail), neg))
          END (* IF *)
        ELSIF (rl.head = orSym AND NOT neg) OR (rl.head = andSym AND neg) THEN
          IF rl.tail = NIL THEN RETURN falseSym
          ELSE RETURN DistOrs(CNFWork(rl.tail.head, neg),
                              CNFWork(RefList.Cons(rl.head, rl.tail.tail),
                                      neg))
          END (* IF *)
        ELSIF rl.head = existsSym THEN
          VAR notBod := Not(QuantBody(rl));
              patSpec := QuantPatSpec(rl);
              univ := MkQuant(forallSym, rl.tail.head, patSpec, notBod);
          BEGIN
            RETURN NegIf(univ, neg)
          END (* BEGIN *)
        ELSIF FormIsLit(rl) THEN
          RETURN CanonicalizeLit(rl, neg)
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    <*ASSERT NOT ISTYPE(form, RefList.T) OR
             NARROW(form, RefList.T).tail # NIL *>
    IF neg THEN RETURN Not(form)
    ELSE RETURN form
    END (* IF *)
  END CNFWork;

PROCEDURE CanonicalizeLit(lit: T; neg := FALSE): T RAISES { Error } =
  BEGIN
    TYPECASE lit OF
    | RefList.T(rl) =>
        IF rl.head = notSym THEN
	  TYPECASE rl.tail.head OF
	  | RefList.T(rl2) =>
	      IF rl2.head = existsSym THEN
		VAR notBod := Not(QuantBody(rl2));
                    patSpec := QuantPatSpec(rl2);
                    univ := MkQuant(forallSym, rl2.tail.head, patSpec, notBod);
                BEGIN
                  RETURN NegIf(univ, NOT neg)
		END (* BEGIN *)
	      END (* IF *)
	  ELSE
	  END (* TYPECASE *);
          RETURN CanonicalizeLit(rl.tail.head, NOT neg)
	ELSIF rl.head = diffSym OR rl.head = eqSym THEN
	  IF RefList.Length(rl) # 3 THEN
	    RAISE Error("=, # require 2 arguments.")
	  END (* IF *);
	  VAR e := rl.tail.head; f := rl.tail.tail.head; s, t: T; BEGIN
	    IF Compare(f, e) = -1 THEN s := f; t := e
	    ELSE s := e; t := f
	    END (* IF *);
	    IF rl.head = diffSym AND neg OR rl.head = eqSym AND NOT neg THEN
	      RETURN RefList.List3(eqSym, s, t)
	    ELSE
	      RETURN RefList.List2(notSym, RefList.List3(eqSym, s, t))
	    END (* IF *)
	  END (* BEGIN *)
	ELSIF rl.head = distClassSym THEN
	  RETURN RefList.Cons(distClassSym, RefListSort.SortD(rl.tail, Compare))
	ELSIF ineqs.member(rl.head) THEN
	  IF RefList.Length(rl) # 3 THEN
	    RAISE Error("Inequalities require 2 arguments.")
	  END (* IF *);
	  VAR e := rl.tail.head; f := rl.tail.tail.head; s, t: T;
	      cmp := Compare(e, f); rel: Atom.T;
	  BEGIN
	    IF cmp = -1 OR cmp = 0 THEN
	      s := e; t := f;
	      IF rl.head = ltSym THEN rel := geSym; neg := NOT neg
	      ELSIF rl.head = leSym THEN rel := gtSym; neg := NOT neg
	      ELSE rel := rl.head
	      END (* IF *)
	    ELSE
	      s := f; t := e;
	      IF rl.head = gtSym THEN rel := geSym; neg := NOT neg
	      ELSIF rl.head = geSym THEN rel := gtSym; neg := NOT neg
	      ELSIF rl.head = ltSym THEN rel := gtSym
	      ELSIF rl.head = leSym THEN rel := geSym
	      ELSE <*ASSERT FALSE*>
	      END (* IF *)
	    END (* IF *);
            RETURN NegIf(RefList.List3(rel, s, t), NOT neg)
	  END (* BEGIN *)
	ELSE
          RETURN NegIf(lit, NOT neg)
        END (* IF *)
    ELSE
        RETURN lit
    END (* TYPECASE *)
  END CanonicalizeLit;

(* Returns "TRUE" iff "e > f" in the total order corresponding to a
   fairly arbitrary partial order on terms. *)
PROCEDURE Compare(e, f: T): [-1..1] =
  TYPE SxType = { Null, Int, Atom, RefList, Real, Other };
       RefInt = REF INTEGER; RefReal = REF LONGREAL;
  VAR et, ft: SxType; BEGIN
    TYPECASE e OF <*NOWARN*>
    | NULL => et := SxType.Null
    | RefInt => et := SxType.Int
    | RefReal => et := SxType.Real
    | Atom.T => et := SxType.Atom
    | RefList.T => et := SxType.RefList
    ELSE et := SxType.Other
    END (* TYPECASE *);
    TYPECASE f OF <*NOWARN*>
    | NULL => ft := SxType.Null
    | RefInt => ft := SxType.Int
    | RefReal => ft := SxType.Real
    | Atom.T => ft := SxType.Atom
    | RefList.T => ft := SxType.RefList
    ELSE ft := SxType.Other
    END (* TYPECASE *);
    IF et < ft THEN RETURN -1
    ELSIF ft < et THEN RETURN 1
    ELSE
      CASE et OF
      | SxType.Null, SxType.Other =>
          RETURN 0
      | SxType.Int =>
          RETURN Integer.Compare(NARROW(e, RefInt)^, NARROW(f, RefInt)^)
      | SxType.Real =>
          VAR er := NARROW(e, RefReal)^; fr := NARROW(f, RefReal)^; BEGIN
            IF er > fr THEN RETURN 1
            ELSIF fr > er THEN RETURN -1
            ELSE RETURN 0
            END (* IF *)
          END (* BEGIN *)
      | SxType.Atom =>
          RETURN Integer.Compare(Atom.Hash(e), Atom.Hash(f))
      | SxType.RefList =>
          VAR rle: RefList.T := e; rlf: RefList.T := f;
              headCmp := Compare(rle.head, rlf.tail);
          BEGIN
            IF headCmp # 0 THEN RETURN headCmp
            ELSE RETURN Compare(rle.tail, rlf.tail)
            END (* IF *)
          END (* BEGIN *)
      END (* CASE *)
    END (* IF *)
  END Compare;

PROCEDURE CNFSize(form: T;
                  VAR (*OUT*) conjs: CARDINAL;
                  VAR (*OUT*) maxWidth: CARDINAL) =
  BEGIN CNFSizeWork(form, TRUE, conjs, maxWidth)
  END CNFSize;


PROCEDURE CNFSizeWork(form: T; sense := TRUE;
                  VAR (*OUT*) conjs: CARDINAL;
                  VAR (*OUT*) maxWidth: CARDINAL) =
  BEGIN
    TYPECASE form OF
    | RefList.T(rl) =>
        IF rl.head = notSym THEN
          CNFSizeWork(rl.tail.head, NOT sense, conjs, maxWidth)
        ELSIF rl.head = iffSym THEN
          VAR lhs := rl.tail.head; rhs := rl.tail.tail.head; BEGIN
            CNFSizeWork(And(Or(Not(lhs), rhs), Or(Not(rhs), lhs)), sense,
                        conjs, maxWidth)
          END (* BEGIN *)
        ELSIF rl.head = impliesSym THEN
          VAR lhs := rl.tail.head; rhs := rl.tail.tail.head; BEGIN
            CNFSizeWork(Or(Not(lhs), rhs), sense, conjs, maxWidth)
          END (* BEGIN *)
        ELSIF rl.head = expliesSym THEN
          VAR lhs := rl.tail.head; rhs := rl.tail.tail.head; BEGIN
            CNFSizeWork(Or(Not(rhs), lhs), sense, conjs, maxWidth)
          END (* BEGIN *)
        ELSIF rl.head = andSym AND sense OR rl.head = orSym AND NOT sense THEN
          conjs := 0; maxWidth := 0;
          VAR tl := rl.tail; BEGIN
            WHILE tl # NIL DO
              VAR conjs1, maxWidth1: CARDINAL; BEGIN
                CNFSizeWork(tl.head, sense, conjs1, maxWidth1);
                conjs := conjs + conjs1;
                maxWidth := MAX(maxWidth, maxWidth1)
              END (* BEGIN *);
              tl := tl.tail
            END (* WHILE *)
          END (* BEGIN *)
        ELSIF rl.head = orSym AND sense OR rl.head = andSym AND NOT sense THEN
          conjs := 1; maxWidth := 0;
          VAR tl := rl.tail; BEGIN
            WHILE tl # NIL DO
              VAR conjs1, maxWidth1: CARDINAL; BEGIN
                CNFSizeWork(tl.head, sense, conjs1, maxWidth1);
                conjs := conjs * conjs1;
                maxWidth := maxWidth + maxWidth1
              END (* BEGIN *);
              tl := tl.tail
            END (* WHILE *)
          END (* BEGIN *)
        ELSE
          (* Literal *)
          conjs := 1; maxWidth := 1
        END (* IF *)
    ELSE
        (* Literal *)
        conjs := 1; maxWidth := 1
    END (* TYPECASE *)
  END CNFSizeWork;

PROCEDURE And(f1, f2: T): T =
  PROCEDURE AppendConjunct(conj: RefList.T; f: T): T =
    (* Requires that "conj" is a (possibly empty) conjunction, and
       that"f" is neither "trueSym" or "falseSym".
       Returns a formula equivalent to "(AND conj f)", possibly
       modifying "conj" destructively. *)
    BEGIN
      TYPECASE f OF
      | RefList.T(rl) =>
          IF rl.head = andSym THEN
            RETURN RefList.Append(conj, rl.tail)
          END (* IF *);
      ELSE
      END (* TYPECASE *);
      RETURN RefList.Append(conj, RefList.List1(f));
    END AppendConjunct;
  BEGIN
    IF f1 = falseSym OR f2 = falseSym THEN
      RETURN falseSym
    ELSIF f1 = trueSym THEN
      RETURN f2
    ELSIF f2 = trueSym THEN
      RETURN f1
    ELSE
      VAR res: T := RefList.List1(andSym); BEGIN
        res := AppendConjunct(res, f1);
        res := AppendConjunct(res, f2);
        RETURN res
      END (* BEGIN *)
    END (* IF *)
  END And;

PROCEDURE Or(f1, f2: T): T =
  PROCEDURE AppendDisjunct(disj: RefList.T; f: T): T =
    (* Requires that "disj" is a (possibly empty) disjunction, and
       that"f" is neither "trueSym" or "falseSym".
       Returns a formula equivalent to "(OR disj f)". *)
    BEGIN
      TYPECASE f OF
      | RefList.T(rl) =>
          IF rl.head = orSym THEN
            RETURN RefList.Append(disj, rl.tail)
          END (* IF *)
      ELSE
      END (* TYPECASE *);
      RETURN RefList.Append(disj, RefList.List1(f))
    END AppendDisjunct;
  BEGIN
    IF f1 = trueSym OR f2 = trueSym THEN
      RETURN trueSym
    ELSIF f1 = falseSym THEN
      RETURN f2
    ELSIF f2 = falseSym THEN
      RETURN f1
    ELSE
      VAR res: RefList.T := RefList.List1(orSym); BEGIN
        res := AppendDisjunct(res, f1);
        res := AppendDisjunct(res, f2);
        <*ASSERT res.tail # NIL*>
        RETURN res
      END (* BEGIN *)
    END (* IF *)
  END Or;

PROCEDURE Not(f: T): T =
  BEGIN
    TYPECASE f OF
    | Atom.T(at) =>
        IF at = trueSym THEN RETURN falseSym
        ELSIF at = falseSym THEN RETURN trueSym
        END (* IF *)
    | RefList.T(rl) =>
        IF rl.head = notSym THEN RETURN rl.tail.head END (* IF *)
    ELSE
    END (* TYPECASE *);
    RETURN RefList.List2(notSym, f)
  END Not;

PROCEDURE Equal(f1, f2: T): BOOLEAN =
  BEGIN
    IF TYPECODE(f1) = TYPECODE(f2) THEN
      TYPECASE f1 OF
      | NULL =>
          RETURN TRUE
      | RefList.T(f1rl) =>
          VAR f2rl: RefList.T := f2; BEGIN
            RETURN Equal(f1rl.head, f2rl.head) AND Equal(f1rl.tail, f2rl.tail)
          END (* BEGIN *)
      ELSE
          RETURN f1 = f2
      END (* TYPECASE *)
    ELSE
      RETURN FALSE
    END (* IF *)
  END Equal;

PROCEDURE Sub1(f: T; v: Atom.T; val: T): T =
  VAR sub := NEW(AtomRefTbl.Default).init(); BEGIN
    EVAL sub.put(v, val);
    RETURN Sub(f, sub)
  END Sub1;

PROCEDURE Sub(f: T; sub: AtomRefTbl.T): T =
  BEGIN RETURN SubWork(f, sub, NEW(AtomSetDef.T).init()) END Sub;

PROCEDURE SubWork(f: T; sub: AtomRefTbl.T; exc: AtomSet.T): T =
  BEGIN
    TYPECASE f OF
    | NULL => RETURN NIL
    | Atom.T(at) =>
        VAR ra: REFANY; BEGIN
          IF NOT exc.member(at) AND sub.get(at, ra) THEN RETURN ra
          ELSE RETURN at
          END (* IF *)
        END (* BEGIN *)
    | RefList.T(rl) =>
        IF rl.head = forallSym OR rl.head = existsSym THEN
	  VAR qvars: RefList.T := rl.tail.head;
	      toDel: RefList.T := NIL;
	  BEGIN
	    WHILE qvars # NIL DO
	      IF NOT exc.insert(qvars.head) THEN
		toDel := RefList.Cons(qvars.head, toDel)
	      END (* IF *);
	      qvars := qvars.tail
	    END (* WHILE *);
	    VAR patSpec := QuantPatSpec(rl);
		body := QuantBody(rl);
		newPatSpec := SubWork(patSpec, sub, exc);
		newBod := SubWork(body, sub, exc);
	    BEGIN
	      WHILE toDel # NIL DO
		EVAL exc.delete(toDel.head); toDel := toDel.tail
	      END (* WHILE *);
	      IF newBod = body AND newPatSpec = patSpec THEN RETURN rl
	      ELSE RETURN MkQuant(rl.head, rl.tail.head, newPatSpec, newBod)
	      END (* IF *)
	    END (* BEGIN *)
	  END (* BEGIN *);
        ELSE
	  (* Otherwise *)
	  VAR newHead := SubWork(rl.head, sub, exc);
	      newTail := SubWork(rl.tail, sub, exc);
	  BEGIN
	    IF newHead = rl.head AND newTail = rl.tail THEN RETURN rl
	    ELSE RETURN RefList.Cons(newHead, newTail)
	    END (* IF *)
	  END (* BEGIN *)
        END (* IF *)
    ELSE
        RETURN f
    END (* TYPECASE *)
  END SubWork;

PROCEDURE ComposeSub(sub0, sub1: AtomRefTbl.T): AtomRefTbl.T =
  VAR res := NEW(AtomRefTbl.Default).init(); BEGIN
    VAR iter0 := sub0.iterate();
        v0: Atom.T; val0: T;
    BEGIN
      WHILE iter0.next(v0, val0) DO
        EVAL res.put(v0, Sub(val0, sub1))
      END (* WHILE *)
    END (* BEGIN *);
    VAR iter1 := sub1.iterate();
        v1: Atom.T; val1, val0: T; 
    BEGIN
      WHILE iter1.next(v1, val1) DO
        IF NOT sub0.get(v1, val0) THEN
          EVAL res.put(v1, val1)
        END (* IF *)
      END (* WHILE *)
    END (* BEGIN *);
    RETURN res
  END ComposeSub;
    
PROCEDURE ComposeSubD(sub0, sub1: AtomRefTbl.T) =
  BEGIN
    VAR iter0 := sub0.iterate();
        v0: Atom.T; val0: T;
    BEGIN
      WHILE iter0.next(v0, val0) DO
        (* This isn't allowed by the table spec, but is by the
           implementation. *)
        EVAL sub0.put(v0, Sub(val0, sub1))
      END (* WHILE *)
    END (* BEGIN *);
    VAR iter1 := sub1.iterate();
        v1: Atom.T; val1, val0: T; 
    BEGIN
      WHILE iter1.next(v1, val1) DO
        IF NOT sub0.get(v1, val0) THEN
          EVAL sub0.put(v1, val1)
        END (* IF *)
      END (* WHILE *)
    END (* BEGIN *)
  END ComposeSubD;

PROCEDURE ComposeSub1(sub0: AtomRefTbl.T; v: Atom.T; val: T): AtomRefTbl.T =
  VAR res := NEW(AtomRefTbl.Default).init(); BEGIN
    VAR iter0 := sub0.iterate();
        v0: Atom.T; val0: T;
    BEGIN
      WHILE iter0.next(v0, val0) DO
        EVAL res.put(v0, Sub1(val0, v, val))
      END (* WHILE *)
    END (* BEGIN *);
    VAR val0: T; BEGIN
      IF NOT sub0.get(v, val0) THEN
        EVAL res.put(v, val)
      END (* IF *)
    END (* BEGIN *);
    RETURN res
  END ComposeSub1;

PROCEDURE ComposeSub1D(sub0: AtomRefTbl.T; v: Atom.T; val: T) =
  BEGIN
    VAR iter0 := sub0.iterate();
        v0: Atom.T; val0: T;
    BEGIN
      WHILE iter0.next(v0, val0) DO
        (* This isn't allowed by the table spec, but is by the
           implementation. *)
        EVAL sub0.put(v0, Sub1(val0, v, val))
      END (* WHILE *)
    END (* BEGIN *);
    VAR val0: T; BEGIN
      IF NOT sub0.get(v, val0) THEN
        EVAL sub0.put(v, val)
      END (* IF *)
    END (* BEGIN *)
  END ComposeSub1D;


PROCEDURE VarsOccurFree(f: T; vs: REFANY): BOOLEAN =
  BEGIN
    TYPECASE f OF
    | NULL => RETURN FALSE
    | Atom.T(at) =>
        TYPECASE vs OF <*NOWARN*>
        | Atom.T => RETURN at = vs
        | RefList.T(rl) => RETURN RefList.Member(rl, at)
        END (* TYPECASE *);
    | RefList.T(rl) =>
        TYPECASE rl.head OF
        | Atom.T(at) =>
            IF at = forallSym OR at = existsSym THEN
              VAR qvars: RefList.T := rl.tail.head; BEGIN
                TYPECASE vs OF <*NOWARN*>
                | Atom.T =>
                    IF RefList.Member(qvars, vs) THEN
                      RETURN FALSE
                    ELSE
                      RETURN VarsOccurFree(RefList.Nth(rl, 2), vs)
                    END (* IF *)
                | RefList.T(vsRl) =>
                    VAR vs2 := RefList.Append(vsRl, NIL); (* "Copy" *)
                        qvar := qvars;
                    BEGIN
                      WHILE qvar # NIL DO
                        IF RefList.Member(vsRl, qvar.head) THEN
                          vs2 := RefListDelete(vs2, qvar.head)
                        END (* IF *);
                        qvar := qvar.tail
                      END (* WHILE *);
                      IF vs2 = NIL THEN RETURN FALSE
                      ELSE RETURN VarsOccurFree(QuantBody(rl), vs2)
                      END (* IF *)
                    END (* BEGIN *);
                END (* TYPECASE *)
              END (* BEGIN *)
            END (* IF *)
        ELSE
        END (* TYPECASE *);
        RETURN VarsOccurFree(rl.head, vs) OR VarsOccurFree(rl.tail, vs)
    ELSE
        RETURN FALSE
    END (* TYPECASE *)
  END VarsOccurFree;

PROCEDURE QuantifiedSubformula(f: T): BOOLEAN =
  BEGIN
    TYPECASE f OF
    | NULL => RETURN FALSE
    | RefList.T(rl) =>
        TYPECASE rl.head OF
        | Atom.T(at) =>
            IF at = forallSym OR at = existsSym THEN RETURN TRUE END (* IF *)
        ELSE
        END (* TYPECASE *);
        VAR subs := rl.tail; res := FALSE; BEGIN
          WHILE subs # NIL AND NOT res DO
            res := res OR QuantifiedSubformula(subs.head);
            subs := subs.tail
          END (* WHILE *);
          RETURN res
        END (* BEGIN *)
    ELSE
        RETURN FALSE
    END (* TYPECASE *)
  END QuantifiedSubformula;

PROCEDURE OnlyUniversalPos(f: T): BOOLEAN =
  BEGIN RETURN OnlyUniversalPosWork(f, neg := FALSE) END OnlyUniversalPos;

PROCEDURE OnlyUniversalPosWork(f: T; neg: BOOLEAN): BOOLEAN =
  BEGIN
    TYPECASE f OF
    | NULL => RETURN TRUE
    | RefList.T(rl) =>
        TYPECASE rl.head OF <*NOWARN*>
        | Atom.T(at) =>
            IF at = forallSym AND neg OR at = existsSym AND NOT neg THEN
              RETURN FALSE
            ELSIF at = forallSym OR at = existsSym THEN
              RETURN OnlyUniversalPosWork(QuantBody(rl), neg)
            ELSIF at = notSym THEN
              RETURN OnlyUniversalPosWork(rl.tail.head, NOT neg)
            ELSIF at = impliesSym THEN
              RETURN OnlyUniversalPosWork(rl.tail.head, NOT neg) AND
                     OnlyUniversalPosWork(rl.tail.tail.head, neg)
            ELSIF at = expliesSym THEN
              RETURN OnlyUniversalPosWork(rl.tail.tail.head, NOT neg) AND
                     OnlyUniversalPosWork(rl.tail.head, neg)
            ELSIF at = iffSym THEN
              RETURN NOT QuantifiedSubformula(rl.tail.head) AND
                     NOT QuantifiedSubformula(rl.tail.tail.head)
            ELSE
              VAR subs := rl.tail; res := TRUE; BEGIN
                WHILE subs # NIL AND res DO
                  res := res AND OnlyUniversalPosWork(subs.head, neg);
                  subs := subs.tail
                END (* WHILE *);
                RETURN res
              END (* BEGIN *)
            END (* IF *)
        END (* TYPECASE *);
    ELSE
        RETURN TRUE
    END (* TYPECASE *)
  END OnlyUniversalPosWork;

VAR simpQuantDebug := FALSE;

PROCEDURE SimplifyOneQuant(q: T; onePoint := TRUE): T RAISES { Error } =
  BEGIN
    TYPECASE q OF
    | RefList.T(rl) =>
        IF rl.head = forallSym THEN
          q := MkQuant(rl.head, rl.tail.head,
                       QuantPatSpec(rl),
                       CNF(QuantBody(rl)))
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    VAR mod := TRUE; anyMod := FALSE; BEGIN
      WHILE mod DO
        VAR res: T; BEGIN
          mod := SimplifyQuantWork(q, res, onePoint);
          IF mod THEN q := res; anyMod := TRUE; END (* IF *)
        END (* BEGIN *)
      END (* WHILE *);
      IF anyMod THEN q := CNF(q) END (* IF *)
    END (* BEGIN *);
    RETURN q
  END SimplifyOneQuant;

PROCEDURE SimplifyQuants(q: T; onePoint := TRUE): T RAISES { Error } =
  BEGIN
    TYPECASE q OF
    | NULL => RETURN q
    | RefList.T(qrl) =>
        VAR head := qrl.head; BEGIN
          IF boolOps.member(qrl.head) THEN
            VAR args := qrl; simp: T; res: RefList.T := NIL; BEGIN
              REPEAT
                args := args.tail;
                simp := SimplifyQuants(args.head, onePoint)
              UNTIL simp # args.head OR args.tail = NIL;
              IF simp = args.head THEN
                RETURN q
              ELSE
                VAR args2 := qrl.tail; BEGIN
                  WHILE args2 # args DO
                   res := RefList.Cons(args2.head, res); args2 := args2.tail
                  END (* WHILE *)
                END (* BEGIN *);
                res := RefList.Cons(simp, res);
                args := args.tail;
                WHILE args # NIL DO
                  res := RefList.Cons(SimplifyQuants(args.head, onePoint), res);
                  args := args.tail
                END (* WHILE *);
                RETURN RefList.Cons(qrl.head, RefList.ReverseD(res))
              END (* IF *)
            END (* BEGIN *)
          ELSIF NOT (qrl.head = forallSym OR qrl.head = existsSym) THEN
            RETURN q
          ELSE
            (* FORALL or EXISTS *)
	    VAR qvars := RefList.Nth(q, 1);
                patSpec := QuantPatSpec(q);
                body := QuantBody(q);
            BEGIN
	      IF head = existsSym THEN
                body := CNF(body, neg := TRUE);
	      ELSE
		<*ASSERT head = forallSym *>
                body := CNF(body)
              END (* IF *);
              (* Give the one-point rule one chance before simplifying
                 the body. *)
              q := MkQuant(forallSym, qvars, patSpec, body);
              VAR f: T; BEGIN
                IF onePoint AND SimplifyQuantWork(q, f, onePoint) THEN
                  TYPECASE f OF
                  | RefList.T(rl) =>
                      IF rl.head = forallSym THEN
                        qvars := rl.tail.head;
                        patSpec := QuantPatSpec(rl);
                        body := QuantBody(rl)
                      ELSE
                        IF head = existsSym THEN RETURN Not(SimplifyQuants(rl))
                        ELSE RETURN SimplifyQuants(rl)
                        END (* IF *)
                      END (* IF *)
                  ELSE
                      IF head = existsSym THEN RETURN Not(f)
                      ELSE RETURN f
                      END (* IF *)
                  END (* TYPECASE *)
                END (* IF *)
              END (* BEGIN *);
              (* Now simplify the body and then simplify this formula. *)
              VAR simpBody := SimplifyQuants(body); BEGIN
                IF simpBody # body THEN simpBody := CNF(simpBody) END (* IF *);
                q := MkQuant(forallSym, qvars, patSpec, simpBody)
              END (* BEGIN *)
	    END (* BEGIN *);
            IF simpQuantDebug THEN
	      Wr.PutText(Stdio.stdout,
                         "Attempting to simplify formula:\n    ");
	      Sx.Print(Stdio.stdout, q);
	      Wr.PutText(Stdio.stdout, "\n\n");
	      Wr.Flush(Stdio.stdout)
            END (* IF *);
	    VAR mod := TRUE; BEGIN
	      WHILE mod AND ISTYPE(q, RefList.T) AND
                NARROW(q, RefList.T).head = forallSym DO
		mod := FALSE;
		VAR f: T; BEGIN
		  IF SimplifyQuantWork(q, f, onePoint) THEN
                    IF simpQuantDebug THEN
		      Wr.PutText(Stdio.stdout, "...simplified to:\n    ");
		      Sx.Print(Stdio.stdout, f);
		      Wr.PutText(Stdio.stdout, "\n\n");
		      Wr.Flush(Stdio.stdout)
                    END (* IF *);
		    mod := TRUE; q := f
		  END (* IF *)
		END (* BEGIN *)
	      END (* WHILE *);
	      IF mod AND ISTYPE(q, RefList.T) AND
                NARROW(q, RefList.T).head # forallSym THEN
                q := SimplifyQuants(q)
              END (* IF *)
	    END (* BEGIN *);
	    IF head = existsSym THEN q := Not(q) END (* IF *);
	    RETURN q
          END (* IF *)
        END (* BEGIN *)
    ELSE
        RETURN q
    END (* TYPECASE *)
  END SimplifyQuants;


PROCEDURE SimplifyQuantWork(q: T; VAR f: T; onePoint := TRUE): BOOLEAN
     RAISES { Error } =
  (* Requires "q" to be a universally quantified formula "q" whose body is
     in CNF.  If "q" can be simplified, returns "TRUE" and sets "f" to
     the simplified form.  Otherwise, returns "FALSE". *)
  VAR qrl: RefList.T; BEGIN
    IF ISTYPE(q, RefList.T) THEN qrl := q
    ELSE RETURN FALSE
    END (* IF *);
    IF qrl.head # forallSym THEN
      RETURN FALSE
    END (* IF *);
    IF RefList.Length(qrl) # 3 THEN
      RAISE Error("Quantifier requires 2 arguments.")
    END (* IF *);
    VAR qvars := RefList.Nth(qrl, 1);
        patSpec := QuantPatSpec(qrl);
        form := QuantBody(qrl);
    BEGIN
      (* Eliminate unused quantified variables. *)
      VAR qvar: RefList.T := qvars; mod := FALSE; BEGIN
        WHILE qvar # NIL DO
	  IF NOT VarsOccurFree(form, qvar.head) THEN
	    qvars := RefListDelete(qvars, qvar.head); mod := TRUE
	  END (* IF *);
	  qvar := qvar.tail
	END (* WHILE *);
	IF mod THEN
	  IF qvars = NIL THEN f := form
	  ELSE f := RefList.List3(qrl.head, qvars, form)
	  END (* IF *);
	  RETURN TRUE
	END (* IF *)
      END (* BEGIN *);
      IF onePoint THEN
	TYPECASE form OF
	| RefList.T(rl) =>
	    VAR mod := FALSE; BEGIN
              IF rl.head = andSym THEN
		VAR conjs := rl.tail; res: T := trueSym; BEGIN
		  WHILE conjs # NIL DO
		    VAR simpConj: T; BEGIN
		      IF SimplifyDisjunction(qvars, conjs.head,
					     simpConj) THEN
			res := And(simpConj, res);
			mod := TRUE
		      ELSE
			res := And(conjs.head, res)
		      END (* IF *)
		    END (* BEGIN *);
		    conjs := conjs.tail
		  END (* WHILE *);
		  IF mod THEN f := res END (* IF *)
		END (* BEGIN *)
	      ELSIF rl.head = orSym THEN
		mod := SimplifyDisjunction(qvars, rl, f)
	      ELSE
		VAR v, i, val, const: T; BEGIN
		  IF SimplifyLit(qvars, rl, v, i, val, const) THEN
                    f := falseSym; RETURN TRUE
		  END (* IF *)
		END (* BEGIN *);
	      END (* IF *);
	      IF mod THEN
		f := MkQuant(forallSym, qvars, patSpec, f);
		RETURN TRUE
	      END (* IF *)
	    END (* BEGIN *)
	ELSE
	END (* TYPECASE *)
      END (* IF *);
      (* Otherwise, distribute FORALL over AND. *)
      TYPECASE form OF
      | RefList.T(rl) =>
	  IF rl.head = andSym THEN
	    VAR oldConjs := rl.tail; newConjs : RefList.T := NIL; BEGIN
	      WHILE oldConjs # NIL DO
		newConjs := RefList.Cons(
				MkQuant(forallSym, qvars, patSpec, 
                                        oldConjs.head),
				newConjs);
		oldConjs := oldConjs.tail
	      END (* WHILE *);
	      f := RefList.Cons(andSym, RefList.ReverseD(newConjs));
	      RETURN TRUE
	    END (* BEGIN *)
	  END (* IF *)
      ELSE
      END (* TYPECASE *);
      (* Now we can check for literals that contain no quantified
         variables, or for disjunctions consisting of a single universal
         quantification. *)
      TYPECASE form OF
      | RefList.T(rl) =>
	  VAR res: T; BEGIN
	    IF rl.head = andSym THEN
	      VAR conjs := rl.tail; BEGIN
		WHILE conjs # NIL DO
		  IF QuantCheckDisjunction(qvars, conjs.head, res) THEN
		    f := RefList.List3(
			     andSym, res,
			     MkQuant(
				 forallSym, qvars, patSpec,
				 RefList.Cons(
				     andSym,
				     RefListDelete(rl.tail,
						   conjs.head))));
		    RETURN TRUE
		  END (* IF *);
		  conjs := conjs.tail;
		END (* WHILE *)
	      END (* BEGIN *)
	    ELSIF rl.head = orSym THEN
	      IF QuantCheckDisjunction(qvars, rl, res) THEN
		f := res; RETURN TRUE
	      END (* IF *)
	    ELSIF QuantCheckLit(qvars, rl, res, collapse := TRUE) THEN
	      f := res; RETURN TRUE
	    END (* IF *)
	  END (* BEGIN *)
      ELSE
      END (* TYPECASE *);
    END (* BEGIN *);
    RETURN FALSE
  END SimplifyQuantWork;

PROCEDURE SimplifyDisjunction(qvars: RefList.T;
                              disj: T;
                              VAR f: T): BOOLEAN RAISES { Error } =
  (* Assumes that "disj" is a conjunct of the conjunctive normal form
     of a formula quantified over the variables in the list of
     Atom.T's "qvars".  If one or more of the literals in "disj" imply
     a point value for a quantified variable, substitute that (those) value(s)
     for that (those) variable(s) in the rest of the disjunction, set
     "f" to the formula after substitution, and return "TRUE".
     Otherwise, return "FALSE". *)
  VAR v, i, val, const: T; BEGIN
    TYPECASE disj OF
    | RefList.T(rl) =>
        IF rl.head = orSym THEN
          IF RefList.Length(rl.tail) > 1 THEN
	    VAR allLits := rl.tail; lits: RefList.T;
		mod := TRUE; anyMod := FALSE;
	    BEGIN
	      WHILE mod DO
		mod := FALSE; lits := allLits;
		WHILE lits # NIL AND NOT mod DO
		  IF SimplifyLit(qvars, lits.head, v, i, val, const) THEN
                    IF val # NIL THEN
                      allLits := RefListDelete(allLits, lits.head);
                      allLits := QSub(allLits, v, i, val, const);
                      mod := TRUE
                    ELSE
                      VAR occurs := FALSE; lits2 := rl.tail; BEGIN
                        WHILE lits2 # NIL AND NOT occurs DO
                          IF lits2 # lits THEN
                            occurs := occurs OR VarsOccurFree(lits2.head, v)
                          END (* IF *);
                          lits2 := lits2.tail
                        END (* WHILE *);
                        IF NOT occurs THEN
                          allLits := RefListDelete(allLits, lits.head);
                          mod := TRUE
                        END (* IF *)
                      END (* BEGIN *)
                    END (* IF *)
		  END (* IF *);
		  lits := lits.tail
		END (* BEGIN *);
		anyMod := anyMod OR mod
	      END (* WHILE *);
	      IF anyMod THEN
		IF allLits = NIL THEN f := falseSym
		ELSIF allLits.tail = NIL THEN f := allLits.head
		ELSE f := RefList.Cons(orSym, allLits)
		END (* IF *)
	      END (* IF *);
	      RETURN anyMod
            END (* BEGIN *)
          ELSIF SimplifyLit(qvars, rl.tail.head, v, i, val, const) THEN
            f := falseSym; RETURN TRUE
          END (* IF *)
        ELSIF SimplifyLit(qvars, rl, v, i, val, const) THEN
          f := falseSym; RETURN TRUE
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    RETURN FALSE
  END SimplifyDisjunction;


PROCEDURE SimplifyLit(qvars: RefList.T; lit: T;
                      VAR (*OUT*) v, index, val, const: T): BOOLEAN
    RAISES { Error } =
  (* If "lit" is of the form "(EQ x E)", where "x" is a member of
     "qvars" and "x" does not occur in "E", returns "TRUE" and sets
     "v" to "x" and "val", "index", and "const" to "NIL".

     If "lit" is of the form "(NOT (EQ x E))", where "x" is a member of
     "qvars" and "x" does not occur in "E", returns "TRUE" and sets
     "v" to "x", "val" to "E", and "index" and "const" to "NIL".

     If "lit" is equivalent to "(NOT (FORALL (t) x[t] = E))", where
     "x" is a member of "qvars" and does not occur free in "E",
     returns "TRUE" and sets "v" to "x", "index" to "t", "val" to "E",
     and "const" to "NIL".

     If "lit" is equivalent to "(NOT (FORALL (t) (IFF x[t] = C) P)",
     where "x" is a member of "qvars" and does not occur free in "P",
     and "C" contains neither "x" nor any variable in "qvars" (it is a
     ground term), returns "TRUE", and sets "v" to "x", "index" to
     "t", "val" to "P", and "const" to "C". 

     If none of these situations obtains, returns "FALSE"; the values
     of the "OUT" parameters is undefined.
  *)
  BEGIN
    index := NIL; const := NIL;
    TYPECASE lit OF
    | RefList.T(rl) =>
        IF rl.head = eqSym THEN
          VAR lhsRA := RefList.Nth(rl, 1);
              rhsRA := RefList.Nth(rl, 2);
          BEGIN
            IF ISTYPE(lhsRA, Atom.T) AND
              RefList.Member(qvars, lhsRA) AND
              NOT VarsOccurFree(rhsRA, lhsRA) THEN
              v := lhsRA; val := NIL; RETURN TRUE
            ELSIF ISTYPE(rhsRA, Atom.T) AND
              RefList.Member(qvars, rhsRA) AND
              NOT VarsOccurFree(lhsRA, rhsRA) THEN
              v := rhsRA; val := NIL; RETURN TRUE
            ELSE
              RETURN FALSE
            END (* IF *)
          END (* BEGIN *)
        ELSIF rl.head = notSym THEN
          TYPECASE rl.tail.head OF
          | RefList.T(rl2) =>
              rl := rl2
          ELSE
              RETURN FALSE
          END (* TYPECASE *)
        ELSE
          RETURN FALSE
        END (* IF *);
        IF rl.head = eqSym THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error("EQ requires 2 arguments.")
          END (* IF *);
          VAR lhs := RefList.Nth(rl, 1);
              lhsQuant := RefList.Member(qvars, lhs);
              rhs := RefList.Nth(rl, 2);
              rhsQuant := RefList.Member(qvars, rhs);
          BEGIN
            IF lhsQuant OR rhsQuant THEN
              IF lhsQuant THEN v := lhs; val := rhs
              ELSE v := rhs; val := lhs
              END (* IF *);
              RETURN NOT VarsOccurFree(val, v)
            END (* IF *)
          END (* BEGIN *)
        ELSIF rl.head = forallSym THEN
          VAR q2vars: RefList.T := RefList.Nth(rl, 1);
              body := QuantBody(rl);
          BEGIN
            IF RefList.Length(q2vars) = 1 THEN
              VAR q2var: Atom.T := q2vars.head; BEGIN
                TYPECASE body OF
                | RefList.T(rl2) =>
                    IF rl2.head = eqSym THEN
                      VAR lhsRA := RefList.Nth(rl2, 1);
                          lhsMatch := MatchSel(lhsRA, qvars, q2var);
                          rhsRA := RefList.Nth(rl2, 2);
                          rhsMatch := MatchSel(rhsRA, qvars, q2var);
                      BEGIN
                        IF lhsMatch OR rhsMatch THEN
			  IF lhsMatch THEN
			    VAR lhs: RefList.T := lhsRA; BEGIN
			      v := lhs.tail.head;
			      index := lhs.tail.tail.head;
			      val := rhsRA
			    END (* BEGIN *)
			  ELSE
			    VAR rhs: RefList.T := rhsRA; BEGIN
			      v := rhs.tail.head;
			      index := rhs.tail.tail.head;
			      val := lhsRA
			    END (* BEGIN *)
			  END (* IF *);
                          RETURN NOT VarsOccurFree(val, v)
                        END (* IF *)
                      END (* BEGIN *)
                    END (* IF *)
                ELSE
                END (* TYPECASE *);
                (* The IFF one-point rule, scalar and map versions. *)
                VAR absVarEqs := FindAbsVarEqConsts(body, qvars); BEGIN
                  WHILE absVarEqs # NIL DO
		    VAR absVarEq: RefList.T := absVarEqs.head; BEGIN
                      const := absVarEq.tail.tail.head;
                      VAR p := CNF(RefList.List3(iffSym, body, absVarEq)); BEGIN
                        IF NOT VarsOccurFree(p, qvars) THEN
                          val := p;
                          RETURN TRUE
                        END (* IF *)
                      END (* BEGIN *)
                    END (* BEGIN *);
		    absVarEqs := absVarEqs.tail
                  END (* WHILE *)
                END (* BEGIN *);
                VAR sels := FindSelectEqConsts(body, qvars, q2var); BEGIN
                  WHILE sels # NIL DO
		    VAR selEq: RefList.T := sels.head;
                        sel: RefList.T := selEq.tail.head;
		    BEGIN
                      v := sel.tail.head;
                      index := sel.tail.tail.head;
                      const := selEq.tail.tail.head;
                      VAR p := CNF(RefList.List3(iffSym, body, selEq)); BEGIN
                        IF NOT VarsOccurFree(p, qvars) THEN
                          val := p; RETURN TRUE
                        END (* IF *)
                      END (* BEGIN *)
                    END (* BEGIN *);
		    sels := sels.tail
                  END (* WHILE *)
                END (* BEGIN *)
              END (* BEGIN *)
            END (* IF *)
          END (* BEGIN *)
        END (* IF *)
    ELSE
    END (* TYPECASE *);
    RETURN FALSE
  END SimplifyLit;

PROCEDURE FindSelectEqConsts(body: T; qvars: RefList.T;
                             index: Atom.T): RefList.T =
  VAR res: RefList.T := NIL; BEGIN
    FindSelectEqConstsWork(body, qvars, index, res);
    RETURN res
  END FindSelectEqConsts;

PROCEDURE FindSelectEqConstsWork(body: T; qvars: RefList.T; index: Atom.T;
                                 VAR res: RefList.T) =
  BEGIN
    TYPECASE body OF
    | NULL =>
    | RefList.T(rl) =>
        IF RefList.Length(rl) = 3 AND rl.head = eqSym THEN
          IF MatchSel(rl.tail.head, qvars, index) THEN
            TYPECASE rl.tail.tail.head OF
            | Atom.T(at) =>
                IF NOT VarsOccurFree(qvars, at) AND at # index THEN
                  res := RefList.Cons(rl, res)
                END (* IF *)
            ELSE
            END (* TYPECASE *)
          ELSIF MatchSel(rl.tail.tail.head, qvars, index) THEN
            TYPECASE rl.tail.head OF
            | Atom.T(at) =>
                IF NOT VarsOccurFree(qvars, at) AND at # index THEN
                  res := RefList.Cons(
                             RefList.List3(eqSym, rl.tail.tail.head,
                                           rl.tail.head),
                             res)
                END (* IF *)
            ELSE
            END (* TYPECASE *)
          ELSE
            FindSelectEqConstsWork(rl.head, qvars, index, res);
            FindSelectEqConstsWork(rl.tail, qvars, index, res)
          END (* IF *)
        ELSE
          FindSelectEqConstsWork(rl.head, qvars, index, res);
          FindSelectEqConstsWork(rl.tail, qvars, index, res)
        END (* IF *)
    ELSE
    END (* TYPECASE *)
  END FindSelectEqConstsWork;

PROCEDURE FindAbsVarEqConsts(body: T; qvars: RefList.T): RefList.T =
  VAR res: RefList.T := NIL; BEGIN
    FindAbsVarEqConstsWork(body, qvars, res);
    RETURN res
  END FindAbsVarEqConsts;

PROCEDURE FindAbsVarEqConstsWork(body: T; qvars: RefList.T;
                                 VAR res: RefList.T) =
  BEGIN
    TYPECASE body OF
    | NULL =>
    | RefList.T(rl) =>
        IF RefList.Length(rl) = 3 AND rl.head = eqSym THEN
          IF RefList.Member(qvars, rl.tail.head) THEN
            TYPECASE rl.tail.tail.head OF
            | Atom.T(at) =>
                IF NOT VarsOccurFree(qvars, at) THEN
                  res := RefList.Cons(rl, res)
                END (* IF *)
            ELSE
            END (* TYPECASE *)
          ELSIF RefList.Member(qvars, rl.tail.tail.head) THEN
            TYPECASE rl.tail.head OF
            | Atom.T(at) =>
                IF NOT VarsOccurFree(qvars, at) THEN
                  res := RefList.Cons(
                             RefList.List3(eqSym, rl.tail.tail.head,
                                           rl.tail.head),
                             res)
                END (* IF *)
            ELSE
            END (* TYPECASE *)
          ELSE
            FindAbsVarEqConstsWork(rl.head, qvars, res);
            FindAbsVarEqConstsWork(rl.tail, qvars, res)
          END (* IF *)
        ELSE
          FindAbsVarEqConstsWork(rl.head, qvars, res);
          FindAbsVarEqConstsWork(rl.tail, qvars, res)
        END (* IF *)
    ELSE
    END (* TYPECASE *)
  END FindAbsVarEqConstsWork;

PROCEDURE MatchSel(r: REFANY; qvars: RefList.T; q2var: Atom.T): BOOLEAN =
  BEGIN
    TYPECASE r OF
    | RefList.T(rl) =>
        RETURN
          rl.head = selectSym
            AND RefList.Member(qvars, rl.tail.head)
            AND q2var = rl.tail.tail.head;
    ELSE
        RETURN FALSE
    END (* TYPECASE *)
  END MatchSel;

PROCEDURE QSub(f: REFANY; v, i: Atom.T; val, const: T): T =
  (* "f" is either a "T" or a list of "T"'s.  If "i" is "NIL",
     substitutes "val" for "v" in "f" and returns the result.  If "i"
     is non-"NIL" and "const" is "NIL", then: for all subterms of "f"
     of the form "select(v, j)", let "valSub" be the result of
     substituting "j" for "i" in "val", and substitute "valSub" for
     "select(v, j)" in "f".  If both "i" and "const" are non-NIL, then
     for all subterms of "f" of the form "(EQ select(v, j) const)",
     let "valSub" be the result of substituting "j" for "i" in "val",
     and substitute "valSub" for "(EQ select(v, j) const)" in "f".

     Return the final value of "p". 
  *)
  BEGIN
    IF i = NIL THEN RETURN Sub1(f, v, val)
    ELSE RETURN QSubWork(f, v, i, val, const)
    END (* IF *)
  END QSub;

PROCEDURE QSubWork(f: REFANY; v, i: Atom.T; val, const: T): T =
  BEGIN
    TYPECASE f OF
    | NULL => RETURN NIL
    | RefList.T(rl) =>
        TYPECASE rl.head OF
        | Atom.T(at) =>
            IF at = selectSym AND const = NIL THEN
              VAR map := RefList.Nth(rl, 1);
                  ind := RefList.Nth(rl, 2);
              BEGIN
                IF map = v THEN RETURN Sub1(val, i, ind) END (* IF *)
              END (* BEGIN *)
            ELSIF at = eqSym AND const # NIL THEN
              VAR sel: RefList.T; ok := FALSE;
              PROCEDURE IsSel(ra: REFANY): BOOLEAN =
                BEGIN
                  TYPECASE ra OF
                  | RefList.T(rl) => RETURN rl.head = selectSym
                  ELSE
                  END (* TYPECASE *);
                  RETURN FALSE
                END IsSel;
              BEGIN
                IF IsSel(rl.tail.head) AND Equal(rl.tail.tail.head, const) THEN
                  sel := rl.tail.head; ok := TRUE
                ELSIF IsSel(rl.tail.tail.head) AND
                  Equal(rl.tail.head, const) THEN
                  sel := rl.tail.tail.head; ok := TRUE
                END (* IF *);
                IF ok THEN
                  VAR map := RefList.Nth(sel, 1);
                      ind := RefList.Nth(sel, 2);
                  BEGIN
                    IF map = v THEN RETURN Sub1(val, i, ind) END (* IF *)
                  END (* BEGIN *)
                END (* IF *)
              END (* BEGIN *)
            ELSIF at = forallSym OR at = existsSym THEN
              VAR qvars: RefList.T := rl.tail.head;
                  patSpec := QuantPatSpec(rl);
                  body := QuantBody(rl);
              BEGIN
                IF RefList.Member(qvars, v) OR
                  (i # NIL AND RefList.Member(qvars, i)) THEN
                  RETURN f
                ELSE
                  RETURN MkQuant(at, qvars, patSpec,
                                 QSub(body, v, i, val, const))
                END (* IF *)
              END (* BEGIN *)
            END (* IF *)
        ELSE
        END (* TYPECASE *);
        RETURN RefList.Cons(QSub(rl.head, v, i, val, const),
                            QSub(rl.tail, v, i, val, const))
    ELSE
        RETURN f
    END (* TYPECASE *)
  END QSubWork;

PROCEDURE QuantCheckDisjunction(qvars: RefList.T; disj: T;
                                VAR (*OUT*) res: T): BOOLEAN =
  VAR resLit: T; BEGIN
    TYPECASE disj OF
    | RefList.T(rl) =>
        TYPECASE rl.head OF
        | Atom.T(at) =>
            IF at = orSym THEN
              VAR disjs := rl.tail; BEGIN
                WHILE disjs # NIL DO
                  IF QuantCheckLit(qvars, disjs.head, resLit) THEN
                    res := RefList.List3(
                               orSym, resLit,
                               MkQuant(forallSym, qvars, NIL,
                                       RefList.Cons(orSym,
                                                    RefListDelete(
                                                        rl.tail,
                                                        disjs.head))));
                    RETURN TRUE
                  END (* IF *);
                  disjs := disjs.tail
                END (* WHILE *);
                RETURN FALSE
              END (* BEGIN *)
            END (* IF *)
        ELSE
        END (* TYPECASE *)
    ELSE
    END (* TYPECASE *);
    (* Try as a literal. *)
    IF QuantCheckLit(qvars, disj, resLit, collapse := TRUE) THEN
      res := resLit; RETURN TRUE
    ELSE
      RETURN FALSE
    END (* IF *)
  END QuantCheckDisjunction;

PROCEDURE QuantCheckLit(qvars: RefList.T; lit: T; VAR (*OUT*) res: T;
                        collapse: BOOLEAN := FALSE): BOOLEAN =
  BEGIN
    IF NOT VarsOccurFree(lit, qvars) THEN
      res := lit; RETURN TRUE
    END (* IF *);
    IF collapse THEN
      TYPECASE lit OF
      | RefList.T(rl) =>
          TYPECASE rl.head OF
          | Atom.T(at) =>
              IF at = forallSym THEN
                res := MkQuant(forallSym,
                               RefListUnion(qvars, RefList.Nth(rl, 1)),
                               NIL, RefList.Nth(rl, 2));
                RETURN TRUE
              ELSIF at = notSym THEN
                TYPECASE rl.tail.head OF
                | RefList.T(rl2) =>
                    TYPECASE rl2.head OF
                    | Atom.T(at2) =>
                        IF at2 = existsSym THEN
                          res := MkQuant(
                                     forallSym,
                                     RefListUnion(qvars, RefList.Nth(rl2, 1)),
                                     QuantPatSpec(rl2),
                                     Not(QuantBody(rl2)));
                          RETURN TRUE
                        END (* IF *)
                    ELSE
                    END (* TYPECASE *)
                ELSE
                END (* TYPECASE *)
              END (* IF *)
          ELSE
          END (* TYPECASE *)
      ELSE
      END (* TYPECASE *)
    END (* IF *);
    RETURN FALSE
  END QuantCheckLit;

PROCEDURE RefListDelete(rl: RefList.T; sx: REFANY): RefList.T =
  (* Returns the list formed by deleting the first instance, if any, of "sx"
     from "rl". *)
  BEGIN
    IF rl = NIL THEN
      RETURN NIL
    ELSIF rl.head = sx THEN
      RETURN rl.tail
    ELSE
      RETURN RefList.Cons(rl.head, RefListDelete(rl.tail, sx))
    END (* IF *)
  END RefListDelete;

PROCEDURE RefListUnion(rl1, rl2: RefList.T): RefList.T =
  (* Assumes that "rl1" and "rl2" are lists with no duplicate members.
     Returns a list whose members are the set union of the members of
     "rl1" and "rl2". *)
  VAR rl3 := RefList.Append(rl1, NIL); BEGIN
    WHILE rl2 # NIL DO
      IF NOT RefList.Member(rl1, rl2.head) THEN
        rl3 := RefList.Cons(rl2.head, rl3)
      END (* IF *);
      rl2 := rl2.tail
    END (* WHILE *);
    RETURN rl3
  END RefListUnion;

PROCEDURE SkolemizeOuter(f: T; VAR (*IN/OUT*) rs: RenameState): T
    RAISES { Error } =
  VAR sub := NEW(AtomRefTbl.Default).init(); BEGIN
    IF rs = NIL THEN rs := NEW(RenameState).init() END (* IF *);
    RETURN SkolemizeOuterWork(f, TRUE, FALSE, sub, rs)
  END SkolemizeOuter;

(* "SkolemizeOuterWork(f, sense, seenU, sub, rs)" 
   If "qv" is a quantified variable whose quantifier is removed, "sub"
   is modified to map "qv" to a new name "qv%n".  If "rs" maps
   "qv" to a value "m", "n" is chosen to be greater than "m", and
   "rs" is modified to map "qv" to "n".
*)
PROCEDURE SkolemizeOuterWork(f: T; sense, seenU: BOOLEAN;
                             sub: AtomRefTbl.T;
                             rs: RenameState): T
    RAISES { Error } =
    VAR result: T;
BEGIN
    TYPECASE f OF
    | NULL =>
        <*ASSERT FALSE*>
    | RefList.T(rl) =>
        IF Prover.skolemizeOuterTrace > 0 THEN
          Wr.PutText(Stdio.stdout, "; PredSx.SkolemizeOuterWork: ");
          SxPrint.Print(Stdio.stdout, rl, MatchingRule.EmptySub,
                        Prover.skolemizeOuterTrace,
                        Prover.skolemizeOuterTrace);
          Wr.PutText(Stdio.stdout, "\n");
          Wr.Flush(Stdio.stdout)
        END;
        IF (rl.head = existsSym AND sense) OR
          (rl.head = forallSym AND NOT sense) THEN
          (* Introduce a skolem constant. *)
          VAR vars: RefList.T := rl.tail.head;
              patSpec := QuantPatSpec(rl);
              body := QuantBody(rl);
              subUndos: RefList.T := NIL;
          BEGIN
            WHILE vars # NIL DO
              VAR v: Atom.T := vars.head; ra: REFANY; BEGIN
                IF seenU THEN
                  IF sub.delete(v, ra) THEN
                    subUndos := RefList.Cons(RefList.List2(v, ra), subUndos)
                  END (* IF *)
                ELSE
                  VAR i := rs.inc(v); BEGIN
		    VAR ra: REFANY; BEGIN
		      IF NOT sub.get(v, ra) THEN ra := NIL END (* IF *);
		      EVAL sub.put(v, Atom.FromText(
					  Atom.ToText(v) & SkolemSep &
					  Fmt.Int(i)));
		      subUndos := RefList.Cons(RefList.List2(v, ra), subUndos)
		    END (* BEGIN *)
                  END (* BEGIN *)
                END (* IF *)
              END (* BEGIN *);
              vars := vars.tail
            END (* WHILE *);
            VAR res: REFANY; BEGIN
	      IF NOT seenU THEN
		res := SkolemizeOuterWork(body, sense, seenU, sub, rs)
	      ELSE
		res := MkQuant(
                           existsSym, rl.tail.head, patSpec,
                           SkolemizeOuterWork(body, sense, seenU, sub, rs))
	      END (* IF *);
              WHILE subUndos # NIL DO
                VAR undo: RefList.T := subUndos.head;
                    v: Atom.T := undo.head;
                    val := undo.tail.head;
                BEGIN
                  IF val = NIL THEN
                    EVAL sub.delete(v, val)
                  ELSE
                    EVAL sub.put(v, val)
                  END (* IF *)
                END (* BEGIN *);
                subUndos := subUndos.tail
              END (* WHILE *);
              result :=  res
            END (* BEGIN *)
          END (* BEGIN *)
        ELSIF (rl.head = forallSym AND sense) OR
              (rl.head = existsSym AND NOT sense) THEN
          VAR vars: RefList.T := rl.tail.head;
              patSpec := QuantPatSpec(rl);
              body := QuantBody(rl);
              subUndos: RefList.T := NIL;
          BEGIN
            WHILE vars # NIL DO
              VAR v: Atom.T := vars.head; ra: REFANY; BEGIN
                EVAL rs.inc(v, undo := FALSE);
                IF sub.get(v, ra) THEN
                  subUndos := RefList.Cons(RefList.List2(v, ra), subUndos);
                  EVAL sub.delete(v, ra)
                END (* IF *)
              END (* BEGIN *);
              vars := vars.tail
            END (* WHILE *);
            VAR res :=  MkQuant(
                            forallSym, rl.tail.head, Sub(patSpec, sub),
                            SkolemizeOuterWork(body, sense, TRUE, sub, rs));
            BEGIN
              WHILE subUndos # NIL DO
                VAR undo: RefList.T := subUndos.head; BEGIN
                  EVAL sub.put(undo.head, undo.tail.head)
                END (* BEGIN *);
                subUndos := subUndos.tail
              END (* WHILE *);
              vars := rl.tail.head;
              WHILE vars # NIL DO
                VAR v: Atom.T := vars.head; BEGIN
                  rs.dec(v)
                END (* BEGIN *);
                vars := vars.tail
              END (* WHILE *);
              result :=  res;
            END (* BEGIN *)
          END (* BEGIN *)
        ELSIF rl.head = labelSym OR rl.head = lblNegSym OR
              rl.head = lblPosSym THEN
          VAR lblSym: Atom.T; BEGIN
            IF sense = (rl.head = lblPosSym) THEN
              lblSym := lblPosSym
            ELSE
              lblSym := lblNegSym
            END (* IF *);
            result :=  RefList.List3(lblSym, rl.tail.head,
                               SkolemizeOuterWork(
                                   rl.tail.tail.head,
                                   sense, seenU, sub, rs))
          END (* BEGIN *)
        ELSIF rl.head = notSym THEN
          result :=  SkolemizeOuterWork(rl.tail.head, NOT sense, seenU, sub, rs)
        ELSIF (rl.head = andSym AND sense) OR
              (rl.head = orSym AND NOT sense) THEN
          IF RefList.Length(rl) = 1 THEN
            result :=  trueSym
          ELSIF RefList.Length(rl) = 2 THEN
            result :=  SkolemizeOuterWork(rl.tail.head, sense, seenU, sub, rs);
          ELSE
            VAR 
              first  := SkolemizeOuterWork(
                          rl.tail.head, sense, seenU, sub, rs);
              second := SkolemizeOuterWork(
                          RefList.Cons(rl.head, rl.tail.tail),
                          sense, seenU, sub, rs);
            BEGIN
              result :=  And(first, second)
            END
          END (* IF *)
        ELSIF (rl.head = orSym AND sense) OR
              (rl.head = andSym AND NOT sense) THEN
          IF RefList.Length(rl) = 1 THEN
            result :=  falseSym
          ELSIF RefList.Length(rl) = 2 THEN
            result :=  SkolemizeOuterWork(rl.tail.head, sense, seenU, sub, rs)
          ELSE
            VAR
                first  := SkolemizeOuterWork(
                            rl.tail.head, sense, seenU, sub, rs);
                second := SkolemizeOuterWork(
                            RefList.Cons(rl.head, rl.tail.tail),
                            sense, seenU, sub, rs);
            BEGIN
              result :=  Or(first, second)
            END
          END (* IF *)
        ELSIF (rl.head = impliesSym) THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error("Implies requires two arguments.")
          END (* IF *);
          result :=  SkolemizeOuterWork(
                        RefList.List3(orSym, Not(rl.tail.head),
                                      rl.tail.tail.head),
                        sense, seenU, sub, rs)
        ELSIF (rl.head = expliesSym) THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error("Explies requires two arguments.")
          END (* IF *);
          result :=  SkolemizeOuterWork(
                        RefList.List3(orSym, Not(rl.tail.tail.head),
                                      rl.tail.head),
                        sense, seenU, sub, rs)
        ELSIF (rl.head = iffSym) THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error("Iff requires two arguments.")
          END (* IF *);
          VAR lhs := rl.tail.head;
              rhs := rl.tail.tail.head;
          BEGIN
            result :=  SkolemizeOuterWork(
                          RefList.List3(
                              andSym,
                              RefList.List3(orSym, Not(lhs), rhs),
                                 RefList.List3(orSym, Not(rhs), lhs)),
                       sense, seenU, sub, rs)
          END (* BEGIN *)
        ELSE (* Literal function application... *)
          VAR res: RefList.T := NIL; args := rl.tail; BEGIN
            WHILE args # NIL DO
              VAR arg := Sub(args.head, sub); BEGIN
                res := RefList.Cons(arg, res);
                SkolemizeOuterTerm(arg, rs)
              END (* BEGIN *);
              args := args.tail
            END (* WHILE *);
            result :=  NegIf(RefList.Cons(rl.head, RefList.ReverseD(res)), sense)
          END (* BEGIN *)
        END (* IF *)

    | Atom.T(at) =>
        VAR img: REFANY; BEGIN
          IF sub.get(at, img) THEN
	    IF img = falseSym AND NOT sense THEN result :=  trueSym
	    ELSIF img = trueSym AND NOT sense THEN result :=  falseSym
	    ELSE result :=  NegIf(img, sense)
	    END (* IF *)
          ELSE
            result :=  NegIf(at, sense)
          END (* IF *)
        END (* BEGIN *)
    ELSE
        result :=  NegIf(f, sense)
    END (* TYPECASE *);
    IF Prover.skolemizeOuterTrace > 0 THEN
      Wr.PutText(Stdio.stdout, "; PredSx.SkolemizeOuterWork: returning ");
      SxPrint.Print(Stdio.stdout, result, MatchingRule.EmptySub,
                    Prover.skolemizeOuterTrace,
                    Prover.skolemizeOuterTrace);
      Wr.PutText(Stdio.stdout, "\n");
      Wr.Flush(Stdio.stdout)
    END;
    RETURN result;
  END SkolemizeOuterWork;

PROCEDURE SkolemizeOuterTerm(t: T; rs: RenameState) =
  BEGIN
    TYPECASE t OF
    | NULL =>
    | Atom.T(at) =>
        VAR i: INTEGER; BEGIN
          IF NOT rs.get(at, i) THEN EVAL rs.inc(at) END (* IF *)
        END (* BEGIN *)
    | RefList.T(rl) =>
        SkolemizeOuterTerm(rl.head, rs); SkolemizeOuterTerm(rl.tail, rs)
    ELSE
        RETURN
    END (* TYPECASE *)
  END SkolemizeOuterTerm;

PROCEDURE IsAStar(f: T; sense := TRUE): BOOLEAN =
  BEGIN
    TYPECASE f OF
    | RefList.T(rl) =>
        VAR head := rl.head; BEGIN
          IF head = existsSym AND sense OR head = forallSym AND NOT sense THEN
            RETURN FALSE
          ELSIF head = forallSym AND sense OR
                head = existsSym AND NOT sense THEN
            RETURN IsAStar(QuantBody(rl), sense)
          ELSIF head = notSym THEN
            RETURN IsAStar(rl.tail.head, NOT sense)
          ELSIF head = andSym OR head = orSym THEN
            VAR args := rl.tail; BEGIN
              WHILE args # NIL DO
                IF NOT IsAStar(args.head, sense) THEN
                  RETURN FALSE
                END (* IF *);
                args := args.tail
              END (* WHILE *);
              RETURN TRUE
            END (* BEGIN *)
          ELSIF head = impliesSym THEN
            RETURN IsAStar(rl.tail.head, NOT sense)
               AND IsAStar(rl.tail.tail.head, sense)
          ELSIF head = impliesSym THEN
            RETURN IsAStar(rl.tail.tail.head, NOT sense)
               AND IsAStar(rl.tail.head, sense)
          ELSIF head = iffSym THEN
            RETURN IsAStar(rl.tail.head, sense)
               AND IsAStar(rl.tail.head, NOT sense)
               AND IsAStar(rl.tail.tail.head, sense)
               AND IsAStar(rl.tail.tail.head, NOT sense)
          ELSE
            RETURN TRUE
          END (* IF *)
        END (* BEGIN *)
    ELSE
        RETURN TRUE
    END (* TYPECASE *)
  END IsAStar;

PROCEDURE NegIf(f: T; sense: BOOLEAN): T =
  BEGIN
    IF NOT sense THEN RETURN Not(f)
    ELSE RETURN f
    END (* IF *)
  END NegIf;

PROCEDURE FreeVars(f: T): AtomSet.T =
  VAR exc, res := NEW(AtomSetList.T).init(); BEGIN
    FreeVarsWork(f, exc, res);
    RETURN res
  END FreeVars;

PROCEDURE FreeVarsWork(f: T; exc, res: AtomSet.T) =
  BEGIN
    TYPECASE f OF
    | NULL => RETURN;
    | RefList.T(rl) =>
        IF rl.head = forallSym OR rl.head = existsSym THEN
          VAR vars: RefList.T := rl.tail.head;
              body := QuantBody(rl);
              newExc := NEW(AtomSetList.T).init();
          BEGIN
            WHILE vars # NIL DO
              IF NOT exc.insert(vars.head) THEN
                EVAL newExc.insert(vars.head)
              END (* IF *);
              vars := vars.tail
            END (* WHILE *);
            FreeVarsWork(body, exc, res);
            vars := rl.tail.head;
            VAR newExcIter := newExc.iterate(); v: Atom.T; BEGIN
              WHILE newExcIter.next(v) DO
                EVAL exc.delete(v)
              END (* WHILE *)
            END (* WHILE *)
          END (* BEGIN *)
        ELSE (* FreeVars of args of list *)
          VAR args := rl.tail; BEGIN
            WHILE args # NIL DO
              FreeVarsWork(args.head, exc, res); args := args.tail
            END (* WHILE *)
          END (* BEGIN *)
        END (* IF *)
    | Atom.T(at) =>
        IF NOT exc.member(at) THEN
          EVAL res.insert(at)
        END (* IF *)
    ELSE
    END (* TYPECASE *)
  END FreeVarsWork;

PROCEDURE FormIsLit(f: T): BOOLEAN =
  BEGIN
    TYPECASE f OF
    | Atom.T =>
        RETURN TRUE
    | RefList.T(rl) =>
        VAR at := rl.head; BEGIN
(**)
          IF at = andSym
            OR at = orSym
            OR at = impliesSym
            OR at = expliesSym
            OR at = iffSym
            OR at = proofSym THEN
            RETURN FALSE
          ELSIF at = notSym THEN
            RETURN FormIsLit(rl.tail.head)
          ELSE
            RETURN TRUE
          END (* IF *)
(**)
(*
          IF at = eqSym
            OR at = diffSym
            OR at = ltSym
            OR at = gtSym
            OR at = leSym
            OR at = geSym 
            OR at = forallSym THEN
            RETURN TRUE
          ELSIF at = notSym THEN
            RETURN FormIsLit(rl.tail.head)
          ELSE
            RETURN FALSE
          END (* IF *)
*)
        END (* BEGIN *)
    ELSE
        RETURN FALSE
    END (* TYPECASE *)
  END FormIsLit;

PROCEDURE FormIsClause(f: T): BOOLEAN =
  BEGIN
    TYPECASE f OF
    | RefList.T(rl) =>
        IF rl.head = orSym THEN
          VAR res := TRUE; disjs := rl.tail; BEGIN
            WHILE res AND disjs # NIL DO
              res := res AND FormIsClause(disjs.head);
              disjs := disjs.tail
            END (* WHILE *);
            RETURN res
          END (* BEGIN *)
        ELSE
          RETURN FormIsLit(f)
        END (* IF *)
    ELSE
        RETURN FormIsLit(f)
    END (* TYPECASE *)
  END FormIsClause;

PROCEDURE FormIsInCNF(f: T): BOOLEAN =
  BEGIN
    TYPECASE f OF
    | RefList.T(rl) =>
        IF rl.head = andSym THEN
          VAR res := TRUE; conjs := rl.tail; BEGIN
            WHILE res AND conjs # NIL DO
              res := res AND FormIsInCNF(conjs.head);
              conjs := conjs.tail
            END (* WHILE *);
            RETURN res
          END (* BEGIN *)
        ELSE
          RETURN FormIsClause(f)
        END (* IF *)
    ELSE
        RETURN FormIsClause(f)
    END (* TYPECASE *)
  END FormIsInCNF;

REVEAL
  RenameState = RenameStatePublic BRANDED OBJECT
    t: AtomIntTbl.T;
    undos: AtomSeq.T;
   OVERRIDES
    init := RSInit;
    get := RSGet;
    inc := RSInc;
    dec := RSDec;
    push := RSPush;
    pop := RSPop;
  END (* OBJECT *);

PROCEDURE RSInit(self: RenameState): RenameState =
  BEGIN
    self.t := NEW(AtomIntTbl.Default).init();
    self.undos := NEW(AtomSeq.T).init(1000);
    RETURN self
  END RSInit;

PROCEDURE RSGet(self: RenameState; a: Atom.T; VAR i: INTEGER): BOOLEAN =
  BEGIN RETURN self.t.get(a, i) END RSGet;

PROCEDURE RSInc(self: RenameState; a: Atom.T; undo := TRUE): INTEGER =
  VAR i: INTEGER; BEGIN
    IF NOT self.t.get(a, i) THEN i := -1 END (* IF *);
    INC(i);
    EVAL self.t.put(a, i);
    IF undo THEN self.undos.addhi(a) END (* IF *);
    RETURN i
  END RSInc;

(* "Dec"'s must match "Inc"'s, hence the assert. *)
PROCEDURE RSDec(self: RenameState; a: Atom.T) =
  VAR i: INTEGER; b := self.t.get(a, i); BEGIN
    <*ASSERT b *>
    DEC(i);
    IF i < 0 THEN
      EVAL self.t.delete(a, i)
    ELSE
      EVAL self.t.put(a, i)
    END (* IF *)
  END RSDec;

PROCEDURE RSPush(self: RenameState) =
  BEGIN self.undos.addhi(NIL) END RSPush;

PROCEDURE RSPop(self: RenameState) =
  BEGIN
    WHILE self.undos.size() > 0 DO
      VAR at := self.undos.remhi(); BEGIN
        IF at = NIL THEN EXIT END (* IF *);
        VAR i: INTEGER; b: BOOLEAN; BEGIN
          b := self.t.get(at, i); <*ASSERT b*>
          DEC(i);
          IF i < 0 THEN
            EVAL self.t.delete(at, i)
          ELSE
            EVAL self.t.put(at, i)
          END (* IF *)
        END (* BEGIN *)
      END (* BEGIN *)
    END (* WHILE *)
  END RSPop;

TYPE LoopholeArr = ARRAY [0..(BYTESIZE(LONGREAL) DIV BYTESIZE(Word.T))-1] OF Word.T;

PROCEDURE Hash(f: T): Word.T =
  BEGIN
    TYPECASE f OF <*NOWARN*>
    | NULL =>
        RETURN 0
    | RefList.T(rl) =>
        RETURN Word.Xor(Hash(rl.head), Word.Shift(Hash(rl.tail), 1))
    | Atom.T(at) =>
        RETURN Atom.Hash(at)
    | REF INTEGER(ri) =>
        RETURN ri^
    | REF LONGREAL(rl) =>
        VAR res: Word.T := 0;
            arr := LOOPHOLE(rl^, LoopholeArr);
        BEGIN
          FOR i := 0 TO LAST(arr) DO
            INC(res, arr[i])
          END (* FOR *);
          RETURN res
        END (* BEGIN *)
    END (* TYPECASE *)
  END Hash;

PROCEDURE QuantPatSpec(rl: RefList.T): RefList.T =
  BEGIN
    IF rl.tail.tail.tail # NIL THEN
      RETURN rl.tail.tail.head
    ELSE
      RETURN NIL
    END (* IF *)
  END QuantPatSpec;

PROCEDURE QuantBody(rl: RefList.T): T =
  BEGIN
    IF rl.tail.tail.tail # NIL THEN
      RETURN rl.tail.tail.tail.head
    ELSE
      RETURN rl.tail.tail.head
    END (* IF *)
  END QuantBody;

PROCEDURE MkQuant(q: Atom.T; vars, patSpec: RefList.T; body: T): T =
  BEGIN
    IF patSpec = NIL THEN
      RETURN RefList.List3(q, vars, body)
    ELSE
      RETURN RefList.Cons(q, RefList.List3(vars, patSpec, body))
    END (* IF *);
  END MkQuant;

VAR initDone: BOOLEAN; (* Initialized to FALSE by linker *)

VAR ineqs: AtomSet.T;

PROCEDURE Init() =
  BEGIN 
    IF NOT initDone THEN
      falseSym := Atom.FromText("FALSE");
      trueSym := Atom.FromText("TRUE");
      eqSym := Atom.FromText("EQ");
      diffSym := Atom.FromText("NEQ");
      distClassSym := Atom.FromText("DISTINCT");
      andSym := Atom.FromText("AND");
      orSym := Atom.FromText("OR");
      notSym := Atom.FromText("NOT");
      impliesSym := Atom.FromText("IMPLIES");
      expliesSym := Atom.FromText("EXPLIES");
      iffSym := Atom.FromText("IFF");
      proofSym := Atom.FromText("PROOF");
      plusSym := Atom.FromText("+");
      minusSym := Atom.FromText("-");
      timesSym := Atom.FromText("*");
      selectSym := Atom.FromText("select");
      storeSym := Atom.FromText("store");
      ltSym := Atom.FromText("<");
      gtSym := Atom.FromText(">");
      leSym := Atom.FromText("<=");
      geSym := Atom.FromText(">=");
      forallSym := Atom.FromText("FORALL");
      existsSym := Atom.FromText("EXISTS");
      patsSym := Atom.FromText("PATS");
      noPatsSym := Atom.FromText("NOPATS");
      mpatSym := Atom.FromText("MPAT");
      promoteSym := Atom.FromText("PROMOTE");
      plungeSym := Atom.FromText("PLUNGE");
      labelSym := Atom.FromText("LBL");
      lblPosSym := Atom.FromText("LBLPOS");
      lblNegSym := Atom.FromText("LBLNEG");
      lblNameQuantSym := Atom.FromText("LBLNAME_QUANT");
      lblNameAndSym := Atom.FromText("LBLNAME_AND");
      lblNameOrSym := Atom.FromText("LBLNAME_OR");
      orderSym := Atom.FromText("ORDER");
      defPredSym := Atom.FromText("DEFPRED");
      defPredMapSym := Atom.FromText("DEFPREDMAP");
      ineqs := NEW(AtomSetList.T).fromArray(
                                   ARRAY OF Atom.T{ltSym, gtSym,
                                                   leSym, geSym});
      relOps := NEW(AtomSetDef.T).fromArray(
                                   ARRAY OF Atom.T{eqSym, diffSym,
                                                   ltSym, gtSym,
                                                   leSym, geSym});
      allOps := NEW(AtomSetDef.T).fromArray(
                                   ARRAY OF Atom.T{falseSym, trueSym,
                                                   eqSym, diffSym,
                                                   andSym, orSym, notSym,
                                                   impliesSym, iffSym,
                                                   expliesSym,
                                                   ltSym, gtSym, leSym, geSym,
                                                   forallSym, existsSym,
                                                   patsSym, noPatsSym,
                                                   mpatSym});
      boolOps := NEW(AtomSetDef.T).fromArray(
                                   ARRAY OF Atom.T{andSym, orSym, notSym,
                                                   impliesSym, iffSym,
                                                   expliesSym,
                                                   proofSym});
      initDone := TRUE
    END (* IF *)
  END Init;

BEGIN
  Init()
END PredSx.
