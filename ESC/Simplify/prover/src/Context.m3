(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1992 DEC *)
(* Last modified on Wed May 15 22:21:23 PDT 2002 by saxe    *)
(*      modified on Fri Oct 23 15:11:33 PDT 1998 by gnelson *)
(*      modified on Fri Nov  1 15:50:12 PST 1996 by detlefs *)

UNSAFE MODULE Context EXPORTS Context, ContextPrivate;
(* UNSAFE only to import RTHeapRep. *)

IMPORT PredSx, Enode, FPrint, Match, Simplex, Clause, ClausePrivate,
       Orders, AF, PropVar, ProxyProp, PromoteSet, ContextUndoRec,
       Prover, LabelName, PredDefs, ClauseList, MatchingRule;
IMPORT ContextUndoRecSeq;
IMPORT Wr, Time, Word, Fmt, Env, Sx, Scan, Random, Text,
       RTCollector, RTutils, RTHeapRep;
IMPORT RefList, IntSeq, RefSeq, RefIntTbl, FPRefTbl;

(* For debugging *)
IMPORT Stdio, Thread;
<*FATAL Wr.Failure, Thread.Alerted, Sx.PrintError *>

TYPE
  StatRec = RECORD
    time0, lastNUStart, totNUTime: Time.T := 0.0D0; 
    nSplits, splitsBeforeNU,
    nuStartPushes,
    unrestrictedNuMatches,
    unrestrictedNuMatchCLs,
    clausePlunges,
      plungeValidCheap, plungeValidExpensive,
      plungeNoEffect, plungeNonUnit,
      plungeUnitCheap, plungeUnitExpensive,
      plungeEmptyCheap, plungeEmptyExpensive,
    maxHeap,
    clauseInsertAttempts, clauseInsertsDone,
    clausePromotes,
    clausesScanned, clauseScanningRounds,
      clausesScannedValid, clausesScannedUnit, clausesScannedEmpty := 0
  END (* RECORD *);

  FlagRec = OBJECT
    ops: OpEnabledArr;
    promoteClauseEnabled, litSplitEnabled, sat: BOOLEAN;
    rightMostLits, rightMostClauses: INTEGER;
    inGoalSubproof: BOOLEAN;
    fruitlessSplitLimit: INTEGER;
  END (* OBJECT *);

VAR
  lsSizeStack: IntSeq.T;
  quiescenceDepthStack: IntSeq.T;
  undoStack: ContextUndoRecSeq.T;
  flagStack: RefSeq.T;
  litStack: RefSeq.T;
  rightMostLits: INTEGER := 0;
       (* number of 'rightmost' literals in "litStack" *)
  clauses, nuMatches: Clause.T;
  rightMostClauses: INTEGER := 0;
       (* number of 'rightmost' clauses in "clauses" *)
  promote: PromoteSet.T;
  nuMatchFPs: FPRefTbl.T;
  disposedClauses: RefSeq.T; (* Of Clause.T *)
  lastSplit: Clause.T;
  stats: StatRec;
  promoteClauseEnabled, litSplitEnabled: BOOLEAN;
  immedPromotes: INTEGER;
  (* Debugging flags. *)
  debug: INTEGER;
  doPushLog, doPromoted, doPlungeLog, logIndented, doNUClauses: BOOLEAN;
  doRandomClauseOrder, printContradictions, doNUClauseUIDs: BOOLEAN;
  heapDumpIntrvl, enodeStatIntrvl: INTEGER;

PROCEDURE Init() =
  BEGIN
    lsSizeStack := NEW(IntSeq.T).init(20);
    lsSizeStack.addhi(0);
    quiescenceDepthStack := NEW(IntSeq.T).init(20);
    quiescenceDepthStack.addhi(Prover.initialQuiescenceDepth); (* Needed? *)
    undoStack := NEW(ContextUndoRecSeq.T).init(100);
    flagStack := NEW(RefSeq.T).init(20);
    litStack := NEW(RefSeq.T).init(20);
    clauses := NEW(Clause.T);
    nuMatches := NEW(Clause.T);
    promote := NEW(PromoteSet.T).init(Prover.promoteSize);
    nuMatchFPs := NEW(FPRefTbl.Default).init(100);
    disposedClauses := NEW(RefSeq.T).init();
    lastSplit := NIL;
    ResetStats();
    pushes := 0;
    promoteClauseEnabled := TRUE;
    immedPromotes := Prover.maxImmedPromote;

    litSplitEnabled := TRUE;
    sat := TRUE;
    opsEnabled := OpEnabledArr{ TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE };

    AF.Init();
    PropVar.Init();
    ProxyProp.Init();
    Enode.Init();
    Match.MInit();
    MatchingRule.Init();
    Simplex.Init();
    ClausePrivate.Init();
    ClauseList.Init();
    Orders.Init();
    PredDefs.Init();
    LabelName.Init();
    rs := NEW(PredSx.RenameState).init();

    VAR debugLevel := Env.Get("PROVER_DEBUG"); BEGIN
      IF debugLevel # NIL THEN
        debug := Scan.Int(debugLevel) <*NOWARN*>
      END (* IF *)
    END (* BEGIN *);
    doPushLog := Env.Get("PROVER_PUSH_LOG") # NIL;
    doPromoted := Env.Get("PROVER_LOG_PROMOTES") # NIL;
    doPlungeLog := Env.Get("PROVER_PLUNGE_LOG") # NIL;
    logIndented := TRUE;
    doNUClauses := Env.Get("PROVER_PRINT_NU_CLAUSES") # NIL;
    doRandomClauseOrder := Env.Get("PROVER_RANDOM_CLAUSE_ORDER") # NIL;
    VAR intrvl := Env.Get("PROVER_HEAP_INTRVL"); BEGIN
      IF intrvl = NIL THEN heapDumpIntrvl := -1
      ELSE heapDumpIntrvl := Scan.Int(intrvl) <*NOWARN*>
      END (* IF *)
    END (* BEGIN *);
    VAR intrvl := Env.Get("PROVER_ENODE_STAT_INTRVL"); BEGIN
      IF intrvl = NIL THEN enodeStatIntrvl := -1
      ELSE enodeStatIntrvl := Scan.Int(intrvl) <*NOWARN*>
      END (* IF *)
    END (* BEGIN *);
    printContradictions := Env.Get("PROVER_PRINT_CONTRA") # NIL;
    doNUClauseUIDs := Env.Get("PROVER_PRINT_NU_NO_UIDS") = NIL;
    IF nonClausalPlunge THEN
      Prover.envVars := Prover.envVars & "PROVER_NON_CLAUSE_PLUNGE\n"
    END (* IF *)

  END Init;

PROCEDURE InGoalSubproof(): BOOLEAN =
  BEGIN
    RETURN inGoalSubproof;
(*
    RETURN rightMostLits = 0 AND rightMostClauses = 0
*)
  END InGoalSubproof;

PROCEDURE InLabelSubproof(): BOOLEAN =
  BEGIN
    RETURN inGoalSubproof OR
           LabelName.anyAtSignLabels
  END InLabelSubproof;

PROCEDURE ResetStats() =
  BEGIN
    stats := StatRec{};
    stats.time0 := Time.Now()
  END ResetStats;

PROCEDURE Assert(lit: AF.Lit) =
  BEGIN Propagate(lit);
  END Assert;

PROCEDURE Push() =
  BEGIN
    IF DoDebug(0) AND NOT inD1P THEN
      IndentAndPut(pushes, "Context.Push.\n", 2)
    END (* IF *);
    
    lsSizeStack.addhi(litStack.size());
    quiescenceDepthStack.addhi(quiescenceDepth);
    UndoStackPush(ContextUndoRec.Tag.Mark, c := NIL);
    flagStack.addhi(NEW(FlagRec, ops := opsEnabled,
                        promoteClauseEnabled := promoteClauseEnabled,
                        litSplitEnabled := litSplitEnabled,
                        sat := sat,
                        rightMostLits := rightMostLits,
                        rightMostClauses := rightMostClauses,
                        inGoalSubproof := inGoalSubproof,
                        fruitlessSplitLimit := fruitlessSplitLimit));
    AF.Push();
    PropVar.Push();
    ProxyProp.Push();
    Simplex.Push();
    Enode.Push();
    Match.MPush();
    MatchingRule.Push();
    Orders.Push();
    PredDefs.Push();
    LabelName.Push();
    ClauseList.Push();
    rs.push();
    INC(pushes)
  END Push;
  
PROCEDURE Pop(incrementScores := TRUE) =
  (* Restore "C" to its last saved state.  That is, set "C :=
     SC:hipop()". *)
  VAR oldMatchDepth := Match.depth; BEGIN
    poppedASubproof := FALSE;
    stats.maxHeap := MAX(stats.maxHeap, HeapBytes());
    VAR lsSize := lsSizeStack.remhi(); BEGIN
      WHILE litStack.size() > lsSize DO EVAL litStack.remhi() END (* WHILE *)
    END (* BEGIN *);
    quiescenceDepth := quiescenceDepthStack.remhi();
    (* We must undo clause list operations before the loop below,
       because the UnrestrictedNUMatch undo code requires it. *)
    ClauseList.Pop();
    LOOP
      VAR top := undoStack.remhi(); BEGIN
        CASE top.type OF
        | ContextUndoRec.Tag.Mark =>
            EXIT
        | ContextUndoRec.Tag.DeleteLiteral =>
            VAR c := top.c; pred := top.predll; delLit := top.ll; BEGIN
              IF pred = NIL THEN
                delLit.tail := c.lits;
                c.lits := delLit
              ELSE
                delLit.tail := pred.tail;
                pred.tail := delLit
              END (* IF *)
            END (* BEGIN *)
        | ContextUndoRec.Tag.AddClauseFP =>
            VAR ra: REFANY; 
                b := nuMatchFPs.delete(top.c.fp, ra);
            BEGIN
              <*ASSERT b*>
            END (* BEGIN *)
        | ContextUndoRec.Tag.DisposeClause =>
            EVAL disposedClauses.remhi()
        | ContextUndoRec.Tag.StartSubProof =>
            inGoalSubproof := FALSE;
            IF Prover.tacticTrace THEN
              Wr.PutText(Stdio.stdout,
                "; Context.Pop: popping a subproof, renormalizing scores\n");
              Wr.Flush(Stdio.stdout)
            END;
            IF promote.size() # 0 THEN
              promote := promote.init(Prover.promoteSize)
            END (* IF *);
            VAR l := clauses.succ; BEGIN
              WHILE l # NIL DO
                l.score := NormScore(l.score); l := l.succ
              END (* WHILE *)
            END; (* BEGIN *)
            tightenBoundsUseful := FALSE;
            poppedASubproof := TRUE
        | ContextUndoRec.Tag.PromoteClause =>
            IF top.c.promoted = ClausePrivate.PromoteState.Immed THEN
              INC(immedPromotes)
            END (* IF *);
            top.c.promoted := ClausePrivate.PromoteState.Not
        | ContextUndoRec.Tag.UnrestrictedNUMatch =>
            nuMatches.succ := clauses.succ;
            nuMatches.succ.pred := nuMatches;
            clauses.succ := NIL;
            DEC(Match.depth)
        | ContextUndoRec.Tag.TightenBounds =>
            tightenBoundsUseful := TRUE
            (* Either
                 (1) the call being popped was a leaf call that led to a
                     contradiction (in which case it really was 'useful'),
                 (2) the call being popped was a leaf call that led to a
                     satisfying assignment (in which case it wasn't 'useful',
                     but we're about to pop all the way back until we get
                     out of the current label subproof, which means that
                     we'll pop out of the current goal subproof and set
                     "tightenBoundsUseful" to "FALSE", so it doesn't matter), or
                 (3) the call being popped was a promoted call (in which case
                     our heuristic is to let the "tightenBounds" tactic remain
                     promoted, regardless of the outcome of the call being
                     popped).
            *)              
        END (* CASE *)
      END (* BEGIN *)
    END (* LOOP *);
    (* EXP *)
    (* Ensure that if the match depth changes, it does so before we
       test whether it did :-) *)
    Match.MPop();
    IF NOT inD1P THEN
      IF stats.lastNUStart # 0.0D0 AND
        pushes < stats.nuStartPushes THEN
        stats.totNUTime := stats.totNUTime + Time.Now() - stats.lastNUStart;
        stats.lastNUStart := 0.0D0
      END (* IF *);
      IF caseSplitDebug AND LabelName.traceTriggers > 0 AND InGoalSubproof() THEN
        Wr.PutText(Stdio.stdout,
                   "Popping to push depth " & Fmt.Int(pushes-1) & ".\n")
      END (* IF *);

      VAR promoted: Clause.T := NIL; BEGIN
        IF Match.depth < oldMatchDepth THEN
          promoted := MatchForayReturn();
          IF caseSplitDebug AND LabelName.traceTriggers > 0 THEN
            Wr.PutText(Stdio.stdout,
                       "End of NU Matching foray(" & Fmt.Int(Match.depth) &
                       ".\n")
          END (* IF *)
        END (* IF *);
        IF incrementScores AND NOT Prover.noMeritScoring THEN
        (* Update clause scores of ancestors. *)
          VAR splitAncstr := lastSplit; inc := 1.0; BEGIN
            WHILE splitAncstr # NIL AND inc > 0.0 DO
              splitAncstr.score := splitAncstr.score + inc;
              promote.updateScore(splitAncstr, splitAncstr.score);
              IF splitAncstr.mr # NIL THEN 
                splitAncstr.mr.score := splitAncstr.mr.score + inc
              END (* IF *);
              splitAncstr := splitAncstr.parent;
              inc := inc * 0.5
            END (* WHILE *)
          END (* BEGIN *)
        END (* IF *);
        IF doPushLog THEN
          VAR str: TEXT; BEGIN
            IF  lastSplit # NIL THEN
              str := Fmt.Int(lastSplit.uid MOD 1000)
            ELSE
              str := "NIL"
            END (* IF *);
            IndentAndPut(pushes - 2 + (Match.depth*2), "Pop(" & str & ")..");
            IF promoted # NIL THEN
              Wr.PutText(Stdio.stdout, "*** Promote(" &
                Fmt.Int(promoted.uid MOD 1000) & "), rule# = " &
                Fmt.Int(promoted.mr.id) & ", size(promote) = " &
                Fmt.Int(promote.size()));
              IF doPromoted THEN
                Wr.PutText(Stdio.stdout, "\n");
                Sx.Print(Stdio.stdout, Clause.ToSx(promoted, TRUE));
                Wr.PutText(Stdio.stdout, "\nFingerprint is " &
                  FPrint.ToText(promoted.fp))
              END (* IF *);
              Wr.Flush(Stdio.stdout)
            END (* IF *);
            logIndented := FALSE
          END (* BEGIN *)
        END (* IF *)
      END (* BEGIN *);
      IF doNUClauses AND Match.depth = 1 AND
        clauses.succ # NIL AND clauses.succ.lits.tail = NIL THEN
        VAR lit: AF.Lit := clauses.succ.lits.head;
            litSx := AF.LitToSx(lit, normForm := TRUE);
            hash := PredSx.Hash(litSx);
        BEGIN
          Wr.PutText(Stdio.stdout,
                     "\nAsserting literal, depth " & Fmt.Int(pushes) &
                     ": (#" & Fmt.Int(hash MOD 10000));
          IF doNUClauseUIDs THEN
            Wr.PutText(Stdio.stdout, "[");
            IF NOT lit.sense THEN
              Wr.PutText(Stdio.stdout, "N")
            END (* IF *);
            Wr.PutText(Stdio.stdout, Fmt.Int(lit.af.id) & "]")
          END (* IF *);
          Wr.PutText(Stdio.stdout, " ");
          Sx.Print(Stdio.stdout, litSx);
          Wr.PutText(Stdio.stdout, ")\n");
          Wr.Flush(Stdio.stdout)
        END (* BEGIN *)
      END (* IF *);
      lastSplit := clauses.succ
    END (* IF *);
    VAR fr: FlagRec := flagStack.remhi(); BEGIN
      opsEnabled := fr.ops;
      promoteClauseEnabled := fr.promoteClauseEnabled;
      litSplitEnabled := fr.litSplitEnabled;
      sat := fr.sat;
      rightMostLits := fr.rightMostLits;
      rightMostClauses := fr.rightMostClauses;
      inGoalSubproof := fr.inGoalSubproof;
      fruitlessSplitLimit := fr.fruitlessSplitLimit
    END (* BEGIN *);
    AF.Pop();
    PropVar.Pop();
    ProxyProp.Pop();
    Simplex.Pop();
    Enode.Pop();
    MatchingRule.Pop();
    Orders.Pop();
    PredDefs.Pop();
    LabelName.Pop();
    rs.pop();
    DEC(pushes);
    opsEnabled[Ops.ScanClauses] := TRUE;
    IF DoDebug(0) THEN IndentAndPut(pushes, "Context.Pop.\n", 2) END (* IF *);
    (* If the match depth changed, then the push corresponding to this
       pop was the artificial one inserted by unrestricted non-unit
       matching, so do another. *)
    IF Match.depth < oldMatchDepth THEN Pop() END (* IF *);
  END Pop;

PROCEDURE MatchForayReturn(): Clause.T =
  VAR c := nuMatches.succ;
      maxScore := -1.0; maxClause: Clause.T := NIL;
      dum: REAL;
  BEGIN
    WHILE c # NIL DO
      IF c.score > maxScore AND NOT promote.member(c, dum) THEN
        maxScore := c.score; maxClause := c
      END (* IF *);
      c := c.succ
    END (* WHILE *);
    (* Whether or not we promoted anything, we'll take
       this opportunity to normalize the scores of the
       restored clause list and the promoted set. We
       want to start with promotes more imporant than
       basic clauses. *)
    promote.normalize(0.0);
    VAR l := clauses.succ; BEGIN
      WHILE l # NIL DO
        l.score := NormScore(l.score); l := l.succ
      END (* WHILE *)
    END (* BEGIN *);
    FOR i := 0 TO disposedClauses.size()-1 DO
      VAR c: Clause.T := disposedClauses.get(i); BEGIN
        c.score := NormScore(c.score)
      END (* BEGIN *)
    END (* FOR *);
    IF maxClause # NIL THEN
      VAR dum: REAL; BEGIN
        <*ASSERT NOT promote.member(maxClause, dum) *>
        promote.insert(maxClause, 1.0+NormScore(maxClause.score))
      END (* BEGIN *)
    END (* IF *);
    RETURN maxClause
  END MatchForayReturn;

PROCEDURE UndoStackPush(type: ContextUndoRec.Tag; c: Clause.T;
                        ll, predll: AF.LitList := NIL) =
  BEGIN
    IF type = ContextUndoRec.Tag.DeleteLiteral AND c = NIL THEN
      <*ASSERT FALSE*>
    END (* IF *);
    undoStack.addhi(ContextUndoRec.T{type, c, ll, predll})
  END UndoStackPush;

PROCEDURE Top(kind := Prover.ResKind.Counterexample;
              labelsOnly: BOOLEAN := FALSE;
              includeSimplex := TRUE): Prover.ProveRes =
  VAR res: RefList.T; BEGIN
    IF labelsOnly THEN
      res := NIL
    ELSE
      Enode.ComputeSxSizes();
      res := Enode.Top();
      IF includeSimplex THEN
        res := RefList.AppendD(Simplex.Top(), res);
      END;
(*
      res := RefList.AppendD(Orders.Top(), res);
      res := RefList.AppendD(PredDefs.Top(), res);
*)
      res := RefList.AppendD(PropVar.Top(), res);
    END;
    RETURN NEW(Prover.ProveRes,
               kind := kind,
               context := res,
               lbls := LabelName.Atoms(markReported :=
                                       kind = Prover.ResKind.Counterexample))
  END Top;

PROCEDURE FinishStats() =
  BEGIN
    stats.maxHeap := MAX(stats.maxHeap, HeapBytes());
    IF stats.lastNUStart # 0.0D0 THEN
      stats.totNUTime := stats.totNUTime + Time.Now() - stats.lastNUStart
    END (* IF *);
  END FinishStats;

PROCEDURE Stats() =
  VAR totTime := Time.Now() - stats.time0; BEGIN
    IF Text.Length(Prover.envVars) > 0 THEN
      Wr.PutText(
          Stdio.stdout, "The following environment variables were set:\n");
      Wr.PutText(Stdio.stdout, Prover.envVars);
      Wr.PutText(Stdio.stdout, "\n")
    END (* IF *);
    
    Wr.PutText(
        Stdio.stdout, "Did " & Fmt.Int(stats.nSplits) &
        " case splits, " & Fmt.Int(stats.splitsBeforeNU) & 
        " before matching, " &
        Fmt.Int(stats.nSplits - stats.splitsBeforeNU) & " after.\n");
    Wr.PutText(
        Stdio.stdout, "Total time is " &
        Fmt.LongReal(totTime, prec := 4) &
        "; " & Fmt.LongReal(totTime - stats.totNUTime, prec := 4) &
        " before NU matching, " & 
        Fmt.LongReal(stats.totNUTime, prec := 4) & " after.\n");
    Wr.PutText(Stdio.stdout,
               "Max heap size is " & BytesToStr(stats.maxHeap) & ".\n");

    Wr.PutText(Stdio.stdout,
               "Reached unrestricted NU matching " &
               Fmt.Int(stats.unrestrictedNuMatches) &
               " times");
    IF stats.unrestrictedNuMatches > 0 THEN
      VAR avg := FLOAT(stats.unrestrictedNuMatchCLs) /
                 FLOAT(stats.unrestrictedNuMatches);
      BEGIN
        Wr.PutText(Stdio.stdout,
                   "; average clause list length: " &
                   Fmt.Real(avg, Fmt.Style.Auto, prec := 3) & ".\n")
      END (* BEGIN *)
    ELSE
      Wr.PutText(Stdio.stdout, ".\n")
    END (* IF *);

    Wr.PutText(Stdio.stdout, "Scanned " & Fmt.Int(stats.clausesScanned) &
      " clauses in " & Fmt.Int(stats.clauseScanningRounds) &
      " rounds of clause scanning:\n");
    IF stats.clausesScanned > 0 THEN
      Wr.PutText(Stdio.stdout, "  " &
        Fmt.Int(stats.clausesScannedValid) &
        " were valid (" &
        Fmt.Real(100.0*FLOAT(stats.clausesScannedValid)/
                       FLOAT(stats.clausesScanned), prec := 3) & "%),\n");
      Wr.PutText(Stdio.stdout, "  " & Fmt.Int(stats.clausesScannedUnit) &
        " became units(" &
        Fmt.Real(100.0 *FLOAT(stats.clausesScannedUnit)/
                       FLOAT(stats.clausesScanned), prec := 3) & "%), and\n");
      Wr.PutText(Stdio.stdout, "  " & Fmt.Int(stats.clausesScannedEmpty) &
        " became empty (" &
        Fmt.Real(100.0*FLOAT(stats.clausesScannedEmpty)/
                       FLOAT(stats.clausesScanned), prec := 3) & "%),\n");
    END (* IF *);

    Wr.PutText(Stdio.stdout, "Plunged on " & Fmt.Int(stats.clausePlunges) &
      " clauses:\n");
    Wr.PutText(Stdio.stdout, "  " &
      Fmt.Int(stats.plungeValidCheap + stats.plungeValidExpensive) &
      " were valid (" & Fmt.Int(stats.plungeValidCheap) & " cheap, " &
      Fmt.Int(stats.plungeValidExpensive) & " expensive);\n");
    Wr.PutText(Stdio.stdout, "  " & Fmt.Int(stats.plungeNoEffect) &
      " were unchanged;\n");
    Wr.PutText(Stdio.stdout, "  " & Fmt.Int(stats.plungeNonUnit) &
      " were reduced, but not to units;\n");
    Wr.PutText(Stdio.stdout, "  " & Fmt.Int(stats.plungeUnitCheap + stats.plungeUnitExpensive) &
      " were reduced to units (" & Fmt.Int(stats.plungeUnitCheap) & " cheap and " &
      Fmt.Int(stats.plungeUnitExpensive) & " expensive);\n");
    Wr.PutText(Stdio.stdout, "  " &
      Fmt.Int(stats.plungeEmptyCheap + stats.plungeEmptyExpensive) &
      " were reduced to empty clauses (" &
      Fmt.Int(stats.plungeEmptyCheap) & " cheap, " &
      Fmt.Int(stats.plungeEmptyExpensive) & " expensive)\n");

    Wr.PutText(Stdio.stdout, "Promoted " & Fmt.Int(stats.clausePromotes) &
      " clauses.\n");
    Wr.PutText(Stdio.stdout, "Attempted to add " &
      Fmt.Int(stats.clauseInsertAttempts) & " clauses to clause set; " &
      Fmt.Int(stats.clauseInsertsDone) & " succeeded.\n");

    Enode.Stats();
    Simplex.Stats();
    ProxyProp.Stats();
    LabelName.Stats();
    AF.Stats();

    Wr.Flush(Stdio.stdout)
  END Stats;

PROCEDURE Propagate(lit: AF.Lit) =
  BEGIN
    IF Prover.propagateTrace THEN
      Wr.PutText(Stdio.stdout,
        ";   Context.Propagate: propagating literal   ");
      AF.PrintLit(Stdio.stdout, lit);
      Wr.PutText(Stdio.stdout, "\n");
      Wr.Flush(Stdio.stdout)
    END;
    litStack.addhi(lit);
    IF lit.rightMost THEN INC(rightMostLits) END;
    opsEnabled[Ops.AssertLits] := TRUE
  END Propagate;

<*UNUSED*>
PROCEDURE PropagateProxy(lit: AF.Lit; mr: MatchingRule.T) =
  BEGIN
    TYPECASE lit.af OF
    | ProxyProp.T(p) =>
        VAR c, w: CARDINAL; BEGIN
          ProxyProp.CNFSize(p, lit.sense, c, w);
          IF (c-1) * (w-1) < Prover.maxProxyPropCNF THEN
            VAR cnf := ProxyProp.CNF(p, lit.sense); 
                score := 0.0;
            BEGIN
              IF lit.rightMost THEN
                score := ProxyProp.RightMostScore
              END (* IF *);
              WHILE cnf # NIL DO
                AddClause(NEW(Clause.T, lits := cnf.head,
                              score := score, mr := mr).init());
                cnf := cnf.tail
              END (* WHILE *);
            END (* BEGIN *)
          ELSE
            <*ASSERT FALSE*>
            Propagate(lit)       <*NOWARN*>
          END (* IF *)
        END (* BEGIN *)
    ELSE
        Propagate(lit)
    END (* TYPECASE *)
  END PropagateProxy;


PROCEDURE AssertLits() =
  VAR first := TRUE; lsSize := lsSizeStack.gethi(); BEGIN (* For logging *)
    IF litStack.size() > lsSize THEN
      opsEnabled[Ops.ScanClauses] := TRUE
    END (* IF *);
    WHILE litStack.size() > lsSize AND sat DO
      VAR lit: AF.Lit := litStack.remhi(); BEGIN
        EtpConsiderLit();
        IF lit.rightMost THEN
          DEC(rightMostLits);
          <*ASSERT rightMostLits >= 0*>
(*
          IF InGoalSubproof() THEN StartGoalSubproof() END
*)
        END;
        IF DoDebug(0) THEN
          IndentAndPut(pushes, "", 2);
          AF.PrintLit(Stdio.stdout, lit);
          Wr.PutText(Stdio.stdout, "\n");
          Wr.Flush(Stdio.stdout)
        END (* IF *);
        (* Add any asserted labels... *)
(*        IF NOT inD1P THEN *)
          AssertLabels(lit);
          IF NOT sat THEN RETURN END
(*        END *) (* IF *); 
        VAR tres := lit.af.get(); BEGIN
          CASE tres OF
          | AF.TruthVal.TrueAsserted, AF.TruthVal.FalseAsserted =>
              (* For logging. *)
              IF first THEN RedundantAssertion() END (* IF *);
              sat := (lit.sense = (tres = AF.TruthVal.TrueAsserted))
          | AF.TruthVal.True, AF.TruthVal.False =>
              sat := (lit.sense = (tres = AF.TruthVal.True));
              IF sat THEN sat := lit.af.assert(lit) END (* IF *);
              IF sat THEN lit.af.set(lit.sense, TRUE) END (* IF *)
          ELSE
              lit.af.set(lit.sense, TRUE);
              sat := lit.af.assert(lit)
          END (* IF *)
        END (* BEGIN *);
        first := FALSE; (* For logging *)
        IF printContradictions AND NOT inD1P AND NOT sat THEN
          IndentAndPut(pushes, "CONTRADICTION (lit): ", 2);
          Sx.Print(Stdio.stdout, AF.LitToSx(lit));
          Wr.PutText(Stdio.stdout, "\n");
          Wr.Flush(Stdio.stdout)
        END (* IF *)
      END (* BEGIN *);
    END (* WHILE *);
    opsEnabled[Ops.AssertLits] := FALSE
  END AssertLits;

PROCEDURE StartGoalSubproof() = BEGIN
    UndoStackPush(ContextUndoRec.Tag.StartSubProof, NIL);
    IF Prover.tacticTrace THEN
      Wr.PutText(Stdio.stdout,
        "; Context.GetSplit: starting a subproof\n");
      Wr.Flush(Stdio.stdout)
    END;
  END StartGoalSubproof;

PROCEDURE EtpConsiderLit() = BEGIN END EtpConsiderLit;

PROCEDURE AssertLabels(lit: AF.Lit) =
  VAR lbls := lit.lbls; BEGIN
    WHILE lbls # NIL DO
      VAR lbl: AF.Label := lbls.head; BEGIN
        IF lbl.sense = lit.sense THEN
          sat := sat AND LabelName.Assert(lbl.name)
        END (* IF *)
      END (* BEGIN *);
      lbls := lbls.tail
    END (* WHILE *)
  END AssertLabels;

PROCEDURE RedundantAssertion() = BEGIN END RedundantAssertion;

PROCEDURE ScanClauses() =
  BEGIN 
    INC(scanGeneration);
    IF DoDebug(5) THEN
      Wr.PutText(Stdio.stdout, "Context.ScanClauses: Starting clause set:\n");
      Wr.Flush(Stdio.stdout);
      Clause.PrintList(Stdio.stdout, clauses.succ)
    ELSIF DoDebug(1) THEN
      IndentAndPut(pushes, "Doing ScanClauseList: " &
        Fmt.Int(Clause.ListLength(clauses.succ)) & " --> ", 2);
    END (* IF *);
    INC(stats.clauseScanningRounds);
    ScanClauseList(clauses, harvestProxy := TRUE);
    ScanClauseList(nuMatches, harvestProxy := FALSE);
    IF DoDebug(5) THEN
      Wr.PutText(Stdio.stdout, "Context.ScanClauses: Reduced clause set:\n");
      Wr.Flush(Stdio.stdout);
      Clause.PrintList(Stdio.stdout, clauses.succ)
    ELSIF DoDebug(1) THEN
      Wr.PutText(Stdio.stdout, Fmt.Int(Clause.ListLength(clauses.succ)) & "\n")
    END (* IF *);
    opsEnabled[Ops.ScanClauses] := FALSE
  END ScanClauses;

PROCEDURE ScanClauseList(sentinel: Clause.T; harvestProxy: BOOLEAN) =
  (* Delete all unit or empty clauses from "sentinel.succ", transferring the
     literals they contain onto "litStack" (treating the empty clause as
     containing the literal "AF.falseLit").  Delete any false
     literals from the clauses that contain them.
     Delete any clauses containing true literals. 
  *)
  VAR ll := sentinel.succ; BEGIN
    (* P: All clauses up to but not including "ll" have been processed.
          If "minWidth" is the minimum width of any processed
          clause; if "minWidth" is less than "LAST(INTEGER)", "minP"
          is a clause of that width in "l".
    *)
    WHILE ll # NIL AND sat DO
      INC(stats.clausesScanned);
      VAR doDelete := ProcessClause(ll);
          next := ll.succ;
      BEGIN
        IF doDelete THEN INC(stats.clausesScannedValid) END (* IF *);
        IF ll.lits = NIL THEN
          INC(stats.clausesScannedEmpty);
          Propagate(AF.falseLit);
          IF printContradictions AND NOT inD1P AND ll.mr # NIL THEN
            Wr.PutText(Stdio.stdout,
              "ScanCLauseList: Reduced an instance of rule " &
              Fmt.Int(ll.mr.id) & " to an empty clause.\n");
            Wr.Flush(Stdio.stdout)
          END (* IF *)
        ELSIF ll.lits.tail = NIL THEN
          IF (harvestProxy OR
               NOT ISTYPE(NARROW(ll.lits.head, AF.Lit).af,
                          ProxyProp.T)) THEN
            INC(stats.clausesScannedUnit);
            Propagate(ll.lits.head);
            doDelete := TRUE
          ELSE
            IF Prover.harvestTrace THEN
              Wr.PutText(Stdio.stdout,
                "; Context.ScanClauseList: couldn't harvest a literal\n")
            END
          END
        END (* IF *);
        IF doDelete THEN
          IF sentinel = clauses THEN DisposeClause(ll) END (* IF *);
          ClauseList.Delete(sentinel, ll);
          IF ll.score =  ProxyProp.RightMostScore THEN
            DEC(rightMostClauses);
            <*ASSERT rightMostClauses >= 0*>
          END;
(*
          IF InGoalSubproof() THEN StartGoalSubproof() END
*)
        END (* IF *);
        ll := next
      END (* BEGIN *)
    END (* WHILE *);
    opsEnabled[Ops.GetSplit] := SplitExists()
  END ScanClauseList;

PROCEDURE DisposeClause(cl: Clause.T) =
  BEGIN
    UndoStackPush(ContextUndoRec.Tag.DisposeClause, NIL);
    disposedClauses.addhi(cl)
  END DisposeClause;
    

PROCEDURE ProcessClause(c: Clause.T): BOOLEAN =
  (* Process the clause "c".  Returns "TRUE" if "c" contains a valid
     literal.  Otherwise, returns "FALSE" and deletes all
     unsatisfiable literals from "c".
  *)
  VAR locLits := c.lits; locLitsPred: AF.LitList := NIL; BEGIN
    (* P2: Every literal up to and including "locLitsPred" has been
       processed, unless "locLitsPred" is "NIL", in which case nothing
       has been processed. 
    *)
    WHILE locLits # NIL DO
      VAR lit: AF.Lit := locLits.head;
          stat: AF.TruthVal;
      BEGIN
        IF Prover.deepStatus THEN
          stat := AF.DeepStatus(lit, AF.TruthVal.False, AF.TruthVal.True)
        ELSE
          stat := AF.Status(lit)
        END;
        CASE stat OF
        | AF.TruthVal.True, AF.TruthVal.TrueAsserted =>
            AssertLabels(lit);
            IF stat = AF.TruthVal.True THEN
              Propagate(lit)
            END (* IF *);
            RETURN TRUE
        | AF.TruthVal.False, AF.TruthVal.FalseAsserted =>
            VAR next := locLits.tail; BEGIN
              EVAL DeleteLiteral(c, locLitsPred);
              locLits := next
            END (* BEGIN *)
        ELSE
            locLitsPred := locLits;
            locLits := locLits.tail
        END (* CASE *)
      END (* BEGIN *);
    END (* WHILE *);
    RETURN FALSE
  END ProcessClause;

PROCEDURE SplitExists(): BOOLEAN =
  BEGIN
    RETURN clauses.succ # NIL OR
           nuMatches.succ # NIL AND Match.depth < Prover.maxMatchDepth
  END SplitExists;

VAR clauseAddDebug := Env.Get("PROVER_CLAUSE_ADD_DEBUG") # NIL;

PROCEDURE AddClause(c: Clause.T) =
  BEGIN
    IF clauseAddDebug THEN
      Wr.PutText(Stdio.stdout,
        "Context.AddClause:  Adding clause to current clause set:\n");
      Wr.Flush(Stdio.stdout);
      ClausePrivate.DBGPrint(Stdio.stdout, c);
    END;
    ClauseList.Add(clauses, c, TRUE);
    IF c.score =  ProxyProp.RightMostScore THEN
      INC(rightMostClauses);
    END;
    (* DEBUG
    OneRightmostClauseCheck();
    *)
    opsEnabled[Ops.ScanClauses] := TRUE;
    opsEnabled[Ops.GetSplit] := TRUE
  END AddClause;

PROCEDURE AddClauseToSet(c: Clause.T) =
  VAR b := nuMatchFPs.put(c.fp, NIL); BEGIN
    INC(stats.clauseInsertAttempts);
    IF b THEN
      IF clauseAddDebug THEN
        Wr.PutText(Stdio.stdout,
          "Context.AddClauseToSet:  skipping redundant clause:\n");
        Wr.Flush(Stdio.stdout);
        ClausePrivate.DBGPrint(Stdio.stdout, c);
      END;
    ELSE
      IF clauseAddDebug THEN
        Wr.PutText(Stdio.stdout,
          "Context.AddClauseToSet:  Adding clause to pending clause set:\n");
        Wr.Flush(Stdio.stdout);
        ClausePrivate.DBGPrint(Stdio.stdout, c);
      END;
      UndoStackPush(ContextUndoRec.Tag.AddClauseFP, c);
      ClauseList.Add(nuMatches, c, TRUE);
      INC(stats.clauseInsertsDone)
    END (* IF *);
    IF Prover.noPlunge OR Prover.usePlungeHints THEN
      opsEnabled[Ops.ScanClauses] := TRUE
    END (* IF *);
    opsEnabled[Ops.GetSplit] := SplitExists()
  END AddClauseToSet;


<*UNUSED*>
PROCEDURE OneRightmostClauseCheck() = 
  VAR cl2 := clauses.succ; n := 0; BEGIN
    WHILE cl2 # NIL DO
      IF cl2.score >= 10000.0 THEN
        VAR p := cl2.lits; valid := FALSE; BEGIN
          WHILE p # NIL AND NOT valid DO
            IF AF.Status(p.head) IN AF.TVTrue THEN
              valid := TRUE
            END (* IF *);
            p := p.tail
          END (* WHILE *);
          IF NOT valid THEN INC(n) END (* IF *);
          IF n = 2 THEN
            <*ASSERT FALSE*>
          END (* IF *)
        END (* BEGIN *)
      END (* IF *);
      cl2 := cl2.succ
    END (* WHILE *)
  END OneRightmostClauseCheck;

PROCEDURE DeleteLiteral(c: Clause.T; pred: AF.LitList): AF.Lit =
  (* Delete and return the literal whose predecessor is "pred" from
     the clause "c".  If "pred" is "NIL", delete the first literal. *)
  VAR this: AF.LitList; BEGIN
    IF pred = NIL THEN
      this := c.lits
    ELSE
      this := pred.tail
    END (* IF *);
    IF pred = NIL THEN
      c.lits := this.tail
    ELSE
      pred.tail := this.tail
    END (* IF *);
    this.tail := NIL;
    UndoStackPush(ContextUndoRec.Tag.DeleteLiteral, c, this, pred);
    RETURN this.head
  END DeleteLiteral;

PROCEDURE UnitMatch() RAISES {Prover.Timeout} =
  BEGIN
    IF DoDebug(1) THEN
      IndentAndPut(pushes, "Before MatchUnits.\n", 2)
    END (* IF *);
    Match.UnitMatch();
    opsEnabled[Ops.UnitMatch] := FALSE;
    IF DoDebug(2) THEN
      IndentAndPut(pushes, "After MatchUnits.\n", 2)
    END (* IF *);
  END UnitMatch;

PROCEDURE SelStoreDist() =
  BEGIN
    IF DoDebug(1) THEN
      IndentAndPut(pushes, "Before Sel-store quiescence.\n", 2)
    END (* IF *);
    opsEnabled[Ops.SelStoreDist] := Match.SelStoreDist();
    IF DoDebug(2) THEN
      IndentAndPut(pushes, "After Sel-store quiescence.\n", 2)
    END (* IF *);
  END SelStoreDist;

PROCEDURE RestrictedNUMatch() RAISES {Prover.Timeout} =
  BEGIN
    IF DoDebug(1) THEN
      IndentAndPut(pushes, "Before Restricted NU Matching.\n", 2)
    END (* IF *);

    VAR nu := Match.NonUnitMatch(); BEGIN

      IF NOT Prover.lazySimplexIntern THEN
        AssertLits();
        IF NOT sat THEN
          IF DoDebug(2) THEN
            IndentAndPut(pushes, "After Restricted NU Matching.\n", 2)
          END (* IF *);
          RETURN
        END (* IF *)
      END (* IF *);
      IF NOT Prover.noPlunge THEN
        Depth1PlungeCL(nu, FALSE)
      END (* IF *);

      IF sat THEN
        VAR nu2 := nu; BEGIN
          WHILE nu2 # NIL DO
            VAR c := nu2; BEGIN
              nu2 := nu2.succ;
              c.succ := NIL; c.pred := NIL;
              IF Prover.useImmediatePromote AND c.mr.immedPromote
                AND immedPromotes # 0 THEN
                DEC(immedPromotes);
                c.score := 1.0;
                c.promoted := ClausePrivate.PromoteState.Immed;
                UndoStackPush(ContextUndoRec.Tag.PromoteClause, c);
                INC(stats.clausePromotes);
                AddClause(c)
              ELSE
                AddClauseToSet(c)
              END (* IF *)
            END (* BEGIN *)
          END (* WHILE *)
        END (* BEGIN *)
      END (* IF *)
    END (* BEGIN *);
    IF DoDebug(2) THEN
      IndentAndPut(pushes, "After Restricted NU Matching.\n", 2)
    END (* IF *);
  END RestrictedNUMatch;

PROCEDURE UnrestrictedNUMatching() =
  BEGIN
    IF DoDebug(1) THEN
      IndentAndPut(pushes, "Before unrestricted NU Matching.\n", 2)
    END (* IF *);
    IF caseSplitDebug AND LabelName.traceTriggers > 0 THEN
      Wr.PutText(Stdio.stdout, "NU Matching(" & Fmt.Int(Match.depth) &").\n")
    END (* IF *);
    (* Introduce an artificial push, so that we can pop back to a
       state in which "nuMatches" contains all the clauses we are
       about to split upon.  The corresponding "Pop" will be the only
       one in which "Match.depth" changes. *)
    IF Prover.tacticTrace THEN
      Wr.PutText(Stdio.stdout,
        "; Context.UnrestrictedNUMatching:  Introducing artificial Push()\n");
      Wr.Flush(Stdio.stdout)
    END;
    Push();
    INC(Match.depth);
    Match.maxDepthSeen := MAX(Match.maxDepthSeen, Match.depth);

    <*ASSERT clauses.succ = NIL AND nuMatches.succ # NIL *>
    clauses.succ := nuMatches.succ;
    clauses.succ.pred := clauses;
    nuMatches.succ := NIL;
    UndoStackPush(ContextUndoRec.Tag.UnrestrictedNUMatch, NIL);
    VAR c := clauses.succ; r: Random.T; BEGIN
      IF doRandomClauseOrder THEN r := NEW(Random.Default).init() END (* IF *);
      WHILE c # NIL DO
        IF doRandomClauseOrder THEN
          c.score := r.real()
        ELSIF c.mr # NIL THEN
          c.score := NormScore(c.mr.score)
        ELSE
          c.score := 0.0
        END (* IF *);
        c := c.succ
      END (* WHILE *);
    END (* BEGIN *);
    IF DoDebug(2) THEN
      IndentAndPut(pushes, "After unrestricted NU Matching.\n", 2)
    END (* IF *);
  END UnrestrictedNUMatching;

(* If "c" is valid in the current context, returns "TRUE".  Otherwise,
   sets "newLits" to the list of c's literals that are satisfiable in
   the current context, and returns "FALSE", ensuring that "newLits =
   c.lits" iff no literals were found unsatisfiable. *)
PROCEDURE PlungeOnClause(c: Clause.T;
                         VAR (*OUT*) newLits: AF.LitList): BOOLEAN
    RAISES {Prover.Timeout} =
  VAR ll := c.lits;
      valid, delLit := FALSE;
  BEGIN
    IF c.mr # NIL THEN
      EtpRulePlungeStart(c.mr.id, c.mr.parId);
      IF doPlungeLog THEN
         Wr.PutText(Stdio.stdout,
           "; Context.PlungeOnClause: Plunging on an instance of rule #" &
           Fmt.Int(c.mr.id) & "\n");
         Wr.Flush(Stdio.stdout)
      END
    END (* IF *);
    INC(stats.clausePlunges);
    newLits := NIL;
    VAR notInvalid := 0; firstNotInvalid: AF.Lit; BEGIN
      WHILE ll # NIL AND NOT valid DO
        VAR stat := AF.Status(ll.head); BEGIN
          IF stat IN AF.TVTrue THEN
            AssertLabels(ll.head);
            IF stat = AF.TruthVal.True THEN Propagate(ll.head) END (* IF *);
            valid := TRUE
          END (* IF *);
          IF NOT (stat IN AF.TVFalse) THEN
            INC(notInvalid);
            firstNotInvalid := ll.head
          END (* IF *)
        END (* BEGIN *);
        ll := ll.tail
      END (* WHILE *);
      IF valid THEN
        INC(stats.plungeValidCheap);
        IF doPlungeLog THEN
           Wr.PutText(Stdio.stdout,
             "; Context.PlungeOnClause: result: plungeValidCheap\n");
           Wr.Flush(Stdio.stdout)
        END;
        RETURN TRUE
      ELSIF notInvalid = 0 THEN
        newLits := NIL;
        IF doPlungeLog THEN
           Wr.PutText(Stdio.stdout,
             "; Context.PlungeOnClause: result: plungeEmptyCheap\n");
           Wr.Flush(Stdio.stdout)
        END;
        INC(stats.plungeEmptyCheap);
        RETURN FALSE
      ELSIF notInvalid = 1 THEN
        newLits := RefList.List1(firstNotInvalid);
        IF doPlungeLog THEN
           Wr.PutText(Stdio.stdout,
             "; Context.PlungeOnClause: result: plungeUnitCheap\n");
           Wr.Flush(Stdio.stdout)
        END;
        INC(stats.plungeUnitCheap);
        RETURN FALSE
      END (* IF *);
    END (* BEGIN *);
    ll := c.lits;
    IF Prover.d1PMaxEffort < 2 THEN inD1P := TRUE END (* IF *);
    WHILE ll # NIL DO
      VAR lit: AF.Lit := ll.head;
          tryLit := NOT ISTYPE(lit.af, MatchingRule.AtomF) AND
                    NOT ISTYPE(lit.af, ProxyProp.T);
      BEGIN
        IF tryLit THEN
          Push();
          VAR b: BOOLEAN; BEGIN
            TRY
              b := PlungeAssert(lit);
            FINALLY
              Pop();
            END;
            IF b THEN
              newLits := RefList.Cons(lit, newLits)
            ELSE
              Propagate(AF.Not(lit));
              delLit := TRUE
            END (* IF *);
          END (* BEGIN *)
        ELSE
          newLits := RefList.Cons(lit, newLits)
        END (* IF *)
      END (* BEGIN *);
      ll := ll.tail
    END (* WHILE *);
    IF Prover.d1PMaxEffort < 2 THEN inD1P := FALSE END (* IF *);
    IF newLits = NIL THEN
      IF doPlungeLog THEN
         Wr.PutText(Stdio.stdout,
           "; Context.PlungeOnClause: result: plungeEmptyExpensive\n");
         Wr.Flush(Stdio.stdout)
      END;
      INC(stats.plungeEmptyExpensive); RETURN FALSE
    ELSIF newLits.tail = NIL THEN
      IF doPlungeLog THEN
         Wr.PutText(Stdio.stdout,
           "; Context.PlungeOnClause: result: plungeUnitExpensive\n");
         Wr.Flush(Stdio.stdout)
      END;
      INC(stats.plungeUnitExpensive); RETURN FALSE
    END (* IF *);
    (* Try for valid. *)
    IF Prover.d1PMaxEffort < 2 THEN inD1P := TRUE END (* IF *);
    ll := newLits;
    WHILE ll # NIL AND NOT valid DO
      VAR lit: AF.Lit := ll.head;
          tryLit := NOT ISTYPE(lit.af, MatchingRule.AtomF) AND
                    NOT ISTYPE(lit.af, ProxyProp.T);
      BEGIN
        IF tryLit THEN
          Push();
          VAR b: BOOLEAN; BEGIN
            TRY
              lit.sense := NOT lit.sense;
              b := PlungeAssert(lit);
              lit.sense := NOT lit.sense
            FINALLY
              Pop();
            END;
            IF NOT b THEN
              Propagate(lit);
              valid := TRUE
            END (* IF *)
          END (* BEGIN *)
        END (* IF *)
      END (* BEGIN *);
      ll := ll.tail
    END (* WHILE *);
    IF Prover.d1PMaxEffort < 2 THEN inD1P := FALSE END (* IF *);
    IF NOT delLit THEN
      newLits := c.lits
    ELSIF newLits # NIL AND newLits.tail # NIL THEN
      newLits := ClauseShuffle(newLits)
    END (* IF *);
    IF c.mr # NIL THEN EtpRulePlungeEnd() END (* IF *);
    IF valid THEN
      IF doPlungeLog THEN
         Wr.PutText(Stdio.stdout,
           "; Context.PlungeOnClause: result: plungeValidExpensive\n");
         Wr.Flush(Stdio.stdout)
      END;
      INC(stats.plungeValidExpensive)
    ELSIF newLits = c.lits THEN
      IF doPlungeLog THEN
         Wr.PutText(Stdio.stdout,
           "; Context.PlungeOnClause: result: plungeNoEffect\n");
         Wr.Flush(Stdio.stdout)
      END;
      INC(stats.plungeNoEffect)
    ELSIF newLits = NIL THEN
      IF doPlungeLog THEN
         Wr.PutText(Stdio.stdout,
           "; Context.PlungeOnClause: result: plungeEmptyExpensive\n");
         Wr.Flush(Stdio.stdout)
      END;
      INC(stats.plungeEmptyExpensive)
    ELSIF newLits.tail = NIL THEN
      IF doPlungeLog THEN
         Wr.PutText(Stdio.stdout,
           "; Context.PlungeOnClause: result: plungeUnitExpensive\n");
         Wr.Flush(Stdio.stdout)
      END;
      INC(stats.plungeUnitExpensive)
    ELSE
      IF doPlungeLog THEN
         Wr.PutText(Stdio.stdout,
           "; Context.PlungeOnClause: result: plungeNonUnit\n");
         Wr.Flush(Stdio.stdout)
      END;
      INC(stats.plungeNonUnit)
    END (* IF *);
    RETURN valid
  END PlungeOnClause;


(* Let "p'" be "sense IFF p".  If "p'" is reducible via depth-1 plunging to
   "TRUE", "FALSE", or a conjunction of (non-proxy) literals, returns
   TRUE; otherwise returns FALSE.  If it returns "TRUE", "res" is "NIL" if
   "p'" reduced to "FALSE", a singleton list containing the
   literal "TRUE" if "p'" reduced to "TRUE" or else it is the list of
   non-proxy literals to which "p'" reduced.
*)
PROCEDURE PlungeProxyToUnitConjsOrEmpty(p: ProxyProp.T; sense: BOOLEAN;
                                        VAR (*OUT*) res: AF.LitList): BOOLEAN
    RAISES {Prover.Timeout} =
  VAR ll := p.lits; BEGIN
    res := NIL;
    IF sense THEN
      (* Conjunction. *)
      WHILE ll # NIL DO
        VAR lit: AF.Lit := ll.head;
            stat := AF.Status(lit);
        BEGIN
          (* First cheap... *)
          IF stat IN AF.TVFalse THEN
            res := NIL; RETURN TRUE
          ELSIF NOT stat IN AF.TVTrue THEN
            (* ...then plunge. *)
            TYPECASE lit.af OF
            | ProxyProp.T(p2) =>
                VAR res2: AF.LitList; BEGIN
                  IF NOT PlungeProxyToUnitConjsOrEmpty(p2, lit.sense, res2) THEN
                    RETURN FALSE
                  ELSIF res2 = NIL THEN
                    res := NIL; RETURN TRUE
                  ELSE
                    res := RefList.AppendD(res2, res)
                  END (* IF *)
                END (* BEGIN *)
            | MatchingRule.AtomF =>
                res := RefList.Cons(lit, res)
            ELSE
                Push();
                VAR b: BOOLEAN; BEGIN
                  TRY
                    b := PlungeAssert(lit)
                  FINALLY
                    Pop()
                  END;
                  IF NOT b THEN
                    Propagate(AF.Not(lit));
                    res := NIL; RETURN TRUE
                  END (* IF *)
                END (* BEGIN *);
                Push();
                VAR b: BOOLEAN; BEGIN
                  TRY
                    lit.sense := NOT lit.sense;
                    b := PlungeAssert(lit);
                    lit.sense := NOT lit.sense;
                  FINALLY
                    Pop();
                  END; 
                  IF NOT b THEN
                    Propagate(lit);
                  ELSE
                    res := RefList.Cons(lit, res)
                  END (* IF *)
                END (* BEGIN *)
            END (* TYPECASE *)
          END (* IF *)
        END (* BEGIN *);
        ll := ll.tail
      END (* WHILE *);
      RETURN TRUE
    ELSE
      (* Disjunction *)
      WHILE ll # NIL DO
        VAR lit: AF.Lit := ll.head;
            stat := AF.Status(lit);
        BEGIN
          (* Remember that ProxyProp disjunctions assert negations of
             their literals. *)
          (* First cheap... *)
          IF stat IN AF.TVFalse THEN
            (* = TRUE *)
            res := RefList.List1(AF.trueLit); RETURN TRUE
          ELSIF NOT stat IN AF.TVTrue THEN
            (* not FALSE, so plunge. *)
            TYPECASE lit.af OF
            | ProxyProp.T(p2) =>
                VAR res2: AF.LitList; BEGIN
                  IF NOT PlungeProxyToUnitConjsOrEmpty(
                             p2, sense = lit.sense, res2) THEN
                    RETURN FALSE
                  ELSIF res2 # NIL THEN
                    IF res # NIL THEN
                      RETURN FALSE
                    ELSE
                      res := res2
                    END (* IF *)
                  END (* IF *)
                END (* BEGIN *)
            | MatchingRule.AtomF =>
                IF res # NIL THEN
                  RETURN FALSE
                ELSE
                  res := RefList.List1(AF.Not(lit))
                END (* IF *)
            ELSE
                Push();
                VAR b: BOOLEAN; BEGIN
                  TRY
                    b := PlungeAssert(lit);
                  FINALLY
                    Pop();
                  END;
                  IF NOT b THEN
                    (* the true literal's negation is unsatisfiable;
                       the true literal, and thus the disjunction, is valid. *)
                    Propagate(AF.Not(lit));
                    res := RefList.List1(AF.trueLit)
                  ELSE
                    Push();
                    VAR b: BOOLEAN; BEGIN
                      TRY
                         lit.sense := NOT lit.sense;
                        b := PlungeAssert(lit);
                        lit.sense := NOT lit.sense;
                      FINALLY
                        Pop();
                      END;
                      IF NOT b THEN
                        (* The true literal is unsatisfiable;
                           propagate it's negation. *)
                        Propagate(lit)
                      ELSE
                        IF res # NIL THEN
                          RETURN FALSE
                        ELSE
                          res := RefList.List1(AF.Not(lit))
                        END (* IF *)
                      END (* IF *)
                    END (* BEGIN *)
                  END (* IF *)
                END (* BEGIN *)
            END (* TYPECASE *)
          END (* IF *)
        END (* BEGIN *);
        ll := ll.tail
      END (* WHILE *);
      RETURN TRUE
    END (* IF *)
  END PlungeProxyToUnitConjsOrEmpty;

PROCEDURE EtpRulePlungeStart(<*UNUSED*> id, parId: INTEGER) =
  BEGIN END EtpRulePlungeStart;
PROCEDURE EtpRulePlungeEnd() =
  BEGIN END EtpRulePlungeEnd;

PROCEDURE PlungeAssert(l: AF.Lit): BOOLEAN RAISES {Prover.Timeout} =
  BEGIN
    Assert(l);
    UnitConsequences(Prover.d1PMaxEffort);
    RETURN sat
  END PlungeAssert;

VAR r: Random.T := NIL;

PROCEDURE ClauseShuffle(l: AF.LitList): AF.LitList =
  BEGIN
    CASE Prover.clauseShuffle OF
    | Prover.ClauseShuffle.Reverse =>
        RETURN l
    | Prover.ClauseShuffle.None =>
        RETURN RefList.ReverseD(l)
    | Prover.ClauseShuffle.Rand =>
        VAR len := RefList.Length(l); BEGIN
          CASE len OF
          | 0, 1 => RETURN l
          ELSE
              IF r = NIL THEN
                r := NEW(Random.Default).init();
              END (* IF *);
              VAR res: RefList.T := NIL; BEGIN
                WHILE len > 1 DO
                  VAR ll := l; llpred: RefList.T := NIL;
                      i := r.integer(0, len-1);
                  BEGIN
                    WHILE i > 0 DO
                      llpred := ll; ll := ll.tail; DEC(i)
                    END (* WHILE *);
                    res := RefList.Cons(ll.head, res);
                    IF llpred = NIL THEN l := l.tail
                    ELSE llpred.tail := ll.tail
                    END (* IF *);
                    DEC(len)
                  END (* BEGIN *)
                END (* WHILE *);
                res := RefList.Cons(l.head, res);
                RETURN res
              END (* BEGIN *)
          END (* CASE *)
        END (* BEGIN *)
    END (* CASE *)
  END ClauseShuffle;

VAR nonClausalPlunge := Env.Get("PROVER_NON_CLAUSE_PLUNGE") # NIL;

PROCEDURE Depth1PlungeCL(VAR l: Clause.T; writeUndo: BOOLEAN)
    RAISES {Prover.Timeout} =
  VAR sentinel := NEW(Clause.T);
      nu := l;
  BEGIN
    sentinel.succ := l;
    IF l # NIL THEN l.pred := sentinel END (* IF *);
    IF doPlungeLog THEN
      Wr.PutText(Stdio.stdout, "Plunging on clause list of length " &
        Fmt.Int(Clause.ListLength(l)) & ".\n")
    END (* IF *);
    WHILE nu # NIL AND sat DO
      IF Thread.TestAlert() THEN RAISE Prover.Timeout END;
      VAR c := nu; BEGIN
        nu := nu.succ;
        IF c.mr = NIL OR c.mr.plungeHint THEN
          VAR newLits: AF.LitList; BEGIN
            IF NOT PlungeOnClause(c, newLits) THEN
              IF newLits = NIL THEN
                sat := FALSE
              ELSIF newLits.tail = NIL THEN
                VAR lit: AF.Lit := newLits.head; BEGIN
                  TYPECASE lit.af OF
                  | ProxyProp.T(pp) =>
                      IF nonClausalPlunge THEN
                        VAR conjs: AF.LitList; BEGIN
                          IF PlungeProxyToUnitConjsOrEmpty(
                                 pp, lit.sense, conjs) THEN
                            IF conjs = NIL THEN
                              sat := FALSE
                            ELSE
                              WHILE conjs # NIL DO
                                Propagate(conjs.head); conjs := conjs.tail
                              END (* WHILE *);
                              ClauseList.Delete(sentinel, c, writeUndo)
                            END (* IF *)                            
                          END (* IF *)
                        END (* BEGIN *)
                      ELSIF lit.sense AND ProxyProp.ConjOfNonProxies(pp) THEN
                        VAR lits := pp.lits; BEGIN
                          WHILE lits # NIL DO
                            Propagate(lits.head); lits := lits.tail
                          END (* WHILE *)
                        END (* BEGIN *)
                      END (* IF *)
                  ELSE
                    Propagate(lit);
                    ClauseList.Delete(sentinel, c, writeUndo)
                  END (* TYPECASE *)
                END (* BEGIN *)
              ELSIF newLits # c.lits THEN
                VAR c2 := NEW(Clause.T, lits := AF.LitListCopy(newLits),
                              mr := c.mr).init();
                BEGIN
                  ClauseList.Delete(sentinel, c, writeUndo);
                  ClauseList.Add(sentinel, c2, writeUndo)
                END (* BEGIN *)
              END (* IF *)
            ELSE
              ClauseList.Delete(sentinel, c, writeUndo)
            END (* IF *)
          END (* BEGIN *)
        END (* IF *)
      END (* BEGIN *);
    END (* WHILE *);
    l := sentinel.succ
  END Depth1PlungeCL;

PROCEDURE TightenBounds() RAISES {Prover.Timeout} =
  BEGIN
    UndoStackPush(ContextUndoRec.Tag.TightenBounds, c := NIL);
    sat := Simplex.TightenBounds()
  END TightenBounds;

PROCEDURE UnitConsequences(maxEffort: [0..3] := 3) RAISES {Prover.Timeout} =
  VAR fnurNum := 0; BEGIN
    WHILE sat DO
      IF opsEnabled[Ops.AssertLits] THEN
        IF Prover.tacticTrace THEN
          Wr.PutText(Stdio.stdout,
            "; Context.UnitConsequences: tactic: AssertLits()\n");
          Wr.Flush(Stdio.stdout)
        END;
        AssertLits()
      ELSIF opsEnabled[Ops.ScanClauses] AND maxEffort >= 1 THEN
        IF Prover.tacticTrace THEN
          Wr.PutText(Stdio.stdout,
            "; Context.UnitConsequences: tactic: ScanClauses()\n");
          Wr.Flush(Stdio.stdout)
        END;
        ScanClauses()
      ELSIF opsEnabled[Ops.UnitMatch] AND maxEffort >= 2 THEN
        IF Prover.tacticTrace THEN
          Wr.PutText(Stdio.stdout,
            "; Context.UnitConsequences: tactic: UnitMatch()\n");
          Wr.Flush(Stdio.stdout)
        END;
        UnitMatch()
      ELSIF opsEnabled[Ops.RestrictedNUMatch] AND maxEffort >= 3 AND
        fnurNum < Prover.maxFNURnum THEN
        IF Prover.tacticTrace THEN
          Wr.PutText(Stdio.stdout,
            "; Context.UnitConsequences: tactic: RestrictedNUMatch()\n");
          Wr.Flush(Stdio.stdout)
        END;
        RestrictedNUMatch();
        INC(fnurNum)
      ELSE
        EXIT
      END (* IF *)
    END (* WHILE *)
  END UnitConsequences;

VAR caseSplitDebug := Env.Get("PROVER_SPLIT_DEBUG") # NIL;

PROCEDURE GetSplit(): AF.Lit =
  VAR res: AF.Lit := NIL; BEGIN
    <*ASSERT SplitExists() *>
    IF Prover.litSplit AND litSplitEnabled AND
      clauses.succ # NIL AND clauses.succ.score < 1.0 AND SplitOnMaxLit() THEN
      litSplitEnabled := FALSE
    END (* IF *);
    IF promoteClauseEnabled THEN
      EVAL PromoteClause();
      promoteClauseEnabled := FALSE
    END (* IF *);
    IF clauses.succ = NIL THEN
      <*ASSERT nuMatches.succ # NIL AND Match.depth < Prover.maxMatchDepth *>
      IF Match.depth = 0 THEN
        stats.lastNUStart := Time.Now();
        stats.nuStartPushes := pushes
      END (* IF *);
          
      IF doNUClauses AND Match.depth = 1 THEN
        Enode.ComputeSxSizes();
        Clause.PrintList(Stdio.stdout, clauses.succ, normForm := TRUE)
      END (* IF *);
      UnrestrictedNUMatching();
      <*ASSERT sat*>
      INC(stats.unrestrictedNuMatches);
      INC(stats.unrestrictedNuMatchCLs, Clause.ListLength(clauses.succ));
      IF doPushLog THEN
        VAR msg := "M(" & Fmt.Int(Match.depth-1) & ").."; BEGIN
          IF NOT logIndented THEN
            IndentAndPut(pushes-1+Match.depth*2, msg);
            logIndented := TRUE
          ELSE
            Wr.PutText(Stdio.stdout, msg)
          END (* IF *)
        END (* BEGIN *)
      END (* IF *)
    END (* IF *);
    IF clauses.succ # NIL THEN
      IF DoDebug(1) THEN
        IndentAndPut(pushes, "Before case split.\n", 2)
      END (* IF *);
      (* See if the current clause has any non-proxy literals... *)
      MaxScoreClauseToFront();
      <*ASSERT (rightMostClauses > 0) =
                (clauses.succ.score = ProxyProp.RightMostScore)*>
      IF clauses.succ.score # ProxyProp.RightMostScore AND NOT inGoalSubproof THEN
        inGoalSubproof := TRUE;
        StartGoalSubproof()
      END;
      IF clauses.succ.promoted # ClausePrivate.PromoteState.Immed THEN
        immedPromotes := Prover.maxImmedPromote
      END (* IF *);
      VAR ll := clauses.succ.lits; litNum := 0; BEGIN
        IF clauses.succ.score < ProxyProp.RightMostScore THEN
          (* For non-"rightmost" clauses, favor splitting on
             simple disjuncts first. *)
          WHILE ll # NIL AND res = NIL DO
            VAR lit: AF.Lit := ll.head; BEGIN
              IF NOT ISTYPE(lit.af, ProxyProp.T)
                AND NOT ISTYPE(lit.af, MatchingRule.AtomF)
               THEN
                IF doNUClauses AND Match.depth = 1 THEN
                  VAR litSx := AF.LitToSx(lit, normForm := TRUE);
                      hash := PredSx.Hash(litSx);
                  BEGIN
                    Wr.PutText(
                        Stdio.stdout,
                        "\nAsserting literal, depth " & Fmt.Int(pushes) &
                        ": (#" & Fmt.Int(hash MOD 10000));
                    IF doNUClauseUIDs THEN
                      Wr.PutText(Stdio.stdout, "[");
                      IF NOT lit.sense THEN Wr.PutText(Stdio.stdout, "N")
                      END (* IF *);
                      Wr.PutText(Stdio.stdout, Fmt.Int(lit.af.id) & "]")
                    END (* IF *);
                    Wr.PutText(Stdio.stdout, " ");
                    Sx.Print(Stdio.stdout, litSx);
                    Wr.PutText(Stdio.stdout, ")\n");
                    Wr.Flush(Stdio.stdout)
                  END (* BEGIN *)
                END (* IF *);
                res := lit
              END (* IF *)
            END (* BEGIN *);
            ll := ll.tail; IF res = NIL THEN INC(litNum) END (* IF *)
          END (* WHILE *);
        END (* IF *);
        IF res = NIL THEN 
          res := clauses.succ.lits.head;
          litNum := 0
        END (* BEGIN *);
        IF caseSplitDebug AND LabelName.traceTriggers > 0
(*           AND clauses.succ.score # ProxyProp.RightMostScore *)
        THEN
          Wr.PutText(Stdio.stdout, "%%% Case split, from push depth " &
            Fmt.Int(pushes) & ", with " &
            Fmt.Int(Clause.ListLength(clauses)) &
            " clauses on clause list.\nChose literal #" &
            Fmt.Int(litNum) & " from clause:\n");
          ClausePrivate.DBGPrint(Stdio.stdout, clauses.succ)
        END (* IF *);

        INC(stats.nSplits);
        IF doPushLog THEN
          VAR msg: TEXT; BEGIN
            CASE clauses.succ.promoted OF
            | ClausePrivate.PromoteState.Not => msg := "P.."
            | ClausePrivate.PromoteState.Promoted => msg := "P++"
            | ClausePrivate.PromoteState.Immed => msg := "P@@"
            END (* TYPECASE *);
            IF NOT logIndented THEN
              IndentAndPut(pushes-1+Match.depth*2, msg);
              logIndented := TRUE
            ELSE
              Wr.PutText(Stdio.stdout, msg)
            END (* IF *)
          END (* BEGIN *)
        END (* IF *);
        IF heapDumpIntrvl # -1 AND
          stats.nSplits MOD heapDumpIntrvl = 0 THEN
          Wr.PutText(Stdio.stdout,
                     "\nHeap composition: nSplits = " &
                     Fmt.Int(stats.nSplits) & "; heap size is " &
                     BytesToStr(HeapBytes()) & "; top types are:\n");
          RTCollector.Disable(); RTCollector.Enable(); RTCollector.Collect();
          RTutils.Heap(suppressZeros := TRUE,
                       presentation := RTutils.HeapPresentation.ByByteCount,
                       window := 10);
          Wr.PutText(Stdio.stdout, "\n");
          (* RTHeapPf.Stats.ReportReachable() *)
        END (* IF *);
        IF enodeStatIntrvl # -1 AND
          stats.nSplits MOD enodeStatIntrvl = 0 THEN
          Enode.Stats()
        END (* IF *);
        IF Match.depth = 0 THEN INC(stats.splitsBeforeNU) END (* IF *);
        (* If the new head of the clause list was produced by a
           matching rule, lastSplit becomes it's parent. *)
        IF clauses.succ.mr # NIL AND
          clauses.succ.promoted # ClausePrivate.PromoteState.Not THEN
          clauses.succ.parent := lastSplit
        END (* IF *);
        lastSplit := clauses.succ;
        IF clauses.succ.mr # NIL THEN
          Match.AttributeCaseSplit(clauses.succ.mr.id,
                                   clauses.succ.mr.hash())
        END (* IF *);
        litSplitEnabled := TRUE; promoteClauseEnabled := TRUE
      END (* BEGIN *)
    END (* IF *);
    opsEnabled[Ops.SelStoreDist] := TRUE;
    <*ASSERT res # NIL *>
    RETURN res
  END GetSplit;


(* If there is a clause in "nuMatchSet" whose fingerprint is in
   "pf.promote", and the score of that clause is higher than the
   clause on "pf.l" with highest score (or "pf.l" is empty), then
   moves that clause from "nuMatchSet" to "pf.l" and returns
   "TRUE"; otherwise, returns "FALSE" and modifies nothing. *)
PROCEDURE PromoteClause(): BOOLEAN =
  VAR c := nuMatches.succ; score: REAL; BEGIN
    WHILE c # NIL DO
      IF promote.member(c, score) AND 
        (clauses.succ = NIL OR score > clauses.succ.score) THEN
        c.parent := lastSplit;
        c.promoted := ClausePrivate.PromoteState.Promoted;
        UndoStackPush(ContextUndoRec.Tag.PromoteClause, c);
        ClauseList.Delete(nuMatches, c);
        AddClause(c);
        c.score := score;
        INC(stats.clausePromotes);

        IF doPromoted THEN
          Wr.PutText(Stdio.stdout,
                     "\nAbout to push on promoted clause (score = " &
                     Fmt.Real(score) & "):\n");
          Sx.Print(Stdio.stdout, Clause.ToSx(c, TRUE));
          Wr.PutText(Stdio.stdout, "\nFingerprint is " &
            FPrint.ToText(c.fp));
          Wr.Flush(Stdio.stdout)
        END (* IF *);
        RETURN TRUE
      END (* IF *);
      c := c.succ
    END (* WHILE *);
    (* Otherwise *)
    RETURN FALSE
  END PromoteClause;

TYPE
  LitIntTable = RefIntTbl.Default OBJECT
   OVERRIDES
    keyHash := LitHash;
    keyEqual := LitEqual;
  END (* OBJECT *);

VAR litTable := NEW(LitIntTable);

PROCEDURE SplitOnMaxLit(): BOOLEAN =
  VAR ll := clauses.succ; max: AF.Lit := NIL; maxN := 0; BEGIN
    litTable := litTable.init();
    WHILE ll # NIL DO
      VAR lits := ll.lits; BEGIN
        WHILE lits # NIL DO
          VAR lit: AF.Lit := lits.head; BEGIN
            TYPECASE lit.af OF
            | PropVar.T, MatchingRule.AtomF =>
            ELSE
                VAR i: INTEGER; BEGIN
                  IF NOT litTable.get(lit, i) THEN i := 0 END (* IF *);
                  INC(i);
                  IF i > maxN THEN
                    max := lit; maxN := i
                  END (* IF *);
                  EVAL litTable.put(lit, i)
                END (* BEGIN *)
            END (* TYPECASE *)
          END (* BEGIN *);
          lits := lits.tail
        END (* WHILE *)
      END (* BEGIN *);
      ll := ll.succ
    END (* WHILE *);
    IF clauses.succ # NIL THEN
      VAR iter := litTable.iterate(); litRA: REFANY; n: INTEGER;
          sz := Clause.ListLength(clauses.succ);
          minPct := (Prover.litSplitPct * sz) DIV 100;
          min := MAX(minPct, Prover.litSplitMinClauses);
          max := 0; maxLit: AF.Lit := NIL;
      BEGIN
        WHILE iter.next(litRA, n) DO
          IF n >= min AND n > max THEN
            max := n; maxLit := litRA
          END (* IF *)
        END (* WHILE *);
        IF maxLit # NIL THEN
          Wr.PutText(Stdio.stdout, 
                     "Splitting on a literal that appears in " &
                     Fmt.Int(max) & " of " & Fmt.Int(sz) & " clauses.\n");
          Wr.Flush(Stdio.stdout);
          VAR l := AF.LitCopy(maxLit);
              nl := AF.Not(l);
              cnew := NEW(Clause.T, lits := RefList.List2(l, nl)).init();
          BEGIN
            cnew.score := 1.0;
            AddClause(cnew)
          END (* BEGIN *);
          RETURN TRUE
        END (* IF *)
      END (* BEGIN *)
    END (* IF *);
    (* Otherwise *)
    RETURN FALSE
  END SplitOnMaxLit;

PROCEDURE LitHash(<*UNUSED*> t: LitIntTable; READONLY litRA: REFANY): Word.T =
  VAR lit: AF.Lit := litRA;
      res := lit.af.hashInContext();
  BEGIN
    IF NOT lit.sense THEN res := -res END (* IF *);
    RETURN res
  END LitHash;

PROCEDURE LitEqual(<*UNUSED*> t: LitIntTable;
                   READONLY lit1RA, lit2RA: REFANY): BOOLEAN =
  VAR lit1: AF.Lit := lit1RA;
      lit2: AF.Lit := lit2RA;
  BEGIN
    RETURN lit1.sense = lit2.sense AND lit1.af.equalInContext(lit2.af)
  END LitEqual;

PROCEDURE DoDebug(level: INTEGER): BOOLEAN =
  BEGIN RETURN debug > level AND NOT inD1P
  END DoDebug;

PROCEDURE MaxScoreClauseToFront() =
  (* If "clauses.succ" is not "NIL", move a maximal-score clause to
     "clauses.succ". *)
  VAR maxClause: Clause.T := NIL;
      maxScore := 0.0;
      ll := clauses.succ;
  BEGIN
    WHILE ll # NIL DO
      IF ll.score > maxScore THEN
        maxClause := ll; maxScore := ll.score
      END (* IF *);
      ll := ll.succ
    END (* WHILE *);
    (* If maxClause found, move it to the head of "l". *)
    IF maxClause # NIL THEN
      (* Delete *)
      ClauseList.Delete(clauses, maxClause, writeUndo := FALSE);
      ClauseList.Add(clauses, maxClause, writeUndo := FALSE)
    END (* IF *);
  END MaxScoreClauseToFront;

PROCEDURE IndentAndPut(n: CARDINAL; s: TEXT; factor := 3) =
  BEGIN
    Wr.PutText(Stdio.stdout, "\n");
    FOR i := 1 TO n DO
      FOR k := 1 TO factor DO
        Wr.PutChar(Stdio.stdout, ' ')
      END (* FOR *)
    END (* FOR *);
    Wr.PutText(Stdio.stdout, s); Wr.Flush(Stdio.stdout)
  END IndentAndPut;

PROCEDURE GetNSplits(): INTEGER =
  BEGIN RETURN stats.nSplits
  END GetNSplits;

PROCEDURE NormScore(score: REAL): REAL =
  BEGIN 
    IF score < ProxyProp.RightMostScore THEN
      RETURN 1.0 - 1.0/(score + 1.0)
    ELSE
      RETURN score
    END (* IF *)
  END NormScore;

(* Bytes in the heap. *)
PROCEDURE HeapBytes(): INTEGER =
  VAR pages := RTHeapRep.p1 - RTHeapRep.p0; BEGIN
    RETURN pages * RTHeapRep.BytesPerPage
  END HeapBytes;

PROCEDURE BytesToStr(bytes: INTEGER): TEXT =
  VAR megs := FLOAT(bytes)/FLOAT(1024*1024); BEGIN
    RETURN Fmt.Real(megs, prec := 4) & " MBytes"
  END BytesToStr;

BEGIN
END Context.
