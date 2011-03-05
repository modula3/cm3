(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1992 DEC *)
(* Last modified on Tue Jun  4 12:13:37 PDT 2002 by saxe    *)
(*      modified on Wed Oct 30 14:13:50 PST 1996 by detlefs *)

MODULE Satisfy;

IMPORT Context, AF, Prover, ProxyProp, Match, LabelName, Trit;
IMPORT Env, Wr, Fmt, Stdio, Time;
IMPORT RefSeq, Text;
IMPORT MatchingRule;

IMPORT Orders; (* for tracing *)

IMPORT Thread;
<*FATAL Wr.Failure, Thread.Alerted*>

VAR debug := 0; pushDebug: BOOLEAN; <*NOWARN*>
    doStats, doPushLog, doPromoted, doPlungeLog, logIndented: BOOLEAN;
    doNUClauses, doRandomClauseOrder: BOOLEAN;
    ccCount: INTEGER;
    iters: INTEGER;  (* number of current iteration of main loop *)

TYPE Timer = Thread.Closure OBJECT
               proverThread: Thread.T;
               pauseTime: Time.T;
               mu: MUTEX;
             OVERRIDES
               apply := TimerApply;
             END;

PROCEDURE TimerApply(t: Timer): REFANY =
  BEGIN
   Thread.Pause(t.pauseTime);
   LOCK t.mu DO
     IF t.proverThread # NIL THEN
       Thread.Alert(t.proverThread);
     END
   END;
   RETURN NIL;
  END TimerApply;

PROCEDURE Search(
            lit: AF.Lit;
            cl: Prover.ResClosure;
            ccLimit: INTEGER;
            pruning: BOOLEAN := FALSE): Trit.T
    RAISES { Prover.Error } =
  VAR
    outcome := Trit.True;
        (* Invariant:
             "outcome" = "Trit.False" if any satisfying context has
                            been found so far,
                         "Trit.Unknown" if no satisfying context has
                            been found so far, but part of the search
                            has been skipped due to the "subKillTime"
                            or "subKillIter" limit being reached.
                         "Trit.True" otherwise. *)
    start: Time.T; (* The (approximate) time at the start of the main loop *)
    labelSubproofStartIter: INTEGER := -1;
        (* The iteration number when the prover last entered a
           label subproof (as determined by "Context.InLabelSubproof()").
           Equal to -1 when the prover is not in a label subproof. *)
    labelSubproofStartTime: Time.T;
        (* The (approximate) time when the prover last entered a
           label Subproof (as determined by "Context.InLabelSubproof()").
           Valid when the prover is in a label subproof. *)
    now: Time.T; (* temporary for comparison with various start times *)
    fnurNum := 0;
        (* Number of rounds of restricted non-unit matching performed
           since last case split on the current path.  If this exceeds
           Prover.maxFNURnum, the proof will be aborted with a suspected
           matching loop. *)
    toBeNegated := NEW(RefSeq.T).init();
        (* Stack of literals recording the "true" case splits on the
           current path.  During backtracking, a literal may be popped
           from this stack and its negation may then be used as part
           of the exploration of the "false" side of the case split. *)
    fruitlessSplitBase: INTEGER;
        (* Starting value of "tobeNegated.size()" for applying the "fruitless
           split" heuristic.  Whenever the prover reaches a contradiction,
           it will be the case that there are only "true", non-"rightmost"
           case splits beyond "fruitlessSplitBase" on the current
           path.  Normally, "fruitlessSplitBase" is kept as low as possible,
           consistent with the invariant above.  If the difference between
           "fruitlessSplitBase" and "toBeNegated.size()" exceeds
           "Prover.fruitlessSplitLimit" at the time a contradiction is found,
           the prover backtracks to "fruitlessSplitBase" (retaining
           scoring information, of course), increments "fruitlessSplitBase"
           (to guard against infinite looping), and retries its exploration
           of the search space using the new scoring information. *)
    killTime: Time.T := Prover.killTime;
    killIter: INTEGER := Prover.killIter;
    subKillTime: Time.T := Prover.subKillTime;
    subKillIter: INTEGER := Prover.subKillIter;
    useTightenBounds: BOOLEAN;
    showCC: BOOLEAN;
    killContext: BOOLEAN;
    done: BOOLEAN;
    unitMatchCountdown := Prover.unitMatchLimit;
    nuMatchCountdown := Prover.nuMatchLimit;
    tc := NEW(Timer,
              proverThread := Thread.Self(),
              pauseTime := FLOAT(killTime, LONGREAL),
              mu := NEW(MUTEX));

    proofTimedOut := FALSE;
  BEGIN
    IF killTime <= FLOAT(999999999, LONGREAL) THEN
      EVAL Thread.Fork(tc);
    END;
    IF pruning THEN
      useTightenBounds := FALSE;
      Prover.maxMatchDepth := Prover.maxPruneMatchDepth;
      showCC := FALSE; 
      killContext := FALSE;
    ELSE
      useTightenBounds := Prover.useTB;
      Prover.maxMatchDepth := Prover.defMaxMatchDepth;
      showCC := Prover.showCC;
      killContext := Prover.killContext;
    END;
    iters := 0; 
    Context.fruitlessSplitLimit := Prover.fruitlessSplitLimit;

    (* RTAllocator.TraceAllocSites(TYPECODE(RefList.T)) *)

    logIndented := FALSE;
    Context.ResetStats();
    Context.Push();
    TRY
      Context.inGoalSubproof := FALSE;
      Context.Assert(lit);
      LabelName.StartProof();
      Match.ResetRuleScores();
      ccCount := 0;
      fruitlessSplitBase := toBeNegated.size();
      Context.tightenBoundsUseful := FALSE;
      start := Time.Now();
      labelSubproofStartTime := start;
       
      done := FALSE;
      Context.quiescenceDepth := Prover.initialQuiescenceDepth;

      (* Q1 AND Q2 *)
      WHILE NOT done DO
        now := Time.Now();
        IF labelSubproofStartIter = -1 AND Context.InLabelSubproof() THEN
          labelSubproofStartIter := iters;
          labelSubproofStartTime := now
        END;
        INC(iters);
        IF Prover.orderTrack THEN
          Orders.PrintAll();
        END;
        IF iters > killIter OR proofTimedOut OR
           ccCount = ccLimit THEN
          IF Prover.tacticTrace THEN MTrace("About to quit") END;
          VAR reason: Text.T;
              kind: Prover.ResKind; BEGIN
            IF ccCount = ccLimit THEN
              reason := "reached CCLimit";
              kind := Prover.ResKind.CCLimit
            ELSIF iters > killIter THEN
              reason := "exceeded killIter";
              kind := Prover.ResKind.XKillIter
            ELSE
              reason := "exceeded killTime";
              kind := Prover.ResKind.XKillTime
            END;
            IF Prover.tacticTrace THEN TTrace("quit (" & reason & ")") END;
            IF cl # NIL AND NOT(ccCount = ccLimit AND ccLimit = 1) THEN
              cl.apply(
                Context.Top(kind := kind, labelsOnly := NOT killContext))
            END;
            IF outcome = Trit.True THEN outcome := Trit.Unknown END;
            done := TRUE;
          END (* BEGIN *)
        ELSIF Context.InLabelSubproof() AND
              ( iters - labelSubproofStartIter > subKillIter OR
                now - labelSubproofStartTime > subKillTime ) THEN
          IF Prover.tacticTrace THEN MTrace("About to abandon subproof") END;
          VAR reason: Text.T;
              kind: Prover.ResKind;
              popLit: AF.Lit := NIL; BEGIN
            IF iters - labelSubproofStartIter > subKillIter THEN
              reason := "exceeded subKillIter";
              kind := Prover.ResKind.XSubKillIter
            ELSE
              reason := "exceeded subKillTime";
              kind := Prover.ResKind.XSubKillTime
            END;
            IF Prover.tacticTrace THEN
              TTrace("abandon subproof (" & reason & ")")
            END;
            IF cl # NIL THEN
              cl.apply(Context.Top(kind := kind, labelsOnly := NOT killContext))
            END;
            IF outcome = Trit.True THEN outcome := Trit.Unknown END;
            WHILE Context.InLabelSubproof() AND toBeNegated.size() > 0 DO
              Context.Pop(incrementScores := (* FALSE *) TRUE);
              popLit := toBeNegated.remhi();
              IF Prover.tacticTrace THEN
                PopTrace("backtracking abandoned subproof")
              END;
            END;
            IF Context.InLabelSubproof() THEN
              done := TRUE;
            ELSE 
              labelSubproofStartIter := -1;
              SubsumptionDeny(popLit);
              fnurNum := 0;
              fruitlessSplitBase := toBeNegated.size()
            END (* IF *)
          END (* BEGIN *)
        ELSE
          TRY
            IF Thread.TestAlert() THEN RAISE Prover.Timeout END;
            IF NOT Context.sat THEN
              IF Prover.tacticTrace THEN MTrace("About to backtrack") END;
              unitMatchCountdown := Prover.unitMatchLimit;
              nuMatchCountdown := Prover.nuMatchLimit;
              IF toBeNegated.size() = 0 THEN           
                IF Prover.tacticTrace THEN
                   TTrace("quit (contradiction, nothing left to backtrack)")
                END;
                done := TRUE;
              ELSIF toBeNegated.size() >
                    fruitlessSplitBase + Context.fruitlessSplitLimit THEN
                VAR leafMatchDepth := Match.depth; BEGIN
                  IF Prover.tacticTrace THEN
                   TTrace(
                     "backtrack from contradiction (past fruitless split limit)")
                  END;
                  (* Bump the score of the effective split (and its ancestors) *)
                  Context.Pop(incrementScores := TRUE);
                  EVAL toBeNegated.remhi();
                  IF Prover.tacticTrace THEN
                    PopTrace("fruitless split limit leaf backtrack")
                  END;
                  (* Pop back to depth "fruitlesssSplitBase" without giving any
                     more credit to intermediate splits. *)
                  WHILE toBeNegated.size() > fruitlessSplitBase AND
                        Match.depth = leafMatchDepth DO
                    Context.Pop(incrementScores := (* FALSE *) TRUE);
                    EVAL toBeNegated.remhi();
                    IF Prover.tacticTrace THEN
                      PopTrace("fruitless split backtrack")
                    END;
                  END;
                  IF NOT Context.InLabelSubproof() THEN
                    labelSubproofStartIter := -1;
                  END;
                  fnurNum := 0;
                  IF fruitlessSplitBase < toBeNegated.size() THEN
                    fruitlessSplitBase := toBeNegated.size()
                  ELSE
                    INC(fruitlessSplitBase);
                    Context.fruitlessSplitLimit :=
                      Context.fruitlessSplitLimit + Prover.fruitlessSplitLimit
                  END (* IF *)
                END (* BEGIN *)
              ELSE
                IF Prover.tacticTrace THEN
                     TTrace("backtrack from contradiction")
                END;
                Context.Pop();
                IF NOT Context.InLabelSubproof() THEN
                  labelSubproofStartIter := -1
                END;
                fnurNum := 0;
                SubsumptionDeny(toBeNegated.remhi());
                fruitlessSplitBase := toBeNegated.size();
              END (* IF *);
            ELSIF Context.opsEnabled[Context.Ops.AssertLits] THEN
              IF Prover.tacticTrace THEN TTrace("AssertLits()") END;
              Context.AssertLits();
            ELSIF Context.opsEnabled[Context.Ops.ScanClauses] THEN
              IF Prover.tacticTrace THEN TTrace("ScanClauses()") END;
              Context.ScanClauses();
            ELSIF Context.opsEnabled[Context.Ops.UnitMatch]
                  AND unitMatchCountdown > 0 THEN
              IF Prover.tacticTrace THEN TTrace("UnitMatch()") END;
              DEC(unitMatchCountdown);
              Context.UnitMatch();
            ELSIF Context.opsEnabled[Context.Ops.RestrictedNUMatch] 
                  AND nuMatchCountdown > 0 THEN
              IF Prover.tacticTrace THEN
                MTrace("About to consider restricted non-unit matching")
              END;
              IF fnurNum >= Prover.maxFNURnum THEN
                <*ASSERT fnurNum = Prover.maxFNURnum *>
                IF Prover.tacticTrace THEN
                  TTrace("give up (probable matching loop)")
                END;
                Context.FinishStats();
                RAISE Prover.Error(
                          "Probable matching loop; " &
                          "increase PROVER_MAX_FNUR if you believe otherwise.")
              ELSE
                <*ASSERT Match.depth <= Prover.maxMatchDepth *>
                IF Prover.tacticTrace THEN TTrace("RestrictedNUMatch()") END;
                DEC(nuMatchCountdown);
                Context.RestrictedNUMatch();
                INC(fnurNum)
              END (* IF *)
            ELSIF NOT Prover.noSelStore AND
                Context.opsEnabled[Context.Ops.SelStoreDist] THEN
              IF Prover.tacticTrace THEN TTrace("SelStoreDist()") END;
              Context.SelStoreDist();
            ELSIF Context.opsEnabled[Context.Ops.TightenBounds] AND
                  Context.tightenBoundsUseful AND
                  NOT Prover.noPromoteTB THEN
              IF Prover.tacticTrace THEN TTrace("TightenBounds() (promoted)") END;
              unitMatchCountdown := Prover.unitMatchLimit;
              nuMatchCountdown := Prover.nuMatchLimit;
              Context.TightenBounds();
              Context.tightenBoundsUseful := FALSE
            ELSIF Context.opsEnabled[Context.Ops.GetSplit] THEN
              IF Prover.tacticTrace THEN TTrace("case split") END;
              unitMatchCountdown := Prover.unitMatchLimit;
              nuMatchCountdown := Prover.nuMatchLimit;
              VAR splitLit := Context.GetSplit(); BEGIN
                Context.Push();
                toBeNegated.addhi(splitLit);
                IF NOT Context.InGoalSubproof() THEN
                  fruitlessSplitBase := toBeNegated.size()
                END;
                Context.Assert(splitLit);
                fnurNum := 0
              END (* BEGIN *)
            ELSIF useTightenBounds AND
              Context.opsEnabled[Context.Ops.TightenBounds] THEN
              (* Although "Context.TightenBounds" may not have turned out to
                 produce an immediate contradiction anywhere in the search
                 tree below the last time it was tried on the current path,
                 it might produce one this time, and we don't have anything
                 better to try. *)
              IF Prover.tacticTrace THEN
                TTrace("TightenBounds() (leaf)")
              END;
              unitMatchCountdown := Prover.unitMatchLimit;
              nuMatchCountdown := Prover.nuMatchLimit;
              Context.TightenBounds()
            ELSIF Context.quiescenceDepth > 1 THEN
              IF Prover.tacticTrace THEN
                TTrace("deepen quiescence")
              END;
              Context.quiescenceDepth := Context.quiescenceDepth - 1;
              Context.opsEnabled[Context.Ops.AssertLits] := TRUE;
              Context.opsEnabled[Context.Ops.ScanClauses] := TRUE;
              Context.opsEnabled[Context.Ops.UnitMatch] := TRUE;
              Context.opsEnabled[Context.Ops.SelStoreDist] := TRUE;
              Context.opsEnabled[Context.Ops.RestrictedNUMatch] := TRUE;
              Context.opsEnabled[Context.Ops.GetSplit] := TRUE;
              Context.opsEnabled[Context.Ops.TightenBounds] := TRUE;
              unitMatchCountdown := Prover.unitMatchLimit;
              nuMatchCountdown := Prover.nuMatchLimit;
            ELSE
              IF Prover.tacticTrace THEN TTrace("report counterexample") END;
              IF cl # NIL THEN
                cl.apply(Context.Top(kind := Prover.ResKind.Counterexample,
                                     labelsOnly := NOT showCC))
              END; (* IF *)
              outcome := Trit.False;
              ccCount := ccCount + 1;
              IF ccCount < ccLimit THEN
                VAR popLit: AF.Lit := NIL; BEGIN
                  WHILE toBeNegated.size() > 0 AND Context.InLabelSubproof() DO
                    Context.Pop(incrementScores := (* FALSE *) TRUE);
                    popLit := toBeNegated.remhi();
                    IF Prover.tacticTrace THEN
                      PopTrace("backtracking from counterexample");
                    END; (* IF *)
                  END; (* WHILE *)
                  IF popLit  = NIL OR Context.InLabelSubproof() THEN
                    done := TRUE;
                  ELSE
                    labelSubproofStartIter := -1;         
                    SubsumptionDeny(popLit);
                    fnurNum := 0;
                    fruitlessSplitBase := toBeNegated.size()
                  END (* IF *)
                END; (* BEGIN *)
              END (* IF *)
            END (* IF *)
          EXCEPT
            Prover.Timeout => proofTimedOut := TRUE;
          END; (* TRY *)
        END; (* IF *)
      END; (* WHILE *)
      IF Prover.tacticTrace THEN OTrace(outcome) END;
      Context.FinishStats();
      RETURN outcome
    FINALLY
      LOCK tc.mu DO
        tc.proverThread := NIL
      END;
      (* Undo any pushes done in the loop. *)
      FOR i := 0 TO toBeNegated.size()-1 DO 
        Context.Pop(incrementScores := (* FALSE *) TRUE);
        IF Prover.tacticTrace THEN PopTrace("restoring context") END
      END (* FOR *);
      (* Match the initial push. *)
      Context.Pop(incrementScores := (* FALSE *) TRUE);
      IF Prover.tacticTrace THEN
        PopTrace("restoring context--matching initial push")
      END;
      (* Print out the statistics if desired. *)
      IF doStats THEN
        Context.Stats();
        Wr.PutText(Stdio.stdout, "\nDid " & Fmt.Int(iters) &
          " iterations of the Satisfy.Search loop.\n")
      END (* IF *)
    END (* TRY *)
  END Search;

PROCEDURE GetIters(): INTEGER =
  BEGIN
    RETURN iters
  END GetIters;

PROCEDURE TTrace(message: Text.T) =
  BEGIN
    Trace("(iteration " & Fmt.Int(iters) & 
      ", depth " & Fmt.Int(Context.pushes) & " " & GoalText() &
      ") tactic:  " & message);
  END TTrace;

PROCEDURE OTrace(outcome: Trit.T) =
  BEGIN
    TTrace("return " & Trit.ToText(outcome));
  END OTrace;

PROCEDURE PopTrace(reason: Text.T) =
  BEGIN
    Trace(" (" & reason & "):  popped to depth "
      & Fmt.Int(Context.pushes) & " " & GoalText());
  END PopTrace;

PROCEDURE MTrace(message: Text.T) =
  BEGIN
    Trace(":  " & message)
  END MTrace;

PROCEDURE Trace(message: Text.T) =
  BEGIN
    Wr.PutText(Stdio.stdout,
      "; Satisfy.Search" & message  & "\n");
    Wr.Flush(Stdio.stdout)
  END Trace;

PROCEDURE SubsumptionDeny(lit: AF.Lit) =
  BEGIN
    CASE Prover.useSubsumption OF
    | Prover.SubsumptionChoice.None =>
        lit.af.set(NOT lit.sense, FALSE)
    | Prover.SubsumptionChoice.All =>
        IF ISTYPE(lit.af, MatchingRule.AtomF) THEN
          lit.af.set(NOT lit.sense, FALSE)
        ELSE
          VAR nlit := AF.Not(lit); BEGIN
            nlit.rightMost := FALSE;
            Context.Assert(nlit)
          END (* BEGIN *)
        END (* IF *)
    | Prover.SubsumptionChoice.NonProxy =>
        IF NOT (ISTYPE(lit.af, ProxyProp.T) OR 
                ISTYPE(lit.af, MatchingRule.AtomF)) THEN
          VAR nlit := AF.Not(lit); BEGIN
            nlit.rightMost := FALSE;
            Context.Assert(nlit)
          END (* BEGIN *)
        ELSE
          lit.af.set(NOT lit.sense, FALSE)
        END (* IF *)
    | Prover.SubsumptionChoice.Conjunctive =>
        AssertConjuncts(AF.Not(lit))
    END (* CASE *)
  END SubsumptionDeny;

PROCEDURE AssertConjuncts(lit: AF.Lit) =
  BEGIN
    TYPECASE lit.af OF
    | ProxyProp.T(prox) =>
        IF lit.sense THEN
          VAR lits := prox.lits; BEGIN
            WHILE lits # NIL DO
              AssertConjuncts(lits.head); lits := lits.tail
            END (* WHILE *)
          END (* BEGIN *)
        END (* IF *);
        lit.af.set(lit.sense, FALSE)
    | MatchingRule.AtomF =>
        lit.af.set(lit.sense, FALSE)
    ELSE
        Context.Assert(lit)
    END (* TYPECASE *)
  END AssertConjuncts;

PROCEDURE GoalText():Text.T =
  BEGIN
    IF Context.InGoalSubproof() THEN
      IF LabelName.anyAtSignLabels THEN
        RETURN "[GSP,@]"
      ELSE
        RETURN "[GSP]"
      END
    ELSE
      IF LabelName.anyAtSignLabels THEN
        RETURN "[@]"
      ELSE
        RETURN "[]"
      END
    END (* IF *)
  END GoalText;

BEGIN
  doStats := Env.Get("PROVER_STATS") # NIL;
  doPushLog := Env.Get("PROVER_PUSH_LOG") # NIL;
  doPromoted := Env.Get("PROVER_LOG_PROMOTES") # NIL;
  doPlungeLog := Env.Get("PROVER_PLUNGE_LOG") # NIL;
  doNUClauses := Env.Get("PROVER_PRINT_NU_CLAUSES") # NIL;
  doRandomClauseOrder := Env.Get("PROVER_RANDOM_CLAUSE_ORDER") # NIL;
  IF doRandomClauseOrder THEN
    Prover.envVars := Prover.envVars & "PROVER_RANDOM_CLAUSE_ORDER\n"
  END (* IF *);
  
END Satisfy.
