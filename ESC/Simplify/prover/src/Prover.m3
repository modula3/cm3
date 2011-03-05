(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed May 15 23:53:14 PDT 2002 by saxe                     *)
(*      modified on Fri Oct 23 14:02:44 PDT 1998 by gnelson                  *)
(*      modified on Fri Nov  1 16:07:45 PST 1996 by detlefs                  *)

UNSAFE MODULE Prover;

(* Unsafe only because of importation of RTHeapRep. *)

IMPORT PredSx, Context, Clause, Orders, Satisfy, PredDefs, AF;
IMPORT Sx, Rd, Atom, Env, Fmt, Scan, Text, Wr, Trit;
IMPORT RefList, RefSeq, AtomSet, AtomIntTbl, AtomSetList, AtomList;
IMPORT RTHeapRep, Time;
(* IMPORT RTHeapDebug, RTutils, RTCollector, RTHeapRep; *)

(* For debugging *)
IMPORT Stdio, Thread, SxPrint;
<*FATAL Wr.Failure, Thread.Alerted, Sx.PrintError*>

VAR arity: AtomIntTbl.T;
    bg: RefSeq.T (* OF RefList.T *);
    ops: AtomList.T;
(* "arity" maps atoms to their arities; term variables have arity 0,
   and propositional variables arity -1.  This table is used to
   enforce the constraint that symbols are used with consistent arities.
   "bg" is a stack of "RefList.T"'s, each of which is the list of atoms to
   remove from "arity" to undo the "push" corresponding to the stack element.
   "ops" is the complete list of atoms that have been added to "arity"
   since the last push.
*)

VAR inProof := FALSE;

PROCEDURE Init() RAISES { Error } =
  BEGIN
    PredSx.Init();
    Context.Init();
    Orders.Init0();
    bg := NEW(RefSeq.T).init();
    arity := NEW(AtomIntTbl.Default).init();
    ops := NIL;
    InvalidRes := NEW(ProveRes,
                      kind := ResKind.Counterexample);
    VAR subChoice := Env.Get("PROVER_SUBSUMPTION"); BEGIN
      IF subChoice # NIL THEN
        IF Text.Equal(subChoice, "None") THEN
          useSubsumption := SubsumptionChoice.None
        ELSIF Text.Equal(subChoice, "All") THEN
          useSubsumption := SubsumptionChoice.All
        ELSIF Text.Equal(subChoice, "NonProxy") THEN
          useSubsumption := SubsumptionChoice.NonProxy
        ELSIF Text.Equal(subChoice, "Conjunctive") THEN
          useSubsumption := SubsumptionChoice.Conjunctive
        ELSE
          <*ASSERT FALSE*>
        END (* IF *);
        envVars := envVars & "PROVER_SUBSUMPTION=" & subChoice & "\n"
      END
    END (* BEGIN *);

    UpdateInt(defMaxMatchDepth, "PROVER_MATCH_DEPTH", 0);

    UpdateInt(maxFNURnum, "PROVER_MAX_FNUR",1);

    UpdateInt(maxMatchFNUR, "PROVER_MAX_MATCH_FNUR", 1);
    UpdateInt(minDistClassSize, "PROVER_MIN_DIST", 1);
    SetBool(distClassNeqNodes, "PROVER_DIST_NEQ");
    UpdateInt(maxPruneMatchDepth, "PROVER_PRUNE_MATCH_DEPTH", 0);
    UpdateInt(maxTrueCNF, "PROVER_MAX_TRUE_CNF", 1);
    UpdateInt(maxProxyPropCNF, "PROVER_MAX_PROXY_PROP_CNF", 1);

    VAR shuffleChoice := Env.Get("PROVER_CLAUSE_SHUFFLE"); BEGIN
      IF shuffleChoice # NIL THEN
        IF Text.Equal(shuffleChoice, "None") THEN
          clauseShuffle := ClauseShuffle.None
        ELSIF Text.Equal(shuffleChoice, "Reverse") THEN
          clauseShuffle := ClauseShuffle.Reverse
        ELSIF Text.Equal(shuffleChoice, "Rand") THEN
          clauseShuffle := ClauseShuffle.Rand
        ELSE
          <*ASSERT FALSE*>
        END (* IF *);
        envVars := envVars & "PROVER_CLAUSE_SHUFFLE=" & shuffleChoice & "\n"
      END
    END (* BEGIN *);

    SetBool(nuMatchInstance, "PROVER_NU_MATCH_INST");
    SetBool(unitMatchInstance, "PROVER_U_MATCH_INST");
    UpdateInt(unitMatchLimit, "PROVER_U_MATCH_LIMIT", 1);
    UpdateInt(nuMatchLimit, "PROVER_U_MATCH_LIMIT", 1);

    SetBoolNeg(lazySimplexIntern, "PROVER_NO_LAZY_SIMPLEX_INTERN");
    SetBool(allowNUMatchCycle,"PROVER_ALLOW_NU_CYCLE");
    SetBoolNeg(useImmediatePromote, "PROVER_NO_USE_IMMED_PROMOTE");

    UpdateInt(d1PMaxEffort, "PROVER_D1P_MAX_EFFORT", 0, 3);

    SetBool(litSplit, "PROVER_LIT_SPLIT");
    UpdateInt(litSplitPct, "PROVER_LIT_SPLIT_PCT", 10, 100);
    UpdateInt(litSplitMinClauses, "PROVER_LIT_SPLIT_MIN_CLAUSES", 1);

    UpdateInt(ccLimitGlobal, "PROVER_CC_LIMIT", 1);
    UpdateInt(promoteSize, "PROVER_PROMOTE_SIZE", 0);

    SetBool(allowSinglePatVarPat, "PROVER_ALLOW_1VAR");

    UpdateInt(maxImmedPromote, "PROVER_MAX_IMMED_PROMOTE", 0);

    SetBool(noEnodeStatus, "PROVER_NO_ENODE_STATUS");
    SetBool(intStatus, "PROVER_INT_STATUS");
    SetBool(deepStatus, "PROVER_DEEP_STATUS");
    SetBool(noPlunge, "PROVER_NO_PLUNGE");
    SetBool(usePlungeHints, "PROVER_USE_PLUNGE_HINTS");
    SetBool(noSelStore, "PROVER_NO_SEL_STORE");
    SetBool(noActivate, "PROVER_NO_ACTIVATE");
    UpdateInt(forceActivateStart, "PROVER_FORCE_ACTIVATE_START", 0);
    UpdateInt(forceActivateEnd, "PROVER_FORCE_ACTIVATE_END", 0);
    SetBool(eqOptActivate, "PROVER_EQ_OPT_ACTIVATE");
    SetBool(noLabels, "PROVER_NO_LABELS");
    SetBool(noModTimes, "PROVER_NO_MODTIMES");
    SetBool(noPatElems, "PROVER_NO_PATELEMS");
    SetBool(noObvSimpRedund, "PROVER_NO_OBV_SIMP_REDUND");
    SetBool(noPromoteTB, "PROVER_NO_PROMOTE_TB");
    SetBool(noMinHt, "PROVER_NO_MINHT");
    SetBool(tacticTrace, "PROVER_TACTIC_TRACE");
    SetBool(propagateTrace, "PROVER_PROPAGATE_TRACE");
    SetBool(orderTrack, "PROVER_ORDER_TRACK");

    SetBool(internDebug, "PROVER_INTERN_DEBUG");
    UpdateInt(fruitlessSplitLimit, "PROVER_FRUITLESS_SPLIT_LIMIT", 1);

    UpdateInt(killIter, "PROVER_KILL_ITER", 1);
    UpdateTime(killTime, "PROVER_KILL_TIME", 0, 1000000000);
    UpdateInt(subKillIter, "PROVER_SUB_KILL_ITER", 1);
    UpdateTime(subKillTime, "PROVER_SUB_KILL_TIME", 0, 1000000000);
    SetBoolNeg(useTB, "PROVER_NO_USE_TB");
    SetBool(killContext, "PROVER_KILL_CONTEXT");
    UpdateInt(triggerlessWarnLimit, "PROVER_TRIGGERLESS_WARN_LIMIT", 0);

    SetBool(harvestTrace, "PROVER_HARVEST_TRACE");

    SetBool(oldOutput, "PROVER_OLD_OUTPUT");

    UpdateInt(skolemizeOuterTrace, "PROVER_SKOLEMIZE_TRACE", 0);
    UpdateInt(initialQuiescenceDepth, "PROVER_QUIESCENCE_DEPTH", 1);

    SetBool(noMeritScoring, "PROVER_NO_MERIT_SCORING");
    SetBool(pruneDebug, "PROVER_PRUNE_DEBUG");

    RTHeapRep.RegisterMonitor(NEW(EtpGCMonitorClosure));
    
  END Init;

PROCEDURE SetBool(VAR v: BOOLEAN; name: Text.T) =
  BEGIN
    v := Env.Get(name) # NIL;
    IF v THEN 
      envVars := envVars & name & "\n"
    END (* IF *)
  END SetBool;

PROCEDURE SetBoolNeg(VAR v: BOOLEAN; name: Text.T) =
  BEGIN
    v := Env.Get(name) = NIL;
    IF NOT(v) THEN 
      envVars := envVars & name & "\n"
    END (* IF *)
  END SetBoolNeg;

PROCEDURE UpdateInt(VAR v: INTEGER; name: Text.T;
          min := FIRST(INTEGER); max := LAST(INTEGER)) RAISES { Error } =
  VAR str := Env.Get(name); BEGIN
    IF str # NIL THEN
      envVars := envVars & name & "=" & str & "\n";
      TRY
        v := Scan.Int(str);
      EXCEPT ELSE
        BadInt(name, min, max)
      END;
      IF v < min OR v > max THEN BadInt(name, min, max) END
    END
  END UpdateInt;

PROCEDURE UpdateTime(VAR v: Time.T; name: Text.T;
          min := FIRST(INTEGER); max := LAST(INTEGER))  RAISES { Error } =
  VAR str := Env.Get(name);
      i: INTEGER; r: REAL; BEGIN
    IF str # NIL THEN
      envVars := envVars & name & "=" & str & "\n";
      TRY
        i := Scan.Int(str);
        IF i < min OR i > max THEN BadNumber(name, min, max) END;
        v := FLOAT(i, Time.T);
      EXCEPT ELSE
        TRY
          r := Scan.Real(str);
          IF r < FLOAT(min) OR r > FLOAT(max) THEN BadNumber(name, min, max) END;
          v := FLOAT(r, Time.T);
        EXCEPT ELSE
          BadNumber(name, min, max)
        END
      END
    END (* IF *)
  END UpdateTime;

PROCEDURE BadInt(name: Text.T; min, max: INTEGER) RAISES { Error } =
  BEGIN
    Wr.PutText(Stdio.stdout,
      "Bad value for environment value " & name & "\n" &
      "Expecting an integer");
    IF min > FIRST(INTEGER) THEN
      Wr.PutText(Stdio.stdout, " at least " & Fmt.Int(min));
      IF max < LAST(INTEGER) THEN
        Wr.PutText(Stdio.stdout, " and")
      END
    END;
    IF max < LAST(INTEGER) THEN
      Wr.PutText(Stdio.stdout, " at most " & Fmt.Int(max))
    END;
    Wr.PutText(Stdio.stdout, ".\n");
    Wr.Flush(Stdio.stdout);
    RAISE Error("Bad environment variable.")
  END BadInt;

PROCEDURE BadNumber(name: Text.T; min, max: INTEGER) RAISES { Error } =
  BEGIN
    Wr.PutText(Stdio.stdout,
      "Bad value for environment value " & name & "\n" &
      "Expecting an integer or real");
    IF min > FIRST(INTEGER) THEN
      Wr.PutText(Stdio.stdout, " at least " & Fmt.Int(min));
      IF max < LAST(INTEGER) THEN
        Wr.PutText(Stdio.stdout, " and")
      END
    END;
    IF max < LAST(INTEGER) THEN
      Wr.PutText(Stdio.stdout, " at most " & Fmt.Int(max))
    END;
    Wr.PutText(Stdio.stdout, ".\n");
    Wr.Flush(Stdio.stdout);
    RAISE Error("Bad environment variable.")
  END BadNumber;




TYPE
  EtpGCMonitorClosure = RTHeapRep.MonitorClosure BRANDED OBJECT
   OVERRIDES
    before := EtpGCBefore;
    after := EtpGCAfter;
  END (* OBJECT *);
PROCEDURE EtpGCBefore(<*UNUSED*> self: EtpGCMonitorClosure) =
  BEGIN END EtpGCBefore;
PROCEDURE EtpGCAfter(<*UNUSED*> self: EtpGCMonitorClosure) =
  BEGIN END EtpGCAfter;
      
VAR debug := FALSE;

PROCEDURE Push() =
  BEGIN
    Context.Push();
    bg.addhi(ops);
    ops := NIL;
  END Push;

PROCEDURE Assert(f: PredSx.T; shred: BOOLEAN) RAISES { Error } =
  VAR iter : AtomSet.Iterator; v: Atom.T; i: INTEGER; BEGIN
    IF NOT (inProof OR inPruning) THEN
      triggerlessWarnCountdown := triggerlessWarnLimit
    END;
    CheckPredSyntax(f);
    iter := PredSx.FreeVars(f).iterate();
    WHILE iter.next(v) DO
      IF NOT Context.rs.get(v, i) THEN EVAL Context.rs.inc(v) END (* IF *)
    END (* WHILE *);
    VAR l := Clause.CNF(
                 PredSx.SkolemizeOuter(f, Context.rs), <*NOWARN*>
                 rightMost := FALSE);
    BEGIN
      IF shred THEN Shred(f) END (* IF *);
      IF Env.Get("PROVER_CNF_DEBUG") # NIL THEN
        AF.PrintLit2(Stdio.stdout, l); Wr.Flush(Stdio.stdout)
      END (* IF *);
      Context.Assert(l)
    END (* BEGIN *)
  END Assert;

PROCEDURE UnitConsequences() RAISES {Timeout} =
  BEGIN Context.UnitConsequences()
  END UnitConsequences;

PROCEDURE Pop() RAISES { Error } =
  BEGIN
    IF bg.size() = 0 THEN
      RAISE Error("PopBG of empty background.")
    ELSE
      Context.Pop();
      RemoveOps(bg.remhi()); ops := NIL
    END (* IF *)
  END Pop;

PROCEDURE RemoveOps(deadOps: AtomList.T) =
  BEGIN
    WHILE deadOps # NIL DO
      VAR b: BOOLEAN; dummy: INTEGER; BEGIN
        b := arity.delete(deadOps.head, dummy); <*ASSERT b*>
      END (* BEGIN *);
      deadOps := deadOps.tail
    END (* WHILE *)
  END RemoveOps;

VAR useITE := Env.Get("PROVER_ITE") # NIL;
    iteSym := Atom.FromText("ITE");

PROCEDURE RemoveITE(sx: REFANY): REFANY =
  BEGIN
    TYPECASE sx OF
    | NULL => RETURN sx
    | RefList.T(rl) =>
        IF ISTYPE(rl.head, Atom.T) AND PredSx.relOps.member(rl.head) THEN
          <*ASSERT RefList.Length(rl) = 3*>
          VAR arg1 := rl.tail.head; arg2 := rl.tail.tail.head; BEGIN
            TYPECASE arg1 OF
            | RefList.T(rl1) =>
                IF rl1.head = iteSym THEN
                  <*ASSERT RefList.Length(rl1) = 4*>
                  RETURN RemoveITE(
                             RefList.List3(
                                 PredSx.andSym,
                                 RefList.List3(
                                     PredSx.impliesSym,
                                     rl1.tail.head,
                                     RefList.List3(rl.head, rl1.tail.tail.head,
                                                   arg2)),
                                 RefList.List3(
                                     PredSx.impliesSym,
                                     PredSx.Not(rl1.tail.head),
                                     RefList.List3(rl.head,
                                                   rl1.tail.tail.tail.head,
                                                   arg2))))
                END (* IF *)
            ELSE
            END (* TYPECASE *);
            TYPECASE arg2 OF
            | RefList.T(rl2) =>
                IF rl2.head = iteSym THEN
                  <*ASSERT RefList.Length(rl2) = 4*>
                  RETURN RemoveITE(
                             RefList.List3(
                                 PredSx.andSym,
                                 RefList.List3(
                                     PredSx.impliesSym,
                                     rl2.tail.head,
                                     RefList.List3(rl.head, arg1,
                                                   rl2.tail.tail.head)),
                                 RefList.List3(
                                     PredSx.impliesSym,
                                     PredSx.Not(rl2.tail.head),
                                     RefList.List3(rl.head, arg2,
                                                   rl2.tail.tail.tail.head))))
                END (* IF *)
            ELSE
            END (* TYPECASE *)
          END (* BEGIN *)
        END (* IF *);
        (* Otherwise, recurse on arguments. *)
        RETURN RefList.Cons(RemoveITE(rl.head), RemoveITE(rl.tail))
    ELSE
        RETURN sx
    END (* TYPECASE *)
  END RemoveITE;

PROCEDURE Shred(sx: PredSx.T) =
  BEGIN
    TYPECASE sx OF
    | NULL => (*SKIP*)
    | RefList.T(rl) =>
        Shred(rl.head); Shred(rl.tail); rl.head := NIL; rl.tail := NIL
    ELSE
    END (* TYPECASE *);
  END Shred;

PROCEDURE Prune(sat: ProveRes): ProveRes RAISES { Error } =
  <*FATAL Timeout*>
  BEGIN
    triggerlessWarnCountdown := 0;
    <*ASSERT sat # NIL*>
    IF sat.context = NIL THEN RETURN sat END;
    (* Now prune this of identities, that is, literals whose negations
       are not satisifiable. *)
    IF pruneDebug THEN
      Wr.PutText(Stdio.stdout, "; Prover.Prune: Before pruning, context is:\n");
      VAR sat2: RefList.T := sat.context; BEGIN
        WHILE sat2 # NIL DO
          Wr.PutText(Stdio.stdout, ";    ");
          Sx.Print(Stdio.stdout, sat2.head);
          Wr.PutText(Stdio.stdout, "\n");
          sat2 := sat2.tail
        END (* WHILE *);
        IF sat # NIL THEN Wr.PutText(Stdio.stdout, ";\n") END (* IF *);
        Wr.Flush(Stdio.stdout)
      END (* BEGIN *)
    END (* IF *);
    VAR res: RefList.T := NIL; sat2: RefList.T := sat.context;
      nPushes := 0;
    BEGIN
      inPruning := TRUE;
      TRY
        Push(); INC(nPushes);
        UnitConsequences();
        WHILE sat2 # NIL DO
          VAR form := sat2.head; BEGIN
            IF pruneDebug THEN
              Wr.PutText(Stdio.stdout, "; Prover.Prune: Trying to prove\n");
              Wr.PutText(Stdio.stdout, ";    ");
              Sx.Print(Stdio.stdout, form);
              Wr.PutText(Stdio.stdout, "\n");
              Wr.PutText(Stdio.stdout,
                         "; Prover.Prune: nPushes = " & Fmt.Int(nPushes) &
                         ", " & "Context.pushes = " &
                         Fmt.Int(Context.pushes) & "\n");
              Wr.Flush(Stdio.stdout);
            END (* IF *);
            IF Prove(form,
                     cl := NIL,
                     ccLimit := 1,
                     shred := FALSE,
                     pruning := TRUE) # Trit.True THEN
              IF pruneDebug THEN
                Wr.PutText(Stdio.stdout, "; Prover.Prune: proof failed\n");
                Wr.Flush(Stdio.stdout);
              END (* IF *);
              res := RefList.Cons(sat2.head, res)
            ELSE
              IF pruneDebug THEN
                Wr.PutText(Stdio.stdout, "; Prover.Prune: proof succeeded\n");
                Wr.Flush(Stdio.stdout);
              END (* IF *);
            END (* IF *);
            Push(); INC(nPushes);
            IF pruneDebug THEN
              Wr.PutText(Stdio.stdout,
                       "; Prover.Prune: nPushes = " & Fmt.Int(nPushes) &
                       ", " & "Context.pushes = " &
                       Fmt.Int(Context.pushes) & "\n");
              Wr.Flush(Stdio.stdout);
            END (* IF *);
            Assert(form, shred := FALSE);
            UnitConsequences();
          END (* IF *);
          sat2 := sat2.tail
        END (* WHILE *);
      FINALLY
        IF pruneDebug THEN
          Wr.PutText(Stdio.stdout,
                     "; Prover.Prune: Processing FINALLY clause\n");
          Wr.PutText(Stdio.stdout,
                     "; Prover.Prune: nPushes = " & Fmt.Int(nPushes) & ", "
                     & "Context.pushes = " & Fmt.Int(Context.pushes) & "\n");
          Wr.Flush(Stdio.stdout);
        END (* IF *);
        WHILE nPushes > 0 DO
          Pop(); <*NOWARN*>
          DEC(nPushes);
          IF pruneDebug THEN
            Wr.PutText(Stdio.stdout,
                       "; Prover.Prune: nPushes = " & Fmt.Int(nPushes) &
                       ", " & "Context.pushes = " &
                       Fmt.Int(Context.pushes) & "\n");
            Wr.Flush(Stdio.stdout);
          END (* IF *);
        END (* WHILE *);
      END;
      sat.context := res;
      inPruning := FALSE;
    END (* BEGIN *);
    RETURN sat
  END Prune;

PROCEDURE Prove(
            form: PredSx.T;
            cl: ResClosure;
            ccLimit: INTEGER;
            shred: BOOLEAN;
            pruning := FALSE): Trit.T
    RAISES { Error } =
  VAR
    res: Trit.T;
  BEGIN
    inProof := TRUE;
    triggerlessWarnCountdown := triggerlessWarnLimit;
    IF internDebug THEN
      Wr.PutText(Stdio.stdout, "Prover.Prove: calling ProcessSx on \n");
      Wr.Flush(Stdio.stdout);
      SxPrint.Print(Stdio.stdout, form);
      Wr.PutText(Stdio.stdout, "Prover.Prove: ...\n");
      Wr.Flush(Stdio.stdout);
    END;
    form := ProcessSx(form);
    IF internDebug THEN
      Wr.PutText(Stdio.stdout, "Prover.Prove: ... ProcessSx returned\n");
      Wr.Flush(Stdio.stdout);
    END;
    VAR l: AF.Lit; BEGIN
      TRY
        IF tacticTrace THEN
          Wr.PutText(Stdio.stdout,
            "; Prover.Prove: Doing initial Prover.Push()\n");
          Wr.Flush(Stdio.stdout)
        END (* IF *);
        Push();
        l := Clause.CNF(
               PredSx.SkolemizeOuter(PredSx.Not(form), <*NOWARN*>
                                     Context.rs),
               rightMost := TRUE);
        IF shred THEN Shred(form) END (* IF *);
        IF Env.Get("PROVER_CNF_DEBUG") # NIL THEN
          AF.PrintLit2(Stdio.stdout, l); Wr.Flush(Stdio.stdout)
        END; (* IF *)
        res := Satisfy.Search(
                        l,
                        cl := cl,
                        ccLimit := ccLimit,
                        pruning := pruning)
      FINALLY
        inProof := FALSE;
        IF tacticTrace THEN
          Wr.PutText(Stdio.stdout,
            "; Prover.Prove: Doing final Prover.Pop()\n");
          Wr.Flush(Stdio.stdout)
        END (* IF *);
        Pop()
      END;
      IF tacticTrace THEN
        Wr.PutText(Stdio.stdout, "; Prover.Prove: returning " &
          Trit.ToText(res) & "\n");
        Wr.Flush(Stdio.stdout)
      END (* IF *);
      RETURN res; 
    END
  END Prove;

PROCEDURE PredDef(rl: RefList.T) RAISES { Error } =
  BEGIN
    <*ASSERT rl.head = PredSx.defPredSym OR
             rl.head = PredSx.defPredMapSym *>
    CheckPredDef(rl);
    VAR sig: RefList.T := rl.tail.head;
        ind: Atom.T;
        def: PredSx.T := NIL;
    BEGIN
      IF rl.head = PredSx.defPredMapSym THEN
        ind := rl.tail.tail.head;
        IF rl.tail.tail.tail # NIL THEN
          def := rl.tail.tail.tail.head
        END (* IF *)
      ELSE
        IF rl.tail.tail # NIL THEN
          def := rl.tail.tail.head
        END (* IF *)
      END (* IF *);
      PredDefs.DeclPred(sig.head, sig.tail, ind, def)
    END (* BEGIN *)
  END PredDef;

PROCEDURE ProcessSx(form: PredSx.T): PredSx.T RAISES { Error } =
  BEGIN
    CheckPredSyntax(form); RemoveOps(ops); ops := NIL;
    IF useITE THEN form := RemoveITE(form) END (* IF *);
    IF debug THEN
      Wr.PutText(Stdio.stdout, "After outer quant:\n\n");
      Sx.Print(Stdio.stdout, form);
      Wr.PutText(Stdio.stdout, "\n\n");
      Wr.Flush(Stdio.stdout);
    END (* IF *);
    RETURN form
  END ProcessSx;

PROCEDURE AddAxioms(rd: Rd.T) RAISES { Error } =
  BEGIN
    TRY
      LOOP
        VAR sx: RefList.T := Sx.Read(rd); BEGIN
          Assert(sx, shred := FALSE)
        END (* BEGIN *)
      END (* LOOP *);
    EXCEPT
    | Sx.ReadError =>
        RAISE Error("Sx.ReadError in an axiom.")
    | Rd.EndOfFile =>
        TRY
          Context.UnitConsequences(); Push()
        EXCEPT
          Timeout => RAISE Error(
            "Prover.AddAxioms: unexpected Prover.Timeout")
        END
    END (* TRY *)
  END AddAxioms;


PROCEDURE CheckPredSyntax(sx: PredSx.T) RAISES { Error } =
  BEGIN
    TYPECASE sx OF
    | NULL =>
        RAISE Error("NIL is not a legal PredSx.T")
    | Atom.T(at) =>
        IF at # PredSx.trueSym AND at # PredSx.falseSym THEN
          VAR i: INTEGER; BEGIN
            IF arity.get(at, i) THEN
              IF i # -1 THEN
                RAISE Error(
                          Atom.ToText(at) & " used as propositional and " &
                          "term variable.")
              END (* IF *)
            ELSE
              ops := AtomList.Cons(at, ops);
              EVAL arity.put(at, -1)
            END (* IF *)
          END (* BEGIN *)
        END (* IF *)
    | RefList.T(rl) =>
        IF NOT ISTYPE(rl.head, Atom.T) THEN
          RAISE Error("Predicates must have atom as list head.")
        END (* IF *);
        IF rl.head = PredSx.andSym OR rl.head = PredSx.orSym THEN
          VAR args := rl.tail; BEGIN
            WHILE args # NIL DO
              CheckPredSyntax(args.head); args := args.tail
            END (* WHILE *)
          END (* BEGIN *)
        ELSIF rl.head = PredSx.notSym THEN
          IF RefList.Length(rl) # 2 THEN
            RAISE Error("NOT requires 1 argument.")
          END (* IF *);
          CheckPredSyntax(rl.tail.head)
        ELSIF rl.head = PredSx.labelSym OR
              rl.head = PredSx.lblPosSym OR rl.head = PredSx.lblNegSym THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error("LBL requires 2 arguments.")
          END (* IF *);
          IF NOT ISTYPE(rl.tail.head, Atom.T) THEN
            RAISE Error("label after LBL must be an atom.")
          END (* IF *);
          CheckPredSyntax(rl.tail.tail.head)
        ELSIF rl.head = PredSx.orderSym THEN
          IF RefList.Length(rl) # 3 OR
            NOT ISTYPE(rl.tail.head, Atom.T) OR
            NOT ISTYPE(rl.tail.tail.head, Atom.T) THEN
            RAISE Error("ORDER requires 2 Atom arguments.")
          END (* IF *);
          VAR gt: Atom.T := rl.tail.head;
              ge: Atom.T := rl.tail.tail.head;
              i: INTEGER;
          BEGIN
            IF arity.get(gt, i) THEN
              IF i # 3 THEN
                RAISE Error("Order symbol '" & Atom.ToText(gt) &
                      "' used previously with arity " & Fmt.Int(i-1) & ".")
              END (* IF *)
            ELSE
              EVAL arity.put(gt, 3)
            END (* IF *);
            IF arity.get(ge, i) THEN
              IF i # 3 THEN
                RAISE Error("Order symbol '" & Atom.ToText(ge) &
                      "' used previously with arity " & Fmt.Int(i-1) & ".")
              END (* IF *)
            ELSE
              EVAL arity.put(ge, 3)
            END (* IF *)
          END (* BEGIN *)
        ELSIF rl.head = PredSx.impliesSym OR
              rl.head = PredSx.expliesSym OR
              rl.head = PredSx.iffSym THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error("IMPLIES, EXPLIES, and IFF require 2 arguments.")
          END (* IF *);
          CheckPredSyntax(rl.tail.head);
          CheckPredSyntax(rl.tail.tail.head)
        ELSIF rl.head = PredSx.proofSym THEN
          IF RefList.Length(rl) < 3 THEN
            RAISE Error("PROOF requires at least 2 arguments.")
          END (* IF *);
          (* More to do here! *)
        ELSIF rl.head = PredSx.forallSym OR rl.head = PredSx.existsSym THEN
          IF RefList.Length(rl) # 3 AND RefList.Length(rl) # 4 THEN 
            RAISE Error("FORALL and EXISTS require 2 or 3 arguments.")
          END (* IF *);
          VAR varOps: AtomList.T := NIL; BEGIN
            TYPECASE rl.tail.head OF
            | RefList.T(qvs) =>
                WHILE qvs # NIL DO
                  TYPECASE qvs.head OF
                  | NULL =>
                      RAISE Error("Quantified variable must be an atom.")
                  | Atom.T(qv) =>
                      VAR i: INTEGER; BEGIN
                        IF arity.get(qv, i) THEN
                          IF i # 0 THEN
                            RAISE Error(
                                      "Quantified variable " &
                                      Atom.ToText(qv) &
                                      "is also used as prop var or function.")
                          END (* IF *)
                        ELSE
                          varOps := AtomList.Cons(qv, varOps);
                          EVAL arity.put(qv, 0)
                        END (* IF *)
                      END (* BEGIN *)
                  ELSE
                      RAISE Error("Quantified variable must be an atom.")
                  END (* TYPECASE *);
                  qvs := qvs.tail
                END (* WHILE *);
                TYPECASE rl.tail.tail.head OF
                | NULL =>
                    RAISE Error("Missing quantifier body.")
                | RefList.T(patSpec) =>
                    VAR hd := patSpec.head; BEGIN
                      IF hd = PredSx.patsSym OR hd = PredSx.noPatsSym THEN
                        VAR specs := patSpec.tail; BEGIN
                          WHILE specs # NIL DO
                            IF hd = PredSx.patsSym THEN
                              TYPECASE specs.head OF
                              | NULL =>
                                  RAISE Error("Missing pattern spec.")
                              | RefList.T(spec) =>
                                  IF spec.head = PredSx.mpatSym THEN
                                    VAR terms := spec.tail; BEGIN
                                      WHILE terms # NIL DO
                                        CheckTermSyntax(terms.head,
                                                        isPat := TRUE);
                                        terms := terms.tail
                                      END (* WHILE *)
                                    END (* BEGIN *)
                                  ELSE
                                    CheckTermSyntax(spec, isPat := TRUE)
                                  END (* IF *)
                              ELSE
                                  CheckTermSyntax(specs.head, isPat := TRUE)
                              END (* TYPECASE *)
                            ELSE
                              CheckTermSyntax(specs.head, isPat := TRUE)
                            END (* IF *);
                            specs := specs.tail
                          END (* WHILE *)
                        END (* BEGIN *);
                        IF rl.tail.tail.tail = NIL THEN
                          RAISE Error("Missing quantifier body.")
                        ELSE
                          CheckPredSyntax(rl.tail.tail.tail.head)
                        END (* IF *)
                      ELSE
                        (* No patspec. *)
                        CheckPredSyntax(rl.tail.tail.head);
                        IF rl.tail.tail.tail # NIL THEN
                          RAISE Error("Something follows quantifier body.")
                        END (* IF *)
                      END (* IF *)
                    END (* BEGIN *)
                ELSE
                    CheckPredSyntax(rl.tail.tail.head);
                    IF rl.tail.tail.tail # NIL THEN
                      RAISE Error("Something follows quantifier body.")
                    END (* IF *)
                END (* TYPECASE *);
            ELSE
                RAISE Error("Quantifier requires list of atoms " &
                            "as first argument.")
            END (* TYPECASE *);
            RemoveOps(varOps)
          END (* BEGIN *)
        ELSIF PredSx.relOps.member(rl.head) THEN
          IF RefList.Length(rl) # 3 THEN
            RAISE Error(
                      "Relational operator " & Atom.ToText(rl.head) &
                      " requires 2 arguments.") 
          END (* IF *);
          CheckTermSyntax(rl.tail.head, FALSE);
          CheckTermSyntax(rl.tail.tail.head, FALSE)
        ELSIF rl.head = PredSx.distClassSym THEN
          VAR args := rl.tail; BEGIN
            WHILE args # NIL DO
              CheckTermSyntax(args.head, FALSE); args := args.tail
            END (* WHILE *)
          END (* BEGIN *)
        ELSIF PredDefs.IsPredSym(rl.head) THEN
          VAR i: INTEGER; b := arity.get(rl.head, i); BEGIN
            <*ASSERT b *>
            IF i-1 # RefList.Length(rl.tail) THEN
              RAISE Error("Arity mismatch for predicate symbol '" &
                    Atom.ToText(rl.head) & "'.")
            END (* IF *)
          END (* BEGIN *);
          CheckTermSyntax(rl, FALSE)
        ELSIF rl.head = PredSx.selectSym THEN
          VAR i: INTEGER; b := arity.get(rl.head, i); BEGIN
            <*ASSERT b AND i > 2 *>
            IF i-1 # RefList.Length(rl.tail) THEN
              RAISE Error("Arity mismatch for '" &
                    Atom.ToText(rl.head) & "'.")
            END (* IF *);
            TYPECASE rl.tail.head OF
            | RefList.T(map) =>
                IF PredDefs.IsPredMapSym(map.head) THEN
                  CheckTermSyntax(map)
                ELSE
                  RAISE Error("'select' can only be used as a predicate " &
                        "when applied to a PREDMAP; '" &
                        Atom.ToText(map.head) & "' is not a PREDMAP.")
                END (* IF *)
            ELSE
                RAISE Error("'select' can only be used as a predicate " &
                      "when applied to a PREDMAP.")
            END (* TYPECASE *)
          END (* BEGIN *)
        ELSE
          RAISE Error(
                    "Unknown predicate symbol: " & Atom.ToText(rl.head) & ".")
        END (* IF *)
    ELSE
        RAISE Error("Predicate not an atom or list.")
    END (* TYPECASE *);
  END CheckPredSyntax;

PROCEDURE CheckTermSyntax(sx: REFANY; isPat := FALSE) RAISES { Error } =
  BEGIN
    TYPECASE sx OF
    | NULL =>
        RAISE Error("NIL is not a valid term.")
    | Atom.T(at) =>
        IF PredSx.allOps.member(at) THEN
          RAISE Error("Cannot use " & Atom.ToText(at) & " as a term variable.")
        END (* IF *);
        VAR i: INTEGER; BEGIN
          IF arity.get(at, i) THEN
            IF i = -1 THEN
              RAISE Error(
                        Atom.ToText(at) & " used as both propositional " &
                        "and term variable.")
            ELSIF i # 0 THEN
              RAISE Error(
                        Atom.ToText(at) & " used with as constant and " &
                        "function symbol.")
            END (* IF *)
          ELSE
            ops := AtomList.Cons(at, ops);
            EVAL arity.put(at, 0)
          END (* IF *)
        END (* BEGIN *)
    | RefList.T(rl) =>
        IF NOT ISTYPE(rl.head, Atom.T) THEN
          RAISE Error("Term must have atom as list head.")
        END (* IF *);
        IF PredSx.allOps.member(rl.head) AND
          NOT (isPat AND rl.head = PredSx.diffSym) THEN
          RAISE Error(
                    "Cannot use " & Atom.ToText(rl.head) &
                    " as a function symbol.")
(* FLUX
        ELSIF PredDefs.IsPredSym(rl.head) THEN
          RAISE Error("Symbol '" & Atom.ToText(rl.head) &
                      "' used as both predicate and function symbol.")
*)
        ELSIF useITE AND rl.head = iteSym THEN
          IF RefList.Length(rl) # 4 THEN
            RAISE Error(Atom.ToText(rl.head) & " requires 4 arguments.")
          END (* IF *);
          CheckPredSyntax(rl.tail.head);
          CheckTermSyntax(rl.tail.tail.head);
          CheckTermSyntax(rl.tail.tail.tail.head);
          RETURN
        END (* IF *);
        VAR i: INTEGER; args := rl.tail; argLen := RefList.Length(args)+1;
        BEGIN
          IF arity.get(rl.head, i) THEN
(*
            IF i = -1 THEN
              RAISE Error(
                        Atom.ToText(rl.head) &
                        " used as both propositional " &
                        "and function symbol.")
            ELSIF i # argLen THEN
*)
            IF i <= 0 THEN
              RAISE Error(
                        Atom.ToText(rl.head) &
                        " is used as both a variable and a function symbol.")
            ELSIF i # argLen THEN
              IF NOT ((rl.head = PredSx.plusSym OR rl.head = PredSx.timesSym)
                AND i > 1) THEN
                RAISE Error(
                          Atom.ToText(rl.head) &
                          " used with different arities: " &
                          Fmt.Int(i-1) & " and " & Fmt.Int(argLen-1) & ".")
              END (* IF *)
            END (* IF *)
          ELSE
            ops := AtomList.Cons(rl.head, ops);
            EVAL arity.put(rl.head, argLen)
          END (* IF *);
          WHILE args # NIL DO
            CheckTermSyntax(args.head, isPat); args := args.tail
          END (* WHILE *)
        END (* BEGIN *)
    | REF INTEGER, REF LONGREAL =>
        (* SKIP *)
    ELSE
        RAISE Error("Term of unrecognized type.")
    END (* TYPECASE *)
  END CheckTermSyntax;

PROCEDURE CheckPredDef(rl: RefList.T) RAISES { Error } =
  VAR dvars := NEW(AtomSetList.T).init(); needDef := FALSE; BEGIN
    <*ASSERT rl.head = PredSx.defPredSym OR
             rl.head = PredSx.defPredMapSym *>
    TYPECASE rl.tail.head OF
    | RefList.T(rl2) =>
        VAR rl3 := rl2; BEGIN
          WHILE rl3 # NIL DO
            IF NOT ISTYPE(rl3.head, Atom.T) THEN
              RAISE Error("First part of DEFPRED or DEFPREDMAP must be " &
                    "(predSym args).")
            END (* IF *);
            rl3 := rl3.tail
          END (* WHILE *)
        END (* BEGIN *);
        VAR i: INTEGER; rl3 := rl2.tail; varOps: AtomList.T := NIL;
            predDef: PredDefs.T;
        BEGIN
          IF arity.get(rl2.head, i) THEN
            IF PredDefs.GetPredDef(rl2.head, predDef) OR
              PredDefs.GetPredMapDef(rl2.head, predDef) THEN
              RAISE Error("Multiple definitions for predicate '" &
                          Atom.ToText(rl2.head) & "'.")
            ELSIF PredDefs.IsPredSym(rl2.head) THEN
              IF rl.head # PredSx.defPredSym THEN
                RAISE Error("Predicate '" &
                             Atom.ToText(rl2.head) &
                             "' defined as both PRED and PREDMAP.")
              ELSE
                needDef := TRUE
              END (* IF *)
            ELSIF PredDefs.IsPredMapSym(rl2.head) THEN
              IF rl.head # PredSx.defPredMapSym THEN
                RAISE Error("Predicate '" &
                             Atom.ToText(rl2.head) &
                             "' defined as both PRED and PREDMAP.")
              ELSE
                needDef := TRUE
              END (* IF *)
            ELSE
              RAISE Error("Symbol '" & Atom.ToText(rl2.head) &
                    "' has already been used as a function symbol.")
            END (* IF *)
          END (* IF *);
          WHILE rl3 # NIL DO
            VAR v: Atom.T := rl3.head; BEGIN
              IF arity.get(v, i) AND i # 0 THEN
                RAISE Error("Dummy variable '" & Atom.ToText(v) &
                      "' in predicate definition is used elsewhere as " &
                      "a function or predicate symbol.")
              END (* IF *);
              IF dvars.member(v) THEN
                RAISE Error("Dummy variable '" & Atom.ToText(v) &
                      "' is used multiple times.")
              ELSE
                varOps := AtomList.Cons(v, varOps); EVAL arity.put(v, 0);
                EVAL dvars.insert(v)
              END (* IF *)
            END (* BEGIN *); 
            rl3 := rl3.tail
          END (* WHILE *);
          VAR def: REFANY := NIL; BEGIN
            IF rl.head = PredSx.defPredMapSym THEN
              IF rl.tail.tail = NIL OR
                NOT ISTYPE(rl.tail.tail.head, Atom.T) THEN
                RAISE Error("DEFPREDMAP must have index variable.")
              ELSE
                IF dvars.member(rl.tail.tail.head) THEN
                  RAISE Error("Index variable '" &
                        Atom.ToText(rl.tail.tail.head) &
                        "' is also used as a dummy argument.")
                END (* IF *)
              END (* IF *);
              IF rl.tail.tail.tail # NIL THEN
                def := rl.tail.tail.tail.head;
                IF rl.tail.tail.tail.tail # NIL THEN
                  RAISE Error(
                            "Something follows the predicate of a DEFPREDMAP")
                END (* IF *)
              END (* IF *);
            ELSIF rl.tail.tail # NIL THEN
              def := rl.tail.tail.head;
              IF rl.tail.tail.tail # NIL THEN
                RAISE Error("Something follows the predicate of a DEFPRED")
              END (* IF *)
            END (* IF *);
            IF needDef AND def = NIL THEN
              RAISE Error("Multiple definitions for predicate '" &
                          Atom.ToText(rl2.head) & "'.")
            ELSIF def # NIL THEN
              CheckPredSyntax(def)
            END (* IF *);
            RemoveOps(varOps)
          END (* BEGIN *);
          IF bg.size() > 0 THEN
            ops := AtomList.Cons(rl2.head, ops);
            VAR last := bg.size()-1; top: AtomList.T := bg.get(last); BEGIN 
              bg.put(last, AtomList.Append(top, ops))
            END (* BEGIN *)
          END (* IF *);
          ops := NIL;
          EVAL arity.put(rl2.head, RefList.Length(rl2.tail)+1)
        END (* BEGIN *)
    ELSE
        RAISE Error("First part of DEFPRED must be " &
              "(predSym args).")
    END (* TYPECASE *)
  END CheckPredDef;

BEGIN
  IF useITE THEN
    envVars := envVars & "PROVER_ITE\n"
  END (* IF *);
END Prover.

