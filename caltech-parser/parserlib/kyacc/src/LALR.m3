(* LALR(1) parser table construction.

   Phase 1: Build LR(0) automaton
   Phase 2: Compute LALR(1) lookaheads (Dragon Book Algorithm 4.63)
   Phase 3: Build PDA transition lists with conflict resolution
*)

MODULE LALR;
IMPORT Rule;
IMPORT RuleList;
IMPORT Sym;
IMPORT Pos;
IMPORT PDATrans;
IMPORT PDATransList;
IMPORT PDATransListList;
IMPORT PDATransListOp;
IMPORT IntRuleListTbl;
IMPORT TextIntTbl;
IMPORT TextTextTbl;
IMPORT Term;
IMPORT Fmt;

(* SymList used implicitly via Pos.T.cell *)
(* TextIntTbl used for gStateMap *)

CONST
  IndexShift = 1024;

TYPE
  ItemArr = REF ARRAY OF INTEGER;

  LR0State = RECORD
    kernel: ItemArr;
    items: ItemArr;
    id: INTEGER;
  END;

  PropLink = RECORD
    fromState, fromItem: INTEGER;
    toState, toItem: INTEGER;
  END;

(* ---- Module-level working storage ---- *)
VAR
  gNumRules: INTEGER;
  gAllRules: REF ARRAY OF Rule.T;
  gRuleByNum: REF ARRAY OF Rule.T;
  gMaxCode: INTEGER;
  gCallTbl: IntRuleListTbl.T;
  gPosCache: REF ARRAY OF REF ARRAY OF Pos.T;
  gNullable: REF ARRAY OF BOOLEAN;
  gFirstSet: REF ARRAY OF REF ARRAY OF BOOLEAN;
  gIsTerminal: REF ARRAY OF BOOLEAN;
  gStates: REF ARRAY OF LR0State;
  gNumStates: INTEGER;
  gGoto: REF ARRAY OF REF ARRAY OF INTEGER;
  gStateMap: TextIntTbl.T;
  gCodes: REF ARRAY OF INTEGER;
  gSymNames: REF ARRAY OF TEXT;
  gWarnings: TextTextTbl.T;
  gKernelBase: REF ARRAY OF INTEGER;
  gTotalKernel: INTEGER;
  gLA: REF ARRAY OF REF ARRAY OF BOOLEAN;
  gIsAcceptState: REF ARRAY OF BOOLEAN;

(* ---- Packing ---- *)

PROCEDURE Pack(ruleNum, index: INTEGER): INTEGER =
  BEGIN RETURN ruleNum * IndexShift + index; END Pack;

PROCEDURE UnpackR(item: INTEGER): INTEGER =
  BEGIN RETURN item DIV IndexShift; END UnpackR;

PROCEDURE UnpackI(item: INTEGER): INTEGER =
  BEGIN RETURN item MOD IndexShift; END UnpackI;

(* ---- Sorted array ops ---- *)

PROCEDURE SInsert(a: REF ARRAY OF INTEGER; n, val: INTEGER): INTEGER =
  VAR lo, hi, mid: INTEGER;
  BEGIN
    lo := 0; hi := n;
    WHILE lo < hi DO
      mid := (lo + hi) DIV 2;
      IF a[mid] < val THEN lo := mid + 1;
      ELSIF a[mid] > val THEN hi := mid;
      ELSE RETURN n; END;
    END;
    FOR i := n - 1 TO lo BY -1 DO a[i+1] := a[i]; END;
    a[lo] := val;
    RETURN n + 1;
  END SInsert;

PROCEDURE CopyN(src: REF ARRAY OF INTEGER; n: INTEGER): ItemArr =
  VAR r := NEW(ItemArr, n);
  BEGIN
    FOR i := 0 TO n - 1 DO r[i] := src[i]; END;
    RETURN r;
  END CopyN;

(* ---- Symbol at dot position, -1 if at end ---- *)

PROCEDURE SymAt(ruleNum, index: INTEGER): INTEGER =
  VAR rule := gRuleByNum[ruleNum];
  BEGIN
    IF index >= rule.length THEN RETURN -1; END;
    RETURN Sym.GetCode(gPosCache[ruleNum][index].cell.head);
  END SymAt;

(* ---- Build rule tables ---- *)

PROCEDURE BuildRuleTables(rules: RuleList.T) =
  VAR
    cur := rules;
    rule: Rule.T;
    key: INTEGER;
    value: RuleList.T;
    n, maxRN: INTEGER;
    pos: Pos.T;
  BEGIN
    gNumRules := RuleList.Length(rules);
    gAllRules := NEW(REF ARRAY OF Rule.T, gNumRules);
    n := 0;
    cur := rules;
    WHILE cur # NIL DO gAllRules[n] := cur.head; INC(n); cur := cur.tail; END;

    maxRN := 0;
    FOR i := 0 TO gNumRules - 1 DO
      IF gAllRules[i].number > maxRN THEN maxRN := gAllRules[i].number; END;
    END;
    gRuleByNum := NEW(REF ARRAY OF Rule.T, maxRN + 1);
    FOR i := 0 TO gNumRules - 1 DO
      gRuleByNum[gAllRules[i].number] := gAllRules[i];
    END;

    gCallTbl := NEW(IntRuleListTbl.Default).init(gNumRules);
    cur := rules;
    WHILE cur # NIL DO
      rule := cur.head;
      key := Sym.GetCode(rule.return);
      value := NIL;
      EVAL gCallTbl.get(key, value);
      EVAL gCallTbl.put(key, RuleList.Cons(rule, value));
      cur := cur.tail;
    END;

    gPosCache := NEW(REF ARRAY OF REF ARRAY OF Pos.T, maxRN + 1);
    FOR i := 0 TO maxRN DO
      rule := gRuleByNum[i];
      IF rule # NIL THEN
        gPosCache[i] := NEW(REF ARRAY OF Pos.T, rule.length + 1);
        pos := Pos.Zero(rule);
        FOR j := 0 TO rule.length - 1 DO
          gPosCache[i][j] := pos;
          pos := Pos.Advance(pos);
        END;
        gPosCache[i][rule.length] := pos;
      END;
    END;
  END BuildRuleTables;

(* ---- FIRST sets and nullable ---- *)

PROCEDURE ComputeFirstSets() =
  VAR
    changed: BOOLEAN;
    rule: Rule.T;
    lhs, rhs: INTEGER;
    pos: Pos.T;
    allNull: BOOLEAN;
  BEGIN
    gNullable := NEW(REF ARRAY OF BOOLEAN, gMaxCode + 1);
    gFirstSet := NEW(REF ARRAY OF REF ARRAY OF BOOLEAN, gMaxCode + 1);
    gIsTerminal := NEW(REF ARRAY OF BOOLEAN, gMaxCode + 1);
    FOR i := 0 TO gMaxCode DO
      gNullable[i] := FALSE;
      gFirstSet[i] := NEW(REF ARRAY OF BOOLEAN, gMaxCode + 1);
      FOR j := 0 TO gMaxCode DO gFirstSet[i][j] := FALSE; END;
      gIsTerminal[i] := TRUE;
    END;
    FOR i := 0 TO gNumRules - 1 DO
      gIsTerminal[Sym.GetCode(gAllRules[i].return)] := FALSE;
    END;
    FOR i := 0 TO gMaxCode DO
      IF gIsTerminal[i] THEN gFirstSet[i][i] := TRUE; END;
    END;
    REPEAT
      changed := FALSE;
      FOR i := 0 TO gNumRules - 1 DO
        rule := gAllRules[i];
        lhs := Sym.GetCode(rule.return);
        pos := Pos.Zero(rule);
        allNull := TRUE;
        WHILE pos.cell # NIL DO
          rhs := Sym.GetCode(pos.cell.head);
          FOR j := 0 TO gMaxCode DO
            IF gFirstSet[rhs][j] AND NOT gFirstSet[lhs][j] THEN
              gFirstSet[lhs][j] := TRUE; changed := TRUE;
            END;
          END;
          IF NOT gNullable[rhs] THEN allNull := FALSE; EXIT; END;
          pos := Pos.Advance(pos);
        END;
        IF allNull AND NOT gNullable[lhs] THEN
          gNullable[lhs] := TRUE; changed := TRUE;
        END;
      END;
    UNTIL NOT changed;
  END ComputeFirstSets;

(* ---- LR(0) closure ---- *)

PROCEDURE AppendUniq(buf: REF ARRAY OF INTEGER; n, val: INTEGER): INTEGER =
  (* Append val to buf[0..n-1] if not already present. *)
  BEGIN
    FOR k := 0 TO n - 1 DO
      IF buf[k] = val THEN RETURN n; END;
    END;
    buf[n] := val;
    RETURN n + 1;
  END AppendUniq;

PROCEDURE Closure(kernel: ItemArr; kernelSize: INTEGER;
                  VAR items: ItemArr; VAR itemCount: INTEGER) =
  VAR
    cap := gNumRules * 2 + kernelSize;
    buf := NEW(REF ARRAY OF INTEGER, cap);
    n, i, ruleNum, index, symCode: INTEGER;
    cur: RuleList.T;
  BEGIN
    n := 0;
    FOR k := 0 TO kernelSize - 1 DO n := AppendUniq(buf, n, kernel[k]); END;
    i := 0;
    WHILE i < n DO
      ruleNum := UnpackR(buf[i]);
      index := UnpackI(buf[i]);
      symCode := SymAt(ruleNum, index);
      IF symCode >= 0 AND NOT gIsTerminal[symCode] THEN
        cur := NIL;
        EVAL gCallTbl.get(symCode, cur);
        WHILE cur # NIL DO
          IF n >= NUMBER(buf^) - 1 THEN
            VAR nb := NEW(REF ARRAY OF INTEGER, NUMBER(buf^) * 2); BEGIN
              FOR k := 0 TO n - 1 DO nb[k] := buf[k]; END;
              buf := nb;
            END;
          END;
          n := AppendUniq(buf, n, Pack(cur.head.number, 0));
          cur := cur.tail;
        END;
      END;
      INC(i);
    END;
    items := CopyN(buf, n);
    itemCount := n;
  END Closure;

(* ---- LR(0) goto kernel ---- *)

PROCEDURE GotoKernel(st: LR0State; code: INTEGER;
                     VAR result: ItemArr; VAR count: INTEGER) =
  VAR
    buf := NEW(REF ARRAY OF INTEGER, NUMBER(st.items^));
    n, ruleNum, index: INTEGER;
  BEGIN
    n := 0;
    FOR i := 0 TO LAST(st.items^) DO
      ruleNum := UnpackR(st.items[i]);
      index := UnpackI(st.items[i]);
      IF SymAt(ruleNum, index) = code THEN
        n := SInsert(buf, n, Pack(ruleNum, index + 1));
      END;
    END;
    IF n = 0 THEN result := NIL; count := 0;
    ELSE result := CopyN(buf, n); count := n; END;
  END GotoKernel;

(* ---- State key ---- *)

PROCEDURE KKey(kernel: ItemArr; n: INTEGER): TEXT =
  VAR r := "";
  BEGIN
    FOR i := 0 TO n - 1 DO r := r & Fmt.Int(kernel[i]) & ","; END;
    RETURN r;
  END KKey;

(* ---- Get or create state ---- *)

PROCEDURE GetOrCreate(kernel: ItemArr; kernelSize: INTEGER): INTEGER =
  VAR
    key := KKey(kernel, kernelSize);
    id: INTEGER;
    ci: ItemArr;
    cn: INTEGER;
  BEGIN
    IF gStateMap.get(key, id) THEN RETURN id; END;
    INC(gNumStates);
    id := gNumStates;
    EVAL gStateMap.put(key, id);
    IF id > LAST(gStates^) THEN
      VAR ns := NEW(REF ARRAY OF LR0State, NUMBER(gStates^) * 2); BEGIN
        FOR i := 0 TO LAST(gStates^) DO ns[i] := gStates[i]; END;
        gStates := ns;
      END;
    END;
    Closure(kernel, kernelSize, ci, cn);
    gStates[id].kernel := CopyN(kernel, kernelSize);
    gStates[id].items := ci;
    gStates[id].id := id;
    RETURN id;
  END GetOrCreate;

(* ---- Phase 1: Build LR(0) automaton ---- *)

PROCEDURE BuildLR0(rules: RuleList.T) =
  VAR
    initBuf := NEW(REF ARRAY OF INTEGER, gNumRules);
    initN: INTEGER := 0;
    cur := rules;
    boundary, newBound: REF ARRAY OF INTEGER;
    bCount, nbCount: INTEGER;
    gk: ItemArr;
    gkn, target, code: INTEGER;
    oldN: INTEGER;
  BEGIN
    gStateMap := NEW(TextIntTbl.Default).init(gNumRules * 4);
    gStates := NEW(REF ARRAY OF LR0State, gNumRules * 4);
    gNumStates := 0;

    WHILE cur # NIL DO
      IF Sym.IsStart(cur.head.return) THEN
        initN := SInsert(initBuf, initN, Pack(cur.head.number, 0));
      END;
      cur := cur.tail;
    END;
    EVAL GetOrCreate(initBuf, initN);

    boundary := NEW(REF ARRAY OF INTEGER, 1);
    boundary[0] := 1;
    bCount := 1;

    WHILE bCount > 0 DO
      newBound := NEW(REF ARRAY OF INTEGER, gNumRules * 8);
      nbCount := 0;
      FOR bi := 0 TO bCount - 1 DO
        VAR s := boundary[bi]; BEGIN
          (* Grow goto table *)
          IF s > LAST(gGoto^) THEN
            VAR ng := NEW(REF ARRAY OF REF ARRAY OF INTEGER,
                          NUMBER(gGoto^) * 2);
            BEGIN
              FOR i := 0 TO LAST(gGoto^) DO ng[i] := gGoto[i]; END;
              gGoto := ng;
            END;
          END;
          gGoto[s] := NEW(REF ARRAY OF INTEGER, gMaxCode + 1);
          FOR c := 0 TO gMaxCode DO gGoto[s][c] := 0; END;

          FOR ci := 0 TO LAST(gCodes^) DO
            code := gCodes[ci];
            GotoKernel(gStates[s], code, gk, gkn);
            IF gkn > 0 THEN
              oldN := gNumStates;
              target := GetOrCreate(gk, gkn);
              gGoto[s][code] := target;
              IF target > oldN THEN
                IF nbCount >= NUMBER(newBound^) THEN
                  VAR nb2 := NEW(REF ARRAY OF INTEGER, NUMBER(newBound^)*2);
                  BEGIN
                    FOR i := 0 TO nbCount-1 DO nb2[i] := newBound[i]; END;
                    newBound := nb2;
                  END;
                END;
                newBound[nbCount] := target;
                INC(nbCount);
              END;
            END;
          END;
        END;
      END;
      boundary := newBound;
      bCount := nbCount;
    END;

    Term.WrLn("kyacc: LALR LR(0) states: " & Fmt.Int(gNumStates));
  END BuildLR0;

(* ---- Find packed item in a state's kernel, return index or -1 ---- *)

PROCEDURE FindKI(stateId, packedItem: INTEGER): INTEGER =
  VAR k := gStates[stateId].kernel;
  BEGIN
    FOR i := 0 TO LAST(k^) DO
      IF k[i] = packedItem THEN RETURN i; END;
    END;
    RETURN -1;
  END FindKI;

(* ---- Phase 2: Compute LALR(1) lookaheads ---- *)

PROCEDURE ComputeLookaheads() =
  VAR
    base: INTEGER := 0;
    links: REF ARRAY OF PropLink;
    numLinks: INTEGER := 0;
    linkCap: INTEGER;
    (* Closure workspace *)
    cBuf: REF ARRAY OF INTEGER;
    cLook: REF ARRAY OF REF ARRAY OF BOOLEAN;
    cN: INTEGER;
    item, rn, idx, sc, tgt, tki, gfrom, gto: INTEGER;
    cur: RuleList.T;
    changed: BOOLEAN;
    passes: INTEGER := 0;
  BEGIN
    (* Build kernel index *)
    gKernelBase := NEW(REF ARRAY OF INTEGER, gNumStates + 1);
    base := 0;
    FOR s := 1 TO gNumStates DO
      gKernelBase[s] := base;
      INC(base, NUMBER(gStates[s].kernel^));
    END;
    gTotalKernel := base;

    (* Init lookahead sets *)
    gLA := NEW(REF ARRAY OF REF ARRAY OF BOOLEAN, gTotalKernel);
    FOR i := 0 TO gTotalKernel - 1 DO
      gLA[i] := NEW(REF ARRAY OF BOOLEAN, gMaxCode + 1);
      FOR j := 0 TO gMaxCode DO gLA[i][j] := FALSE; END;
    END;

    (* Seed: state 1 kernel items get EOF *)
    FOR ki := 0 TO LAST(gStates[1].kernel^) DO
      gLA[gKernelBase[1] + ki][0] := TRUE;
    END;

    linkCap := gTotalKernel * 4;
    links := NEW(REF ARRAY OF PropLink, linkCap);
    cBuf := NEW(REF ARRAY OF INTEGER, gNumRules * 2);
    cLook := NEW(REF ARRAY OF REF ARRAY OF BOOLEAN, gNumRules * 2);

    (* For each state, each kernel item, compute closure with dummy token *)
    FOR s := 1 TO gNumStates DO
      FOR ki := 0 TO LAST(gStates[s].kernel^) DO
        gfrom := gKernelBase[s] + ki;

        (* Single-item closure with lookahead tracking *)
        cN := 0;
        item := gStates[s].kernel[ki];

        (* Init closure with this item *)
        cBuf[0] := item;
        cLook[0] := NEW(REF ARRAY OF BOOLEAN, gMaxCode + 2);
        FOR j := 0 TO gMaxCode + 1 DO cLook[0][j] := FALSE; END;
        cLook[0][gMaxCode + 1] := TRUE; (* dummy marker *)
        cN := 1;

        VAR ci := 0; BEGIN
          WHILE ci < cN DO
            rn := UnpackR(cBuf[ci]);
            idx := UnpackI(cBuf[ci]);
            sc := SymAt(rn, idx);
            IF sc >= 0 AND NOT gIsTerminal[sc] THEN
              cur := NIL;
              EVAL gCallTbl.get(sc, cur);
              WHILE cur # NIL DO
                VAR
                  ni := Pack(cur.head.number, 0);
                  fi := -1;
                  lookBuf: REF ARRAY OF BOOLEAN;
                BEGIN
                  (* Find or add in closure *)
                  FOR k := 0 TO cN - 1 DO
                    IF cBuf[k] = ni THEN fi := k; EXIT; END;
                  END;
                  IF fi < 0 THEN
                    fi := cN;
                    (* Grow if needed *)
                    IF cN >= NUMBER(cBuf^) THEN
                      VAR
                        nb := NEW(REF ARRAY OF INTEGER, NUMBER(cBuf^)*2);
                        nl := NEW(REF ARRAY OF REF ARRAY OF BOOLEAN,
                                  NUMBER(cLook^)*2);
                      BEGIN
                        FOR k := 0 TO cN-1 DO
                          nb[k] := cBuf[k]; nl[k] := cLook[k];
                        END;
                        cBuf := nb; cLook := nl;
                      END;
                    END;
                    cBuf[fi] := ni;
                    cLook[fi] := NEW(REF ARRAY OF BOOLEAN, gMaxCode + 2);
                    FOR k := 0 TO gMaxCode + 1 DO cLook[fi][k] := FALSE; END;
                    INC(cN);
                  END;

                  (* Compute FIRST(beta) where beta = suffix after dot+1 *)
                  lookBuf := cLook[fi];
                  VAR
                    rule := gRuleByNum[rn];
                    sp := idx + 1;
                    allN := TRUE;
                    p: Pos.T;
                    c2: INTEGER;
                  BEGIN
                    IF sp <= rule.length - 1 THEN
                      p := gPosCache[rn][sp];
                      WHILE p.cell # NIL DO
                        c2 := Sym.GetCode(p.cell.head);
                        FOR j := 0 TO gMaxCode DO
                          IF gFirstSet[c2][j] THEN lookBuf[j] := TRUE; END;
                        END;
                        IF NOT gNullable[c2] THEN allN := FALSE; EXIT; END;
                        p := Pos.Advance(p);
                      END;
                    END;
                    IF allN THEN
                      FOR j := 0 TO gMaxCode + 1 DO
                        IF cLook[ci][j] THEN lookBuf[j] := TRUE; END;
                      END;
                    END;
                  END;
                END;
                cur := cur.tail;
              END;
            END;
            INC(ci);
          END;
        END;

        (* Extract spontaneous lookaheads and propagation links *)
        FOR ci := 0 TO cN - 1 DO
          rn := UnpackR(cBuf[ci]);
          idx := UnpackI(cBuf[ci]);
          sc := SymAt(rn, idx);
          IF sc >= 0 AND gGoto[s] # NIL AND gGoto[s][sc] > 0 THEN
            tgt := gGoto[s][sc];
            tki := FindKI(tgt, Pack(rn, idx + 1));
            IF tki >= 0 THEN
              gto := gKernelBase[tgt] + tki;
              FOR t := 0 TO gMaxCode DO
                IF cLook[ci][t] AND gIsTerminal[t] THEN
                  gLA[gto][t] := TRUE;
                END;
              END;
              IF cLook[ci][gMaxCode + 1] THEN
                IF numLinks >= linkCap THEN
                  linkCap := linkCap * 2;
                  VAR nl := NEW(REF ARRAY OF PropLink, linkCap); BEGIN
                    FOR k := 0 TO numLinks-1 DO nl[k] := links[k]; END;
                    links := nl;
                  END;
                END;
                links[numLinks] := PropLink{s, ki, tgt, tki};
                INC(numLinks);
              END;
            END;
          END;
        END;
      END;
    END;

    Term.WrLn("kyacc: LALR propagation links: " & Fmt.Int(numLinks));

    (* Propagate *)
    REPEAT
      changed := FALSE;
      INC(passes);
      FOR li := 0 TO numLinks - 1 DO
        gfrom := gKernelBase[links[li].fromState] + links[li].fromItem;
        gto := gKernelBase[links[li].toState] + links[li].toItem;
        FOR t := 0 TO gMaxCode DO
          IF gLA[gfrom][t] AND NOT gLA[gto][t] THEN
            gLA[gto][t] := TRUE; changed := TRUE;
          END;
        END;
      END;
    UNTIL NOT changed;

    Term.WrLn("kyacc: LALR propagation passes: " & Fmt.Int(passes));

  END ComputeLookaheads;

(* ---- Phase 3: Build PDA transition lists ---- *)

PROCEDURE WarnConflict(state: INTEGER; a, b: Rule.T; bKind: TEXT) =
  VAR
    key := "Reduce " & a.name & ", or " & bKind & " " & b.name & "?";
    val := " (LALR state " & Fmt.Int(state) & ")";
  BEGIN
    IF NOT gWarnings.get(key, val) THEN
      EVAL gWarnings.put(key, val);
    END;
  END WarnConflict;

(* Compute reduce actions for state s by doing a full closure
   with actual LALR lookaheads from kernel items.  This correctly
   handles epsilon rules, which are not kernel items and thus
   don't have entries in the LALR lookahead table. *)
PROCEDURE CollectReduces(s: INTEGER;
                         reduceR, reduceC: REF ARRAY OF Rule.T) =
  VAR
    (* Closure with per-item lookahead sets *)
    cBuf: REF ARRAY OF INTEGER;
    cLook: REF ARRAY OF REF ARRAY OF BOOLEAN;
    cN: INTEGER := 0;
    rn, idx, sc, code: INTEGER;
    cur: RuleList.T;
    rule: Rule.T;
  BEGIN
    cBuf := NEW(REF ARRAY OF INTEGER, gNumRules * 2);
    cLook := NEW(REF ARRAY OF REF ARRAY OF BOOLEAN, gNumRules * 2);

    (* Initialize with kernel items and their LALR lookaheads *)
    FOR ki := 0 TO LAST(gStates[s].kernel^) DO
      VAR
        item := gStates[s].kernel[ki];
        gi := gKernelBase[s] + ki;
        la := NEW(REF ARRAY OF BOOLEAN, gMaxCode + 1);
      BEGIN
        FOR j := 0 TO gMaxCode DO la[j] := gLA[gi][j]; END;
        cBuf[cN] := item;
        cLook[cN] := la;
        INC(cN);
      END;
    END;

    (* Closure: expand items with dot before a nonterminal *)
    VAR ci := 0; BEGIN
      WHILE ci < cN DO
        rn := UnpackR(cBuf[ci]);
        idx := UnpackI(cBuf[ci]);
        sc := SymAt(rn, idx);
        IF sc >= 0 AND NOT gIsTerminal[sc] THEN
          cur := NIL;
          EVAL gCallTbl.get(sc, cur);
          WHILE cur # NIL DO
            VAR
              ni := Pack(cur.head.number, 0);
              fi := -1;
              lookBuf: REF ARRAY OF BOOLEAN;
            BEGIN
              FOR k := 0 TO cN - 1 DO
                IF cBuf[k] = ni THEN fi := k; EXIT; END;
              END;
              IF fi < 0 THEN
                fi := cN;
                IF cN >= NUMBER(cBuf^) THEN
                  VAR
                    nb := NEW(REF ARRAY OF INTEGER, NUMBER(cBuf^)*2);
                    nl := NEW(REF ARRAY OF REF ARRAY OF BOOLEAN,
                              NUMBER(cLook^)*2);
                  BEGIN
                    FOR k := 0 TO cN-1 DO
                      nb[k] := cBuf[k]; nl[k] := cLook[k];
                    END;
                    cBuf := nb; cLook := nl;
                  END;
                END;
                cBuf[fi] := ni;
                cLook[fi] := NEW(REF ARRAY OF BOOLEAN, gMaxCode + 1);
                FOR k := 0 TO gMaxCode DO cLook[fi][k] := FALSE; END;
                INC(cN);
              END;

              (* Compute FIRST(beta) where beta = symbols after dot+1 in rule rn *)
              lookBuf := cLook[fi];
              VAR
                prule := gRuleByNum[rn];
                sp := idx + 1;
                allN := TRUE;
                p: Pos.T;
                c2: INTEGER;
                added := FALSE;
              BEGIN
                IF sp <= prule.length - 1 THEN
                  p := gPosCache[rn][sp];
                  WHILE p.cell # NIL DO
                    c2 := Sym.GetCode(p.cell.head);
                    FOR j := 0 TO gMaxCode DO
                      IF gFirstSet[c2][j] AND NOT lookBuf[j] THEN
                        lookBuf[j] := TRUE; added := TRUE;
                      END;
                    END;
                    IF NOT gNullable[c2] THEN allN := FALSE; EXIT; END;
                    p := Pos.Advance(p);
                  END;
                END;
                IF allN THEN
                  FOR j := 0 TO gMaxCode DO
                    IF cLook[ci][j] AND NOT lookBuf[j] THEN
                      lookBuf[j] := TRUE; added := TRUE;
                    END;
                  END;
                END;
              END;
            END;
            cur := cur.tail;
          END;
        END;
        INC(ci);
      END;
    END;

    (* Now check all closure items for completions *)
    FOR ci := 0 TO cN - 1 DO
      rn := UnpackR(cBuf[ci]);
      idx := UnpackI(cBuf[ci]);
      rule := gRuleByNum[rn];
      IF idx >= rule.length THEN
        (* Completed item — use its lookahead for reduces *)
        FOR i := 0 TO LAST(gCodes^) DO
          code := gCodes[i];
          IF cLook[ci][code] THEN
            CASE Rule.Compare(rule, reduceR[code]) OF
            | 1 => reduceC[code] := reduceR[code]; reduceR[code] := rule;
            | 0 => reduceC[code] := rule;
            | -1 =>
            END;
          END;
        END;
      END;
    END;
  END CollectReduces;

PROCEDURE BuildTransitions(VAR outNum: INTEGER): PDATransListList.T =
  VAR
    result: PDATransListList.T := NIL;
    tlist: PDATransList.T;
    trans: PDATrans.T;
    shiftTgt: REF ARRAY OF INTEGER;
    reduceR: REF ARRAY OF Rule.T;
    reduceC: REF ARRAY OF Rule.T;
    rn, idx, code: INTEGER;
    rule: Rule.T;
  BEGIN
    shiftTgt := NEW(REF ARRAY OF INTEGER, gMaxCode + 1);
    reduceR := NEW(REF ARRAY OF Rule.T, gMaxCode + 1);
    reduceC := NEW(REF ARRAY OF Rule.T, gMaxCode + 1);

    FOR s := 1 TO gNumStates DO
      tlist := NIL;

      FOR c := 0 TO gMaxCode DO
        shiftTgt[c] := 0; reduceR[c] := NIL; reduceC[c] := NIL;
      END;

      (* Shifts from goto *)
      IF gGoto[s] # NIL THEN
        FOR ci := 0 TO LAST(gCodes^) DO
          code := gCodes[ci];
          IF gGoto[s][code] > 0 THEN shiftTgt[code] := gGoto[s][code]; END;
        END;
      END;

      (* Reduces: compute full closure with lookaheads from kernel LALR sets,
         then check all completed items for reduce actions *)
      CollectReduces(s, reduceR, reduceC);

      (* Build transitions per code *)
      FOR ci := 0 TO LAST(gCodes^) DO
        code := gCodes[ci];
        VAR hasShift := shiftTgt[code] > 0;
            hasReduce := reduceR[code] # NIL;
        BEGIN
          IF NOT hasShift AND NOT hasReduce THEN
            IF gIsAcceptState[s] AND code = 0 THEN
              trans.code := code;
              trans.kind := PDATrans.ActKind.Accept;
              trans.target := 0;
              tlist := PDATransList.Cons(trans, tlist);
            END;
            (* else Error — omit *)
          ELSIF hasShift AND NOT hasReduce THEN
            trans.code := code;
            trans.kind := PDATrans.ActKind.Shift;
            trans.target := shiftTgt[code];
            tlist := PDATransList.Cons(trans, tlist);
          ELSIF NOT hasShift AND hasReduce THEN
            trans.code := code;
            trans.kind := PDATrans.ActKind.Reduce;
            trans.target := reduceR[code].number;
            tlist := PDATransList.Cons(trans, tlist);
            IF reduceC[code] # NIL AND reduceC[code] # reduceR[code] THEN
              WarnConflict(s, reduceR[code], reduceC[code], "reduce");
            END;
          ELSE
            (* Shift/reduce conflict *)
            VAR bestShift: Rule.T := NIL; BEGIN
              FOR ii := 0 TO LAST(gStates[s].items^) DO
                rn := UnpackR(gStates[s].items[ii]);
                idx := UnpackI(gStates[s].items[ii]);
                IF SymAt(rn, idx) = code THEN
                  rule := gRuleByNum[rn];
                  IF Rule.Compare(rule, bestShift) > 0 THEN
                    bestShift := rule;
                  END;
                END;
              END;
              CASE Rule.Compare(reduceR[code], bestShift, TRUE) OF
              | -1 =>
                trans.code := code;
                trans.kind := PDATrans.ActKind.Shift;
                trans.target := shiftTgt[code];
                tlist := PDATransList.Cons(trans, tlist);
              | 0 =>
                WarnConflict(s, reduceR[code], bestShift, "shift");
                (* Epsilon reduce → shift wins (dangling else).
                   Non-epsilon reduce → reduce wins (decl vs stmt). *)
                IF reduceR[code].length = 0 THEN
                  trans.code := code;
                  trans.kind := PDATrans.ActKind.Shift;
                  trans.target := shiftTgt[code];
                  tlist := PDATransList.Cons(trans, tlist);
                ELSE
                  trans.code := code;
                  trans.kind := PDATrans.ActKind.Reduce;
                  trans.target := reduceR[code].number;
                  tlist := PDATransList.Cons(trans, tlist);
                END;
              | 1 =>
                trans.code := code;
                trans.kind := PDATrans.ActKind.Reduce;
                trans.target := reduceR[code].number;
                tlist := PDATransList.Cons(trans, tlist);
              END;
              IF reduceC[code] # NIL AND reduceC[code] # reduceR[code] THEN
                WarnConflict(s, reduceR[code], reduceC[code], "reduce");
              END;
            END;
          END;
        END;
      END;

      tlist := PDATransListOp.Simplify(tlist);
      result := PDATransListList.Cons(tlist, result);
    END;

    result := PDATransListList.ReverseD(result);
    outNum := gNumStates;
    RETURN result;
  END BuildTransitions;

(* ---- Entry point ---- *)

PROCEDURE Build(rules: RuleList.T;
                codes: REF ARRAY OF INTEGER;
                symNames: REF ARRAY OF TEXT;
                warnings: TextTextTbl.T;
                VAR numStates: INTEGER): PDATransListList.T =
  BEGIN
    gCodes := codes;
    gSymNames := symNames;
    gWarnings := warnings;
    gMaxCode := 0;
    FOR i := 0 TO LAST(codes^) DO
      IF codes[i] > gMaxCode THEN gMaxCode := codes[i]; END;
    END;
    BuildRuleTables(rules);
    gGoto := NEW(REF ARRAY OF REF ARRAY OF INTEGER, gNumRules * 4);
    ComputeFirstSets();
    BuildLR0(rules);

    (* Mark accept states: states reached from state 1 by shifting a
       start symbol.  The canonical builder uses StartStatus.SingleStartSym
       for this; we replicate it with a boolean array.
       If a start symbol has no natural goto from state 1 (because it only
       appears as the return of a start rule, not as a symbol after any dot),
       create a synthetic accept state and add the goto. *)
    VAR
      startCodes: REF ARRAY OF INTEGER;
      nStartCodes: INTEGER := 0;
      acceptState: INTEGER;
    BEGIN
      startCodes := NEW(REF ARRAY OF INTEGER, gMaxCode + 1);
      VAR cur := rules; BEGIN
        WHILE cur # NIL DO
          IF Sym.IsStart(cur.head.return) THEN
            VAR sc := Sym.GetCode(cur.head.return); dup := FALSE; BEGIN
              FOR k := 0 TO nStartCodes - 1 DO
                IF startCodes[k] = sc THEN dup := TRUE; EXIT; END;
              END;
              IF NOT dup THEN
                startCodes[nStartCodes] := sc; INC(nStartCodes);
              END;
            END;
          END;
          cur := cur.tail;
        END;
      END;

      (* We may need to create a synthetic accept state *)
      acceptState := 0;
      FOR si := 0 TO nStartCodes - 1 DO
        IF gGoto[1] = NIL OR gGoto[1][startCodes[si]] = 0 THEN
          (* No natural goto — need synthetic accept state *)
          IF acceptState = 0 THEN
            INC(gNumStates);
            acceptState := gNumStates;
            IF acceptState > LAST(gStates^) THEN
              VAR ns := NEW(REF ARRAY OF LR0State, NUMBER(gStates^) * 2);
              BEGIN
                FOR i := 0 TO LAST(gStates^) DO ns[i] := gStates[i]; END;
                gStates := ns;
              END;
            END;
            IF acceptState > LAST(gGoto^) THEN
              VAR ng := NEW(REF ARRAY OF REF ARRAY OF INTEGER,
                            NUMBER(gGoto^) * 2);
              BEGIN
                FOR i := 0 TO LAST(gGoto^) DO ng[i] := gGoto[i]; END;
                gGoto := ng;
              END;
            END;
            gStates[acceptState].kernel := NEW(ItemArr, 0);
            gStates[acceptState].items := NEW(ItemArr, 0);
            gStates[acceptState].id := acceptState;
          END;
          IF gGoto[1] # NIL THEN
            gGoto[1][startCodes[si]] := acceptState;
          END;
        END;
      END;

      gIsAcceptState := NEW(REF ARRAY OF BOOLEAN, gNumStates + 1);
      FOR i := 0 TO gNumStates DO gIsAcceptState[i] := FALSE; END;
      IF acceptState > 0 THEN gIsAcceptState[acceptState] := TRUE; END;

      (* Also check for natural gotos to start symbols from state 1 *)
      IF gGoto[1] # NIL THEN
        FOR si := 0 TO nStartCodes - 1 DO
          VAR tgt := gGoto[1][startCodes[si]]; BEGIN
            IF tgt > 0 THEN gIsAcceptState[tgt] := TRUE; END;
          END;
        END;
      END;
    END;

    ComputeLookaheads();
    RETURN BuildTransitions(numStates);
  END Build;

BEGIN
END LALR.
