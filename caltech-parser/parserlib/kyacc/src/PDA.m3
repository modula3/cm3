(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PDA.m3,v 1.2 2001-09-19 15:13:58 wagner Exp $ *)

MODULE PDA;
IMPORT PDATrans;
IMPORT PDATransList;
IMPORT PDATransListOp;
IMPORT PDATransListList;
IMPORT PDATransListFlat;
IMPORT Rule;
IMPORT RuleList;
IMPORT RuleListState;
IMPORT RuleListStateTbl;
IMPORT RuleListStateList;
IMPORT TokSpec;
IMPORT CharRange;
IMPORT CharCodes;
IMPORT TextIntTbl;
IMPORT TextTextTbl;
IMPORT Term;
IMPORT Fmt;
IMPORT FmtTable;
IMPORT Sym;
IMPORT Scan, Stdio, Rd, Thread, FloatMode, Lex, Wr;
<* FATAL Wr.Failure, FloatMode.Trap, Lex.Error *>

REVEAL
  T = Public BRANDED OBJECT
    rules: RuleList.T;
    tok: TokSpec.T;
    codes: REF ARRAY OF INTEGER; (* 0 = EOF, 1..255 = CHAR, >255= other *)
    symNames: REF ARRAY OF TEXT; (* indexed by code *)
    numStates: INTEGER := 0;
    statesList: PDATransListList.T := NIL;
  OVERRIDES
    fmtSymbols := FormatSymbols;
    symInfo := SymInfo;
  END;

PROCEDURE FormatSymbols(self: T): TEXT =
  VAR
    fmt := NEW(FmtTable.T).init();
    code: INTEGER;
  BEGIN
    FOR i := 0 TO LAST(self.codes^) DO
      code := self.codes[i];
      fmt.putText("Y{" & Fmt.Int(code) & "," &
        CharCodes.Q(self.symNames[code]) & "}");
    END;
    RETURN fmt.toText();
  END FormatSymbols;

PROCEDURE SymInfo(self: T; VAR numSym, lastCode: INTEGER) =
  BEGIN
    numSym := NUMBER(self.codes^);
    lastCode := self.codes[LAST(self.codes^)];
  END SymInfo;

PROCEDURE BuildCodes(self: T; codeTbl: TextIntTbl.T) =
  VAR
    charCodes := self.tok.charTokens + CharRange.T{'\000'};
    numChar := CharRange.Size(charCodes);
    numOther, numTotal: INTEGER := 0;
    maxOther: INTEGER := 255;
    iter := codeTbl.iterate();
    key: TEXT;
    val: INTEGER;
  BEGIN
    WHILE iter.next(key, val) DO
      maxOther := MAX(maxOther, val);
    END;
    numOther := maxOther - 255;
    self.codes := NEW(REF ARRAY OF INTEGER, numChar + numOther);
    FOR c := FIRST(CHAR) TO LAST(CHAR) DO
      IF c IN charCodes THEN
        self.codes[numTotal] := ORD(c); INC(numTotal);
      END;
    END;
    FOR i := 256 TO maxOther DO
      self.codes[numTotal] := i; INC(numTotal);
    END;
    <* ASSERT numTotal = numChar + numOther *>
    <* ASSERT self.codes[0] = 0 *>
    
    self.symNames := NEW(REF ARRAY OF TEXT, maxOther+1);
    FOR c := FIRST(CHAR) TO LAST(CHAR) DO
      IF c IN charCodes THEN
        self.symNames[ORD(c)] := CharCodes.QC(c);
      END;
    END;
    self.symNames[0] := "EOF";
    iter := codeTbl.iterate();
    WHILE iter.next(key, val) DO
      self.symNames[val] := key;
    END;
  END BuildCodes;

PROCEDURE Warn(warnings: TextTextTbl.T) =
  VAR
    iter := warnings.iterate();
    key, val: TEXT;
  BEGIN
    WHILE iter.next(key, val) DO
      Term.WrLn(key & val);
    END;
  END Warn;

PROCEDURE BuildStatesList(self: T) =
  VAR
    boundary: RuleListStateList.T := NIL;
    cur: RuleListStateList.T;
    estStates := RuleList.Length(self.rules)*2 + LAST(self.codes^)*3;
    stateTab := NEW(RuleListStateTbl.Default).init(estStates);
    curState: RuleListState.T;
    action: RuleListState.Action;
    curTrans: PDATrans.T;
    curTransList: PDATransList.T;
    code: INTEGER;
    warnings := NEW(TextTextTbl.Default).init();
    expandEstimate: INTEGER := 32;
  PROCEDURE GetState(state: RuleListState.T): INTEGER =
    VAR
      result: INTEGER;
    BEGIN
      IF NOT stateTab.get(state, result) THEN
        INC(self.numStates);
        result := self.numStates;
        EVAL stateTab.put(state, result);
        boundary := RuleListStateList.Cons(state, boundary);
      END;
 (*     Term.WrLn("GetState="&Fmt.Int(result));state.ID := result; *)
      RETURN result;
    END GetState;
  BEGIN
    curState := RuleListState.New(self.rules, warnings);
    RuleListState.Expand(curState, expandEstimate);
    EVAL GetState(curState);
    REPEAT
      cur := RuleListStateList.ReverseD(boundary);
      boundary := NIL;
      REPEAT
        curState := cur.head;
(*        Term.WrLn("CurState = " & Fmt.Int(curState.ID) & ": " &
          RuleListState.Format(curState)); *)
        curTransList := NIL;
        FOR i := 0 TO LAST(self.codes^) DO
          code := self.codes[i];
          action := RuleListState.Step(curState, code, self.symNames[code]);
          curTrans.code := code;
          curTrans.kind := action.kind;
          CASE action.kind OF
          | PDATrans.ActKind.Shift =>
            RuleListState.Expand(action.next, expandEstimate);
            curTrans.target:=GetState(action.next);
          | PDATrans.ActKind.Reduce =>
            curTrans.target := action.rule.number;
          ELSE
            curTrans.target := 0;
          END;
(*          Term.WrLn("Make PDATrans: " & Fmt.Int(curState.ID) & ": " & 
            PDATrans.Format(curTrans)); *)
          IF action.kind # PDATrans.ActKind.Error THEN
            curTransList := PDATransList.Cons(curTrans, curTransList);
          END;
        END;
        curTransList := PDATransListOp.Simplify(curTransList);
        self.statesList := PDATransListList.Cons(curTransList,
                                                 self.statesList);
        cur := cur.tail;
      UNTIL cur = NIL;
    UNTIL boundary = NIL;
    self.statesList := PDATransListList.ReverseD(self.statesList);
    <* ASSERT self.numStates = PDATransListList.Length(self.statesList) *>
    Warn(warnings);
  END BuildStatesList;

PROCEDURE BuildStatesArray(self: T) =
  VAR
    cur := self.statesList;
  BEGIN
    self.statesArray := NEW(REF ARRAY OF PDATransList.T, self.numStates+1);
    FOR i := 1 TO LAST(self.statesArray^) DO
      self.statesArray[i] := cur.head;
      cur := cur.tail;
    END;
    PDATransListOp.MergeStates(self.statesArray);
    self.lastShift := LAST(self.statesArray^);
    (* PDATransListOp.PrintArray(self.statesArray, self.lastShift); *)
    PDATransListFlat.Flatten(self.statesArray);
    PDATransListFlat.UnReducedWarning(self.statesArray, self.rules);
  END BuildStatesArray;

PROCEDURE New(rules: RuleList.T;
              tok: TokSpec.T;
              codeTbl: TextIntTbl.T): T =
  VAR
    self := NEW(T, rules := rules, tok := tok);                
  BEGIN
    BuildCodes(self, codeTbl);
    BuildStatesList(self);
    BuildStatesArray(self);
    RETURN self;
  END New;

PROCEDURE Test(self: T) =
  VAR
    curState: INTEGER := 1;
    trans: PDATrans.T;
    rule: Rule.T;
    symbol, preservedToken: INTEGER;
    skipEntries: INTEGER := 0;
    stack: ARRAY [0..1000] OF INTEGER;
    stackPtr: INTEGER := 0;
    <* FATAL Rd.Failure, Rd.EndOfFile, Thread.Alerted *>
  PROCEDURE TransLookup(): PDATrans.T =
    VAR
      cur := self.statesArray[curState];
      default := PDATrans.T{code := symbol,
                            kind := PDATrans.ActKind.Error,
                            target := 0};
      hops: INTEGER := 0;
    BEGIN
      WHILE cur # NIL DO
        IF cur.head.code = -2 THEN
          <* ASSERT cur.head.kind = PDATrans.ActKind.Jump *>
          cur := self.statesArray[cur.head.target];
          INC(hops);
        ELSE
          IF cur.head.code = -1 THEN
            default := cur.head;
          ELSIF cur.head.code = symbol THEN
            Term.WrLn("hops: " & Fmt.Int(hops));
            RETURN cur.head;
          END;
          cur := cur.tail;
        END;
      END;
      Term.WrLn("hops: " & Fmt.Int(hops));
      RETURN default;
    END TransLookup;
  PROCEDURE ShiftBefore(kind: PDATrans.ActKind) =
    BEGIN
      Term.WrLn("shifting anonymously");
      INC(stackPtr); stack[stackPtr] := 0;
      trans.kind := kind;
      IF skipEntries = 0 THEN
        preservedToken := -1;
      END;
    END ShiftBefore;
  BEGIN
    Term.WrLn("\nPDA Test.");
    Term.WrLn("starting in state " & Fmt.Int(curState));
    stack[0] := curState;
    WHILE TRUE DO
      IF skipEntries=2 THEN
        skipEntries := 1;
        Term.WrLn("re-scanning reduced symbol " & Fmt.Int(symbol));
      ELSIF skipEntries=1 AND preservedToken # -1 THEN
        skipEntries := 0;
        symbol := preservedToken;
        Term.WrLn("re-scanning input symbol " & Fmt.Int(symbol));
      ELSE
        skipEntries := 0;
        Term.Wr("input symbol: ");
        Wr.Flush(Stdio.stdout);
        symbol := Scan.Int(Rd.GetLine(Stdio.stdin));
        preservedToken := symbol;
      END;
      trans := TransLookup();
      CASE trans.kind OF
      | PDATrans.ActKind.ShiftReduce => ShiftBefore(PDATrans.ActKind.Reduce);
      | PDATrans.ActKind.ShiftAccept => ShiftBefore(PDATrans.ActKind.Accept);
      ELSE
      END;
      CASE trans.kind OF
      | PDATrans.ActKind.Shift =>
        curState := trans.target;
        Term.WrLn("shifting to state " & Fmt.Int(curState));
        INC(stackPtr); stack[stackPtr] := curState;
      | PDATrans.ActKind.Reduce =>
        rule := RuleList.Nth(self.rules, trans.target - 1);
        <* ASSERT rule.number = trans.target *>
        Term.WrLn("reducing by rule " & Rule.Format(rule, "%debug"));
        DEC(stackPtr, rule.length); curState := stack[stackPtr];
        Term.WrLn("popping to state " & Fmt.Int(curState));
        symbol := Sym.GetCode(rule.return);
        skipEntries := 2;
      | PDATrans.ActKind.Accept =>
        <* ASSERT stackPtr = 1 *>
        Term.WrLn("Accept start symbol on stack");
        IF preservedToken = -1 THEN
          Term.WrLn("Unknown if more input remains");
        ELSIF symbol # 0 THEN
          Term.WrLn("Warning: unparsed input remaining");
        END;
        RETURN;
      | PDATrans.ActKind.Error =>
        Term.WrLn("Syntax Error");
        RETURN;
      ELSE
        <* ASSERT FALSE *>
      END;
    END;
  END Test;

BEGIN
END PDA.
