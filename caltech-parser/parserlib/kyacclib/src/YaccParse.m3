(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: YaccParse.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE YaccParse;
IMPORT Pragma, PragmaRead;
IMPORT FileRdErr;
IMPORT Prec;
IMPORT CharCodes;
IMPORT CharRange;
IMPORT Sym;
IMPORT SymList;
IMPORT SymListParse;
IMPORT Rule;
IMPORT RuleList;
IMPORT Text;
IMPORT TextWr;
IMPORT TextSubs;
IMPORT TextReader;
IMPORT TextPrecTbl;
IMPORT TextBooleanTbl;
IMPORT TextIntTbl;
IMPORT TokSpec;
IMPORT Rd, Thread, Process;
IMPORT Wr, Fmt;
FROM Stdio IMPORT stderr;
<*FATAL Rd.EndOfFile, Rd.Failure, Wr.Failure, Thread.Alerted *>

REVEAL
  T = Public BRANDED OBJECT
    tok: TokSpec.T;
    rd: Rd.T;
    rules: RuleList.T := NIL;
    prec: TextPrecTbl.T;    
    (* rulename -> prec OR
       symname -> prec OR
       @char -> prec
    *)
    start: TextBooleanTbl.T; (* symname -> isStart (no entry for nonstart) *)
    codes: TextIntTbl.T;     (* symname -> code *)
    lastPrec := 0;
    yaccName: TEXT;
    isToken: TextBooleanTbl.T := NIL;
  OVERRIDES
    init := Init;
    fmtRules := FmtRules;
    fmtTypes := FmtTypes;
    getRules := GetRules;
    getCodes := GetCodes;
  END;

TYPE
  SelfPragma = Pragma.T OBJECT
    self: T;
    precKind: Prec.Kind;
    returnSym: Sym.T := NIL;
    lastRuleNo: INTEGER := 0;
  END;

PROCEDURE Warn(message: TEXT; fatal: BOOLEAN := FALSE) =
  BEGIN
    Wr.PutText(stderr, "Warning: " & message & "\n");
    IF fatal THEN
      Process.Exit(1);
    END;
  END Warn;

PROCEDURE ParseStart(p: SelfPragma; rd: Rd.T) =
  VAR
    tr := NEW(TextReader.T).init(Rd.GetLine(rd));
    cur := tr.shatter("\t ","",TRUE);
  BEGIN
    IF cur = NIL THEN
      FileRdErr.E(rd, "expected start symbol");
    END;
    WHILE cur # NIL DO
      IF p.self.start.put(cur.head, TRUE) THEN
        FileRdErr.E(rd,"\""& cur.head&"\" already declared a start symbol");
      END;
      cur := cur.tail;
    END;
  END ParseStart;

PROCEDURE ParsePrec(p: SelfPragma; rd: Rd.T) =
  VAR
    cur: SymList.T;
    key: TEXT;
    val: Prec.T;
(*    pos := Rd.Index(rd);
    peekLine := Rd.GetLine(rd);*)
  BEGIN
(*    Term.WrLn("PeekPrec:" & peekLine);
    Rd.Seek(rd, pos); *)
    cur := SymListParse.Parse(rd, p.self.tok.charTokens);
    INC(p.self.lastPrec);
    WHILE cur # NIL DO
      key := Sym.GetName(cur.head);
      val := NEW(Prec.T, kind := p.precKind, val := p.self.lastPrec);
      EVAL p.self.prec.put(key, val);
(*      Term.WrLn("Putting prec: " & key); *)
      cur := cur.tail;
    END;
  END ParsePrec;

PROCEDURE ParseRule(p: SelfPragma; rd: Rd.T) =
  VAR
    self := p.self;
    pos := Rd.Index(rd);
    peekLine := Rd.GetLine(rd);
    i := Text.FindChar(peekLine, ':');
  BEGIN
    IF i = -1 OR NOT Text.GetChar(peekLine, i-1) IN CharRange.AlphaNum THEN
      IF p.returnSym = NIL THEN
        FileRdErr.E(rd, "Missing return symbol");
      END;
      Rd.Seek(rd, pos);
      INC(p.lastRuleNo);
      self.rules := RuleList.Cons(Rule.FromRd(rd, p.returnSym,
                                              self.tok.charTokens,
                                              p.lastRuleNo),
                                  self.rules);
    ELSE
      p.returnSym := Sym.FromText(Text.Sub(peekLine, 0, i));
    END;
  END ParseRule;

PROCEDURE ParseText(self: T) =
  VAR
    prag := NEW(PragmaRead.T).init();
    parseStart := NEW(SelfPragma, self := self, do := ParseStart);
    parseRule := NEW(SelfPragma, self := self, do := ParseRule);
  PROCEDURE PrecType(kind: Prec.Kind; pragName: TEXT) =
    VAR
      parsePrec := NEW(SelfPragma, self := self,
                       precKind := kind,  do := ParsePrec);
    BEGIN
      prag.add(parsePrec, pragName);
    END PrecType;
  BEGIN
    prag.add(parseStart, "%start");
    prag.add(parseRule, "%rule");
    prag.add(parseRule, "");
    PrecType(Prec.Kind.Left, "%left");
    PrecType(Prec.Kind.Right, "%right");
    PrecType(Prec.Kind.None, "%nonassoc");
    prag.apply(self.rd);
    self.rules := RuleList.ReverseD(self.rules);
  END ParseText;

PROCEDURE LookupSyms(self: T) =
  VAR
    cur := self.rules;
    lastCode: INTEGER := self.tok.lastConstCode;
    iter := self.tok.constTokens.iterate();
    constName: TEXT;
    constCode: INTEGER;
  BEGIN
    WHILE iter.next(constName, constCode) DO
      EVAL self.codes.put(constName, constCode);
    END;
    WHILE cur # NIL DO
      Rule.LookupSyms(cur.head, self.prec, self.start, self.codes,
                      self.tok.constTokens, lastCode);
      cur := cur.tail;
    END;
  END LookupSyms;

PROCEDURE CheckPrecs(self: T) =
  VAR
    iter := self.prec.iterate();
    key: TEXT;
    val: Prec.T;
  BEGIN
    WHILE iter.next(key, val) DO
      IF NOT val.used THEN
        Warn("precedence not used: " & key);
      END;
    END;
  END CheckPrecs;

PROCEDURE CheckToks(self: T) =
  VAR
    cur := self.tok.tokens;
    val: INTEGER;
  BEGIN
    WHILE cur # NIL DO
      IF NOT self.codes.get(cur.head, val) THEN
        Warn("token not used: " & cur.head);
      END;
      cur := cur.tail;
    END;
  END CheckToks;

PROCEDURE FmtRules(self: T; form: TEXT): TEXT =
  VAR
    cur := self.rules;
    wr := TextWr.New();
  BEGIN
    WHILE cur # NIL DO
      Wr.PutText(wr, Rule.Format(cur.head, form, cur.tail = NIL));
      cur := cur.tail;
    END;
    RETURN TextWr.ToText(wr);
  END FmtRules;

PROCEDURE CheckSyms(self: T) =
  VAR
    cur := self.tok.tokens;
    curRule := self.rules;
    definedToken := NEW(TextBooleanTbl.Default).init();
    iter := self.codes.iterate();
    iterStart := self.start.iterate();
    key: TEXT;
    val: INTEGER;
    bool: BOOLEAN;
    hasStart := FALSE;
  BEGIN
    WHILE cur # NIL DO
      EVAL definedToken.put(cur.head, TRUE);
      cur := cur.tail;
    END;
    WHILE curRule # NIL DO
      IF curRule.head.length = 1 THEN
        IF Sym.GetCode(curRule.head.return) =
          Sym.GetCode(curRule.head.syms.head) THEN
          Warn(curRule.head.name & " might loop for a while");
        END;
      END;
      hasStart := hasStart OR Sym.IsStart(curRule.head.return);
      key := Sym.GetName(curRule.head.return);
      IF definedToken.get(key, bool) AND bool THEN
        Warn(CharCodes.Q(key) & " is a token");
      END;
      EVAL definedToken.put(key, FALSE);
      curRule := curRule.tail;
    END;
    IF NOT hasStart THEN
      Warn("No start symbols!!", TRUE);
    END;
    WHILE iter.next(key, val) DO
      IF val >= 256 THEN
        IF NOT definedToken.get(key, bool) THEN
          Warn("Grammar symbol not defined: " & CharCodes.Q(key));
        END;
      END;
    END;
    WHILE iterStart.next(key, bool) DO
      IF NOT definedToken.get(key, bool) THEN
        Warn("Start symbol not defined: " & CharCodes.Q(key));
      ELSIF bool THEN
        Warn("Token used as start symbol: " & CharCodes.Q(key));
      END;
    END;
    self.isToken := definedToken;
  END CheckSyms;

PROCEDURE FmtTypes(self: T; form: TEXT; tokenTypes: BOOLEAN): TEXT =
  VAR
    iter := self.codes.iterate();
    key, sup: TEXT;
    val, dummy: INTEGER;
    bool: BOOLEAN;
    wr := TextWr.New();
    subs := NEW(TextSubs.T).init();
  BEGIN
    <* ASSERT self.isToken # NIL *> (* call CheckSyms first *)
    WHILE iter.next(key, val) DO
      bool := FALSE;
      EVAL self.isToken.get(key, bool);
      bool := bool = tokenTypes;
      IF bool AND NOT self.tok.constTokens.get(key, dummy) THEN
        IF self.start.get(key, bool) THEN
          <* ASSERT bool *>
          sup := "StartType";
        ELSE
          sup := "OtherType";
        END;
        subs.add("%name", key);
        subs.add("%sup", sup);
        subs.add("%yacc", self.yaccName);
        subs.add("%code", Fmt.Int(val));
        Wr.PutText(wr, subs.apply(form));
      END;
    END;
    RETURN TextWr.ToText(wr);
  END FmtTypes; 

PROCEDURE Init(self: T; rd: Rd.T; tok: TokSpec.T; name: TEXT): T =
  BEGIN
    self.yaccName := name;
    self.rd := rd;
    self.tok := tok;
    self.prec := NEW(TextPrecTbl.Default).init();
    self.start := NEW(TextBooleanTbl.Default).init();
    self.codes := NEW(TextIntTbl.Default).init();
    ParseText(self);
    LookupSyms(self);
    CheckSyms(self);
    CheckPrecs(self);
    CheckToks(self);
    RETURN self;
  END Init;

PROCEDURE GetRules(self: T): RuleList.T =
  BEGIN RETURN self.rules; END GetRules;

PROCEDURE GetCodes(self: T): TextIntTbl.T =
  BEGIN RETURN self.codes; END GetCodes;

BEGIN
END YaccParse.
