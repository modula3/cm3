(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE TokSpec;
IMPORT Rd;
IMPORT CharRange;
IMPORT CharCodes;
IMPORT Text;
IMPORT TextSubs;
IMPORT TextList;
IMPORT TextIntTbl;
IMPORT IntTextTbl;
IMPORT Thread;
IMPORT FileRdErr;
IMPORT TextReader;
IMPORT Pragma, PragmaRead;
IMPORT Wr, TextWr;
(* IMPORT Stdio; *)
<* FATAL Rd.EndOfFile, Rd.Failure, Wr.Failure, Thread.Alerted *>
REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    init := Init;
    read := Read;
    error := Error;
    fmtVar := FmtVar;
    fmtOrig := FmtOrig;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    self.tokens := NIL;
    self.varTokens := NIL;
    self.charTokens := CharRange.NoChars;
    self.lastConstCode := ORD(LAST(CHAR));
    self.constTokens := NEW(TextIntTbl.Default).init();
    self.constTokensR := NEW(IntTextTbl.Default).init();
    RETURN self;
  END Init;

TYPE
  TokPragma = Pragma.T OBJECT
    spec: T;
  END;

PROCEDURE ParseCharToken(p: TokPragma; rd: Rd.T) =
  VAR
    self := p.spec;
    line := Rd.GetLine(rd);
    r1 := Text.FindChar(line, '[');
    r2 := Text.FindCharR(line, ']');
  BEGIN
    IF r1 = -1 THEN self.error(rd, "%char: enclose in []"); END;
    IF r2 = -1 THEN self.error(rd, "%char: missing `]'"); END;
    self.charTokens := self.charTokens + 
                           CharRange.FromText(Text.Sub(line, r1, r2-r1+1));
  END ParseCharToken;

PROCEDURE ShatterLine(rd: Rd.T): TextList.T =
  VAR
    line := Rd.GetLine(rd);
    tr := NEW(TextReader.T).init(line);
  BEGIN
(*    Wr.PutText(Stdio.stderr, line & "\n"); *)
    RETURN tr.shatter(" ,", "", TRUE);
  END ShatterLine; 

PROCEDURE ParseConstToken(p: TokPragma; rd: Rd.T) =
  VAR
    self := p.spec;
    cur := ShatterLine(rd);
  BEGIN
    (* DebugPrintList(cur, "Const"); *)
    self.tokens := TextList.Append(self.tokens, cur);
    WHILE cur # NIL DO
      INC(self.lastConstCode);
      EVAL self.constTokens.put(cur.head, self.lastConstCode);
      EVAL self.constTokensR.put(self.lastConstCode, cur.head);
      cur := cur.tail;
    END;
  END ParseConstToken; 

PROCEDURE MakeCharConsts(self: T) =
  VAR
    name: TEXT;
    code: INTEGER;
  BEGIN
    FOR c := FIRST(CHAR) TO LAST(CHAR) DO
      IF c IN self.charTokens THEN
        code := ORD(c);
        name := CharCodes.QC(c);
        EVAL self.constTokens.put(name, code);
        EVAL self.constTokensR.put(code, name);
      END;
    END;
  END MakeCharConsts;

(*
PROCEDURE DebugPrintList(a: TextList.T; m: TEXT) =
  VAR
    cur := a;
  BEGIN
    WHILE cur # NIL DO
      Wr.PutText(Stdio.stderr, m & ": " & cur.head & "\n");
      cur := cur.tail;
    END;
  END DebugPrintList;
*)

PROCEDURE ParseToken(p: TokPragma; rd: Rd.T) =
  VAR
    self := p.spec;
    newTokens := ShatterLine(rd);
  BEGIN
    (* DebugPrintList(newTokens, "VAR"); *)
    self.tokens := TextList.Append(self.tokens, newTokens);
    self.varTokens := TextList.Append(self.varTokens, newTokens);
  END ParseToken;

PROCEDURE CheckDuplicates(self: T) =
  VAR
    cur := self.tokens;
    check := NEW(TextIntTbl.Default).init();
    dummy: INTEGER;
  BEGIN
    WHILE cur # NIL DO
      IF check.get(cur.head, dummy) THEN
        self.error(NIL, "duplicate token: " & CharCodes.Q(cur.head));
      ELSE
        EVAL check.put(cur.head, dummy);
      END;
      cur := cur.tail;
    END;
  END CheckDuplicates;

PROCEDURE Read(self: T; from: Rd.T) =
  VAR
    charToken := NEW(TokPragma, spec := self, do := ParseCharToken);
    const := NEW(TokPragma, spec := self, do := ParseConstToken);
    token := NEW(TokPragma, spec := self, do := ParseToken);
    prag := NEW(PragmaRead.T).init();
  BEGIN
    prag.add(charToken, "%char");
    prag.add(const, "%const");
    prag.add(token, "%token");
    prag.add(token, "");
    prag.apply(from);
    MakeCharConsts(self);
    CheckDuplicates(self);
  END Read;

PROCEDURE Error(<*UNUSED*>self: T; rd: Rd.T; message: TEXT) =
  BEGIN
    FileRdErr.E(rd, message);
  END Error;

PROCEDURE FmtVar(self: T; form: TEXT): TEXT =
  VAR
    cur := self.varTokens;
    wr := TextWr.New();
    subs: TextSubs.T;
  BEGIN
    WHILE cur # NIL DO
      subs := NEW(TextSubs.T).init();
      subs.add("%name", cur.head);
      Wr.PutText(wr, subs.apply(form));
      cur := cur.tail;
    END;
    RETURN TextWr.ToText(wr);
  END FmtVar;

PROCEDURE FmtOrig(self: T; tokMN: TEXT): TEXT =
  VAR
    tokMNa: TEXT;
  BEGIN
    IF tokMN = NIL THEN
      tokMNa := "";
    ELSE
      tokMNa := tokMN & ".Original_";
    END;
    RETURN self.fmtVar("  Original_%name = " &tokMNa& "%name;\n");
  END FmtOrig;

BEGIN
END TokSpec.
