(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ExtBody.m3,v 1.2 2001-09-19 15:14:22 wagner Exp $ *)

MODULE ExtBody;
IMPORT FileRdErr;
IMPORT Scan;
IMPORT ExtSection;
IMPORT Override;
IMPORT BracedCode;
IMPORT CharRange;
IMPORT LoadSpec;
IMPORT TextSubs;
IMPORT TextReader;
IMPORT Pragma;
IMPORT PragmaRead;
IMPORT Text;
IMPORT TextTextTbl;
IMPORT Wr, TextWr;
IMPORT Rd, TextRd;
IMPORT Thread;
IMPORT Import;
IMPORT FloatMode, Lex;
IMPORT Fmt;
IMPORT InitGen;
(* IMPORT Term; *)

<* FATAL Rd.EndOfFile, Rd.Failure, Wr.Failure, Thread.Alerted *>
<* FATAL FloatMode.Trap, Lex.Error *>

TYPE
  Self = OBJECT
    secondPass: TextSubs.T;
    type, proc: Override.T;
    pragSubs: TextTextTbl.T;
    spec: LoadSpec.Info;
    typeExt := "";
  END;

  SelfPragma = Pragma.T OBJECT
    self: Self;
    key: TEXT;
  END;

PROCEDURE PrintAlloc(self: Self; body: TEXT; VAR cur: INTEGER; wr: Wr.T) =
  VAR
    rd := TextRd.New(body);
    start, len: INTEGER;
    name: TEXT;
  BEGIN
    (* hopefully only lexers use this *)
    Rd.Seek(rd, cur);
    WHILE Rd.GetChar(rd) IN CharRange.AlphaNum DO END; Rd.UnGetChar(rd);
    WHILE Rd.GetChar(rd) IN CharRange.WhiteSpace DO END; Rd.UnGetChar(rd);
    start := Rd.Index(rd);
    TRY
      WHILE Rd.GetChar(rd) IN CharRange.AlphaNum DO END; Rd.UnGetChar(rd);
    EXCEPT
      Rd.EndOfFile =>
    END;
    len := Rd.Index(rd) - start;
    name := Text.Sub(body, start, len);
    EVAL self.spec.allocTypes.put(name, NIL);
    Wr.PutText(wr, "VAR result:" & name & ":=NewPT(self.allocate_" & name &
      ",TYPECODE(" & name & "));BEGIN BEGIN\n    ");
    Wr.PutText(wr, FixTheBody(self, BracedCode.GetAhead(rd), 0, NIL));
    Wr.PutText(wr, "\n   END;RETURN result;END");
    cur := Rd.Index(rd);
  END PrintAlloc;

PROCEDURE FixTheBody(self: Self; body: TEXT; argCount: INTEGER;rd:Rd.T): TEXT =
  VAR
    argUsed := NEW(REF ARRAY OF BOOLEAN, argCount+1);
    cur, last, save: INTEGER := 0;
    c: CHAR;
    wr := TextWr.New();
    pre: TEXT := "";
  PROCEDURE DoVal() =
    BEGIN
      IF cur = Text.Length(body) OR Text.GetChar(body, cur) # '.' THEN
        Wr.PutText(wr, ".val");
      END;
    END DoVal;
  BEGIN
    FOR i := 1 TO LAST(argUsed^) DO argUsed[i] := FALSE; END;
    WHILE cur # Text.Length(body) DO
      cur := Text.FindChar(body, '$', last);
      IF cur = -1 THEN
        cur := Text.Length(body);
      END;
      Wr.PutText(wr, Text.Sub(body, last, cur-last));
      IF cur < Text.Length(body) THEN
        INC(cur);
        IF cur = Text.Length(body) THEN
          c := ' ';
        ELSE
          c := Text.GetChar(body, cur);
        END;
        CASE c OF
        | '1'..'9' =>
          save := cur;
          Wr.PutChar(wr, 'n');
          WHILE cur < Text.Length(body) AND
            Text.GetChar(body, cur) IN CharRange.Digit DO
            Wr.PutChar(wr, Text.GetChar(body, cur));
            INC(cur);
          END;
          save := Scan.Int(Text.Sub(body, save, cur-save));
          IF save > argCount THEN
            FileRdErr.E(rd, "parameter out of range: $" & Fmt.Int(save));
          END;
          argUsed[save] := TRUE;
          DoVal();
        | 'R' =>
          PrintAlloc(self, body, cur, wr);
        | '$' =>
          INC(cur);
          Wr.PutText(wr, "result");
          DoVal();
        ELSE
          Wr.PutText(wr, "self.getText()");
        END;
        last := cur;
      END;
    END;
    FOR i := 1 TO argCount DO
      IF NOT argUsed[i] THEN
        pre := pre & "EVAL n" & Fmt.Int(i) & ";";
      END;
    END;
    RETURN pre & TextWr.ToText(wr);
  END FixTheBody;

PROCEDURE ProcSubs(self: Self; form, body: TEXT; argCount: INTEGER;
                   rd: Rd.T): TEXT =
  VAR
    subs := NEW(TextSubs.T).init();
  BEGIN
    (*    Term.WrLn("ProcSubs: " & form & "/" & body); *)
    subs.add("\\\n", "");
    subs.add("%body", FixTheBody(self, body, argCount, rd));
    subs.add("%yaccName", self.spec.methMN);
    RETURN subs.apply(form);
  END ProcSubs;

PROCEDURE ParseProc(p: SelfPragma; rd: Rd.T) =
  VAR
    self := p.self;
    name, mn, bodyform := "";
    c: CHAR;
    frag: TEXT;
    argCount: INTEGER;
  BEGIN
    WHILE Rd.GetChar(rd) IN CharRange.WhiteSpace DO END;
    Rd.UnGetChar(rd);
    REPEAT
      c := Rd.GetChar(rd);
      IF c IN CharRange.AlphaNum THEN name := name & Text.FromChar(c);END;
    UNTIL NOT c IN CharRange.AlphaNum;
    IF c = '{' THEN Rd.UnGetChar(rd);END;
    frag := BracedCode.GetAhead(rd);
    IF c = ':' THEN
      IF Text.Length(frag) # 0 THEN
        EVAL self.spec.types.get(name, mn);
        self.type.add(name, "  " & name & " = " & mn & "." & name &
          " BRANDED \"" & self.spec.outMN & "." & name & "\"" &
          " OBJECT\n    " & frag & "\n  END;\n", rd);
        frag := InitGen.Get("\n    result.%name :=%val;",frag);
        IF self.spec.kind # 'y' THEN
          IF NOT Text.Equal(frag, "") THEN
            FileRdErr.E(rd,"Token fields cannot be automatically initialized");
          END;
        END;
        self.secondPass.add("(*%TYPEINIT%" & name & "%*)",frag);
      END;
      IF self.spec.kind#'l' THEN self.typeExt := "_" & name;END;
    ELSE
      IF Text.Length(frag) # 0 THEN
        name := name & self.typeExt;
        EVAL self.spec.procs.get(name, bodyform);
        argCount := 0;
        EVAL self.spec.argCount.get(name, argCount);
        self.proc.add(name, ProcSubs(self, bodyform, frag, argCount,rd), rd);
      END;
    END;    
  END ParseProc;

PROCEDURE ExtraOver(self: Self) =
  VAR
    argCount: INTEGER;
    proc, body, type: TEXT;
    iter := self.spec.procs.iterate();
  BEGIN
    IF self.spec.kind = 'y' THEN
      WHILE iter.next(proc, body) DO
        IF NOT self.proc.overridden(proc) THEN
          EVAL self.spec.retType.get(proc, type);
          EVAL self.spec.argCount.get(proc, argCount);
          IF self.type.overridden(type) THEN
            self.proc.add(proc,
                          ProcSubs(self, body,
                                   "(* just allocating the new type *)",
                                   argCount, NIL), NIL);
          END;
        END;
      END;
    END;
  END ExtraOver;

PROCEDURE ParseSubs(p: SelfPragma; rd: Rd.T) =
  VAR
    prev: TEXT;
  BEGIN
    EVAL p.self.pragSubs.get(p.key, prev);
    EVAL p.self.pragSubs.put(p.key, prev & BracedCode.Match(rd));
    Rd.UnGetChar(rd);
    (* PragmaRead will eat the '}' *)
  END ParseSubs;

PROCEDURE Parse(from: Rd.T; READONLY spec: LoadSpec.Info): T =
  VAR
    prag := NEW(PragmaRead.T).init();
    subs := NEW(TextSubs.T).init();
    self := NEW(Self,
                secondPass := NEW(TextSubs.T).init(),
                type := NEW(Override.T).init(spec.types),
                proc := NEW(Override.T).init(spec.procs),
                spec := spec,
                pragSubs := NEW(TextTextTbl.Default).init());
    parseProc := NEW(SelfPragma, self := self, do := ParseProc);
    import := NEW(Import.T).init();
  PROCEDURE SubsPrags() =
    VAR
      pragma := ExtSection.GetText(spec.kind, ExtSection.T.Pragma);
      cur := NEW(TextReader.T).init(pragma).shatter("\t\n ","",TRUE);
      parseSubs: SelfPragma;
    BEGIN
      WHILE cur # NIL DO
        EVAL self.pragSubs.put(cur.head, "");
        parseSubs := NEW(SelfPragma, self := self,
                         key := cur.head, do := ParseSubs);
        prag.add(parseSubs, cur.head);
        cur := cur.tail;
      END;
    END SubsPrags;
  PROCEDURE PragSubs() =
    VAR
      iter := self.pragSubs.iterate();
      key, value: TEXT;
    BEGIN
      WHILE iter.next(key, value) DO
        subs.add(key, value);
      END;
    END PragSubs;
  PROCEDURE AllocFmt(form: TEXT): TEXT =
    VAR
      subs: TextSubs.T;
      iter := self.spec.allocTypes.iterate();
      name, dummy: TEXT;
      wr := TextWr.New();
    BEGIN
      WHILE iter.next(name, dummy) DO
        subs := NEW(TextSubs.T).init();
       (* subs.add("%tok", self.spec.tokMN); *)
        subs.add("%name", name);
        Wr.PutText(wr, subs.apply(form));
      END;
      RETURN TextWr.ToText(wr);
    END AllocFmt;
  BEGIN
    prag.add(parseProc, "%proc");
    prag.add(parseProc, "");
    SubsPrags();      (* add pragmas that substitute text *)
    prag.apply(from); (* read input *)
    PragSubs();       (* make substitutions collected by SubsPrags *)
    ExtraOver(self);  (* override remaining procs returning ext types *)
    subs.add("\\\n","");
    subs.add("%gen","(* generated by kext *)");
    self.type.importRemaining();
    subs.add("%gnTypes", self.type.getText());
    subs.add("%gnProcs", self.secondPass.apply(self.proc.getText()));
    subs.add("%ovr", self.proc.getProcAssignText());
    subs.add("%tok", spec.tokMN);
    subs.add("%name", spec.outMN);
    subs.add("%orig", spec.orig);
    subs.add("%tokOrig", spec.tokOrig);
    subs.add("%tkimp", subs.apply(ExtSection.Res("extform.tokimport.i3")));
    import.addModule(spec.tokMN);
    IF spec.methMN # NIL THEN
      import.addModule(spec.methMN);
      subs.add("%meth", spec.methMN);
    END;
    subs.add("%import", import.toDeclaration());
   subs.add("%alloc",AllocFmt("    allocate_%name: Allocator := NIL;\n"));
    subs.add("%purge",AllocFmt("\n      + Purge(self.allocate_%name)"));
    RETURN subs;
  END Parse;

BEGIN
END ExtBody.
