(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE LoadSpec;
IMPORT ExtSection;
IMPORT TokSpec;
IMPORT LexParse, YaccParse;
IMPORT TextList;
IMPORT TextSubs;
IMPORT Sym;
IMPORT Rule;
IMPORT RuleList;
IMPORT CharCodes;
IMPORT Rd;
IMPORT TextIntTbl;
IMPORT TextTextTbl;
IMPORT Wr, Thread, Process;
FROM Stdio IMPORT stderr;
<* FATAL Wr.Failure, Thread.Alerted *>
REVEAL
  T = Public BRANDED OBJECT
    i: Info;
    tok: TokSpec.T;
  OVERRIDES
    init := Init;
    setTarget := SetTarget;
    readSpec := ReadSpec;
    get := Get;
  END;

PROCEDURE Error(message: TEXT) =
  BEGIN
    Wr.PutText(stderr, "LoadSpec: " & message & "\n");
    Process.Exit(1);
  END Error;

PROCEDURE Init(self: T): T =
  BEGIN
    self.i := Info{types := NEW(TextTextTbl.Default).init(),
                   procs := NEW(TextTextTbl.Default).init(),
                   tokMN := NIL,
                   methMN := NIL,
                   outMN := NIL,
                   kind := '\000',
                   orig := NIL, tokOrig := NIL,
                   allocTypes := NEW(TextTextTbl.Default).init(),
                   retType := NIL,
                   argCount := NEW(TextIntTbl.Default).init()};
    self.tok := NIL;
    RETURN self;
  END Init;

PROCEDURE AddTypes(self: T; base: TEXT; t: TextList.T) =
  VAR
    cur := t;
    orig: TEXT;
  BEGIN
    WHILE cur # NIL DO
      IF self.i.types.put(cur.head, base) THEN
        EVAL self.i.types.get(cur.head, orig);
        Error("type " & CharCodes.Q(cur.head) & " defined in interfaces" &
          CharCodes.Q(base) & " and " & CharCodes.Q(orig));
      END;
      cur := cur.tail;
    END;
  END AddTypes;

PROCEDURE ReadTok(self: T; from: Rd.T; base: TEXT) =
  VAR
    tok := NEW(TokSpec.T).init();
  BEGIN
    IF self.tok # NIL THEN
      Error(CharCodes.Q(base) & " is the second token interface");
    END;
    tok.read(from);
    self.tok := tok;
    self.i.tokMN := base;
    self.i.kind := 't';
    AddTypes(self, base, tok.varTokens);
  END ReadTok;

PROCEDURE MethFile(self: T; base: TEXT; kind: CHAR) =
  BEGIN
    IF self.tok = NIL THEN
      Error("'.t' file must be given first");
    END;
    IF self.i.methMN # NIL THEN
      Error("multiple '.l'/'.y' files; confusion about what T should be");
    END;
    self.i.methMN := base;
    self.i.kind := kind;
  END MethFile;

PROCEDURE ReadLex(self: T; from: Rd.T; base: TEXT) =
  VAR
    procform := ExtSection.GetText('l', ExtSection.T.Proc);
    lex: LexParse.T;
    cur: TextList.T;
    subs: TextSubs.T;
  BEGIN
    MethFile(self, base, 'l');
    lex := LexParse.New(from, self.tok);
    cur := lex.names;
    WHILE cur # NIL DO
      subs := NEW(TextSubs.T).init();
      subs.add("\\\n","");
      subs.add("%name", cur.head);
      EVAL self.i.procs.put(cur.head, subs.apply(procform));
      cur := cur.tail;
    END;
  END ReadLex;

PROCEDURE ReadYacc(self: T; from: Rd.T; base: TEXT) =
  VAR
    procform := ExtSection.GetText('y', ExtSection.T.Proc);
    yacc: YaccParse.T;
    cur: RuleList.T;
    types: TextList.T := NIL;
    typeTab := NEW(TextTextTbl.Default).init();
    iter: TextTextTbl.Iterator;
    name, dummy: TEXT;
  BEGIN
    self.i.retType := NEW(TextTextTbl.Default).init();
    MethFile(self, base, 'y');
    yacc := NEW(YaccParse.T).init(from, self.tok, base);
    cur := yacc.getRules();
    WHILE cur # NIL DO
      EVAL self.i.procs.put(cur.head.name,
                            Rule.Format(cur.head, procform, TRUE));
      EVAL typeTab.put(Sym.GetName(cur.head.return), NIL);
      EVAL self.i.retType.put(cur.head.name, Sym.GetName(cur.head.return));
      EVAL self.i.argCount.put(cur.head.name, Rule.CountParams(cur.head));
      cur := cur.tail;
    END;
    self.i.allocTypes := typeTab;
    iter := typeTab.iterate();
    WHILE iter.next(name, dummy) DO
      types := TextList.Cons(name, types);
    END;
    AddTypes(self, base, types);
    self.i.orig := yacc.fmtTypes("  Original_%name = " & base &
                       ".Original_%name;\n", FALSE);
    self.i.tokOrig := self.tok.fmtOrig(base);
    
  END ReadYacc;

PROCEDURE ReadSpec(self: T; from: Rd.T; base: TEXT; kind: CHAR) =
  BEGIN
    CASE kind OF
    | 't' => ReadTok(self, from, base);
    | 'l' => ReadLex(self, from, base);
    | 'y' => ReadYacc(self, from, base);
    ELSE
      Error(" unknown extension character: " & CharCodes.QC(kind));
    END;
  END ReadSpec;

PROCEDURE SetTarget(self: T; base: TEXT) =
  BEGIN self.i.outMN := base; END SetTarget;

PROCEDURE Get(self: T): Info =
  BEGIN RETURN self.i; END Get;

BEGIN
END LoadSpec.
