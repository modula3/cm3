(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Jun  3 12:47:10 1994 by luca                   *)
(*      modified on Mon Jun 29 19:17:19 1992 by knaff          *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:54:30 1997
 *)

MODULE SynParse;
IMPORT Text, SynWr, SynLocation, Fmt, SynScan, TextRefTbl;

REVEAL 
  T = 
    TPublic BRANDED "SynParse.T" OBJECT
      sc: SynScan.T;
      env: GrammarEnv;
      failedName: TEXT;
    OVERRIDES
      Grammar:=GetGrammarEnv; Scanner:=GetScanner; Writer:=GetWriter;
      ReadNonTerminal:=ReadNonTerminal; Read:=Read; Lookup:=Lookup; 
      Add:=Add; Extend:=Extend; ExtendIter:=ExtendIter;      
    END;

  Grammar = Tree BRANDED "SynParse.Grammar" OBJECT END ;
  GrammarEnvRoot = 
    BRANDED "SynParse.GrammarEnvRoot" OBJECT
      table: TextRefTbl.T;
    END;

TYPE
  ParGram =
    BRANDED "SynParse.ParGram" OBJECT
    grammar : Grammar;
    args: Args ;
  END;

PROCEDURE Setup() =
  BEGIN 
    noArgs := NEW(REF ARRAY OF INTEGER,0);
  END Setup;

VAR setupDone := FALSE;

PROCEDURE PackageSetup() =
  BEGIN 
    IF NOT setupDone THEN
      setupDone := TRUE;
      SynLocation.PackageSetup();
      SynScan.Setup();
      Setup();
    END;
  END PackageSetup;

PROCEDURE New(swr: SynWr.T; env: GrammarEnv; 
              stackSize: CARDINAL := 10240): T =
  BEGIN
    RETURN 
      NEW(T, stack := NEW(REF ARRAY OF Tree, stackSize),
          sc := SynScan.New(swr), env:=env, failedName := "");
  END New;

PROCEDURE Msg(p: T; msg: TEXT) = 
  VAR swr: SynWr.T;
  BEGIN 
    swr := SynScan.GetWriter(p.sc);
    IF NOT Text.Empty(msg) THEN 
      SynWr.Text(swr, msg); 
      SynWr.Char(swr, '\n');
      SynWr.Flush(swr); 
    END; 
  END Msg;

PROCEDURE Fault(p: T; msg: TEXT) RAISES {Fail} = 
  BEGIN 
    Msg(p, msg);
    RAISE Fail;
  END Fault;

(* Default methods returning NIL *) 
PROCEDURE BuildNoAction(<*UNUSED*>self: Action; 
                        <*UNUSED*>g: T; 
                        <*UNUSED*>base: INTEGER; 
                        <*UNUSED*>READONLY info: SynLocation.Info): Tree =
  BEGIN
    RETURN NIL 
  END BuildNoAction;

PROCEDURE BuildNoGivenKeyword(<*UNUSED*>self: GivenKeyword;
                              <*UNUSED*>g: T; 
                              <*UNUSED*>READONLY info: SynLocation.Info):
  Tree = 
  BEGIN
    RETURN NIL
  END BuildNoGivenKeyword;

PROCEDURE BuildNoGivenIdentifier(<*UNUSED*>self: GivenIdentifier; 
                                 <*UNUSED*>g: T;
                                 <*UNUSED*>READONLY info: SynLocation.Info):
  Tree = 
  BEGIN
    RETURN NIL
  END BuildNoGivenIdentifier;

PROCEDURE BuildNoGivenName(<*UNUSED*>self: GivenName;
                           <*UNUSED*>g: T;
                           <*UNUSED*>READONLY info: SynLocation.Info): Tree =
  BEGIN
    RETURN NIL
  END BuildNoGivenName;

PROCEDURE BuildNoGivenDelimiter(<*UNUSED*>self: GivenDelimiter; 
                                <*UNUSED*>g: T;
                                <*UNUSED*>READONLY info: SynLocation.Info):
  Tree = 
  BEGIN
    RETURN NIL
  END BuildNoGivenDelimiter;

PROCEDURE BuildNoIdentifier(<*UNUSED*>self: Identifier; 
                            <*UNUSED*>g: T; 
                            <*UNUSED*>name: TEXT; 
                            <*UNUSED*>READONLY info: SynLocation.Info): Tree =
  BEGIN
    RETURN NIL
  END BuildNoIdentifier;

PROCEDURE BuildNoName(<*UNUSED*>self: Name;
                      <*UNUSED*>g: T;
                      <*UNUSED*>name: TEXT;
                      <*UNUSED*>READONLY info: SynLocation.Info): Tree =
  BEGIN
    RETURN NIL
  END BuildNoName;

PROCEDURE BuildNoQuotedChar(<*UNUSED*>self: QuotedChar;
                            <*UNUSED*>g: T; 
                            <*UNUSED*>char: CHAR;
                            <*UNUSED*>READONLY info: SynLocation.Info): Tree =
  BEGIN
    RETURN NIL
  END BuildNoQuotedChar;

PROCEDURE BuildNoInteger(<*UNUSED*>self: Integer;
                         <*UNUSED*>g: T; 
                         <*UNUSED*>int: INTEGER; 
                         <*UNUSED*>READONLY info: SynLocation.Info): Tree =
  BEGIN
    RETURN NIL
  END BuildNoInteger; 

PROCEDURE BuildNoReal(<*UNUSED*>self: Real; 
                      <*UNUSED*>g: T; 
                      <*UNUSED*>real: LONGREAL;
                      <*UNUSED*>READONLY info: SynLocation.Info): Tree =
  BEGIN 
    RETURN NIL
  END BuildNoReal; 

PROCEDURE BuildNoQuotedString(<*UNUSED*>self: QuotedString;
                              <*UNUSED*>g: T; 
                              <*UNUSED*>string: TEXT; 
                              <*UNUSED*>READONLY info: SynLocation.Info): 
  Tree = 
  BEGIN 
    RETURN NIL
  END BuildNoQuotedString; 

PROCEDURE BuildNoEof(<*UNUSED*>self: Eof; 
                     <*UNUSED*>g: T; 
                     <*UNUSED*>READONLY info: SynLocation.Info): Tree = 
  BEGIN 
    RETURN NIL
  END BuildNoEof;

  PROCEDURE GetScanner(g: T): SynScan.T =
  BEGIN
    RETURN g.sc
  END GetScanner;

  PROCEDURE GetWriter(g: T): SynWr.T =
  BEGIN
    RETURN SynScan.GetWriter(g.sc);
  END GetWriter;

  PROCEDURE GetGrammarEnv(g: T): GrammarEnv =
  BEGIN
    RETURN g.env
  END GetGrammarEnv;

  PROCEDURE ReadNonTerminal(g: T; named: TEXT)
      : Tree RAISES {Fail, SynScan.Fail, SynScan.NoReader} =
  VAR args: Args;
  BEGIN
    RETURN g.Read(g.Lookup(named, (*out*) args));
    (* -- should have zero arguments. *)
  END ReadNonTerminal;

  PROCEDURE Read(g: T; gram: Grammar; base: INTEGER:=0)
      : Tree RAISES {Fail, SynScan.NoReader} =
    VAR max: INTEGER; tree: Tree; failed: Grammar;
    BEGIN
      max:=0;
      tree:=Read1(g, gram, base, (*in-out*)max, (*out*)failed);
      IF failed#NIL THEN 
	Reset(g, base+max); 
        Error(g, failed); 
        SynScan.SyntaxMsg(g.sc);
        RAISE Fail;
      END;
      RETURN tree;
    END Read;

  (* To be called when Read fails to reset the parse state without
     giving an error message. Set stackLevel=base+max, for the base given
     to, and the max returned by, Read. *)
  PROCEDURE Reset(g: T; stackLevel: INTEGER) =
    BEGIN
      FOR i:=0 TO stackLevel DO g.stack[i]:=NIL END;
    END Reset;

  (* To be called when Read fails, to give an error message. 
     Should be followed by SynScan.SyntaxMsg(); RAISE Fail. *)
  PROCEDURE Error(g: T; failed: Grammar) =
    VAR info: SynLocation.Info; swr: SynWr.T;
    BEGIN
      swr := SynScan.GetWriter(g.sc);
      SynScan.CurrentLocationInfo(g.sc, (*out*)info);
      SynWr.Text(swr, "Parsing " & g.failedName & " ", loud:=TRUE); 
	SynLocation.PrintLocation(swr, failed.location);
	SynWr.Char(swr, '\n', loud:=TRUE);
      SynWr.Flush(swr, loud:=TRUE);
    END Error;

PROCEDURE NewEnv(): GrammarEnv =
  BEGIN
    RETURN NEW(GrammarEnv, table:=NEW(TextRefTbl.Default).init(100));
  END NewEnv;

PROCEDURE List(item1,item2,item3,item4,item5,item6,item7,item8, 
    item9, item10, item11, item12, item13, item14, item15, item16,
    item17, item18, item19, item20: Grammar:=NIL; 
  rest: GrammarList:=NIL): GrammarList =
  VAR list: GrammarList;
  BEGIN
    list:=rest;
    IF item20#NIL THEN list:=NEW(GrammarList, first:=item20, rest:=list) END;
    IF item19#NIL THEN list:=NEW(GrammarList, first:=item19, rest:=list) END;
    IF item18#NIL THEN list:=NEW(GrammarList, first:=item18, rest:=list) END;
    IF item17#NIL THEN list:=NEW(GrammarList, first:=item17, rest:=list) END;
    IF item16#NIL THEN list:=NEW(GrammarList, first:=item16, rest:=list) END;
    IF item15#NIL THEN list:=NEW(GrammarList, first:=item15, rest:=list) END;
    IF item14#NIL THEN list:=NEW(GrammarList, first:=item14, rest:=list) END;
    IF item13#NIL THEN list:=NEW(GrammarList, first:=item13, rest:=list) END;
    IF item12#NIL THEN list:=NEW(GrammarList, first:=item12, rest:=list) END;
    IF item11#NIL THEN list:=NEW(GrammarList, first:=item11, rest:=list) END;
    IF item10#NIL THEN list:=NEW(GrammarList, first:=item10, rest:=list) END;
    IF item9#NIL THEN list:=NEW(GrammarList, first:=item9, rest:=list) END;
    IF item8#NIL THEN list:=NEW(GrammarList, first:=item8, rest:=list) END;
    IF item7#NIL THEN list:=NEW(GrammarList, first:=item7, rest:=list) END;
    IF item6#NIL THEN list:=NEW(GrammarList, first:=item6, rest:=list) END;
    IF item5#NIL THEN list:=NEW(GrammarList, first:=item5, rest:=list) END;
    IF item4#NIL THEN list:=NEW(GrammarList, first:=item4, rest:=list) END;
    IF item3#NIL THEN list:=NEW(GrammarList, first:=item3, rest:=list) END;
    IF item2#NIL THEN list:=NEW(GrammarList, first:=item2, rest:=list) END;
    IF item1#NIL THEN list:=NEW(GrammarList, first:=item1, rest:=list) END;
    RETURN list;
  END List;

  PROCEDURE Store(position: INTEGER; grammar: Grammar): Grammar =
  BEGIN
    RETURN NEW(Storage, item:=grammar, position:=position);
  END Store;

PROCEDURE VerifyArgs(g: T; args1,args2: Args; name: TEXT) RAISES {Fail}=
  BEGIN
    IF NUMBER(args1^) # NUMBER(args2^) THEN
      SynScan.SyntaxMsg(g.sc, "Bad number of arguments: "&name);
      RAISE Fail;
    END;
    FOR i:= 0 TO NUMBER(args1^)-1 DO
      IF args1^[i] # args2^[i] THEN
        SynScan.SyntaxMsg(g.sc, 
          "Arguments number "&Fmt.Int(i)&" differ: "&name);
        RAISE Fail;
      END;
    END;
  END VerifyArgs;

PROCEDURE Lookup(g: T; name: TEXT; VAR (*out*) args: Args): Grammar 
    RAISES {Fail}=
    VAR
      value : REFANY;
    BEGIN
      IF NOT g.env.table.get(name, (*out*) value) THEN 
        SynScan.SyntaxMsg(g.sc, "Unbound non-terminal: "&name);
        RAISE Fail;
      END;
      TYPECASE value OF 
      | NULL =>
          SynScan.SyntaxMsg(g.sc, 
           "Non-Terminal bound to Nil object: "&name);
          RAISE Fail;
      | ParGram(node) => 
        args := node.args;
        RETURN node.grammar ;
      ELSE
        SynScan.SyntaxMsg(g.sc, 
         "Non-Terminal not bound to ParGram object: "&name);
        RAISE Fail;
      END;
    END Lookup; 
      
  PROCEDURE Add(g: T;
                name: TEXT;
                grammar: Grammar; 
                args: Args := NIL) RAISES {Fail} =
  VAR 
    value: REFANY;
    newParGram: ParGram;
  BEGIN
    IF args = NIL THEN args := noArgs END;
    IF g.env.table.get(name, (*VAR OUT*) value) THEN 
	SynScan.SyntaxMsg(g.sc, "Duplicated non-terminal: "&name);
        RAISE Fail;
    END;
    newParGram := NEW(ParGram,
                      grammar := grammar,
                      args := args);
    EVAL g.env.table.put(name, newParGram);
  END Add;

  PROCEDURE Extend(g: T;
                   name: TEXT; 
                   grammar: Grammar;
                   args: Args := NIL) RAISES {Fail} =
  VAR 
    args2: Args;
    grammar2: Grammar;
  BEGIN
    IF args = NIL THEN args := noArgs END;
    grammar2 := g.Lookup(name,args2);
    VerifyArgs(g, args ,args2, name);
    TYPECASE grammar2 OF
    | Choice(node) => IF node.choice=NIL THEN RETURN END
    ELSE
    END;
    EVAL 
      g.env.table.put(
            name,
            NEW(ParGram,
                args := args2 ,
                grammar := NEW(Choice,
                               choice:=
                                 NEW(GrammarList, 
                                     first:=grammar2, 
                                     rest:= NEW(GrammarList, 
                                                first:=grammar, 
                                                rest:= NIL)))));
  END Extend;

  PROCEDURE ExtendIter(g: T;
                       name: TEXT; 
                       iterPosPresent: BOOLEAN; 
                       iterPos: INTEGER; 
                       grammar: Grammar;
                       args: Args := NIL) RAISES {Fail} =
  VAR
    args2: Args;
  BEGIN
    IF args = NIL THEN args := noArgs END;
    TYPECASE g.Lookup(name,args2) OF
    | Iter(node) =>
      VerifyArgs(g, args, args2, name);
      IF iterPosPresent AND (iterPos#node.accumPosition) THEN
        SynScan.SyntaxMsg(g.sc, "Does not mach iteration position: _"
          & Fmt.Int(iterPos));
        RAISE Fail;
      END;
	node.iter :=
           NEW(Choice, choice:=
             NEW(GrammarList, first:=node.iter, rest:=
                                       NEW(GrammarList, first:=grammar, rest:=
                                                                 NIL)));
      ELSE
        SynScan.SyntaxMsg(g.sc, "Not a grammar iteration: "&name);
        RAISE Fail;
      END;
    END ExtendIter;

  (* Parse according to the given gram/env. The base should
     be the current stack level (usually 0); max should be 0.
     If parsing fails it returns failed#NIL; then Reset
     should be called, followed by either "SynScan.Reset()" or 
     "Error(failed); SynScan.SyntaxMsg(); RAISE Fail" *)
  PROCEDURE Read1(g: T; gram: Grammar;
	base: INTEGER; VAR (*in-out*) max: INTEGER;
    	VAR (*out*) failed: Grammar; name: TEXT:=NIL): Tree 
    	RAISES {Fail, SynScan.NoReader} =
    VAR tree: Tree;
    BEGIN
      IF name = NIL THEN
        TYPECASE( gram ) OF
          | NonTerminal(node) => name := node.name ;
        ELSE
            name := "toplevel";
        END;
      END;      
      TRY
	(* base is in-out so the stack can be cleaned up properly
	   even on Fail exceptions occurring during parsing. *)
	tree:=Read0(g, gram, (*in-out*)base, 0, (*in-out*)max, 
	  (*out*)failed, name);
      EXCEPT SynScan.Fail, Fail =>
	Reset(g, base+max);
        RAISE Fail;
      END;
      RETURN tree;
    END Read1;

  PROCEDURE Read0(g: T; gram: Grammar;
	VAR (*in-out*) base: INTEGER; 
	  (* Base of the current stack frame *)
	minWrite: INTEGER;
	  (* Min writable index in the stack frame, for Choose and Iter *)
	VAR (*in-out*) max: INTEGER; 
          (* Max index used so far in the stack frame *)
    	VAR (*out*) failed: Grammar; name: TEXT:=NIL): Tree 
    	RAISES {Fail, SynScan.Fail, SynScan.NoReader} =
  (*  A NIL result means that a client Build did not care about
      generating a parse grammar. *)
  BEGIN
    TYPECASE gram OF
    | NonTerminal(node) =>
        VAR
          grammar: Grammar;
          args: Args;
          tree: Tree;
          saveBase, saveMax: INTEGER;
        BEGIN
          saveBase := base; saveMax := max;
          INC(base,max);
          max := 0;
          (* look up subgrammar *)
          grammar := g.Lookup(node.name,args);
          (* check bounds *)
          IF NUMBER(args^) # NUMBER(node.args^) THEN
            Fault(g, "Bad argument count calling "&node.name);
          END;
          (* copy the arguments *)
          FOR i:= 0 TO NUMBER(node.args^)-1 DO
            g.stack[base+args^[i]] := g.stack[saveBase+node.args^[i]];
            IF args^[i] > max THEN
              max := args^[i]+1;
            END;
          END;
          tree := 
            Read0(g, grammar, 
                  (*in-out*)base, 0, (*in-out*) max,
                  (*out*) failed, node.name);
          FOR i:=0 TO max-1 DO g.stack[base+i]:=NIL END;
          base := saveBase; max := saveMax;
          IF failed#NIL THEN RETURN NIL; END;
          RETURN tree;
        END;
    | Storage(node) =>
        VAR
          tree: Tree;
        BEGIN
	  tree := 
	    Read0(g, node.item, (*in-out*)base, 0, (*in-out*)max, 
	      (*out*)failed, name);
	  IF failed#NIL THEN RETURN NIL END;
	  IF node.position<0 THEN
	    Fault(g, "Invalid index: _" & Fmt.Int(node.position));
	  END;
	  IF node.position<minWrite THEN
	    Fault(g, "Invalid index: _" & Fmt.Int(node.position) & 
	      " is too small to be storeable within this" &
	      " choice or iteration branch");
	  END;
	  IF g.stack[base+node.position] # NIL THEN
	    Fault(g, "Redefinition of: _" & Fmt.Int(node.position));
	  END;
	  g.stack[base+node.position] := tree;
	  max := MAX(max, node.position+1);
	  RETURN NIL;
	END;
     | Action(node) =>
        VAR
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  EVAL 
	    Read0(g, node.grammar, (*in-out*)base, 0, (*in-out*)max, 
	      (*out*)failed, name);
	  IF failed#NIL THEN RETURN NIL END;
	  RETURN node.Build(node, g, base, (*in*)locInfo);
	END;
     | GivenKeyword(node) =>
        VAR
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.HaveTokenKey(g.sc, node.key) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, (*in*)locInfo);
	END;
     | GivenIdentifier(node) =>
        VAR
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.HaveTokenIde(g.sc, node.ide) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, (*in*)locInfo);
	END;
     | GivenName(node) =>
        VAR
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.HaveTokenName(g.sc, node.text) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, (*in*)locInfo);
	END;
     | GivenDelimiter(node) =>
        VAR
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.HaveTokenDelim(g.sc, node.delim) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, (*in*)locInfo);
	END;
    | Identifier(node) =>
        VAR
          ide: TEXT;
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.GetTokenIde(g.sc, (*out*)ide) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, ide, (*in*)locInfo);
	END;
    | Name(node) =>
        VAR
          text: TEXT;
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.GetTokenName(g.sc, (*out*)text) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, text, (*in*)locInfo);
	END;
     | Eof(node) =>
        VAR
          locInfo: SynLocation.Info;
        BEGIN
          SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
          IF SynScan.GetTokenEof(g.sc) THEN failed:=NIL
          ELSE failed:=gram; g.failedName:=name; RETURN NIL;
          END;
          RETURN node.Build(node, g, (*in*) locInfo);
        END;
     | QuotedChar(node) =>
        VAR
          char: CHAR;
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.GetTokenChar(g.sc, (*out*)char) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, char, (*in*)locInfo);
	END;
     | Integer(node) =>
        VAR
          int: INTEGER; 
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.GetTokenInt(g.sc, (*out*)int) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, int, (*in*)locInfo);
	END;
     | Real(node) =>
        VAR
          real: LONGREAL;
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.GetTokenReal(g.sc, (*out*)real) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, real, (*in*)locInfo);
	END;
     | QuotedString(node) =>
        VAR
          string: TEXT;
          locInfo: SynLocation.Info;
        BEGIN
	  SynScan.CurrentLocationInfo(g.sc, (*out*)locInfo);
	  IF SynScan.GetTokenString(g.sc, (*out*)string) THEN failed:=NIL
	  ELSE failed:=gram; g.failedName:=name; RETURN NIL;
	  END;
	  RETURN node.Build(node, g, string, (*in*)locInfo);
	END;
     | Sequence(node) =>
        VAR 
          gramList: GrammarList;
        BEGIN
          gramList := node.items;
          failed := NIL;
          LOOP
            IF gramList=NIL THEN EXIT END;
            EVAL 
	      Read0(g, gramList.first, (*in-out*)base, 0, (*in-out*)max, 
	        (*out*)failed, name);
	    IF failed#NIL THEN EXIT END;
	    gramList := gramList.rest; 
          END;
        END;
	RETURN NIL;
    | Choice(node) =>
        VAR
          tree: Tree;
          list: GrammarList; 
          scanPoint: INTEGER;
          saveMax: INTEGER;
        BEGIN
	  list := node.choice;
	  saveMax := max;
	  LOOP
	    IF list=NIL THEN 
	      failed := gram; g.failedName:=name; RETURN NIL;
	    END;
	    scanPoint := SynScan.ScanPoint(g.sc);
	    tree := 
	      Read0(g, list.first, (*in-out*) base, saveMax, (*in-out*) max, 
	        (*out*) failed, name);
	    FOR i:=saveMax TO max-1 DO g.stack[base+i]:=NIL END;
	    max := saveMax;
	    IF failed=NIL THEN RETURN tree END;
	    IF failed#NIL AND scanPoint#SynScan.ScanPoint(g.sc) 
	    THEN RETURN NIL 
	    END;
	    list := list.rest;
	  END;
	END;
    | Iter(node) =>
        VAR
          tree: Tree;
          scanPoint: INTEGER;
          saveMax: INTEGER;
        BEGIN
          tree := 
	    Read0(g, node.base, (*in-out*)base, 0, (*in-out*)max, 
	      (*out*) failed, name);
	  IF failed#NIL THEN RETURN NIL END;
	  IF node.accum THEN
	    IF node.accumPosition<0 THEN
	      Fault(g, "Invalid index: _" & Fmt.Int(node.accumPosition));
	    END;
	    IF node.accumPosition<minWrite THEN
	      Fault(g, "Invalid index: _" & Fmt.Int(node.accumPosition) & 
	        " is too small to be storeable within this" &
	        " choice or iteration branch");
	    END;
	    g.stack[base+node.accumPosition] := tree;
	    max := MAX(max, node.accumPosition+1);
	  END;
	  saveMax := max;
	  LOOP
	    scanPoint := SynScan.ScanPoint(g.sc);
	    tree := 
	      Read0(g, node.iter, (*in-out*)base, saveMax, (*in-out*)max, 
	        (*out*)failed, name);
	    FOR i:=saveMax TO max-1 DO g.stack[base+i]:=NIL END;
	    max := saveMax;
	    IF failed#NIL AND scanPoint#SynScan.ScanPoint(g.sc) 
	    THEN RETURN NIL 
	    END;
	    IF failed#NIL THEN
	      failed:=NIL; 
	      IF node.accum THEN
	        RETURN g.stack[base+node.accumPosition];
	      ELSE RETURN NIL;
	      END;
	    END;
	    IF node.accum THEN
	      g.stack[base+node.accumPosition] := tree;
	    END;
	  END;
	END;
    ELSE 
      Fault(g, "SynParse.Read0");
      <*ASSERT FALSE*>
   END;
  END Read0;

BEGIN
END SynParse.

(*

      UndoAdd(name: TEXT) RAISES {Fail};
        (* Remove a definition, and give an error if not found. *)

  PROCEDURE UndoAdd(g: T; name: TEXT) RAISES {Fail} =
  VAR 
    value: REFANY;
  BEGIN
    IF NOT g.env.table.delete(name, (*VAR OUT*) value) THEN
      SynScan.ErrorMsg(g.sc, "GrammarEnv.UndoAdd: could not find: "&name);
      RAISE Fail;
    END;
  END UndoAdd;

      UndoExtend(name: TEXT; grammar: Grammar) RAISES {Fail};
        (* Undo an extension, or give an error if not possible. *)

  PROCEDURE UndoExtend(g: T; name: TEXT; grammar: Grammar)
    RAISES {Fail} =
  VAR 
    args: Args;
  BEGIN
    TYPECASE grammar OF
    | Choice(node) => IF node.choice=NIL THEN RETURN END
    ELSE
    END;
    TYPECASE g.Lookup(name,args) OF
    | Choice(node) =>
	IF grammar # node.choice.rest.first THEN
          SynScan.ErrorMsg(g.sc, "GrammarEnv.UndoExtend: bad undo: "&name);
          RAISE Fail;
	END;
        EVAL 
          g.env.table.put(name, 
                        NEW(ParGram,
                            args := args,
                            grammar := node.choice.first));
    ELSE 
      SynScan.ErrorMsg(g.sc, "GrammarEnv.UndoExtend failed: "&name);
      RAISE Fail;
    END;
  END UndoExtend;

      UndoExtendIter(name: TEXT; grammar: Grammar) RAISES {Fail};
        (* Undo an Iter extension, or give anerror if not possible. *)

  PROCEDURE UndoExtendIter(g: T; name: TEXT; grammar: Grammar)
    RAISES {Fail} =
  VAR
    args: Args;
  BEGIN
    TYPECASE g.Lookup(name,args) OF
    | Iter(iterNode) =>
	TYPECASE iterNode.iter OF
	| Choice(choiceNode) =>
	    IF grammar # choiceNode.choice.rest.first THEN
              SynScan.ErrorMsg(g.sc, 
                               "GrammarEnv.UndoExtendIter: bad undo: "&name);
              RAISE Fail;
	    END;
            iterNode.iter:=choiceNode.choice.first;
	ELSE 
          SynScan.ErrorMsg(g.sc, "GrammarEnv.UndoExtendIter failed: "&name);
          RAISE Fail;
	END;
    ELSE 
      SynScan.ErrorMsg(g.sc, "GrammarEnv.UndoExtendIter failed: "&name);
      RAISE Fail;
    END;
  END UndoExtendIter;

*)
