(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Jun  3 12:45:29 1994 by luca                       *)
(*      modified on Thu Jun 25 02:37:59 1992 by knaff          *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:05:10 1997
 *)

MODULE MetaParser ;

IMPORT Constructor, Rd, SynParse, TextRefTbl, 
       SynWr, SynLocation, Builder, SynScan, Fmt,
       Text;

<* FATAL Crash *>

REVEAL
  Grammar = Grammar_public BRANDED "MetaParser.Grammar" OBJECT
    gram: SynParse.Grammar := NIL;
  END; (* object *)

PROCEDURE NewClauseList(actionTable : ActionTable; fileName: TEXT; rd: Rd.T)
    : ClauseList RAISES {SynParse.Fail, SynScan.Fail, SynScan.NoReader} =
  BEGIN
    Builder.actionTable := actionTable;
    SynScan.PushInput(Constructor.metaParser.Scanner(), 
                      fileName, rd, TRUE, TRUE);
    RETURN Constructor.metaParser.Read(Constructor.grammar);
  END NewClauseList;

PROCEDURE AddClauseList(tree: SynParse.Tree; p: SynParse.T) 
    RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE tree OF
    | NULL => RETURN
    | ClauseList(clauseList) =>
      MergeEnv(clauseList, p);
      BeKeywordsOfClauseList(clauseList, 
        SynScan.GetKeywordSet(p.Scanner())); 
    ELSE
      RAISE SynParse.Fail ;
    END;
  END AddClauseList;

PROCEDURE NewParser(swr: SynWr.T;
                    actionTable : ActionTable; fileName: TEXT; rd: Rd.T)
    : SynParse.T 
    RAISES {SynParse.Fail, SynScan.Fail, SynScan.NoReader} =
  VAR clauseList: SynParse.Tree; parser: SynParse.T;
  BEGIN
    clauseList := NewClauseList(actionTable, fileName, rd);
    parser := SynParse.New(swr, SynParse.NewEnv());
    AddClauseList(clauseList, parser);
    RETURN parser;
  END NewParser;

PROCEDURE Setup() =
  BEGIN
  END Setup;

PROCEDURE PackageSetup(wr: SynWr.T) RAISES {SynParse.Fail} =
  BEGIN
    Constructor.Setup(wr);
    Setup();
  END PackageSetup;


PROCEDURE NewActionTable(): ActionTable =
  VAR
    actions: ActionTable;
  BEGIN
    actions := NEW(TextRefTbl.Default).init();

    (* link in default actions *)
    Constructor.LinkInSelects(actions);
    RETURN actions;
  END NewActionTable;

EXCEPTION Crash;

PROCEDURE TableFromArray(<*NOWARN*>sourceTable : ActionProcTable;
                          table: ActionTable ) =
(* transforms an array of (Text, Procedure) pairs into an actionTable *)
VAR
  tmp : REF ActionProc ;

BEGIN
  IF table = NIL THEN
    RAISE Crash ;
  END;

  FOR i := FIRST(sourceTable) TO LAST(sourceTable) DO
    tmp := NEW(REF ActionProc);
    tmp^ := sourceTable[i].proc ;
    EVAL table.put(sourceTable[i].name,tmp );
  END;
END TableFromArray ;

PROCEDURE Register(name: TEXT; proc: ActionProc;
                          table: ActionTable ) =
VAR
  ref : REF ActionProc ;
BEGIN
  IF table = NIL THEN
    RAISE Crash;
  END;
  ref := NEW(REF ActionProc);
  ref^ := proc;
  EVAL table.put(name,ref);
END Register;

PROCEDURE IdentifierToTree(<*UNUSED*>self: SynParse.Identifier; 
                           <*UNUSED*>p: SynParse.T; name: TEXT;
                     READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN 
      NEW( TextTemp,
           location := SynLocation.NewLineLocation(info) ,
           text := name );
  END IdentifierToTree;

PROCEDURE NameToTree(<*UNUSED*>self: SynParse.Name; 
                     <*UNUSED*>p: SynParse.T; name: TEXT;
                     READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN 
      NEW( TextTemp,
           location := SynLocation.NewLineLocation(info) ,
           text := name );
  END NameToTree;


PROCEDURE IntegerToTree(<*UNUSED*>self: SynParse.Integer; 
                        <*UNUSED*>p: SynParse.T; int: INTEGER;
                  READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN
      NEW( IntegerTemp,
           location := SynLocation.NewLineLocation(info) ,
           int := int ) ;
  END IntegerToTree;

PROCEDURE RealToTree(<*UNUSED*>self: SynParse.Real; 
                     <*UNUSED*>p: SynParse.T; real: LONGREAL;
               READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN
      NEW( RealTemp,
           location := SynLocation.NewLineLocation(info) ,
           real := real );
  END RealToTree;

PROCEDURE CharToTree(<*UNUSED*>self: SynParse.QuotedChar; 
                     <*UNUSED*>p: SynParse.T; char: CHAR;
               READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN
      NEW(TextTemp,
          location := SynLocation.NewLineLocation(info) ,
          text := Text.FromChar(char ) );
  END CharToTree;

PROCEDURE StringToTree(<*UNUSED*>self: SynParse.QuotedString; 
                       <*UNUSED*>p: SynParse.T; string: TEXT;
                 READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN
      NEW(TextTemp,
          location := SynLocation.NewLineLocation(info) ,
          text := string);
  END StringToTree;


PROCEDURE TextToTree(<*UNUSED*>self: SynParse.QuotedString; 
                     <*UNUSED*>p: SynParse.T; text: TEXT ;
                 READONLY info: SynLocation.Info): SynParse.Tree =
  BEGIN
    RETURN
      NEW(TextTemp,
          location := SynLocation.NewLineLocation(info) ,
          text := text );
  END TextToTree;

PROCEDURE GInt(p: SynParse.T; loc :INTEGER): INTEGER RAISES {SynParse.Fail}=
  BEGIN
    RETURN
      XInt(p.Writer(), p.stack[loc]);
  END GInt;

PROCEDURE GReal(p: SynParse.T; loc :INTEGER): LONGREAL RAISES {SynParse.Fail}=
  BEGIN
    RETURN
      XReal(p.Writer(), p.stack[loc]);
  END GReal;

PROCEDURE GText(p: SynParse.T; loc :INTEGER): TEXT RAISES {SynParse.Fail}=
  BEGIN
    RETURN
      XText(p.Writer(), p.stack[loc]);
  END GText;


PROCEDURE GBool(p: SynParse.T; loc: INTEGER): BOOLEAN RAISES {SynParse.Fail}=
  BEGIN
    RETURN
      XBool(p.Writer(), p.stack[loc]);
  END GBool;

PROCEDURE XInt(wr: SynWr.T; tree: SynParse.Tree): INTEGER RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE tree OF
    | NULL =>
    | IntegerTemp(node) => RETURN node.int ;
    ELSE
    END;
    TypeError(wr, "Text",tree);
    <*ASSERT FALSE*>
  END XInt;

PROCEDURE XReal(wr: SynWr.T; tree: SynParse.Tree): LONGREAL RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE tree OF
    | NULL =>
    | RealTemp(node) => RETURN node.real ;
    ELSE
    END;
    TypeError(wr, "Text",tree);
    <*ASSERT FALSE*>
  END XReal;

PROCEDURE XText(wr: SynWr.T; tree: SynParse.Tree): TEXT RAISES {SynParse.Fail}=
  BEGIN
    TYPECASE tree OF
    | NULL =>
    | IntegerTemp(node) => RETURN Fmt.Int(node.int);
    | RealTemp(node)    => RETURN Fmt.LongReal(node.real);
    | TextTemp(node)    => RETURN node.text ;
    ELSE
    END;
    TypeError(wr,"Text",tree);
    <*ASSERT FALSE*>
  END XText;

PROCEDURE XBool(wr: SynWr.T; tree: SynParse.Tree): BOOLEAN RAISES {SynParse.Fail}=
  VAR
    text : TEXT;
    array : REF ARRAY OF CHAR;
    l : INTEGER;
  BEGIN
    TYPECASE tree OF
    | NULL =>
    | IntegerTemp(node) => RETURN node.int # 0;
    | RealTemp(node)    => RETURN node.real # 0.0d0 ;
    | TextTemp(node)    => 
      text := node.text;
      l := Text.Length(text);
      array := NEW( REF ARRAY OF CHAR, l );
      FOR i := 0 TO l - 1 DO
        array[i] := Text.GetChar(text,i);
      END;
      text := Text.FromChars(array^);
      IF Text.Equal(text,"on") OR Text.Equal(text,"true") THEN
        RETURN TRUE;
      ELSIF Text.Equal(text,"off") OR Text.Equal(text,"false") THEN
        RETURN FALSE;
      END;
    ELSE
    END;
    TypeError(wr,"Boolean",tree);
    <*ASSERT FALSE*>
  END XBool; 

PROCEDURE TypeError(wr: SynWr.T; type: TEXT; tree: SynParse.Tree) 
  RAISES {SynParse.Fail}=
  BEGIN
    IF tree = NIL THEN
      SynWr.Text(wr, "Got NIL pointer instead of "&type, loud:=TRUE);      
    ELSE      
      SynWr.Text(wr, type & " expected at ", loud:=TRUE);
      SynLocation.PrintLocation(wr, tree.location);      
    END;
    SynWr.Text(wr, "\n", loud:=TRUE);
    SynWr.Flush(wr, loud:=TRUE);
    RAISE SynParse.Fail;
  END TypeError;

  PROCEDURE MergeEnv(list: ClauseList; p: SynParse.T) RAISES {SynParse.Fail} =
    BEGIN
      LOOP
        IF list=NIL THEN RETURN END;
	IF list.extend THEN
	  IF list.extendIter THEN
	    p.ExtendIter(list.ide.text, list.iterPosPresent, list.iterPos,
		list.gram, list.args);
	  ELSE
	    p.Extend(list.ide.text, list.gram, list.args);
	  END;
	ELSE
	  p.Add(list.ide.text, list.gram, list.args);
	END;
        list := list.rest;
      END;
    END MergeEnv;

(*
  PROCEDURE BeKeywords(list: ClauseList; keySet: SynScan.KeywordSet) RAISES {SynParse.Fail} =
    BEGIN
      LOOP
        IF list=NIL THEN RETURN END;
	BeKeywordsOfGram(list.gram, keySet);
        list := list.rest;
      END;
    END BeKeywords;
*)

  PROCEDURE BeKeywordsOfGramList(list: SynParse.GrammarList; keySet: SynScan.KeywordSet) RAISES {SynParse.Fail} =
    BEGIN
      LOOP
        IF list=NIL THEN RETURN END;
	BeKeywordsOfGram(list.first, keySet);
        list := list.rest;
      END;
    END BeKeywordsOfGramList;

    (* AK following procedure added *)
  PROCEDURE BeKeywordsOfClauseList(list: ClauseList; keySet: SynScan.KeywordSet) RAISES {SynParse.Fail} =
    BEGIN
      LOOP
        IF list=NIL THEN RETURN END;
	BeKeywordsOfGram(list.gram, keySet);
        list := list.rest;
      END;
    END BeKeywordsOfClauseList;

  PROCEDURE BeKeywordsOfGram(gram: SynParse.Grammar; keySet: SynScan.KeywordSet) RAISES {SynParse.Fail} =
    BEGIN
      TYPECASE gram OF
      | NULL =>
      | SynParse.NonTerminal =>
      | SynParse.Storage(node) => BeKeywordsOfGram(node.item, keySet);
      | SynParse.Action(node) => BeKeywordsOfGram(node.grammar, keySet);
      | SynParse.GivenKeyword(node) => 
	  EVAL SynScan.BeKeyword(node.key, keySet);
      | SynParse.Identifier, SynParse.Name, SynParse.QuotedChar, 
          SynParse.Integer, SynParse.Real, SynParse.QuotedString, 
          SynParse.GivenDelimiter =>
      | SynParse.Sequence(node) => 
	  BeKeywordsOfGramList(node.items, keySet);
      | SynParse.Choice(node) => BeKeywordsOfGramList(node.choice, keySet);
      | SynParse.Iter(node) =>
	  BeKeywordsOfGram(node.base, keySet); 
	  BeKeywordsOfGram(node.iter, keySet);
      | SynParse.GivenName =>
      | SynParse.Eof =>
      ELSE <*ASSERT FALSE*>
      END;
    END BeKeywordsOfGram;

    PROCEDURE PrintGram(wr: SynWr.T; gram : SynParse.Grammar)=
      BEGIN
      TYPECASE gram OF
      | NULL =>
      | SynParse.NonTerminal(node) =>
        SynWr.Text(wr, node.name);
      | SynParse.Storage(node) => 
        PrintGram(wr, node.item);
        SynWr.Text(wr, Fmt.Int(node.position));
      | SynParse.Action(node) => 
        PrintGram(wr, node.grammar);
        SynWr.Text(wr, " :: Action ");
      | SynParse.GivenKeyword(node) =>
        SynWr.Text(wr, "\"" & node.key & "\"" );
      | SynParse.GivenName(node) =>
        SynWr.Text(wr, "\"~" & node.text & "\"" );
      | SynParse.Identifier =>
        SynWr.Text(wr, " identifier ");
      | SynParse.QuotedChar =>
        SynWr.Text(wr, " quoted char ");
      | SynParse.Integer =>
        SynWr.Text(wr, " integer ");
      | SynParse.Real =>
        SynWr.Text(wr, " real ");
      | SynParse.QuotedString =>
        SynWr.Text(wr, " quoted string ");        
      | SynParse.GivenDelimiter(node) =>
        SynWr.Text(wr, "\' ");
        SynWr.Char(wr, node.delim);
        SynWr.Text(wr, "\' ");
      | SynParse.Sequence(node) =>
        SynWr.Text(wr, " [ "); 
        PrintGramList(wr, node.items);
        SynWr.Text(wr, " ] "); 
      | SynParse.Choice(node) => 
        SynWr.Text(wr, " { "); 
        PrintGramList(wr, node.choice);
        SynWr.Text(wr, " } "); 
      | SynParse.Iter(node) =>
        SynWr.Text(wr, " ( "); 
        PrintGram(wr, node.base);
        SynWr.Text(wr, " * "); 
        PrintGram(wr, node.base);
        SynWr.Text(wr, " ) "); 
      | SynParse.Eof =>
        SynWr.Text(wr, "EOF"); 
      ELSE <*ASSERT FALSE*>
      END;
    END PrintGram;

  PROCEDURE PrintGramList(wr: SynWr.T; list: SynParse.GrammarList) = 
  BEGIN
    WHILE list#NIL DO
      PrintGram(wr, list.first);
      list:=list.rest;
      SynWr.Char(wr, ' ');
    END;
  END PrintGramList;



  PROCEDURE PrintClauseList(wr: SynWr.T; list: ClauseList) =
  BEGIN
    WHILE list#NIL DO
      SynWr.Text(wr, " " & list.ide.text);
      SynWr.Text(wr, "::= " );
      PrintGram(wr, list.gram);
      (* IF list.extend OR list.extendIter THEN
         SynWr.Text(wr, "(extended)");
      END; *)
      list:=list.rest;
      SynWr.Char(wr, '\n');
    END;
  END PrintClauseList;

BEGIN
END MetaParser .

(*

  PROCEDURE UndoMergeEnv(list: ClauseList; p: SynParse.T) 
      RAISES {SynParse.Fail} ;

  PROCEDURE UndoMergeEnv(list: ClauseList; p: SynParse.T) RAISES {SynParse.Fail} =
    BEGIN
      LOOP
        IF list=NIL THEN RETURN END;
	IF list.inserted THEN
	  IF list.extend THEN
	    IF list.extendIter THEN
	      p.UndoExtendIter(list.ide.text, list.gram);
	    ELSE
	      p.UndoExtend(list.ide.text, list.gram);
	    END;
	  ELSE
	    p.UndoAdd(list.ide.text);
	  END;
	END;
	list.inserted := FALSE;
        list := list.rest;
      END;
    END UndoMergeEnv;

 MergeEnv:    list.inserted := TRUE;

   Note that
   AddClauseList has a side-effect on the "inserted" fields of clauseList,
   which are used (only) to undo grammar extensions.
   
*)
