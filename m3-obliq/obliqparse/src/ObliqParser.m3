
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObliqParser;
IMPORT Thread, Rd, TextRd, Bundle, ObliqBdl, SynWr, SynScan, SynParse, MetaParser, ObParseFrame, ObTree, ObValue, ObParseTree, ObFrame, Obliq, SynLocation, ObErr, Process;
FROM ObValue IMPORT Error, Exception;

  VAR metaParserMutex: Thread.Mutex;
      obliqClauseList: MetaParser.ClauseList;

  VAR setupDone := FALSE;

  PROCEDURE PackageSetup(wr: SynWr.T) =
  BEGIN
    SynParse.PackageSetup();
    TRY
      MetaParser.PackageSetup(wr); (* NOWARN *)
    EXCEPT
    | SynParse.Fail =>
      Process.Crash("Fatal error trying to parse MetaSyn grammar");
    END;
    Obliq.PackageSetup(wr);
    
    IF NOT setupDone THEN
      setupDone := TRUE;
      ObFrame.Setup();
      ObParseTree.Setup();
      ObParseFrame.Setup();
      metaParserMutex := NEW(Thread.Mutex);
      obliqClauseList := NIL;
    END;
  END PackageSetup;

  PROCEDURE New(swr: SynWr.T): SynParse.T =
  VAR parser: SynParse.T; actions: MetaParser.ActionTable;
  BEGIN
    LOCK metaParserMutex DO
      TRY
        IF obliqClauseList = NIL THEN
          actions := MetaParser.NewActionTable();
          ObParseFrame.RegisterActions(actions);
          ObParseTree.RegisterActions(actions); (* NOWARN *)
          obliqClauseList := 
              MetaParser.NewClauseList(actions, "obliq.gr",
                                       TextRd.New(Bundle.Get(ObliqBdl.Get(),
                                                             "ObliqGram")));
        END;
        parser := SynParse.New(swr, SynParse.NewEnv());
        MetaParser.AddClauseList(obliqClauseList, parser);
      EXCEPT
      | SynParse.Fail, SynScan.Fail, SynScan.NoReader =>
        Process.Crash("Failed trying to parse Obliq grammar.");
      END;
    END;
    SynScan.SetPrompt(parser.Scanner(), "- ", "  ");
    RETURN parser;
  END New;

  PROCEDURE ReadFrom(p: T; rdName: TEXT; rd: Rd.T; closeRd: BOOLEAN;
    generateEOF: BOOLEAN := TRUE) =
  BEGIN
    SynScan.PushInput(p.Scanner(), rdName, rd, closeRd, generateEOF);    
  END ReadFrom;
  
  PROCEDURE ParseTerm(p: SynParse.T): Obliq.Term RAISES {Error, Eof} =
  BEGIN
      TRY
        TYPECASE p.ReadNonTerminal("phrase") OF
        | ObTree.PhraseTerm(node) =>
          RETURN node.term;
        ELSE
          SynParse.Fault(p,
            "ObliqParser.ParseTerm: parsed a phrase that is not a term");
          <*ASSERT FALSE*>
        END;
      EXCEPT
      | SynScan.NoReader =>
          RAISE Eof;    
      | SynParse.Fail, SynScan.Fail => 
          Obliq.RaiseError("Static Error");
          <*ASSERT FALSE*>
      END;
  END ParseTerm;

  PROCEDURE ParsePhrase(p: SynParse.T): Obliq.Phrase RAISES {Error, Eof} =
  BEGIN
      TRY
        RETURN p.ReadNonTerminal("phrase");
      EXCEPT
      | SynScan.NoReader =>
          RAISE Eof;    
      | SynParse.Fail, SynScan.Fail => 
          Obliq.RaiseError("Static Error");
          <*ASSERT FALSE*>
      END;
  END ParsePhrase;

  PROCEDURE EvalPhrase(p: SynParse.T; phrase: Phrase; VAR (*in-out*) env: Env; 
    loc: Location:=NIL): Obliq.Val(*or NIL*) RAISES {Error, Exception} =
  VAR val: ObValue.Val;
  BEGIN
    IF loc=NIL THEN loc:=SourceLocation("Obliq.EvalPhrase") END;
    val := NIL;
    TRY
      TYPECASE phrase OF
      | NULL =>
      | ObFrame.Quit => Process.Exit();
      | ObFrame.Load(node) =>
	    ObFrame.LoadFile(p.Scanner(), node.name);
      | ObFrame.Import(node) =>
	    ObFrame.ImportFrame(p.Scanner(), node.name, env);
      | ObFrame.Module(node) =>
	    ObFrame.ModuleFrame(p.Scanner(), node.name, node.for,
	      node.imports, env);
      | ObFrame.AddHelp(node) =>
	    ObFrame.AddHelpFrame(node.name, node.sort, node.short,
                                 node.long, env); 
      | ObFrame.EndModule(node) =>
	    ObFrame.ModuleEnd(p.Scanner(), node.ideList);
      | ObFrame.Establish(node) =>
	    env := ObFrame.EstablishFrame(p.Writer(), node.name, 
                                          node.for, env);
      | ObFrame.Save(node) =>
	    env := ObFrame.SaveFrame(p.Writer(), node.name, node.name, env);
      | ObFrame.Delete(node) =>
	    env := ObFrame.DeleteFrame(p.Writer(), node.name, env);
      | ObFrame.Qualify(node) =>
	    env := ObFrame.QualifyFrame(p.Writer(), env, node.ideList);
      | ObTree.PhraseCommand, 
        ObTree.PhraseTerm =>
          val := Obliq.EvalPhrase(p.Writer(), phrase, (*in-out*) env, loc);
      ELSE
        Obliq.RaiseError("Static Error, unknown phrase", loc);
      END;
      RETURN val;
    EXCEPT
    | ObErr.Fail => 
        Obliq.RaiseError("Static Error", loc);
        <*ASSERT FALSE*>
    END;
  END EvalPhrase;

  PROCEDURE SourceLocation(where: TEXT): SynLocation.T =
  BEGIN
    RETURN
      SynLocation.NewLineLocation(
        SynLocation.Info{fileName:=where, line:=0, lineChar:=0, char:=0});
  END SourceLocation;

BEGIN
END ObliqParser.
