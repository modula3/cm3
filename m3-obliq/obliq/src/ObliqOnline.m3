(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Jul  7 21:52:08 1998
 *)

MODULE ObliqOnline;
IMPORT Rd, SynWr, Stdio, SynLocation, Text, SynScan, MetaParser, Fmt,
       SynParse, ObTree, ObValue, ObPrintValue, ObEval, ObCommand,
       Env, Pathname, Obliq, ObliqParser, ObliqPrinter, ObLibOnline,
       ObLibOnlineHelp, OSError, FileRd, RTProcess, Process,
       ParseParams, TextRd;

(* ============ Online flags ============ *)

CONST
  Version = 2; Enhancement = 2; BugFix = 1;
  DefaultPrintDepth = 4;

PROCEDURE ShowVersion(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(wr, self.name & " "
	  & Fmt.Int(Version) & "." 
	  & Fmt.Int(Enhancement) & "."
	  & Fmt.Int(BugFix));
	SynWr.NewLine(wr);
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END ShowVersion;

VAR showAfterParsing: BOOLEAN := FALSE;

PROCEDURE ShowAfterParsing(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(wr, self.name & " {On Off} is ");
	IF showAfterParsing THEN SynWr.Text(wr, "On");
	ELSE SynWr.Text(wr, "Off"); END;
	SynWr.NewLine(wr);
      ELSIF Text.Equal(arg, "On") THEN showAfterParsing:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN showAfterParsing:=FALSE;
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END ShowAfterParsing;

PROCEDURE ShowNetObjMsgs(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(wr, self.name & " {On Off} is ");
	IF ObValue.showNetObjMsgs THEN SynWr.Text(wr, "On");
	ELSE SynWr.Text(wr, "Off"); END;
	SynWr.NewLine(wr);
      ELSIF Text.Equal(arg, "On") THEN ObValue.showNetObjMsgs:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN ObValue.showNetObjMsgs:=FALSE;
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END ShowNetObjMsgs;

PROCEDURE TraceExecution(wr: SynWr.T; self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(wr, self.name & " {On Off} is ");
	IF ObEval.traceExecution THEN SynWr.Text(wr, "On");
	ELSE SynWr.Text(wr, "Off"); END;
	SynWr.NewLine(wr);
      ELSIF Text.Equal(arg, "On") THEN ObEval.traceExecution:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN ObEval.traceExecution:=FALSE;
      ELSE
	SynWr.Text(wr, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(wr);
      END;
    END TraceExecution; 

(* 
PROCEDURE PrintAfterParsing(wr: SynWr.T; term: ObTree.Term; env: Obliq.Env) =
  BEGIN
    IF showAfterParsing THEN
      SynWr.Beg(wr, 2);
      SynWr.Text(wr, "Parsed term: ");
      SynWr.Break(wr);
      ObPrintTree.PrintTerm(wr, term, env.checkEnv, 100);
      SynWr.End(wr);
      SynWr.NewLine(wr); 
      SynWr.Flush(wr);
    END;
  END PrintAfterParsing;
*)

(* ============ Do it  ============ *)

REVEAL T = 
  TPublic BRANDED "ObliqOnline.T" OBJECT
    parser: SynParse.T;
  END;

PROCEDURE New(swr: SynWr.T; greetings: TEXT:=""; 
    loadDotObliq: BOOLEAN:=TRUE; env: Obliq.Env := NIL):T =
    VAR interpreter: T; filename: Pathname.T; rd: Rd.T;
  BEGIN
    IF swr=NIL THEN swr:=SynWr.out END;
    IF env=NIL THEN env:=Obliq.EmptyEnv(swr) END;
    
    interpreter := NEW(T, env:=env, 
                       swr:=swr, parser:=ObliqParser.New(swr));
    ObLibOnline.RegisterScanner(interpreter.parser.Scanner());

    IF NOT Text.Empty(greetings) THEN
      SynWr.Text(interpreter.swr, "\n" & greetings & "\n\n");
      SynWr.Flush(interpreter.swr);
    END;

    IF loadDotObliq THEN
      TRY
        filename := Pathname.Join(Env.Get("HOME"), ".obliq", NIL);
        rd := FileRd.Open(filename);
        SynWr.Text(interpreter.swr, "Loading '" & filename & "'\n");
        SynWr.Flush(interpreter.swr);
        Interact(interpreter, filename, rd, TRUE, TRUE);
        SynWr.Flush(interpreter.swr);

        WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
          WHILE pp.keywordPresent("-load") DO
            WITH filename = pp.getNext() DO
              rd := TextRd.New("load " & filename & ";\n");
              Interact(interpreter, filename, rd, TRUE, TRUE);
              SynWr.Flush(interpreter.swr);
            END;
          END;
        END;
      EXCEPT OSError.E, ParseParams.Error =>
      END;
    END;

    RETURN interpreter;
  END New;

PROCEDURE Interact(interpreter: T; rdName: TEXT:=""; rd: Rd.T:=NIL; 
    closeRd: BOOLEAN:=FALSE; generateEOF: BOOLEAN := TRUE) =
  VAR
    oldEnv: Obliq.Env;
    phrase: ObTree.Phrase;
    value: ObValue.Val; 
    printDepth: INTEGER;
  BEGIN
    IF rd=NIL THEN rd:=Stdio.stdin END;
    ObliqParser.ReadFrom(interpreter.parser, rdName, rd, 
      closeRd, generateEOF);    
    LOOP
      TRY
        SynScan.FirstPrompt(interpreter.parser.Scanner());
        phrase := ObliqParser.ParsePhrase(interpreter.parser);
        oldEnv := interpreter.env;
        value := 
          ObliqParser.EvalPhrase(interpreter.parser, phrase, 
            (*in-out*) interpreter.env);
        TYPECASE phrase OF
        | NULL =>
        | ObTree.PhraseTerm(node) =>
            IF node.printDepth >=0 THEN
              printDepth := node.printDepth;
            ELSIF SynScan.TopLevel(interpreter.parser.Scanner()) THEN 
              printDepth:=DefaultPrintDepth; 
            ELSE 
              printDepth:=0;
            END;
	    TYPECASE node.term OF
	    | ObTree.TermLet(node) =>
	      ObPrintValue.PrintPhraseLet(interpreter.swr, 
	        interpreter.env.checkEnv, oldEnv.checkEnv,
	        interpreter.env.valueEnv, oldEnv.valueEnv, node.var, 
                interpreter.env.libEnv, printDepth+1);
	    ELSE
	      ObPrintValue.PrintVal(interpreter.swr, 
	        value, interpreter.env.libEnv, 
	        interpreter.env.checkEnv, printDepth);
                SynWr.NewLine(interpreter.swr);
	    END;
	ELSE
        END;
	SynWr.Flush(interpreter.swr);
      EXCEPT
      | ObliqParser.Eof => 
          EXIT;
      | ObValue.Error(packet) => 
          IF NOT Text.Equal(packet.msg, "Static Error") THEN
            ObValue.ErrorMsg(interpreter.swr, packet);
            ErrorDetectedMsg(interpreter.parser, packet.location);
          END;
      | ObValue.Exception(packet) => 
          ObValue.ExceptionMsg(interpreter.swr, packet);
          ErrorDetectedMsg(interpreter.parser, packet.location);
      END;
    END;
  END Interact;

PROCEDURE ErrorDetectedMsg(parser: SynParse.T; loc: SynLocation.T) =
  VAR currInfo: SynLocation.Info; sc: SynScan.T; swr: SynWr.T;
  BEGIN
    sc := parser.Scanner();
    swr := SynScan.GetWriter(sc);
    SynScan.CurrentLocationInfo(sc, (*out*)currInfo);
    IF Text.Empty(currInfo.fileName) THEN
      SynWr.Text(swr, "Error detected ", loud:=TRUE);
      SynLocation.PrintLineDifference(swr, loc, currInfo.line);
      SynWr.NewLine(swr, loud:=TRUE);
    END;
    SynWr.Flush(swr, loud:=TRUE);
  END ErrorDetectedMsg;

(* ============ Setup  ============ *)

(* VAR mainThread: Thread.T; *)

PROCEDURE OnInterrupt() =
  BEGIN
    (* Thread.Alert(mainThread); *)
    ObEval.interrupt := TRUE;
  END OnInterrupt;

PROCEDURE SignalSetup() =
  VAR old: RTProcess.InterruptHandler;
  BEGIN
    old := RTProcess.OnInterrupt(OnInterrupt);
  END SignalSetup;

PROCEDURE Setup(swr: SynWr.T) =
BEGIN
  (* Thread.IncDefaultStackSize(64*1024); *)
  (* mainThread := Thread.Self(); *)

  SynLocation.PackageSetup();
  SynParse.PackageSetup();
  TRY
    MetaParser.PackageSetup(swr); (* NOWARN *)
  EXCEPT
  | SynParse.Fail =>
    Process.Crash("Fatal error trying to parse MetaSyn grammar");
  END;
  Obliq.PackageSetup(swr);
  ObliqParser.PackageSetup(swr);
  ObliqPrinter.PackageSetup(swr);
  SignalSetup();

  ObCommand.Register(ObTree.doCommandSet,
    NEW(ObCommand.T, name:="Version", sortingName:=" Version",
        Exec:=ShowVersion));
      
  showAfterParsing := FALSE;
  ObCommand.Register(ObTree.doCommandSet,
    NEW(ObCommand.T, name:="ShowParsing", sortingName:="ShowParsing", 
        Exec:=ShowAfterParsing));

  ObCommand.Register(ObTree.doCommandSet,
    NEW(ObCommand.T, name:="ShowNetObjMsgs", sortingName:="ShowNetObjMsgs", 
        Exec:=ShowNetObjMsgs));

  ObCommand.Register(ObTree.doCommandSet,
    NEW(ObCommand.T, name:="TraceExecution", sortingName:="TraceExecution", 
        Exec:=TraceExecution));

    ObLibOnline.Setup();
  ObLibOnlineHelp.Setup();
END Setup;

BEGIN
END ObliqOnline.

(* Old SIGINT handling.
*** Non-portable: this is the MIPS version
PROCEDURE OnSIGINT(sig, code: Ctypes.int; 
  scp: UNTRACED REF Usignal.struct_sigcontext) =
  BEGIN
    (* Thread.Alert(mainThread); *)
    ObEval.interrupt := TRUE;
  END OnSIGINT;

*** Non-portable: this is the SPARC version
PROCEDURE OnSIGINT(sig, code: Ctypes.int; 
  scp: UNTRACED REF Usignal.struct_sigcontext;
  (* SPARC *) addr: ADDRESS) =
  BEGIN
    (* Thread.Alert(mainThread); *)
    ObEval.interrupt := TRUE;
  END OnSIGINT;
 
PROCEDURE SignalSetup() =
  VAR vec, ovec: Usignal.struct_sigvec;
  BEGIN
    vec.sv_handler := OnSIGINT;
    vec.sv_mask := Usignal.empty_sv_mask;
    vec.sv_flags := 0;
    EVAL Usignal.sigvec(Usignal.SIGINT, (*var*)vec, (*var*)ovec);
  END SignalSetup;
*)
