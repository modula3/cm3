(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObliqOnline;
IMPORT Rd, SynWr, Stdio, SynLocation, Text, SynScan, MetaParser, Fmt, SynParse, ObTree, ObValue, ObPrintValue, ObEval, ObCommand, Env, Pathname, Obliq, ObliqParser, ObliqPrinter, ObLibOnline, ObLibOnlineHelp, OSError, FileRd, RTProcess;

(* ============ Online flags ============ *)

CONST
  Version = 1; Enhancement = 1; BugFix = 0;
  DefaultPrintDepth = 4;

PROCEDURE ShowVersion(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(SynWr.out, self.name & " "
	  & Fmt.Int(Version) & "." 
	  & Fmt.Int(Enhancement) & "."
	  & Fmt.Int(BugFix));
	SynWr.NewLine(SynWr.out);
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END ShowVersion;

VAR showAfterParsing: BOOLEAN := FALSE;

PROCEDURE ShowAfterParsing(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(SynWr.out, self.name & " {On Off} is ");
	IF showAfterParsing THEN SynWr.Text(SynWr.out, "On");
	ELSE SynWr.Text(SynWr.out, "Off"); END;
	SynWr.NewLine(SynWr.out);
      ELSIF Text.Equal(arg, "On") THEN showAfterParsing:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN showAfterParsing:=FALSE;
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END ShowAfterParsing;

PROCEDURE ShowNetObjMsgs(self: ObCommand.T; arg: TEXT; <*UNUSED*>data: REFANY:=NIL) =
    BEGIN
      IF Text.Equal(arg, "!") OR Text.Equal(arg, "?") THEN
	SynWr.Text(SynWr.out, self.name & " {On Off} is ");
	IF ObValue.showNetObjMsgs THEN SynWr.Text(SynWr.out, "On");
	ELSE SynWr.Text(SynWr.out, "Off"); END;
	SynWr.NewLine(SynWr.out);
      ELSIF Text.Equal(arg, "On") THEN ObValue.showNetObjMsgs:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN ObValue.showNetObjMsgs:=FALSE;
      ELSE
	SynWr.Text(SynWr.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	SynWr.NewLine(SynWr.out);
      END;
    END ShowNetObjMsgs;

(* 
PROCEDURE PrintAfterParsing(term: ObTree.Term; env: Obliq.Env) =
  BEGIN
    IF showAfterParsing THEN
      SynWr.Beg(SynWr.out, 2);
      SynWr.Text(SynWr.out, "Parsed term: ");
      SynWr.Break(SynWr.out);
      ObPrintTree.PrintTerm(SynWr.out, term, env.checkEnv, 100);
      SynWr.End(SynWr.out);
      SynWr.NewLine(SynWr.out); 
      SynWr.Flush(SynWr.out);
    END;
  END PrintAfterParsing;
*)

(* ============ Do it  ============ *)

REVEAL T = 
  TPublic BRANDED OBJECT
    parser: SynParse.T;
  END;

PROCEDURE New(greetings: TEXT:=""; swr: SynWr.T:=NIL; 
    loadDotObliq: BOOLEAN:=TRUE; env: Obliq.Env := NIL):T =
    VAR interpreter: T; filename: Pathname.T; rd: Rd.T;
  BEGIN
    IF swr=NIL THEN swr:=SynWr.out END;
    IF env=NIL THEN env:=Obliq.EmptyEnv() END;
    
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
      EXCEPT OSError.E =>
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

PROCEDURE Setup() =
<*FATAL SynParse.Fail*>
BEGIN
  (* Thread.IncDefaultStackSize(64*1024); *)
  (* mainThread := Thread.Self(); *)

  SynLocation.PackageSetup();
  SynParse.PackageSetup();
  MetaParser.PackageSetup();
  Obliq.PackageSetup();
  ObliqParser.PackageSetup();
  ObliqPrinter.PackageSetup();
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
