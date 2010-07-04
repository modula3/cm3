(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObLibM3;
IMPORT SynLocation, Text, ObLib, ObValue, Obliq, Rd, Wr, Process, FloatMode,
  Thread, Stdio, Pipe, FileRd, FileWr, OSError, Fmt, Lex, TextRd, TextWr, 
  NetObj, Pickle2 AS Pickle, Word, SharedObj, Random;

  VAR setupDone := FALSE;

  PROCEDURE PackageSetup() =
  BEGIN
    IF NOT setupDone THEN
      setupDone := TRUE;
      Setup();
    END;
  END PackageSetup;

  PROCEDURE Setup() =
  BEGIN
    SetupRd();
    SetupWr();
    SetupLex();
    SetupFmt();
    SetupWord();
    SetupPickle();
    SetupProc();
    SetupRandom();
  END Setup;

(* ============ "rd" package ============ *)

TYPE

  RdCode = {Failure, EofFailure, New, Stdin, Open, GetChar, Eof, UnGetChar, 
            CharsReady, GetText, GetLine, Index, Length, Seek, Close, 
            Intermittent, Seekable, Closed};

  RdOpCode =  
    ObLib.OpCode OBJECT
        code: RdCode;
      END;
    
  PackageRd = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalRd;
      END;

  PROCEDURE IsRd(self: ValRd; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF ValRd(oth)=> RETURN self.rd = oth.rd;
    ELSE RETURN FALSE END;
  END IsRd;

  PROCEDURE CopyRd(self: ObValue.ValAnything; <*UNUSED*>tbl: ObValue.Tbl;
                   <*UNUSED*>loc: SynLocation.T): ObValue.ValAnything =
  BEGIN
    RETURN self;
  END CopyRd;

  VAR rdFailureException, rdEofFailureException: ObValue.ValException;

  PROCEDURE NewRdOC(name: TEXT; arity: INTEGER; code: RdCode)
    : RdOpCode =
  BEGIN
    RETURN NEW(RdOpCode, name:=name, arity:=arity, code:=code);
  END NewRdOC;

  VAR true, false: ObValue.ValBool;

  PROCEDURE SetupRd() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(RdCode));
    opCodes^ :=
      OpCodes{
      NewRdOC("failure", -1, RdCode.Failure), 
      NewRdOC("eofFailure", -1, RdCode.EofFailure), 
      NewRdOC("new", 1, RdCode.New), 
      NewRdOC("stdin", -1, RdCode.Stdin),
      NewRdOC("open", 2, RdCode.Open),
      NewRdOC("getChar", 1, RdCode.GetChar), 
      NewRdOC("eof", 1, RdCode.Eof), 
      NewRdOC("unGetChar", 1, RdCode.UnGetChar), 
      NewRdOC("charsReady", 1, RdCode.CharsReady), 
      NewRdOC("getText", 2, RdCode.GetText), 
      NewRdOC("getLine", 1, RdCode.GetLine), 
      NewRdOC("index", 1, RdCode.Index), 
      NewRdOC("length", 1, RdCode.Length), 
      NewRdOC("seek", 2, RdCode.Seek), 
      NewRdOC("close", 1, RdCode.Close), 
      NewRdOC("intermittent", 1, RdCode.Intermittent), 
      NewRdOC("seekable", 1, RdCode.Seekable), 
      NewRdOC("closed", 1, RdCode.Closed)
      };
    ObLib.Register(
      NEW(PackageRd, name:="rd", opCodes:=opCodes));
    rdFailureException := NEW(ObValue.ValException, name:="rd_failure");
    rdEofFailureException := NEW(ObValue.ValException, name:="rd_eofFailure");
    true := NEW(ObValue.ValBool, bool:=TRUE);
    false := NEW(ObValue.ValBool, bool:=FALSE);
  END SetupRd;

  PROCEDURE EvalRd(self: PackageRd; opCode: ObLib.OpCode; 
                   <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
      temp: BOOLEAN; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
    VAR rd1: Rd.T; text1: TEXT; int1: INTEGER := 0;
        fileSys1: ObValue.ValFileSystem;
    BEGIN
      TRY
        CASE NARROW(opCode, RdOpCode).code OF
        | RdCode.Failure => 
            RETURN rdFailureException;
        | RdCode.EofFailure => 
            RETURN rdEofFailureException;
        | RdCode.New => 
            TYPECASE args[1] OF | ObValue.ValText(node) => text1:=node.text;
            ELSE
              ObValue.BadArgType(1, "text", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            rd1 := TextRd.New(text1);
            RETURN NEW(ValRd, what:="<a reader>", tag:="Reader", picklable:=FALSE, rd:=rd1);
       | RdCode.Stdin => 
            RETURN NEW(ValRd, what:="<stdin reader>", tag:="Reader", picklable:=FALSE, 
              rd:=Stdio.stdin);
       | RdCode.Open => 
            TYPECASE args[1] OF | ObValue.ValFileSystem(node) => fileSys1:=node;
            ELSE 
              ObValue.BadArgType(1, "file system", self.name, opCode.name, 
                                 loc); 
              <*ASSERT FALSE*>
            END;
            TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
            ELSE
              ObValue.BadArgType(2, "text", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            rd1 := fileSys1.remote.OpenRead(text1);
            RETURN NEW(ValRd, what:="<'" & text1 & "' reader>", 
                       tag := "Reader",
                       picklable:=FALSE, rd:=rd1);
        | RdCode.GetChar => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            IF Rd.Closed(rd1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            RETURN NEW(ObValue.ValChar, char:=Rd.GetChar(rd1));
        | RdCode.Eof => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            IF Rd.Closed(rd1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
	    IF Rd.EOF(rd1) THEN RETURN true ELSE RETURN false END;
        | RdCode.UnGetChar => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            IF Rd.Closed(rd1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            Rd.UnGetChar(rd1);
            RETURN ObValue.valOk;
        | RdCode.CharsReady => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            IF Rd.Closed(rd1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            RETURN NEW(ObValue.ValInt, int:=Rd.CharsReady(rd1), temp:=temp);
        | RdCode.GetText => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE
              ObValue.BadArgType(2, "int", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            IF Rd.Closed(rd1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            IF int1<0 THEN
              ObValue.BadArgVal(2, "non-negative", self.name, opCode.name,loc);
              <*ASSERT FALSE*>
            END;
            RETURN ObValue.NewText(Rd.GetText(rd1, int1));
        | RdCode.GetLine => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            IF Rd.Closed(rd1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            RETURN ObValue.NewText(Rd.GetLine(rd1));
        | RdCode.Index => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            IF Rd.Closed(rd1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            RETURN NEW(ObValue.ValInt, int:=Rd.Index(rd1), temp:=temp);
        | RdCode.Length => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            IF Rd.Closed(rd1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            IF Rd.Intermittent(rd1) THEN
              ObValue.BadArgVal(1,"non-intermittent",self.name,
                                opCode.name,loc);
              <*ASSERT FALSE*>
            END;
            RETURN NEW(ObValue.ValInt, int:=Rd.Length(rd1), temp:=temp);
        | RdCode.Seek => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>            
            END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE 
              ObValue.BadArgType(2, "int", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            IF Rd.Closed(rd1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            IF NOT Rd.Seekable(rd1) THEN
              ObValue.BadArgVal(1, "seekable", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            IF int1<0 THEN
              ObValue.BadArgVal(2,"non-negative", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            Rd.Seek(rd1, int1);
            RETURN ObValue.valOk;
        | RdCode.Close => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            Rd.Close(rd1);
            RETURN ObValue.valOk;
        | RdCode.Intermittent => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
	    IF Rd.Intermittent(rd1) THEN RETURN true ELSE RETURN false END;
        | RdCode.Seekable => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
	    IF Rd.Seekable(rd1) THEN RETURN true ELSE RETURN false END;
        | RdCode.Closed => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE 
              ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
	    IF Rd.Closed(rd1) THEN RETURN true ELSE RETURN false END;
        ELSE
          ObValue.BadOp(self.name, opCode.name, loc);<*ASSERT FALSE*>
        END;
      EXCEPT
      | Rd.Failure, ObValue.ServerError =>
          ObValue.RaiseException(rdFailureException, 
                               self.name & "_" & opCode.name, loc);
          <*ASSERT FALSE*>
      | Rd.EndOfFile => 
          ObValue.RaiseException(rdEofFailureException,
                               self.name & "_" & opCode.name, loc);
          <*ASSERT FALSE*>
      | Thread.Alerted =>
          ObValue.RaiseException(ObValue.threadAlerted, 
                               self.name&"_"&opCode.name, loc);
          <*ASSERT FALSE*>
      | NetObj.Error(atoms) =>
          ObValue.RaiseNetException(self.name&"_"&opCode.name, atoms, loc);
          <*ASSERT FALSE*>
      END;
    END EvalRd;

(* ============ "wr" package ============ *)

TYPE

  WrCode = {Failure, New, Stdout, Stderr, Open, OpenAppend, ToText, PutChar, 
            PutText, Flush, Index, Length, Seek, Close, Buffered,
            Seekable, Closed};

  WrOpCode =  
    ObLib.OpCode OBJECT
        code: WrCode;
      END;
    
  PackageWr = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalWr;
      END;

  PROCEDURE IsWr(self: ValWr; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF ValWr(oth)=> RETURN self.wr = oth.wr;
    ELSE RETURN FALSE END;
  END IsWr;

  PROCEDURE CopyWr(self: ObValue.ValAnything; <*UNUSED*>tbl: ObValue.Tbl;
                   <*UNUSED*>loc: SynLocation.T): ObValue.ValAnything =
  BEGIN
    RETURN self;
  END CopyWr;

  VAR wrFailureException: ObValue.ValException;

  PROCEDURE NewWrOC(name: TEXT; arity: INTEGER; code: WrCode)
    : WrOpCode =
  BEGIN
    RETURN NEW(WrOpCode, name:=name, arity:=arity, code:=code);
  END NewWrOC;

  PROCEDURE SetupWr() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(WrCode));
    opCodes^ :=
      OpCodes{
      NewWrOC("failure", -1, WrCode.Failure), 
      NewWrOC("new", 0, WrCode.New), 
      NewWrOC("stdout", -1, WrCode.Stdout),
      NewWrOC("stderr", -1, WrCode.Stderr),
      NewWrOC("open", 2, WrCode.Open),
      NewWrOC("openAppend", 2, WrCode.OpenAppend),
      NewWrOC("toText", 1, WrCode.ToText), 
      NewWrOC("putChar", 2, WrCode.PutChar), 
      NewWrOC("putText", 2, WrCode.PutText), 
      NewWrOC("flush", 1, WrCode.Flush), 
      NewWrOC("index", 1, WrCode.Index), 
      NewWrOC("length", 1, WrCode.Length), 
      NewWrOC("seek", 2, WrCode.Seek), 
      NewWrOC("close", 1, WrCode.Close), 
      NewWrOC("buffered", 1, WrCode.Buffered), 
      NewWrOC("seekable", 1, WrCode.Seekable), 
      NewWrOC("closed", 1, WrCode.Closed)
      };
    ObLib.Register(
      NEW(PackageWr, name:="wr", opCodes:=opCodes));
    wrFailureException := NEW(ObValue.ValException, name:="wr_failure");
  END SetupWr;

  PROCEDURE EvalWr(self: PackageWr; opCode: ObLib.OpCode; 
                   <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
      temp: BOOLEAN; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
    VAR wr1: Wr.T; text1: TEXT; char1: CHAR; int1: INTEGER := 0;
      fileSys1: ObValue.ValFileSystem;
    BEGIN
      TRY
        CASE NARROW(opCode, WrOpCode).code OF
        | WrCode.Failure => 
            RETURN wrFailureException;
        | WrCode.New =>
            wr1 := TextWr.New();
            RETURN NEW(ValWr, what:="<a writer>", tag:="Writer", 
                picklable:=FALSE, wr:=wr1);
        | WrCode.Stdout => 
            RETURN NEW(ValWr, what:="<stdout writer>", tag:="Writer", 
              picklable:=FALSE, wr:=Stdio.stdout);
        | WrCode.Stderr => 
            RETURN NEW(ValWr, what:="<stderr writer>", tag:="Writer", 
              picklable:=FALSE, wr:=Stdio.stderr);
        | WrCode.Open => 
            TYPECASE args[1] OF | ObValue.ValFileSystem(node) => fileSys1:=node;
            ELSE ObValue.BadArgType(1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
            ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            wr1 := fileSys1.remote.OpenWrite(text1);
            RETURN NEW(ValWr, what:="<'" & text1 & "' writer>", tag:="Writer", 
                       picklable:=FALSE, wr:=wr1);
        | WrCode.OpenAppend => 
            TYPECASE args[1] OF | ObValue.ValFileSystem(node) => fileSys1:=node;
            ELSE ObValue.BadArgType(1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
            ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            wr1 := fileSys1.remote.OpenAppend(text1);
            RETURN NEW(ValWr, what:="<'" & text1 & "' writer>", tag:="Writer", 
                       picklable:=FALSE, wr:=wr1);
       | WrCode.ToText =>
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE wr1 OF
            | TextWr.T(wr) =>
                RETURN ObValue.NewText(TextWr.ToText(wr));
            ELSE
              ObValue.BadArgVal(1, "locally produced by wr_new", self.name, 
                              opCode.name, loc);<*ASSERT FALSE*>
            END;
        | WrCode.PutChar => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValChar(node) => char1 := node.char;
            ELSE ObValue.BadArgType(2, "char", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            IF Wr.Closed(wr1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);<*ASSERT FALSE*>
            END;
            Wr.PutChar(wr1, char1);
            RETURN ObValue.valOk;
        | WrCode.PutText => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
            ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            IF Wr.Closed(wr1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);<*ASSERT FALSE*>
            END;
            Wr.PutText(wr1, text1);
            RETURN ObValue.valOk;
        | WrCode.Flush => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            IF Wr.Closed(wr1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);<*ASSERT FALSE*>
            END;
            Wr.Flush(wr1);
            RETURN ObValue.valOk;
        | WrCode.Index => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            IF Wr.Closed(wr1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);<*ASSERT FALSE*>
            END;
            RETURN NEW(ObValue.ValInt, int:=Wr.Index(wr1), temp:=temp);
        | WrCode.Length => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            IF Wr.Closed(wr1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);<*ASSERT FALSE*>
            END;
            RETURN NEW(ObValue.ValInt, int:=Wr.Length(wr1), temp:=temp);
        | WrCode.Seek => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            IF Wr.Closed(wr1) THEN
              ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);<*ASSERT FALSE*>
            END;
            IF NOT Wr.Seekable(wr1) THEN
              ObValue.BadArgVal(1, "seekable", self.name, opCode.name, loc);<*ASSERT FALSE*>
            END;
            IF int1<0 THEN
              ObValue.BadArgVal(2, "non-negative", self.name, opCode.name, loc);<*ASSERT FALSE*>
            END;
            Wr.Seek(wr1, int1);
            RETURN ObValue.valOk;
        | WrCode.Close => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            Wr.Close(wr1);
            RETURN ObValue.valOk;
        | WrCode.Buffered => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
	    IF Wr.Buffered(wr1) THEN RETURN true ELSE RETURN false END;
        | WrCode.Seekable => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
	    IF Wr.Seekable(wr1) THEN RETURN true ELSE RETURN false END;
        | WrCode.Closed => 
            TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
            ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
	    IF Wr.Closed(wr1) THEN RETURN true ELSE RETURN false END;
        ELSE
          ObValue.BadOp(self.name, opCode.name, loc);<*ASSERT FALSE*>
        END;
      EXCEPT
      | Wr.Failure, ObValue.ServerError =>
          ObValue.RaiseException(wrFailureException, 
                               self.name & "_" & opCode.name, loc);
          <*ASSERT FALSE*>
      | Thread.Alerted =>
          ObValue.RaiseException(ObValue.threadAlerted, 
                               self.name&"_"&opCode.name, loc);
          <*ASSERT FALSE*>
      | NetObj.Error(atoms) =>
          ObValue.RaiseNetException(self.name&"_"&opCode.name, atoms, loc);
          <*ASSERT FALSE*>
      END;
    END EvalWr;

(* ============ "lex" package ============ *)

TYPE

  LexCode = {Failure, Scan, Skip, Match, Bool, Int, Real};

  LexOpCode =  
    ObLib.OpCode OBJECT
        code: LexCode;
      END;
    
  PackageLex = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalLex;
      END;

  VAR lexFailureException: ObValue.ValException;

  PROCEDURE NewLexOC(name: TEXT; arity: INTEGER; code: LexCode)
    : LexOpCode =
  BEGIN
    RETURN NEW(LexOpCode, name:=name, arity:=arity, code:=code);
  END NewLexOC;

  PROCEDURE SetupLex() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(LexCode));
    opCodes^ :=
      OpCodes{
      NewLexOC("failure", -1, LexCode.Failure),
      NewLexOC("scan", 2, LexCode.Scan),
      NewLexOC("skip", 2, LexCode.Skip),
      NewLexOC("match", 2, LexCode.Match),
      NewLexOC("bool", 1, LexCode.Bool),
      NewLexOC("int", 1, LexCode.Int),
      NewLexOC("real", 1, LexCode.Real)
      };
    ObLib.Register(
      NEW(PackageLex, name:="lex", opCodes:=opCodes));
    lexFailureException := NEW(ObValue.ValException, name:="lex_failure");
  END SetupLex;

  PROCEDURE EvalLex(self: PackageLex; opCode: ObLib.OpCode; 
                    <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
      temp: BOOLEAN; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
    VAR text1: TEXT; rd1: Rd.T;
    BEGIN
      TRY
        CASE NARROW(opCode, LexOpCode).code OF
        | LexCode.Failure => 
            RETURN lexFailureException;
        | LexCode.Scan => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
            ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN ObValue.NewText(Lex.Scan(rd1, CharSet(text1)));
        | LexCode.Skip => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
            ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            Lex.Skip(rd1, CharSet(text1));
            RETURN ObValue.valOk;
        | LexCode.Match => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValText(node) => text1:=node.text;
            ELSE ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            Lex.Match(rd1, text1);
            RETURN ObValue.valOk;
        | LexCode.Bool => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
	    IF Lex.Bool(rd1) THEN RETURN true ELSE RETURN false END;
        | LexCode.Int => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN NEW(ObValue.ValInt, int:=Lex.Int(rd1, 10), temp:=temp);
        | LexCode.Real => 
            TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
            ELSE ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN NEW(ObValue.ValReal, real:=Lex.LongReal(rd1), temp:=temp);
        ELSE
          ObValue.BadOp(self.name, opCode.name, loc);<*ASSERT FALSE*>
        END;
      EXCEPT 
      | Lex.Error, FloatMode.Trap =>
          ObValue.RaiseException(lexFailureException, 
                               self.name & "_" & opCode.name, loc);
          <*ASSERT FALSE*>
      | Rd.Failure =>
          ObValue.RaiseException(rdFailureException, 
                               self.name & "_" & opCode.name, loc);
          <*ASSERT FALSE*>
      | Thread.Alerted =>
          ObValue.RaiseException(ObValue.threadAlerted, 
                               self.name&"_"&opCode.name, loc);
          <*ASSERT FALSE*>
      END;
    END EvalLex;

PROCEDURE CharSet(text: TEXT): SET OF CHAR =
  VAR s: SET OF CHAR;
  BEGIN
    s := SET OF CHAR{};
    FOR i:=0 TO Text.Length(text)-1 DO
      s := s + SET OF CHAR{Text.GetChar(text,i)};
    END;
    RETURN s;
 END CharSet;
    
(* ============ "fmt" package ============ *)

TYPE

  FmtCode = {PadLft, PadRht, Bool, Int, Real};

  FmtOpCode =  
    ObLib.OpCode OBJECT
        code: FmtCode;
      END;
    
  PackageFmt = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalFmt;
      END;

  PROCEDURE NewFmtOC(name: TEXT; arity: INTEGER; code: FmtCode)
    : FmtOpCode =
  BEGIN
    RETURN NEW(FmtOpCode, name:=name, arity:=arity, code:=code);
  END NewFmtOC;

  PROCEDURE SetupFmt() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(FmtCode));
    opCodes^ :=
      OpCodes{
      NewFmtOC("padLft", 2, FmtCode.PadLft),
      NewFmtOC("padRht", 2, FmtCode.PadRht),
      NewFmtOC("bool", 1, FmtCode.Bool),
      NewFmtOC("int", 1, FmtCode.Int),
      NewFmtOC("real", 1, FmtCode.Real)
      };
    ObLib.Register(
      NEW(PackageFmt, name:="fmt", opCodes:=opCodes));
  END SetupFmt;

  PROCEDURE EvalFmt(self: PackageFmt; opCode: ObLib.OpCode; 
                    <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
                    <*UNUSED*>temp: BOOLEAN; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error} =
    VAR text1: TEXT; bool1: BOOLEAN; int1: INTEGER := 0; real1: LONGREAL;
    BEGIN
        CASE NARROW(opCode, FmtOpCode).code OF
        | FmtCode.PadLft => 
            TYPECASE args[1] OF | ObValue.ValText(node) => text1 := node.text;
            ELSE ObValue.BadArgType(1, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN ObValue.NewText(Fmt.Pad(text1, int1, ' ', Fmt.Align.Left));
        | FmtCode.PadRht => 
            TYPECASE args[1] OF | ObValue.ValText(node) => text1 := node.text;
            ELSE ObValue.BadArgType(1, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN ObValue.NewText(Fmt.Pad(text1, int1, ' ', Fmt.Align.Right));
        | FmtCode.Bool => 
            TYPECASE args[1] OF | ObValue.ValBool(node) => bool1 := node.bool;
            ELSE ObValue.BadArgType(1, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            IF bool1 THEN RETURN ObValue.NewText("true");
	    ELSE RETURN ObValue.NewText("false"); END;
        | FmtCode.Int => 
            TYPECASE args[1] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN ObValue.NewText(Fmt.Int(int1));
        | FmtCode.Real => 
            TYPECASE args[1] OF | ObValue.ValReal(node) => real1 := node.real;
            ELSE ObValue.BadArgType(1, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN ObValue.NewText(Fmt.LongReal(real1, literal:=TRUE));
        ELSE
          ObValue.BadOp(self.name, opCode.name, loc);<*ASSERT FALSE*>
        END;
    END EvalFmt;

(* ============ "word" package ============ *)

TYPE

  WordCode = {Not, And, Or, Xor, Shift, Rotate};

  WordOpCode =  
    ObLib.OpCode OBJECT
        code: WordCode;
      END;
    
  PackageWord = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalWord;
      END;

  PROCEDURE NewWordOC(name: TEXT; arity: INTEGER; code: WordCode)
    : WordOpCode =
  BEGIN
    RETURN NEW(WordOpCode, name:=name, arity:=arity, code:=code);
  END NewWordOC;

  PROCEDURE SetupWord() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(WordCode));
    opCodes^ :=
      OpCodes{
      NewWordOC("bitnot",    1, WordCode.Not),
      NewWordOC("bitand",    2, WordCode.And),
      NewWordOC("bitor",     2, WordCode.Or),
      NewWordOC("bitxor",    2, WordCode.Xor),
      NewWordOC("bitshift",  2, WordCode.Shift),
      NewWordOC("bitrotate", 2, WordCode.Rotate)
      };
    ObLib.Register(
      NEW(PackageWord, name:="word", opCodes:=opCodes));
  END SetupWord;

  PROCEDURE EvalWord(self: PackageWord; opCode: ObLib.OpCode; 
                     <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
                     <*UNUSED*>temp: BOOLEAN; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error} =
    VAR int1, int2: INTEGER := 0;
    BEGIN
        CASE NARROW(opCode, WordOpCode).code OF
        | WordCode.Not => 
            TYPECASE args[1] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN Obliq.NewInt(Word.Not (int1));
        | WordCode.And => 
            TYPECASE args[1] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int2 := node.int;
            ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN Obliq.NewInt(Word.And (int1, int2));
        | WordCode.Or => 
            TYPECASE args[1] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int2 := node.int;
            ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN Obliq.NewInt(Word.Or (int1, int2));
        | WordCode.Xor => 
            TYPECASE args[1] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int2 := node.int;
            ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN Obliq.NewInt(Word.Xor (int1, int2));
        | WordCode.Shift => 
            TYPECASE args[1] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int2 := node.int;
            ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN Obliq.NewInt(Word.Shift (int1, int2));
        | WordCode.Rotate => 
            TYPECASE args[1] OF | ObValue.ValInt(node) => int1 := node.int;
            ELSE ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            TYPECASE args[2] OF | ObValue.ValInt(node) => int2 := node.int;
            ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
            RETURN Obliq.NewInt(Word.Rotate (int1, int2));
        ELSE
          ObValue.BadOp(self.name, opCode.name, loc);<*ASSERT FALSE*>
        END;
    END EvalWord;


(* ============ "pickle" package ============ *)

CONST CurrentPickleVersion = 2;
TYPE PickleVersion = 
  BRANDED "ObliqPickleVersion" OBJECT version: INTEGER END;

TYPE

  PickleCode = {Failure, Write, Read};

  PickleOpCode =  
    ObLib.OpCode OBJECT
        code: PickleCode;
      END;
    
  PackagePickle = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalPickle;
      END;

  VAR pickleFailureException: ObValue.ValException;

  PROCEDURE NewPickleOC(name: TEXT; arity: INTEGER; code: PickleCode)
    : PickleOpCode =
  BEGIN
    RETURN NEW(PickleOpCode, name:=name, arity:=arity, code:=code);
  END NewPickleOC;

  PROCEDURE SetupPickle() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(PickleCode));
    opCodes^ :=
      OpCodes{
      NewPickleOC("failure", -1, PickleCode.Failure),
      NewPickleOC("write", 2, PickleCode.Write),
      NewPickleOC("read", 1, PickleCode.Read)
      };
    ObLib.Register(
      NEW(PackagePickle, name:="pickle", opCodes:=opCodes));
    pickleFailureException := NEW(ObValue.ValException, name:="pickle_failure");
  END SetupPickle;

  PROCEDURE EvalPickle(self: PackagePickle; opCode: ObLib.OpCode; 
                       <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
                       <*UNUSED*>temp: BOOLEAN; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
    VAR wr1: Wr.T; rd1: Rd.T;
    BEGIN
      TRY
      CASE NARROW(opCode, PickleOpCode).code OF
      | PickleCode.Failure => 
          RETURN pickleFailureException;
      | PickleCode.Write => 
          TYPECASE args[1] OF | ValWr(node) => wr1 := node.wr;
          ELSE ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
          Pickle.Write(wr1, NEW(PickleVersion, version:=CurrentPickleVersion));
          Pickle.Write(wr1, 
            ObValue.CopyValToLocal(args[2], ObValue.NewTbl(), loc));
          RETURN ObValue.valOk;
      | PickleCode.Read => 
          TYPECASE args[1] OF | ValRd(node) => rd1 := node.rd;
          ELSE ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
          TYPECASE Pickle.Read(rd1) OF
          | PickleVersion(p) =>
            IF p.version # CurrentPickleVersion THEN 
              RAISE Pickle.Error("");
            END;
          ELSE RAISE Pickle.Error("");
          END;
          RETURN 
            ObValue.CopyLocalToVal(Pickle.Read(rd1), ObValue.NewTbl(), loc);
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc);<*ASSERT FALSE*>
      END;
      EXCEPT
      | Pickle.Error =>
          ObValue.RaiseException(pickleFailureException, opCode.name, loc);
          <*ASSERT FALSE*>
      | Wr.Failure =>
          ObValue.RaiseException(wrFailureException, opCode.name, loc);
          <*ASSERT FALSE*>
      | Rd.Failure =>
          ObValue.RaiseException(rdFailureException, opCode.name, loc);
          <*ASSERT FALSE*>
      | Rd.EndOfFile => 
          ObValue.RaiseException(rdEofFailureException, opCode.name, loc);
          <*ASSERT FALSE*>
      | Thread.Alerted =>
          ObValue.RaiseException(ObValue.threadAlerted, 
                               self.name&"_"&opCode.name, loc);
          <*ASSERT FALSE*>
      | NetObj.Error(atoms) =>
          ObValue.RaiseNetException(self.name&"_"&opCode.name, atoms, loc);
          <*ASSERT FALSE*>
      | SharedObj.Error(atoms) =>
          ObValue.RaiseSharedException(self.name&"_"&opCode.name, atoms, loc);
          <*ASSERT FALSE*>
      END;
    END EvalPickle;

(* ============ "process" package ============ *)

TYPE

  ProcCode = {New, In, Out, Err, Complete, Filter};

  ProcOpCode =  
    ObLib.OpCode OBJECT
        code: ProcCode;
      END;
    
  PackageProc = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalProc;
      END;

  PROCEDURE IsProc(self: ValProc; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF ValProc(oth)=> RETURN self.proc = oth.proc;
    ELSE RETURN FALSE END;
  END IsProc;

  PROCEDURE CopyProc(<*UNUSED*>self: ObValue.ValAnything; <*UNUSED*>tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error} =
  BEGIN
    ObValue.RaiseError("Cannot copy processes", loc);<*ASSERT FALSE*>
  END CopyProc;

  PROCEDURE NewProcOC(name: TEXT; arity: INTEGER; code: ProcCode)
    : ProcOpCode =
  BEGIN
    RETURN NEW(ProcOpCode, name:=name, arity:=arity, code:=code);
  END NewProcOC;

  PROCEDURE SetupProc() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(ProcCode));
    opCodes^ :=
      OpCodes{
      NewProcOC("new", 3, ProcCode.New),
      NewProcOC("in", 1, ProcCode.In),
      NewProcOC("out", 1, ProcCode.Out),
      NewProcOC("err", 1, ProcCode.Err),
      NewProcOC("complete", 1, ProcCode.Complete),
      NewProcOC("filter", 3, ProcCode.Filter)};
    ObLib.Register(
      NEW(PackageProc, name:="process", opCodes:=opCodes));
    ObValue.InhibitTransmission(TYPECODE(ValProc), 
      "processes cannot be transmitted/duplicated");
  END SetupProc;

  PROCEDURE EvalProc(self: PackageProc; opCode: ObLib.OpCode; 
                     <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
      temp: BOOLEAN; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
    TYPE Texts = REF ARRAY OF TEXT;
    VAR val: ObValue.Val; text1: TEXT; size, int: INTEGER;
      proc: Process.T; proc1: ValProc; texts: Texts;
      stdinR, stdinW, stdoutR, stdoutW, stderrR, stderrW: Pipe.T;
      stdinWr: FileWr.T; stdoutRd, stderrRd: FileRd.T;
      bool1: BOOLEAN; array1: REF ObValue.Vals;
      processor1: ObValue.ValProcessor;
    BEGIN
      TRY
      CASE NARROW(opCode, ProcOpCode).code OF
      | ProcCode.New => 
          TYPECASE args[1] OF 
          | ObValue.ValProcessor(node) => processor1:=node;
          ELSE ObValue.BadArgType(1, "processor", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
          IF processor1 # ObValue.localProcessor THEN
            ObValue.BadArgVal(1, "the local processor", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF 
          | ObValue.ValArray(node) => array1:=node.remote.Obtain();
          ELSE ObValue.BadArgType(2, "array", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
          TYPECASE args[3] OF | ObValue.ValBool(node) => bool1:=node.bool;
          ELSE ObValue.BadArgType(3, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
          size := NUMBER(array1^);
          IF size=0 THEN
            ObValue.BadArgVal(2, "non-empty", self.name, opCode.name, loc);<*ASSERT FALSE*>
          END;
          texts := NEW(Texts, size);
          FOR i := 0 TO size-1 DO
            TYPECASE array1^[i] OF
            | ObValue.ValText(node) => texts^[i] := node.text;
            ELSE 
              ObValue.BadArgType(2, "array(text)", self.name,
                                 opCode.name,loc);
              <*ASSERT FALSE*>
            END;
          END;
          Pipe.Open((*out*)stdinR, (*out*)stdinW);
          Pipe.Open((*out*)stdoutR, (*out*)stdoutW);      
          IF bool1 THEN 
            stderrR := stdoutR; stderrW := stdoutW;
          ELSE
            Pipe.Open((*out*)stderrR, (*out*)stderrW);
          END;  
          proc := 
            Process.Create(texts^[0], 
              SUBARRAY(texts^, 1, NUMBER(texts^)-1),
              NIL, NIL, 
              stdinR, stdoutW, stderrW); 
          stdinR.close();
          stdoutW.close();
          IF NOT bool1 THEN stderrW.close(); END;
          stdinWr := NEW(FileWr.T).init(stdinW);
          stdoutRd := NEW(FileRd.T).init(stdoutR);
          IF bool1 THEN
            stderrRd := stdoutRd;
          ELSE
            stderrRd := NEW(FileRd.T).init(stderrR);
          END;
          RETURN NEW(ValProc, what:="<a process>", tag:="Process",
                     picklable:=FALSE, 
            proc:=proc,
            in := 
              NEW(ValWr, what:="<a process stdin writer>",
                  tag:="Writer", picklable:=FALSE, 
                wr:=stdinWr), 
            out := 
              NEW(ValRd, what:="<a process stdout reader>", 
                  tag:="Reader",picklable:=FALSE, 
                  rd := stdoutRd),
            err := 
              NEW(ValRd, what:="<a process stderr reader>",
                  tag:="Reader", picklable:=FALSE, 
                  rd := stderrRd));
      | ProcCode.In => 
          TYPECASE args[1] OF | ValProc(node) => proc1 := node;
          ELSE ObValue.BadArgType(1, "process", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN proc1.in;
      | ProcCode.Out => 
          TYPECASE args[1] OF | ValProc(node) => proc1 := node;
          ELSE ObValue.BadArgType(1, "process", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN proc1.out;
      | ProcCode.Err => 
          TYPECASE args[1] OF | ValProc(node) => proc1 := node;
          ELSE ObValue.BadArgType(1, "process", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN proc1.err;
      | ProcCode.Complete => 
          TYPECASE args[1] OF | ValProc(node) => proc1 := node;
          ELSE ObValue.BadArgType(1, "process", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          int := Process.Wait(proc1.proc);
          Wr.Close(proc1.in.wr);
          Rd.Close(proc1.out.rd);
          Rd.Close(proc1.err.rd);
          RETURN NEW(ObValue.ValInt, int := int, temp:=temp);
      | ProcCode.Filter => 
          TYPECASE args[1] OF 
          | ObValue.ValProcessor(node) => processor1:=node;
          ELSE ObValue.BadArgType(1, "processor", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
          IF processor1 # ObValue.localProcessor THEN
            ObValue.BadArgVal(1, "the local processor", self.name, opCode.name, loc);<*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF 
          | ObValue.ValArray(node) => array1:=node.remote.Obtain();
          ELSE ObValue.BadArgType(2, "array", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
          TYPECASE args[3] OF | ObValue.ValText(node) => text1:=node.text;
          ELSE ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>END;
          size := NUMBER(array1^);
          IF size=0 THEN
            ObValue.BadArgVal(2, "non-empty", self.name, opCode.name, loc);<*ASSERT FALSE*>
          END;
          texts := NEW(Texts, size);
          FOR i := 0 TO size-1 DO
            TYPECASE array1^[i] OF
            | ObValue.ValText(node) => texts^[i] := node.text;
            ELSE ObValue.BadArgType(2, "array(text)", self.name, 
                   opCode.name, loc);<*ASSERT FALSE*>
            END;
          END;
          Pipe.Open((*out*)stdinR, (*out*)stdinW);
          Pipe.Open((*out*)stdoutR, (*out*)stdoutW);      
          proc := 
            Process.Create(texts^[0], 
              SUBARRAY(texts^, 1, NUMBER(texts^)-1),
              NIL, NIL, 
              stdinR, stdoutW, stdoutW); 
          stdinR.close();
          stdoutW.close();
          stdinWr := NEW(FileWr.T).init(stdinW);
          stdoutRd := NEW(FileRd.T).init(stdoutR);
          Wr.PutText(stdinWr, text1);
          Wr.Close(stdinWr);
          val := ObValue.NewText(Rd.GetText(stdoutRd, LAST(CARDINAL)));
          Rd.Close(stdoutRd);
          EVAL Process.Wait(proc);
          RETURN val;
 (* -- LocalPipe
      | ProcCode.Pipe => 
          TYPECASE args[1] OF | ObValue.ValBool(node) => bool1:=node.bool;
          ELSE ObValue.BadArgType(1, "bool", self.name, opCode.name, loc); END;
          rd1 := NEW(LocalPipe.Reaader);
          wr1 := NEW(LocalPipe.Writer);
          LocalPipe.Init(rd1, wr1, NOT bool1);
          RETURN ObValue.NewObject(
            NEW(ObValue.ObjFieldValue, 
              label:="r",
              val:=NEW(ValRd, what:="<a pipe reader>", tag:="Reader",
                       picklable:=FALSE, rd:=rd1),
              rest:=NEW(ObValue.ObjFieldValue(
                label:="w",
                val:=NEW(ValWr, what:="<a pipe writer>",
                     tag:="Writer", picklable:=FALSE, 
                  wr:=wr1),
                rest:=NIL)));
            );
 *)
       ELSE
        ObValue.BadOp(self.name, opCode.name, loc);<*ASSERT FALSE*>
      END;
      EXCEPT
      | Rd.Failure, Wr.Failure, Thread.Alerted, OSError.E =>
          ObValue.RaiseError(self.name&"_"&opCode.name, loc);<*ASSERT FALSE*>
      | NetObj.Error(atoms) =>
              ObValue.RaiseNetException(
                               self.name&"_"&opCode.name, atoms, loc);<*ASSERT FALSE*>
      END;
    END EvalProc;

(* ============ "random" package ============ *)

TYPE
  RandomCode = {Int, Real};

  RandomOpCode =  
    ObLib.OpCode OBJECT
        code: RandomCode;
      END;
    
  PackageRandom = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalRandom;
      END;

  PROCEDURE NewRandomOC(name: TEXT; arity: INTEGER; code: RandomCode)
    : RandomOpCode =
  BEGIN
    RETURN NEW(RandomOpCode, name:=name, arity:=arity, code:=code);
  END NewRandomOC;

  PROCEDURE SetupRandom() =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(RandomCode));
    opCodes^ :=
      OpCodes{
      NewRandomOC("int",  2, RandomCode.Int),
      NewRandomOC("real", 2, RandomCode.Real)
      };
    ObLib.Register(
      NEW(PackageRandom, name:="random", opCodes:=opCodes));
  END SetupRandom;

  PROCEDURE EvalRandom(self: PackageRandom; opCode: ObLib.OpCode; 
                       <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
                       <*UNUSED*>temp: BOOLEAN; loc: SynLocation.T)
      : ObValue.Val RAISES {ObValue.Error} =
    VAR real1, real2: LONGREAL; int1, int2: INTEGER := 0;
    BEGIN
      CASE NARROW(opCode, RandomOpCode).code OF
      | RandomCode.Int => 
          TYPECASE args[1] OF | ObValue.ValInt(node) => int1:=node.int;
          ELSE ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValInt(node) => int2:=node.int;
          ELSE ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          LOCK randomMu DO
            RETURN Obliq.NewInt(random.integer(int1,int2))
          END;
      | RandomCode.Real => 
          TYPECASE args[1] OF | ObValue.ValReal(node) => real1:=node.real;
          ELSE ObValue.BadArgType(1, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          TYPECASE args[2] OF | ObValue.ValReal(node) => real2:=node.real;
          ELSE ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*> END;
          LOCK randomMu DO
            RETURN Obliq.NewReal(random.longreal(real1,real2))
          END;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc);<*ASSERT FALSE*> 
      END;
    END EvalRandom;

VAR 
  randomMu := NEW(MUTEX);
  random := NEW(Random.Default).init();

BEGIN
END ObLibM3.
