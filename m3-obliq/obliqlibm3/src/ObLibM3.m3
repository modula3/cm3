(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: 
 * Last Modified On: Sun Aug 30 22:59:32 1998
 *)

MODULE ObLibM3;

IMPORT SynLocation, Text, ObLib, ObValue, Obliq, Rd, Wr, TextRefTbl,
       Refany, FloatMode, Thread, Stdio, Pipe, FileRd, FileWr,
       OSError, Fmt, Lex, TextRd, TextWr, NetObj, Pickle2 AS Pickle,
       Word, Pathname, SharedObj, Random, SynWr, TCP, IP, ConnRW,
       ObError, TCPSpecial, TCPPeer, TCPExtras, File, Atom, Process,
       M3Config;

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
    SetupOS();
    SetupDir();
    SetupPath();
    SetupLex();
    SetupFmt();
    SetupWord();
    SetupPickle();
    SetupProc();
    SetupRandom();
    SetupTcp();
    SetupDict();
  END Setup;

(* ============ "rd" package ============ *)

TYPE
  RdCode = {Failure, EofFailure, New, Stdin, Open, GetChar, Eof, UnGetChar,
            CharsReady, GetText, GetLine, Index, Length, Seek, Close,
            Intermittent, Seekable, Closed};
  ValRd = ObValue.ValRd;

  RdOpCode = ObLib.OpCode OBJECT code: RdCode;  END;

  PackageRd = ObLib.T OBJECT OVERRIDES Eval := EvalRd; END;

VAR rdFailureException, rdEofFailureException: ObValue.ValException;

PROCEDURE NewRdOC (name: TEXT; arity: INTEGER; code: RdCode): RdOpCode =
  BEGIN
    RETURN NEW(RdOpCode, name := name, arity := arity, code := code);
  END NewRdOC;

VAR true, false: ObValue.ValBool;

PROCEDURE SetupRd () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(RdCode));
    opCodes^ :=
      OpCodes{
        NewRdOC("failure", -1, RdCode.Failure),
        NewRdOC("eofFailure", -1, RdCode.EofFailure),
        NewRdOC("new", 1, RdCode.New), NewRdOC("stdin", -1, RdCode.Stdin),
        NewRdOC("open", 2, RdCode.Open),
        NewRdOC("getChar", 1, RdCode.GetChar),
        NewRdOC("eof", 1, RdCode.Eof),
        NewRdOC("unGetChar", 1, RdCode.UnGetChar),
        NewRdOC("charsReady", 1, RdCode.CharsReady),
        NewRdOC("getText", 2, RdCode.GetText),
        NewRdOC("getLine", 1, RdCode.GetLine),
        NewRdOC("index", 1, RdCode.Index),
        NewRdOC("length", 1, RdCode.Length),
        NewRdOC("seek", 2, RdCode.Seek), NewRdOC("close", 1, RdCode.Close),
        NewRdOC("intermittent", 1, RdCode.Intermittent),
        NewRdOC("seekable", 1, RdCode.Seekable),
        NewRdOC("closed", 1, RdCode.Closed)};
    ObLib.Register(NEW(PackageRd, name := "rd", opCodes := opCodes));
    rdFailureException := NEW(ObValue.ValException, name := "rd_failure");
    rdEofFailureException :=
      NEW(ObValue.ValException, name := "rd_eofFailure");
    true := NEW(ObValue.ValBool, bool := TRUE);
    false := NEW(ObValue.ValBool, bool := FALSE);
  END SetupRd;

PROCEDURE EvalRd (                      self  : PackageRd;
                                        opCode: ObLib.OpCode;
                  <*UNUSED*>            arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                                        temp  : BOOLEAN;
                  <* UNUSED *>          swr   : SynWr.T;
                                        loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    rd1     : Rd.T;
    text1   : TEXT;
    int1    : INTEGER;
    fileSys1: ObValue.ValFileSystem;
  BEGIN
    TRY
      CASE NARROW(opCode, RdOpCode).code OF
      | RdCode.Failure => RETURN rdFailureException;
      | RdCode.EofFailure => RETURN rdEofFailureException;
      | RdCode.New =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          rd1 := TextRd.New(text1);
          RETURN ObValue.NewRd(rd1);
      | RdCode.Stdin =>
          RETURN ObValue.NewRd(Stdio.stdin, what := "<stdin reader>");
      | RdCode.Open =>
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          rd1 := fileSys1.remote.OpenRead(text1);
          RETURN ObValue.NewRd(rd1, what := "<'" & text1 & "' reader>");
      | RdCode.GetChar =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Rd.Closed(rd1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValChar, char := Rd.GetChar(rd1));
      | RdCode.Eof =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
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
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
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
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Rd.Closed(rd1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN
            NEW(ObValue.ValInt, int := Rd.CharsReady(rd1), temp := temp);
      | RdCode.GetText =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Rd.Closed(rd1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              2, "non-negative", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN ObValue.NewText(Rd.GetText(rd1, int1));
      | RdCode.GetLine =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
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
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Rd.Closed(rd1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValInt, int := Rd.Index(rd1), temp := temp);
      | RdCode.Length =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Rd.Closed(rd1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Rd.Intermittent(rd1) THEN
            ObValue.BadArgVal(
              1, "non-intermittent", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValInt, int := Rd.Length(rd1), temp := temp);
      | RdCode.Seek =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
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
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              2, "non-negative", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          Rd.Seek(rd1, int1);
          RETURN ObValue.valOk;
      | RdCode.Close =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          Rd.Close(rd1);
          RETURN ObValue.valOk;
      | RdCode.Intermittent =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Rd.Intermittent(rd1) THEN RETURN true ELSE RETURN false END;
      | RdCode.Seekable =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Rd.Seekable(rd1) THEN RETURN true ELSE RETURN false END;
      | RdCode.Closed =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Rd.Closed(rd1) THEN RETURN true ELSE RETURN false END;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
    EXCEPT
    | Rd.Failure, ObValue.ServerError =>
        ObValue.RaiseException(
          rdFailureException, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | Rd.EndOfFile =>
        ObValue.RaiseException(
          rdEofFailureException, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    END;
  END EvalRd;

(* ============ "wr" package ============ *)

TYPE

  WrCode = {Failure, New, Stdout, Stderr, Open, OpenAppend, ToText,
            PutChar, PutText, Flush, Index, Length, Seek, Close, Buffered,
            Seekable, Closed};
  ValWr = ObValue.ValWr;

  WrOpCode = ObLib.OpCode OBJECT code: WrCode;  END;

  PackageWr = ObLib.T OBJECT OVERRIDES Eval := EvalWr; END;

VAR wrFailureException: ObValue.ValException;

PROCEDURE NewWrOC (name: TEXT; arity: INTEGER; code: WrCode): WrOpCode =
  BEGIN
    RETURN NEW(WrOpCode, name := name, arity := arity, code := code);
  END NewWrOC;

PROCEDURE SetupWr () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(WrCode));
    opCodes^ := OpCodes{NewWrOC("failure", -1, WrCode.Failure),
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
                        NewWrOC("closed", 1, WrCode.Closed)};
    ObLib.Register(NEW(PackageWr, name := "wr", opCodes := opCodes));
    wrFailureException := NEW(ObValue.ValException, name := "wr_failure");
  END SetupWr;

PROCEDURE EvalWr (                    self  : PackageWr;
                                      opCode: ObLib.OpCode;
                  <*UNUSED*>          arity : ObLib.OpArity;
                             READONLY args  : ObValue.ArgArray;
                                      temp  : BOOLEAN;
                  <*UNUSED*>          swr   : SynWr.T;
                                      loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    wr1     : Wr.T;
    text1   : TEXT;
    char1   : CHAR;
    int1    : INTEGER;
    fileSys1: ObValue.ValFileSystem;
  BEGIN
    TRY
      CASE NARROW(opCode, WrOpCode).code OF
      | WrCode.Failure => RETURN wrFailureException;
      | WrCode.New =>
          wr1 := TextWr.New();
          RETURN ObValue.NewWr(wr1);
      | WrCode.Stdout =>
          RETURN ObValue.NewWr(Stdio.stdout, "<stdout writer>");
      | WrCode.Stderr =>
          RETURN ObValue.NewWr(Stdio.stderr, "<stderr writer>");
      | WrCode.Open =>
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          wr1 := fileSys1.remote.OpenWrite(text1);
          RETURN ObValue.NewWr(wr1, "<'" & text1 & "' writer>");
      | WrCode.OpenAppend =>
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          wr1 := fileSys1.remote.OpenAppend(text1);
          RETURN ObValue.NewWr(wr1, "<'" & text1 & "' writer>");
      | WrCode.ToText =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE wr1 OF
          | TextWr.T (wr) => RETURN ObValue.NewText(TextWr.ToText(wr));
          ELSE
            ObValue.BadArgVal(
              1, "locally produced by wr_new", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
      | WrCode.PutChar =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValChar (node) => char1 := node.char;
          ELSE
            ObValue.BadArgType(2, "char", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Wr.Closed(wr1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Wr.PutChar(wr1, char1);
          RETURN ObValue.valOk;
      | WrCode.PutText =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Wr.Closed(wr1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Wr.PutText(wr1, text1);
          RETURN ObValue.valOk;
      | WrCode.Flush =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Wr.Closed(wr1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Wr.Flush(wr1);
          RETURN ObValue.valOk;
      | WrCode.Index =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Wr.Closed(wr1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValInt, int := Wr.Index(wr1), temp := temp);
      | WrCode.Length =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Wr.Closed(wr1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValInt, int := Wr.Length(wr1), temp := temp);
      | WrCode.Seek =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Wr.Closed(wr1) THEN
            ObValue.BadArgVal(1, "non-closed", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF NOT Wr.Seekable(wr1) THEN
            ObValue.BadArgVal(1, "seekable", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              2, "non-negative", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Wr.Seek(wr1, int1);
          RETURN ObValue.valOk;
      | WrCode.Close =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Wr.Close(wr1);
          RETURN ObValue.valOk;
      | WrCode.Buffered =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Wr.Buffered(wr1) THEN RETURN true ELSE RETURN false END;
      | WrCode.Seekable =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Wr.Seekable(wr1) THEN RETURN true ELSE RETURN false END;
      | WrCode.Closed =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Wr.Closed(wr1) THEN RETURN true ELSE RETURN false END;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
    EXCEPT
    | Wr.Failure, ObValue.ServerError =>
        ObValue.RaiseException(
          wrFailureException, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    END;
  END EvalWr;

(* ============ "os" package ============ *)

TYPE

  OSCode = {Error, Type, Target, NewPipe};

  OSOpCode = ObLib.OpCode OBJECT code: OSCode;  END;

  PackageOS = ObLib.T OBJECT OVERRIDES Eval := EvalOS; END;

PROCEDURE NewOSOC (name: TEXT; arity: INTEGER; code: OSCode): OSOpCode =
  BEGIN
    RETURN NEW(OSOpCode, name := name, arity := arity, code := code);
  END NewOSOC;

PROCEDURE SetupOS () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(OSCode));
    opCodes^ := OpCodes{NewOSOC("error", -1, OSCode.Error),
                        NewOSOC("type", -1, OSCode.Type),
                        NewOSOC("target", -1, OSCode.Target),
                        NewOSOC("newPipe", 0, OSCode.NewPipe)};
    ObLib.Register(NEW(PackageOS, name := "os", opCodes := opCodes));
  END SetupOS;

PROCEDURE EvalOS (                    self  : PackageOS;
                                      opCode: ObLib.OpCode;
                  <*UNUSED*>          arity : ObLib.OpArity;
                  <*UNUSED*> READONLY args  : ObValue.ArgArray;
                  <*UNUSED*>          temp  : BOOLEAN;
                  <*UNUSED*>          swr   : SynWr.T;
                                      loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    pipeR, pipeW: Pipe.T;
    pipeRd: Rd.T;
    pipeWr: Wr.T; 
    array1: ARRAY [0..1] OF ObValue.Val;
  BEGIN
    TRY
      CASE NARROW(opCode, OSOpCode).code OF
      | OSCode.Error => RETURN ObValue.osError;
      | OSCode.Type => RETURN Obliq.NewText(M3Config.OS_TYPE);
      | OSCode.Target => RETURN Obliq.NewText(M3Config.TARGET);
      | OSCode.NewPipe =>
          Pipe.Open( (*out*)pipeR, (*out*) pipeW);
          pipeWr := NEW(FileWr.T).init(pipeW);
          pipeRd := NEW(FileRd.T).init(pipeR);
          array1[0] := ObValue.NewWr(pipeWr, "<a pipe writer>");
          array1[1] := ObValue.NewRd(pipeRd, "<a pipe reader>");
          RETURN ObValue.NewArray(array1);
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
    EXCEPT
    | OSError.E =>
        ObValue.RaiseException(
          ObValue.osError, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalOS;

(* ============ "dir" package ============ *)

TYPE

  DirCode = {Failure, 
             GetAbsolutePathname, CreateDirectory, DeleteDirectory,
             DeleteFile, Rename, Iterate, IteratorNext,
             IteratorNextWithStatus, IteratorClose, Status, SetModificationTime};

  DirOpCode = ObLib.OpCode OBJECT code: DirCode;  END;

  PackageDir = ObLib.T OBJECT OVERRIDES Eval := EvalDir; END;

VAR dirFailureException: ObValue.ValException;

PROCEDURE NewDirOC (name: TEXT; arity: INTEGER; code: DirCode): DirOpCode =
  BEGIN
    RETURN NEW(DirOpCode, name := name, arity := arity, code := code);
  END NewDirOC;

PROCEDURE SetupDir () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(DirCode));
    opCodes^ := OpCodes{NewDirOC("failure", -1, DirCode.Failure),
                        NewDirOC("getAbsolutePathname", 2, 
                                 DirCode.GetAbsolutePathname),
                        NewDirOC("createDirectory", 2, DirCode.CreateDirectory),
                        NewDirOC("deleteDirectory", 2, DirCode.DeleteDirectory),
                        NewDirOC("deleleFile", 2, DirCode.DeleteFile),
                        NewDirOC("rename", 3, DirCode.Rename),
                        NewDirOC("iterate", 2, DirCode.Iterate),
                        NewDirOC("iteratorNext", 1, DirCode.IteratorNext),
                        NewDirOC("iteratorNextWithStatus", 1, 
                                 DirCode.IteratorNextWithStatus),
                        NewDirOC("iteratorClose", 1, DirCode.IteratorClose),
                        NewDirOC("status", 2, DirCode.Status),
                        NewDirOC("setModificationTime", 3, 
                                 DirCode.SetModificationTime)};
    dirFailureException := NEW(ObValue.ValException, name := "dir_failure");
    ObLib.Register(NEW(PackageDir, name := "dir", opCodes := opCodes));
  END SetupDir;

PROCEDURE EvalDir (                    self  : PackageDir;
                                      opCode: ObLib.OpCode;
                  <*UNUSED*>          arity : ObLib.OpArity;
                             READONLY args  : ObValue.ArgArray;
                  <*UNUSED*>          temp  : BOOLEAN;
                  <*UNUSED*>          swr   : SynWr.T;
                                      loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    text1,text2: TEXT;
    real1   : LONGREAL;
    fileSys1: ObValue.ValFileSystem;
    iterator1: ObValue.ValIterator;
  BEGIN
    TRY
      CASE NARROW(opCode, DirOpCode).code OF
      | DirCode.Failure => RETURN dirFailureException;
      | DirCode.GetAbsolutePathname => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN Obliq.NewText(fileSys1.remote.GetAbsolutePathname(text1));
      | DirCode.CreateDirectory => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          fileSys1.remote.CreateDirectory(text1);
          RETURN ObValue.valOk;
      | DirCode.DeleteDirectory => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          fileSys1.remote.DeleteDirectory(text1);
          RETURN ObValue.valOk;
      | DirCode.DeleteFile => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          fileSys1.remote.DeleteFile(text1);
          RETURN ObValue.valOk;
      | DirCode.Rename => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          fileSys1.remote.Rename(text1,text2);
          RETURN ObValue.valOk;
      | DirCode.Iterate => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValIterator, picklable := FALSE,
                     what := "<directory iterator for " & text1 & ">",
                     tag := "Iterator",
                     remote := fileSys1.remote.Iterate(text1));
      | DirCode.Status => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          VAR fields: ARRAY [0..2] OF ObValue.Field;
              status: File.Status := fileSys1.remote.Status(text1);
          BEGIN
            fields[0] := ObValue.Field{"type", 
                                       Obliq.NewText(Atom.ToText(status.type))};
            fields[1] := ObValue.Field{"modificationTime",
                                       Obliq.NewReal(status.modificationTime)};
            fields[2] := ObValue.Field{"size", Obliq.NewInt(status.size)};
            RETURN ObValue.NewObject(fields);
          END;
      | DirCode.SetModificationTime => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValReal (node) => real1 := node.real;
          ELSE
            ObValue.BadArgType(3, "real", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          fileSys1.remote.SetModificationTime(text1,real1);
          RETURN ObValue.valOk;
      | DirCode.IteratorNext => 
          TYPECASE args[1] OF
          | ObValue.ValIterator (node) => iterator1 := node;
          ELSE
            ObValue.BadArgType(
              1, "directory iterator", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          text1 := NIL;
          VAR
            array1: ARRAY [0..1] OF ObValue.Val;
            bool1 := iterator1.remote.Next(text1);
          BEGIN
            array1[0] := Obliq.NewText(text1);
            array1[1] := Obliq.NewBool(bool1);
            RETURN ObValue.NewArray(array1);
          END;
      | DirCode.IteratorNextWithStatus => 
          TYPECASE args[1] OF
          | ObValue.ValIterator (node) => iterator1 := node;
          ELSE
            ObValue.BadArgType(
              1, "directory iterator", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          text1 := NIL;
          VAR
            array1: ARRAY [0..2] OF ObValue.Val;
            status1: File.Status;
            bool1: BOOLEAN;
            fields: ARRAY [0..2] OF ObValue.Field;
          BEGIN
            bool1  := iterator1.remote.NextWithStatus(text1,status1);
            array1[0] := Obliq.NewText(text1);
            array1[1] := Obliq.NewBool(bool1);
            IF bool1 THEN
              fields[0] := ObValue.Field{"type", 
                                         Obliq.NewText(Atom.ToText(status1.type))};
              fields[1] := ObValue.Field{"modificationTime",
                                         Obliq.NewReal(status1.modificationTime)};
              fields[2] := ObValue.Field{"size", Obliq.NewInt(status1.size)};
              array1[2] := ObValue.NewObject(fields);
            ELSE
              array1[2] := ObValue.valOk;
            END;
            RETURN ObValue.NewArray(array1);
          END;
      | DirCode.IteratorClose => 
          TYPECASE args[1] OF
          | ObValue.ValIterator (node) => iterator1 := node;
          ELSE
            ObValue.BadArgType(
              1, "directory iterator", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          iterator1.remote.Close();
          RETURN ObValue.valOk;          
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
    EXCEPT
    | ObValue.ServerError =>
        ObValue.RaiseException(
          dirFailureException, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    END;
  END EvalDir;

(* ============ "path" package ============ *)

TYPE

  PathCode = {Invalid, 
              PathSeparator, SearchPathSeparator, 
              Valid, Decompose, Compose, Absolute, Prefix, Last, Base,
              Join, LastBase, LastExt, ReplaceExt, Parent, Current};

  PathOpCode = ObLib.OpCode OBJECT code: PathCode;  END;

  PackagePath = ObLib.T OBJECT OVERRIDES Eval := EvalPath; END;

PROCEDURE NewPathOC (name: TEXT; arity: INTEGER; code: PathCode): PathOpCode =
  BEGIN
    RETURN NEW(PathOpCode, name := name, arity := arity, code := code);
  END NewPathOC;

PROCEDURE SetupPath () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(PathCode));
    opCodes^ := OpCodes{NewPathOC("invalid",  -1,  PathCode.Invalid),
                        NewPathOC("separator", 1, 
                                  PathCode.PathSeparator),
                        NewPathOC("searchSeparator", 1, 
                                 PathCode.SearchPathSeparator),
                        NewPathOC("valid", 2, PathCode.Valid),
                        NewPathOC("decompose", 2, PathCode.Decompose),
                        NewPathOC("compose", 2, PathCode.Compose),
                        NewPathOC("absolute", 2, PathCode.Absolute),
                        NewPathOC("prefix", 2, PathCode.Prefix),
                        NewPathOC("last", 2, PathCode.Last),
                        NewPathOC("base", 2, PathCode.Base),
                        NewPathOC("join", 4, PathCode.Join),
                        NewPathOC("lastBase", 2, PathCode.LastBase),
                        NewPathOC("lastExt", 2, PathCode.LastExt),
                        NewPathOC("replaceExt", 3, PathCode.ReplaceExt),
                        NewPathOC("parent", 1, PathCode.Parent),
                        NewPathOC("current", 1, PathCode.Current)};
    invalidPathException := NEW(ObValue.ValException, name := "path_invalid");
    ObLib.Register(NEW(PackagePath, name := "path", opCodes := opCodes));
  END SetupPath;

VAR invalidPathException: ObValue.ValException;

PROCEDURE EvalPath (                    self  : PackagePath;
                                      opCode: ObLib.OpCode;
                  <*UNUSED*>          arity : ObLib.OpArity;
                             READONLY args  : ObValue.ArgArray;
                  <*UNUSED*>          temp  : BOOLEAN;
                  <*UNUSED*>          swr   : SynWr.T;
                                      loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    fileSys1: ObValue.ValFileSystem;
    text1,text2,text3: TEXT;
  BEGIN
    TRY
      CASE NARROW(opCode, PathOpCode).code OF
      | PathCode.Invalid => RETURN invalidPathException;
      | PathCode.PathSeparator => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN Obliq.NewText(fileSys1.remote.PathSep());
      | PathCode.SearchPathSeparator => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN Obliq.NewText(fileSys1.remote.PathSearchSep());
      | PathCode.Parent => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
        RETURN Obliq.NewText(fileSys1.remote.PathParent());
      | PathCode.Current => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN Obliq.NewText(fileSys1.remote.PathCurrent());
      | PathCode.Valid => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN Obliq.NewBool(fileSys1.remote.PathValid(text1));
      | PathCode.Decompose => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
        WITH arcs = fileSys1.remote.PathDecompose(text1),
             size = arcs.size(),
             vals = NEW(REF ObValue.Vals, size) DO
          FOR i := 0 TO size-1 DO
            vals^[i] := Obliq.NewText(arcs.get(i));
          END;
          RETURN ObValue.NewArrayFromVals(vals);
        END;
      | PathCode.Compose => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          VAR array: ObValue.ValArray;
          BEGIN
            TYPECASE args[2] OF
            | ObValue.ValArray (node) => array := node;
            ELSE
              ObValue.BadArgType(2, "array", self.name, opCode.name, loc); 
              <*ASSERT FALSE*>
            END;
            WITH size = array.Size() DO
              IF size = 0 THEN
                ObValue.RaiseException(
                    invalidPathException, self.name & "_" & opCode.name, loc);
                <*ASSERT FALSE*>
              END;
              WITH arcs = NEW(Pathname.Arcs).init(size),
                   vals = array.Obtain() DO
                FOR i:=0 TO size-1 DO
                  TYPECASE vals[i] OF
                  | ObValue.ValText (node) => text1 := node.text;
                  ELSE
                    ObValue.BadArgType(1, "array text", self.name, opCode.name,
                                       loc); <*ASSERT FALSE*>
                  END;
                  IF Text.Length(text1) = 0 THEN text1 := NIL END;
                  arcs.addhi(text1);
                END;
                text1 := fileSys1.remote.PathCompose(arcs);
                RETURN Obliq.NewText(text1);
              END;
            END;
          END;
      | PathCode.Absolute => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
        RETURN Obliq.NewBool(fileSys1.remote.PathAbsolute(text1));
      | PathCode.Prefix => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
        RETURN Obliq.NewText(fileSys1.remote.PathPrefix(text1));
      | PathCode.Last => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
        RETURN Obliq.NewText(fileSys1.remote.PathLast(text1));
      | PathCode.Base => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
        RETURN Obliq.NewText(fileSys1.remote.PathBase(text1));
      | PathCode.Join => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValText (node) => text3 := node.text;
          ELSE
            ObValue.BadArgType(4, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Length(text3) = 0 THEN text3 := NIL END;
          RETURN Obliq.NewText(fileSys1.remote.PathJoin(text1,text2,text3));
      | PathCode.LastBase => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
        RETURN Obliq.NewText(fileSys1.remote.PathLastBase(text1));
      | PathCode.LastExt => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
        RETURN Obliq.NewText(fileSys1.remote.PathLastExt(text1));
      | PathCode.ReplaceExt => 
          TYPECASE args[1] OF
          | ObValue.ValFileSystem (node) => fileSys1 := node;
          ELSE
            ObValue.BadArgType(
              1, "file system", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
        RETURN Obliq.NewText(fileSys1.remote.PathReplaceExt(text1,text2));
      END;
    EXCEPT
    | Pathname.Invalid =>
        ObValue.RaiseException(
          invalidPathException, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    END;
  END EvalPath;

(* ============ "lex" package ============ *)

TYPE

  LexCode = {Failure, Scan, ScanNonBlanks, Skip, SkipBlanks, Match,
             Bool, Int, Real};

  LexOpCode = ObLib.OpCode OBJECT code: LexCode;  END;

  PackageLex = ObLib.T OBJECT OVERRIDES Eval := EvalLex; END;

VAR lexFailureException: ObValue.ValException;

PROCEDURE NewLexOC (name: TEXT; arity: INTEGER; code: LexCode): LexOpCode =
  BEGIN
    RETURN NEW(LexOpCode, name := name, arity := arity, code := code);
  END NewLexOC;

PROCEDURE SetupLex () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(LexCode));
    opCodes^ := OpCodes{NewLexOC("failure", -1, LexCode.Failure),
                        NewLexOC("scan", 2, LexCode.Scan),
                        NewLexOC("scanNonBlanks", 1, LexCode.ScanNonBlanks),
                        NewLexOC("skip", 2, LexCode.Skip),
                        NewLexOC("skipBlanks", 1, LexCode.SkipBlanks),
                        NewLexOC("match", 2, LexCode.Match),
                        NewLexOC("bool", 1, LexCode.Bool),
                        NewLexOC("int", 1, LexCode.Int),
                        NewLexOC("real", 1, LexCode.Real)};
    ObLib.Register(NEW(PackageLex, name := "lex", opCodes := opCodes));
    lexFailureException :=
      NEW(ObValue.ValException, name := "lex_failure");
  END SetupLex;

PROCEDURE EvalLex (                    self  : PackageLex;
                                       opCode: ObLib.OpCode;
                   <*UNUSED*>          arity : ObLib.OpArity;
                              READONLY args  : ObValue.ArgArray;
                                       temp  : BOOLEAN;
                   <*UNUSED*>          swr   : SynWr.T;
                                       loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    text1: TEXT;
    rd1  : Rd.T;
  BEGIN
    TRY
      CASE NARROW(opCode, LexOpCode).code OF
      | LexCode.Failure => RETURN lexFailureException;
      | LexCode.Scan =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN ObValue.NewText(Lex.Scan(rd1, CharSet(text1)));
      | LexCode.ScanNonBlanks =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN ObValue.NewText(Lex.Scan(rd1));
      | LexCode.Skip =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Lex.Skip(rd1, CharSet(text1));
          RETURN ObValue.valOk;
      | LexCode.SkipBlanks =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Lex.Skip(rd1);
          RETURN ObValue.valOk;
      | LexCode.Match =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Lex.Match(rd1, text1);
          RETURN ObValue.valOk;
      | LexCode.Bool =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Lex.Bool(rd1) THEN RETURN true ELSE RETURN false END;
      | LexCode.Int =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN
            NEW(ObValue.ValInt, int := Lex.Int(rd1, 10), temp := temp);
      | LexCode.Real =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN
            NEW(ObValue.ValReal, real := Lex.LongReal(rd1), temp := temp);
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
    EXCEPT
    | Lex.Error, FloatMode.Trap =>
        ObValue.RaiseException(
          lexFailureException, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | Rd.Failure =>
        ObValue.RaiseException(
          rdFailureException, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
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

  FmtCode = {PadLft, PadRht, Bool, Int, Real, RealPrec};

  FmtOpCode = ObLib.OpCode OBJECT code: FmtCode;  END;

  PackageFmt = ObLib.T OBJECT OVERRIDES Eval := EvalFmt; END;

PROCEDURE NewFmtOC (name: TEXT; arity: INTEGER; code: FmtCode): FmtOpCode =
  BEGIN
    RETURN NEW(FmtOpCode, name := name, arity := arity, code := code);
  END NewFmtOC;

PROCEDURE SetupFmt () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(FmtCode));
    opCodes^ := OpCodes{NewFmtOC("padLft", 2, FmtCode.PadLft),
                        NewFmtOC("padRht", 2, FmtCode.PadRht),
                        NewFmtOC("bool", 1, FmtCode.Bool),
                        NewFmtOC("int", 1, FmtCode.Int),
                        NewFmtOC("real", 1, FmtCode.Real),
                        NewFmtOC("realPrec", 2, FmtCode.RealPrec)};
    ObLib.Register(NEW(PackageFmt, name := "fmt", opCodes := opCodes));
  END SetupFmt;

PROCEDURE EvalFmt (                    self  : PackageFmt;
                                       opCode: ObLib.OpCode;
                   <*UNUSED*>          arity : ObLib.OpArity;
                              READONLY args  : ObValue.ArgArray;
                   <*UNUSED*>          temp  : BOOLEAN;
                   <*UNUSED*>          swr   : SynWr.T;
                                       loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR
    text1: TEXT;
    bool1: BOOLEAN;
    int1 : INTEGER;
    real1: LONGREAL;
  BEGIN
    CASE NARROW(opCode, FmtOpCode).code OF
    | FmtCode.PadLft =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN ObValue.NewText(Fmt.Pad(text1, int1, ' ', Fmt.Align.Left));
    | FmtCode.PadRht =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN ObValue.NewText(Fmt.Pad(text1, int1, ' ', Fmt.Align.Right));
    | FmtCode.Bool =>
        TYPECASE args[1] OF
        | ObValue.ValBool (node) => bool1 := node.bool;
        ELSE
          ObValue.BadArgType(1, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        IF bool1 THEN
          RETURN ObValue.NewText("true");
        ELSE
          RETURN ObValue.NewText("false");
        END;
    | FmtCode.Int =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN ObValue.NewText(Fmt.Int(int1));
    | FmtCode.Real =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN ObValue.NewText(Fmt.LongReal(real1, literal := TRUE));
    | FmtCode.RealPrec =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN ObValue.NewText(
                 Fmt.LongReal(real1, prec := int1, style := Fmt.Style.Fix));
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
    END;
  END EvalFmt;

(* ============ "word" package ============ *)

TYPE

  WordCode = {Not, And, Or, Xor, Shift, Rotate};

  WordOpCode = ObLib.OpCode OBJECT code: WordCode;  END;

  PackageWord = ObLib.T OBJECT OVERRIDES Eval := EvalWord; END;

PROCEDURE NewWordOC (name: TEXT; arity: INTEGER; code: WordCode):
  WordOpCode =
  BEGIN
    RETURN NEW(WordOpCode, name := name, arity := arity, code := code);
  END NewWordOC;

PROCEDURE SetupWord () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(WordCode));
    opCodes^ := OpCodes{NewWordOC("bitnot", 1, WordCode.Not),
                        NewWordOC("bitand", 2, WordCode.And),
                        NewWordOC("bitor", 2, WordCode.Or),
                        NewWordOC("bitxor", 2, WordCode.Xor),
                        NewWordOC("bitshift", 2, WordCode.Shift),
                        NewWordOC("bitrotate", 2, WordCode.Rotate)};
    ObLib.Register(NEW(PackageWord, name := "word", opCodes := opCodes));
  END SetupWord;

PROCEDURE EvalWord (                    self  : PackageWord;
                                        opCode: ObLib.OpCode;
                    <*UNUSED*>          arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                    <*UNUSED*>          temp  : BOOLEAN;
                    <*UNUSED*>          swr   : SynWr.T;
                                        loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR int1, int2: INTEGER;
  BEGIN
    CASE NARROW(opCode, WordOpCode).code OF
    | WordCode.Not =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN Obliq.NewInt(Word.Not(int1));
    | WordCode.And =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int2 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN Obliq.NewInt(Word.And(int1, int2));
    | WordCode.Or =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int2 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN Obliq.NewInt(Word.Or(int1, int2));
    | WordCode.Xor =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int2 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN Obliq.NewInt(Word.Xor(int1, int2));
    | WordCode.Shift =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int2 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN Obliq.NewInt(Word.Shift(int1, int2));
    | WordCode.Rotate =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int2 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN Obliq.NewInt(Word.Rotate(int1, int2));
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
    END;
  END EvalWord;


(* ============ "pickle" package ============ *)

CONST CurrentPickleVersion = 2;
TYPE PickleVersion = 
  BRANDED "ObliqPickleVersion" OBJECT version: INTEGER END;

TYPE

  PickleCode = {Failure, Write, Read};

  PickleOpCode = ObLib.OpCode OBJECT code: PickleCode;  END;

  PackagePickle = ObLib.T OBJECT OVERRIDES Eval := EvalPickle; END;

VAR pickleFailureException: ObValue.ValException;

PROCEDURE NewPickleOC (name: TEXT; arity: INTEGER; code: PickleCode):
  PickleOpCode =
  BEGIN
    RETURN NEW(PickleOpCode, name := name, arity := arity, code := code);
  END NewPickleOC;

PROCEDURE SetupPickle () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(PickleCode));
    opCodes^ := OpCodes{NewPickleOC("failure", -1, PickleCode.Failure),
                        NewPickleOC("write", 2, PickleCode.Write),
                        NewPickleOC("read", 1, PickleCode.Read)};
    ObLib.Register(
      NEW(PackagePickle, name := "pickle", opCodes := opCodes));
    pickleFailureException :=
      NEW(ObValue.ValException, name := "pickle_failure");
  END SetupPickle;

PROCEDURE EvalPickle (                    self  : PackagePickle;
                                          opCode: ObLib.OpCode;
                      <*UNUSED*>          arity : ObLib.OpArity;
                                 READONLY args  : ObValue.ArgArray;
                      <*UNUSED*>          temp  : BOOLEAN;
                      <*UNUSED*>          swr   : SynWr.T;
                                          loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    wr1: Wr.T;
    rd1: Rd.T;
  BEGIN
    TRY
      CASE NARROW(opCode, PickleOpCode).code OF
      | PickleCode.Failure => RETURN pickleFailureException;
      | PickleCode.Write =>
          TYPECASE args[1] OF
          | ValWr (node) => wr1 := node.wr;
          ELSE
            ObValue.BadArgType(1, "wr", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Pickle.Write(
            wr1, NEW(PickleVersion, version := CurrentPickleVersion));
          (*
          Pickle.Write(
            wr1, ObValue.CopyValToLocal(args[2], ObValue.NewTbl(), loc));
          *)
          Pickle.Write(wr1, args[2]);
          
          RETURN ObValue.valOk;
      | PickleCode.Read =>
          TYPECASE args[1] OF
          | ValRd (node) => rd1 := node.rd;
          ELSE
            ObValue.BadArgType(1, "rd", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE Pickle.Read(rd1) OF
          | PickleVersion (p) =>
              IF p.version # CurrentPickleVersion THEN
                RAISE Pickle.Error("");
              END;
          ELSE
            RAISE Pickle.Error("");
          END;
          (*
          RETURN ObValue.CopyLocalToVal(
                   Pickle.Read(rd1), ObValue.NewTbl(), loc);
          *)
          RETURN Pickle.Read(rd1);
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
    EXCEPT
    | Pickle.Error(t) =>
        ObValue.RaiseException(pickleFailureException, opCode.name &
          " '" & t & "'", loc);
      <*ASSERT FALSE*>
    | Wr.Failure(atoms) =>
        ObValue.RaiseException(wrFailureException, opCode.name & 
          " '" & ObError.AtomListToText(atoms) & "'", loc);
      <*ASSERT FALSE*>
    | Rd.Failure(atoms) =>
        ObValue.RaiseException(rdFailureException, opCode.name & 
          " '" & ObError.AtomListToText(atoms) & "'", loc);
      <*ASSERT FALSE*>
    | Rd.EndOfFile =>
        ObValue.RaiseException(rdEofFailureException, opCode.name &
          " endOfFile", loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    END;
  END EvalPickle;

(* ============ "process" package ============ *)

TYPE

  ProcCode = {New, In, Out, Err, Complete, Filter, GetID, GetMyID,
              GetWorkingDirectory, SetWorkingDirectory};

  ProcOpCode = ObLib.OpCode OBJECT code: ProcCode;  END;

  PackageProc = ObLib.T OBJECT OVERRIDES Eval := EvalProc; END;

PROCEDURE NewProcOC (name: TEXT; arity: INTEGER; code: ProcCode):
  ProcOpCode =
  BEGIN
    RETURN NEW(ProcOpCode, name := name, arity := arity, code := code);
  END NewProcOC;

PROCEDURE SetupProc () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(ProcCode));
    opCodes^ := OpCodes{NewProcOC("new", 4, ProcCode.New),
                        NewProcOC("id", 1, ProcCode.GetID),
                        NewProcOC("myId", -1, ProcCode.GetMyID),
                        NewProcOC("in", 1, ProcCode.In),
                        NewProcOC("out", 1, ProcCode.Out),
                        NewProcOC("err", 1, ProcCode.Err),
                        NewProcOC("complete", 1, ProcCode.Complete),
                        NewProcOC("filter", 4, ProcCode.Filter),
                        NewProcOC("getWorkingDirectory", 1, 
                                  ProcCode.GetWorkingDirectory),
                        NewProcOC("setWorkingDirectory", 2, 
                                  ProcCode.SetWorkingDirectory)};
    ObLib.Register(NEW(PackageProc, name := "process", opCodes := opCodes));
    ObValue.InhibitTransmission(
      TYPECODE(ObValue.ValProcess), "processes cannot be transmitted/duplicated");
  END SetupProc;

PROCEDURE EvalProc (                    self  : PackageProc;
                                        opCode: ObLib.OpCode;
                    <*UNUSED*>          arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                                        temp  : BOOLEAN;
                    <*UNUSED*>          swr   : SynWr.T;
                                        loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  TYPE Texts = REF ARRAY OF TEXT;
  VAR
    val                                               : ObValue.Val;
    text1,text2                                       : TEXT;
    size, int                                         : INTEGER;
    proc                                              : ObValue.RemProcess;
    proc1                                             : ObValue.ValProcess;
    texts                                             : Texts;
    stdinR, stdinW, stdoutR, stdoutW, stderrR, stderrW: Pipe.T := NIL;
    stdinWr                                           : Wr.T;
    stdoutRd, stderrRd                                : Rd.T;
    bool1                                             : BOOLEAN;
    array1                                            : REF ObValue.Vals;
    processor1: ObValue.ValProcessor;
  BEGIN
    TRY
      CASE NARROW(opCode, ProcOpCode).code OF
      | ProcCode.New =>
          TYPECASE args[1] OF
          | ObValue.ValProcessor (node) => processor1 := node;
          ELSE
            ObValue.BadArgType(1, "processor", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          (*
          IF processor1 # ObValue.localProcessor THEN
            ObValue.BadArgVal(
              1, "the local processor", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          *)
          TYPECASE args[2] OF
          | ObValue.ValArray (node) => array1 := node.Obtain();
          ELSE
            ObValue.BadArgType(2, "array", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(3, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(4, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Length(text1) = 0 THEN text1 := NIL END;

          size := NUMBER(array1^);
          IF size = 0 THEN
            ObValue.BadArgVal(2, "non-empty", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;

          texts := NEW(Texts, size);
          FOR i := 0 TO size - 1 DO
            TYPECASE array1^[i] OF
            | ObValue.ValText (node) => texts^[i] := node.text;
            ELSE
              ObValue.BadArgType(
                2, "array(text)", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
          END;
          IF processor1 = ObValue.localProcessor THEN
            TRY
              Pipe.Open( (*out*)stdinR, (*out*) stdinW);
              Pipe.Open( (*out*)stdoutR, (*out*) stdoutW);
              IF bool1 THEN
                stderrR := stdoutR;
                stderrW := NIL;
              ELSE
                Pipe.Open( (*out*)stderrR, (*out*) stderrW);
              END;
              proc := ObValue.NewProcess(
                          Process.Create(texts^[0], 
                                         SUBARRAY(texts^, 1, NUMBER(texts^) - 1),
                                         NIL, text1, stdinR, stdoutW, stderrW));
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
            EXCEPT 
            | OSError.E(e) => 
              TRY IF stdinR # NIL THEN stdinR.close() END EXCEPT OSError.E =>END;
              TRY IF stdoutR # NIL THEN stdoutR.close() END EXCEPT OSError.E =>END;
              TRY IF stderrR # NIL THEN stderrR.close() END EXCEPT OSError.E =>END;
              TRY IF stdinW # NIL THEN stdinW.close() END EXCEPT OSError.E =>END;
              TRY IF stdoutW # NIL THEN stdoutW.close() END EXCEPT OSError.E =>END;
              TRY IF stderrW # NIL THEN stderrW.close() END EXCEPT OSError.E =>END;
              RAISE OSError.E(e);
            END;
          ELSE
            proc := processor1.remote.CreateProcess(
                                   texts^[0], 
                                   SUBARRAY(texts^, 1, NUMBER(texts^) - 1),
                                   NIL, text1, bool1, 
                                   stdinWr, stdoutRd, stderrRd);
            IF bool1 THEN
              <*ASSERT stderrRd = NIL *>
              stderrRd := stdoutRd;
            END;              
          END;
          RETURN NEW(ObValue.ValProcess, 
                     what := "<a process>", tag := "Process",
                     picklable := FALSE, 
                     remote := proc,
                     in := ObValue.NewWr(stdinWr, "<a process stdin writer>"),
                     out := ObValue.NewRd(stdoutRd, "<a process stdout reader>"),
                     err := ObValue.NewRd(stderrRd, "<a process stderr reader>"));
      | ProcCode.GetMyID =>
          RETURN Obliq.NewInt(Process.GetMyID());
      | ProcCode.GetID =>
          TYPECASE args[1] OF
          | ObValue.ValProcess (node) => proc1 := node;
          ELSE
            ObValue.BadArgType(1, "process", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN Obliq.NewInt(proc1.remote.GetID());
      | ProcCode.In =>
          TYPECASE args[1] OF
          | ObValue.ValProcess (node) => proc1 := node;
          ELSE
            ObValue.BadArgType(1, "process", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN proc1.in;
      | ProcCode.Out =>
          TYPECASE args[1] OF
          | ObValue.ValProcess (node) => proc1 := node;
          ELSE
            ObValue.BadArgType(1, "process", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN proc1.out;
      | ProcCode.Err =>
          TYPECASE args[1] OF
          | ObValue.ValProcess (node) => proc1 := node;
          ELSE
            ObValue.BadArgType(1, "process", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN proc1.err;
      | ProcCode.Complete =>
          TYPECASE args[1] OF
          | ObValue.ValProcess (node) => proc1 := node;
          ELSE
            ObValue.BadArgType(1, "process", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          int := proc1.remote.Wait();
          Wr.Close(proc1.in.wr);
          Rd.Close(proc1.out.rd);
          Rd.Close(proc1.err.rd);
          RETURN NEW(ObValue.ValInt, int := int, temp := temp);
      | ProcCode.Filter =>
          TYPECASE args[1] OF
          | ObValue.ValProcessor (node) => processor1 := node;
          ELSE
            ObValue.BadArgType(1, "processor", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          (*
          IF processor1 # ObValue.localProcessor THEN
            ObValue.BadArgVal(
              1, "the local processor", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          *)
          TYPECASE args[2] OF
          | ObValue.ValArray (node) => array1 := node.Obtain();
          ELSE
            ObValue.BadArgType(2, "array", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Length(text1) = 0 THEN text1 := NIL END;

          TYPECASE args[4] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(4, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;

          size := NUMBER(array1^);
          IF size = 0 THEN
            ObValue.BadArgVal(2, "non-empty", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          texts := NEW(Texts, size);
          FOR i := 0 TO size - 1 DO
            TYPECASE array1^[i] OF
            | ObValue.ValText (node) => texts^[i] := node.text;
            ELSE
              ObValue.BadArgType(
                2, "array(text)", self.name, opCode.name, loc); <*ASSERT FALSE*>
            END;
          END;
          IF processor1 = ObValue.localProcessor THEN
            TRY
              Pipe.Open( (*out*)stdinR, (*out*) stdinW);
              Pipe.Open( (*out*)stdoutR, (*out*) stdoutW);
              proc := ObValue.NewProcess(
                          Process.Create(texts^[0], 
                                         SUBARRAY(texts^, 1, NUMBER(texts^) - 1),
                                         NIL, text1, stdinR, stdoutW, NIL));
              stdinR.close();
              stdoutW.close();
              stdinWr := NEW(FileWr.T).init(stdinW);
              stdoutRd := NEW(FileRd.T).init(stdoutR);
            EXCEPT 
            | OSError.E(e) => 
              TRY IF stdinR # NIL THEN stdinR.close() END EXCEPT OSError.E =>END;
              TRY IF stdoutR # NIL THEN stdoutR.close() END EXCEPT OSError.E =>END;
              TRY IF stderrR # NIL THEN stderrR.close() END EXCEPT OSError.E =>END;
              TRY IF stdinW # NIL THEN stdinW.close() END EXCEPT OSError.E =>END;
              TRY IF stdoutW # NIL THEN stdoutW.close() END EXCEPT OSError.E =>END;
              TRY IF stderrW # NIL THEN stderrW.close() END EXCEPT OSError.E =>END;
              RAISE OSError.E(e);
            END;
          ELSE
            proc := processor1.remote.CreateProcess(
                                   texts^[0], 
                                   SUBARRAY(texts^, 1, NUMBER(texts^) - 1),
                                   NIL, text1, TRUE, 
                                   stdinWr, stdoutRd, stderrRd);
            <*ASSERT stderrRd = NIL *>
          END;
          Wr.PutText(stdinWr, text2);
          Wr.Close(stdinWr);
          val := ObValue.NewText(Rd.GetText(stdoutRd, LAST(CARDINAL)));
          Rd.Close(stdoutRd);
          EVAL proc.Wait();
          RETURN val;
      | ProcCode.GetWorkingDirectory =>
          TYPECASE args[1] OF
          | ObValue.ValProcessor (node) => processor1 := node;
          ELSE
            ObValue.BadArgType(1, "processor", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          (*
          IF processor1 # ObValue.localProcessor THEN
            ObValue.BadArgVal(
              1, "the local processor", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          *)
          RETURN ObValue.NewText(processor1.remote.GetWorkingDirectory());
      | ProcCode.SetWorkingDirectory =>
          TYPECASE args[1] OF
          | ObValue.ValProcessor (node) => processor1 := node;
          ELSE
            ObValue.BadArgType(1, "processor", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          (*
          IF processor1 # ObValue.localProcessor THEN
            ObValue.BadArgVal(
              1, "the local processor", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          *)
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          processor1.remote.SetWorkingDirectory(text1);
          RETURN ObValue.valOk;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
    EXCEPT
    | OSError.E, ObValue.ServerError =>
        ObValue.RaiseException(ObValue.osError, self.name & "_" &
                                        opCode.name, loc);
        <*ASSERT FALSE*>
    | Wr.Failure =>
        ObValue.RaiseException(wrFailureException, opCode.name, loc);
      <*ASSERT FALSE*>
    | Rd.Failure =>
        ObValue.RaiseException(rdFailureException, opCode.name, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    END;
  END EvalProc;

(* ============ "random" package ============ *)

TYPE
  RandomCode = {Int, Real};

  RandomOpCode = ObLib.OpCode OBJECT code: RandomCode;  END;

  PackageRandom = ObLib.T OBJECT OVERRIDES Eval := EvalRandom; END;

PROCEDURE NewRandomOC (name: TEXT; arity: INTEGER; code: RandomCode):
  RandomOpCode =
  BEGIN
    RETURN NEW(RandomOpCode, name := name, arity := arity, code := code);
  END NewRandomOC;

PROCEDURE SetupRandom () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(RandomCode));
    opCodes^ := OpCodes{NewRandomOC("int", 2, RandomCode.Int),
                        NewRandomOC("real", 2, RandomCode.Real)};
    ObLib.Register(
      NEW(PackageRandom, name := "random", opCodes := opCodes));
  END SetupRandom;

PROCEDURE EvalRandom (                    self  : PackageRandom;
                                          opCode: ObLib.OpCode;
                      <*UNUSED*>          arity : ObLib.OpArity;
                                 READONLY args  : ObValue.ArgArray;
                      <*UNUSED*>          temp  : BOOLEAN;
                      <*UNUSED*>          swr   : SynWr.T;
                                          loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR
    real1, real2: LONGREAL;
    int1, int2  : INTEGER;
  BEGIN
    CASE NARROW(opCode, RandomOpCode).code OF
    | RandomCode.Int =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int2 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        LOCK randomMu DO
          RETURN Obliq.NewInt(random.integer(int1, int2))
        END;
    | RandomCode.Real =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValReal (node) => real2 := node.real;
        ELSE
          ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        LOCK randomMu DO
          RETURN Obliq.NewReal(random.longreal(real1, real2))
        END;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
    END;
  END EvalRandom;

VAR 
  randomMu := NEW(MUTEX);
  random := NEW(Random.Default).init();

(* ============ "dict" package ============ *)

TYPE
  DictCode = {New, Get, Put, Delete, Size, Iterate, InvalidKey,
	        IteratorInit, IteratorNext};

  DictOpCode = ObLib.OpCode OBJECT code: DictCode;  END;

  PackageDict = ObLib.T OBJECT OVERRIDES Eval := EvalDict; END;

VAR invalidKeyException: ObValue.ValException;

PROCEDURE NewDictOC (name: TEXT; arity: INTEGER; code: DictCode):
  DictOpCode =
  BEGIN
    RETURN NEW(DictOpCode, name := name, arity := arity, code := code);
  END NewDictOC;

PROCEDURE SetupDict () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(DictCode));
    opCodes^ := OpCodes{NewDictOC("new", 0, DictCode.New),
                        NewDictOC("invalidKey", -1, DictCode.InvalidKey),
                        NewDictOC("get", 2, DictCode.Get),
                        NewDictOC("put", 3, DictCode.Put),
                        NewDictOC("delete", 2, DictCode.Delete),
                        NewDictOC("size", 1, DictCode.Size),
                        NewDictOC("iterate", 1, DictCode.Iterate),
                        NewDictOC("iteratorInit", 2, DictCode.IteratorInit),
                        NewDictOC("iteratorNext", 1, DictCode.IteratorNext)};
    ObLib.Register(
      NEW(PackageDict, name := "dict", opCodes := opCodes));
    invalidKeyException := NEW(ObValue.ValException, name := "dict_invalidKey");
  END SetupDict;

PROCEDURE EvalDict (                    self  : PackageDict;
                                          opCode: ObLib.OpCode;
                      <*UNUSED*>          arity : ObLib.OpArity;
                                 READONLY args  : ObValue.ArgArray;
                      <*UNUSED*>          temp  : BOOLEAN;
                      <*UNUSED*>          swr   : SynWr.T;
                                          loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    dict1: TextRefTbl.T;
    iterator1: TextRefTbl.Iterator;
    text1: TEXT;
    ref1 : Refany.T;
  BEGIN
    CASE NARROW(opCode, DictOpCode).code OF
    | DictCode.InvalidKey => RETURN invalidKeyException;
    | DictCode.New =>
      RETURN NewDict(NEW(TextRefTbl.Default).init());
    | DictCode.Get =>
      TYPECASE args[1] OF
      | ValDict (node) => dict1 := node.dict;
      ELSE
        ObValue.BadArgType(1, "dict", self.name, opCode.name, loc); 
        <*ASSERT FALSE*>
      END;
      TYPECASE args[2] OF
      | ObValue.ValText (node) => text1 := node.text;
      ELSE
        ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
        <*ASSERT FALSE*>
      END;
      IF dict1.get(text1,ref1) THEN
        TYPECASE ref1 OF
        | ObValue.Val (v) => RETURN v;
        ELSE
          <*ASSERT FALSE*>
        END;
      ELSE
        ObValue.RaiseException(
          invalidKeyException, self.name & "_" & opCode.name, loc);
        <*ASSERT FALSE*>
      END;
    | DictCode.Put =>
      TYPECASE args[1] OF
      | ValDict (node) => dict1 := node.dict;
      ELSE
        ObValue.BadArgType(1, "dict", self.name, opCode.name, loc); 
        <*ASSERT FALSE*>
      END;
      TYPECASE args[2] OF
      | ObValue.ValText (node) => text1 := node.text;
      ELSE
        ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
        <*ASSERT FALSE*>
      END;
      RETURN Obliq.NewBool(dict1.put(text1,args[3]));
    | DictCode.Delete =>
      TYPECASE args[1] OF
      | ValDict (node) => dict1 := node.dict;
      ELSE
        ObValue.BadArgType(1, "dict", self.name, opCode.name, loc); 
        <*ASSERT FALSE*>
      END;
      TYPECASE args[2] OF
      | ObValue.ValText (node) => text1 := node.text;
      ELSE
        ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
        <*ASSERT FALSE*>
      END;
      IF dict1.delete(text1,ref1) THEN
        TYPECASE ref1 OF
        | ObValue.Val (v) => RETURN v;
        ELSE
          <*ASSERT FALSE*>
        END;
      ELSE
        ObValue.RaiseException(
          invalidKeyException, self.name & "_" & opCode.name, loc);
        <*ASSERT FALSE*>
      END;
    | DictCode.Size =>
      TYPECASE args[1] OF
      | ValDict (node) => dict1 := node.dict;
      ELSE
        ObValue.BadArgType(1, "dict", self.name, opCode.name, loc); 
        <*ASSERT FALSE*>
      END;
      RETURN Obliq.NewInt(dict1.size());
    | DictCode.Iterate =>
      TYPECASE args[1] OF
      | ValDict (node) => dict1 := node.dict;
      ELSE
        ObValue.BadArgType(1, "dict", self.name, opCode.name, loc); 
        <*ASSERT FALSE*>
      END;
      RETURN NewIterator(dict1.iterate());
    | DictCode.IteratorInit =>
      TYPECASE args[2] OF
      | ValDict (node) => dict1 := node.dict;
      ELSE
        ObValue.BadArgType(2, "dict", self.name, opCode.name, loc); 
        <*ASSERT FALSE*>
      END;
      TYPECASE args[1] OF
      | ValIterator (node) => 
        node.iterator := node.iterator.init();
      ELSE
        ObValue.BadArgType(1, "iterator", self.name, opCode.name, loc); 
        <*ASSERT FALSE*>
      END;
      RETURN args[1];
    | DictCode.IteratorNext =>
      TYPECASE args[1] OF
      | ValIterator (node) => iterator1 := node.iterator;
      ELSE
        ObValue.BadArgType(1, "iterator", self.name, opCode.name, loc); 
        <*ASSERT FALSE*>
      END;
      IF iterator1.next(text1,ref1) THEN
        TYPECASE ref1 OF
        | ObValue.Val (v) =>
          WITH arr = ARRAY [0..1] OF Obliq.Val{ObValue.NewText(text1),v} DO
            RETURN Obliq.NewArray(arr);
          END;
        ELSE
          <*ASSERT FALSE*>
        END;
      ELSE
        RETURN Obliq.ok;
      END;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
    END;
  END EvalDict;

PROCEDURE IsDict(self: ValDict; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValDict (oth) => RETURN self.dict = oth.dict;
    ELSE
      RETURN FALSE
    END;
  END IsDict;

PROCEDURE CopyDict(self: ValDict; <*UNUSED*>tbl: ObValue.Tbl;
                   <*UNUSED*>loc: SynLocation.T): ObValue.ValAnything =
  VAR key: TEXT; val: Refany.T;
  BEGIN
    WITH t = NEW(TextRefTbl.Default).init(), i = self.dict.iterate() DO
      WHILE i.next(key,val) DO
        EVAL t.put(key,val);
      END;
      RETURN NewDict(t);
    END;
  END CopyDict;

PROCEDURE IsIterator(self: ValIterator; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValIterator (oth) => RETURN self.iterator = oth.iterator;
    ELSE
      RETURN FALSE
    END;
  END IsIterator;

PROCEDURE CopyIterator(self: ValIterator; <*UNUSED*>tbl: ObValue.Tbl;
                       <*UNUSED*>loc: SynLocation.T): ObValue.ValAnything =
  BEGIN
    RETURN self;
  END CopyIterator; 

PROCEDURE NewDict(tbl: TextRefTbl.T): ObValue.Val =
  BEGIN
    RETURN NEW(ValDict, what := "<a Dictionary>",
                     tag := "Dictionary", picklable := TRUE, 
                     dict := tbl);
  END NewDict; 

PROCEDURE NewIterator(it: TextRefTbl.Iterator): ObValue.Val =
  BEGIN
    RETURN NEW(ValIterator, what := "<a Dictionary.Iterator>",
               tag := "Dictionary`Iterator", picklable := FALSE, 
               iterator := it);
  END NewIterator;

(* ============ "tcp" package ============ *)

PROCEDURE IsConn(self: ValConnector; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValConnector (oth) => RETURN self.conn = oth.conn;
    ELSE
      RETURN FALSE
    END;
  END IsConn;

PROCEDURE CopyConn(self: ValConnector; <*UNUSED*>tbl: ObValue.Tbl;
                   <*UNUSED*>loc: SynLocation.T): ObValue.ValAnything =
  BEGIN
    RETURN self;
  END CopyConn;

PROCEDURE IsTCP(self: ValTCP; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValTCP (oth) => RETURN self.tcp = oth.tcp;
    ELSE
      RETURN FALSE
    END;
  END IsTCP;

PROCEDURE CopyTCP(self: ValTCP; <*UNUSED*>tbl: ObValue.Tbl;
                  <*UNUSED*>loc: SynLocation.T): ObValue.ValAnything =
  BEGIN
    RETURN self;
  END CopyTCP;

TYPE
  TcpCode = {Error, GetHostByName, GetCanonicalByName,
             GetCanonicalByAddr, GetHostAddr, NewConnector,
             GetEndPoint, CloseConnector, Connect, Accept, Close,
             Eof, StartConnect, FinishConnect,
             GetPeer, GetPeerName, MatchPeer, LocalEndpoint, 
             GetRd, GetWr};

  TcpOpCode = ObLib.OpCode OBJECT code: TcpCode;  END;

  PackageTcp = ObLib.T OBJECT OVERRIDES Eval := EvalTcp; END;

VAR tcpErrorException: ObValue.ValException;

PROCEDURE NewTcpOC (name: TEXT; arity: INTEGER; code: TcpCode):
  TcpOpCode =
  BEGIN
    RETURN NEW(TcpOpCode, name := name, arity := arity, code := code);
  END NewTcpOC;

PROCEDURE SetupTcp () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(TcpCode));
    opCodes^ := OpCodes{NewTcpOC("error", -1, TcpCode.Error),
                        NewTcpOC("getHostByName", 1, TcpCode.GetHostByName),
                        NewTcpOC("getCanonicalByName", 1, 
                                 TcpCode.GetCanonicalByName),
                        NewTcpOC("getCanonicalByAddr", 1, 
                                 TcpCode.GetCanonicalByAddr),
                        NewTcpOC("getHostAddr", 0, TcpCode.GetHostAddr),
                        NewTcpOC("newConnector", 1, TcpCode.NewConnector),
                        NewTcpOC("getEndPoint", 1, TcpCode.GetEndPoint),
                        NewTcpOC("closeConnector", 1, TcpCode.CloseConnector),
                        NewTcpOC("connect", 1, TcpCode.Connect),
                        NewTcpOC("accept", 1, TcpCode.Accept),
                        NewTcpOC("close", 1, TcpCode.Close),
                        NewTcpOC("eof", 1, TcpCode.Eof),
                        NewTcpOC("startConnect", 1, TcpCode.StartConnect),
                        NewTcpOC("finishConnect", 2, TcpCode.FinishConnect),
                        NewTcpOC("getPeer", 1, TcpCode.GetPeer),
                        NewTcpOC("getPeerName", 1, TcpCode.GetPeerName),
                        NewTcpOC("matchPeer", 3, TcpCode.MatchPeer),
                        NewTcpOC("localEndpoint", 1, TcpCode.LocalEndpoint),
                        NewTcpOC("getRd", 1, TcpCode.GetRd),
                        NewTcpOC("getWr", 1, TcpCode.GetWr)
                        };
    ObLib.Register(
      NEW(PackageTcp, name := "tcp", opCodes := opCodes));
    tcpErrorException := NEW(ObValue.ValException, name := "tcp_error");
  END SetupTcp;

PROCEDURE EvalTcp (                    self  : PackageTcp;
                                          opCode: ObLib.OpCode;
                      <*UNUSED*>          arity : ObLib.OpArity;
                                 READONLY args  : ObValue.ArgArray;
                      <*UNUSED*>          temp  : BOOLEAN;
                      <*UNUSED*>          swr   : SynWr.T;
                                          loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    conn1: TCP.Connector;
    tcp1 : TCP.T;
    text1: TEXT;
    ep1  : IP.Endpoint;
    addr1: IP.Address;
    real1: LONGREAL;
    int1 : INTEGER;
  BEGIN
    TRY
    CASE NARROW(opCode, TcpOpCode).code OF
    | TcpCode.Error => RETURN tcpErrorException;
    | TcpCode.GetHostByName =>
      TYPECASE args[1] OF
      | ObValue.ValText (node) => text1 := node.text;
      ELSE
        ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
        <*ASSERT FALSE*> 
      END;
      IF IP.GetHostByName(text1, addr1) THEN
        RETURN FromAddress(addr1);
      ELSE
        RETURN ObValue.valOk;
      END;
    | TcpCode.GetCanonicalByName =>
      TYPECASE args[1] OF
      | ObValue.ValText (node) => text1 := node.text;
      ELSE
        ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
        <*ASSERT FALSE*> 
      END;
      RETURN Obliq.NewText(IP.GetCanonicalByName(text1));
    | TcpCode.GetCanonicalByAddr =>
      GetAddress(addr1, args, 1, self, opCode, loc);
      RETURN Obliq.NewText(IP.GetCanonicalByAddr(addr1));
    | TcpCode.GetHostAddr =>
      addr1 := IP.GetHostAddr();
      RETURN FromAddress(addr1);
    | TcpCode.GetEndPoint =>
      TYPECASE args[1] OF
      | ValConnector (node) => conn1 := node.conn;
      ELSE
        ObValue.BadArgType(1, "connector", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      RETURN FromEndPoint(TCP.GetEndPoint(conn1));
    | TcpCode.NewConnector =>
      GetEndPoint(ep1, args, 1, self, opCode, loc);
      RETURN NewConnector(TCP.NewConnector(ep1));
    | TcpCode.CloseConnector =>
      TYPECASE args[1] OF
      | ValConnector (node) => conn1 := node.conn;
      ELSE
        ObValue.BadArgType(1, "connector", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      TCP.CloseConnector(conn1);
      RETURN ObValue.valOk;
    | TcpCode.Connect =>
      GetEndPoint(ep1, args, 1, self, opCode, loc);
      RETURN NewTCP(TCP.Connect(ep1));
    | TcpCode.Accept =>
      TYPECASE args[1] OF
      | ValConnector (node) => conn1 := node.conn;
      ELSE
        ObValue.BadArgType(1, "connector", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      RETURN NewTCP(TCP.Accept(conn1));
    | TcpCode.Close =>
      TYPECASE args[1] OF
      | ValTCP (node) => tcp1 := node.tcp;
      ELSE
        ObValue.BadArgType(1, "tcp", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      TCP.Close(tcp1);
      RETURN ObValue.valOk;
    | TcpCode.Eof =>
      TYPECASE args[1] OF
      | ValTCP (node) => tcp1 := node.tcp;
      ELSE
        ObValue.BadArgType(1, "tcp", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      IF TCPSpecial.EOF(tcp1) THEN RETURN true ELSE RETURN false END;
    | TcpCode.StartConnect =>
      GetEndPoint(ep1, args, 1, self, opCode, loc);
      RETURN NewTCP(TCPSpecial.StartConnect(ep1));
    | TcpCode.FinishConnect =>
      TYPECASE args[1] OF
      | ValTCP (node) => tcp1 := node.tcp;
      ELSE
        ObValue.BadArgType(1, "tcp", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      TYPECASE args[2] OF
      | ObValue.ValReal (node) => real1 := node.real;
      ELSE
        ObValue.BadArgType(2, "real", self.name, opCode.name, loc);
        <*ASSERT FALSE*>
      END;
      IF TCPSpecial.FinishConnect(tcp1,real1) THEN 
        RETURN true
      ELSE
        RETURN false
      END;
    | TcpCode.GetPeer =>
      TYPECASE args[1] OF
      | ValTCP (node) => tcp1 := node.tcp;
      ELSE
        ObValue.BadArgType(1, "tcp", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      RETURN FromEndPoint(TCPPeer.Get(tcp1));
    | TcpCode.GetPeerName =>
      TYPECASE args[1] OF
      | ValTCP (node) => tcp1 := node.tcp;
      ELSE
        ObValue.BadArgType(1, "tcp", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      RETURN Obliq.NewText(TCPPeer.GetName(tcp1));
    | TcpCode.MatchPeer =>
      TYPECASE args[1] OF
      | ValTCP (node) => tcp1 := node.tcp;
      ELSE
        ObValue.BadArgType(1, "tcp", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      GetAddress(addr1, args, 2, self, opCode, loc);
      TYPECASE args[3] OF
      | ObValue.ValInt (node) => int1 := node.int;
      ELSE
        ObValue.BadArgType(3, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
      IF TCPPeer.Match(tcp1,addr1,int1) THEN
        RETURN true
      ELSE
        RETURN false
      END;
    | TcpCode.LocalEndpoint =>
      TYPECASE args[1] OF
      | ValTCP (node) => tcp1 := node.tcp;
      ELSE
        ObValue.BadArgType(1, "tcp", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      RETURN FromEndPoint(TCPExtras.LocalEndpoint(tcp1));
    | TcpCode.GetRd =>
      TYPECASE args[1] OF
      | ValTCP (node) => tcp1 := node.tcp;
      ELSE
        ObValue.BadArgType(1, "tcp", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      WITH rd = ConnRW.NewRd(tcp1) DO
        RETURN ObValue.NewRd(rd, "<a tcp reader>");
      END;
    | TcpCode.GetWr =>
      TYPECASE args[1] OF
      | ValTCP (node) => tcp1 := node.tcp;
      ELSE
        ObValue.BadArgType(1, "tcp", self.name, opCode.name,
                           loc);
        <*ASSERT FALSE*> 
      END;
      WITH wr = ConnRW.NewWr(tcp1) DO
        RETURN ObValue.NewWr(wr, "<a tcp writer>");
      END;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
    EXCEPT
    | IP.Error(atoms) =>
      ObValue.RaiseException(tcpErrorException, 
                             self.name & "_" & opCode.name & ": " &
                             ObError.AtomListToText(atoms), loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalTcp;

PROCEDURE NewConnector(conn: TCP.Connector): ObValue.Val =
  BEGIN
    RETURN NEW(ValConnector, what := "<a TCP.Connector>",
                     tag := "TCP`Connector", picklable := FALSE, 
                     conn := conn);
  END NewConnector;

PROCEDURE NewTCP(tcp: TCP.T): ObValue.Val =
  BEGIN
    RETURN NEW(ValTCP, what := "<a TCP.T>",
                     tag := "TCP`T", picklable := FALSE, 
                     tcp := tcp);
  END NewTCP; 

PROCEDURE FromEndPoint (ep: IP.Endpoint): ObValue.Val =
  VAR arr: ARRAY [0..4] OF INTEGER;
  BEGIN
    FOR i := 0 TO 3 DO arr[i] :=  ep.addr.a[i] END;
    arr[4] := ep.port;
    RETURN Obliq.NewIntArray(arr);
  END FromEndPoint;

PROCEDURE FromAddress (addr: IP.Address): ObValue.Val =
  VAR arr: ARRAY [0..3] OF INTEGER;
  BEGIN
    FOR i := 0 TO 3 DO arr[i] :=  addr.a[i] END;
    RETURN Obliq.NewIntArray(arr);
  END FromAddress; 

PROCEDURE GetEndPoint (VAR ep     : IP.Endpoint;
                           args   : ObValue.ArgArray;
                           idx    : INTEGER;
                           package: ObLib.T;
                           opCode : ObLib.OpCode;
                           loc    : SynLocation.T     )
  RAISES {ObValue.Error, ObValue.Exception} =
  VAR array: ARRAY [0 .. 4] OF INTEGER;
  BEGIN
    TRY
      TYPECASE args[idx] OF
      | ObValue.ValArray (node) =>
          IF node.Size() # 5 THEN
            ObValue.BadArgType(
              idx, "array of 5 integers", package.name, opCode.name, loc);
          ELSE
            Obliq.ToIntArray(node, array, loc);
            FOR i := 0 TO 3 DO ep.addr.a[i] := array[i] END;
            ep.port := array[4];
          END;
      ELSE
        ObValue.BadArgType(idx, "array", package.name, opCode.name, loc);
      END;
    EXCEPT
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, package.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          package.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          package.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    END;
  END GetEndPoint;

PROCEDURE GetAddress (VAR addr   : IP.Address;
                          args   : ObValue.ArgArray;
                          idx    : INTEGER;
                          package: ObLib.T;
                          opCode : ObLib.OpCode;
                          loc    : SynLocation.T     )
  RAISES {ObValue.Error, ObValue.Exception} =
  VAR array: ARRAY [0 .. 4] OF INTEGER;
  BEGIN
    TRY
      TYPECASE args[idx] OF
      | ObValue.ValArray (node) =>
          IF node.Size() # 4 THEN
            ObValue.BadArgType(
              idx, "array of 4 integers", package.name, opCode.name, loc);
          ELSE
            Obliq.ToIntArray(node, array, loc);
            FOR i := 0 TO 3 DO addr.a[i] := array[i] END;
          END;
      ELSE
        ObValue.BadArgType(idx, "array", package.name, opCode.name, loc);
      END;
    EXCEPT
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, package.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          package.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          package.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    END;
  END GetAddress;

BEGIN
END ObLibM3.
