(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:05:05 1997
 *)

MODULE ObLibOnline;
IMPORT SynWr, SynLocation, TextRd, SynScan, ObLib, ObValue,
       ObPrintValue, ObFrame, Pickle2 AS Pickle; 

TYPE

  OnlineCode = {GetSearchPath, SetSearchPath, PushSilence, PopSilence,
                Print, PrintText, PrintFlush, SetPrompt};

  OnlineOpCode = ObLib.OpCode OBJECT code: OnlineCode;  END;

  PackageOnline =
    ObLib.T OBJECT scanner: SynScan.T;  OVERRIDES Eval := EvalOnline; END;

VAR packageOnline: PackageOnline;

PROCEDURE NewOnlineOC (name: TEXT; arity: INTEGER; code: OnlineCode):
  OnlineOpCode =
  BEGIN
    RETURN NEW(OnlineOpCode, name := name, arity := arity, code := code);
  END NewOnlineOC;

PROCEDURE Setup () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(OnlineCode));
    opCodes^ :=
      OpCodes{NewOnlineOC("getSearchPath", 0, OnlineCode.GetSearchPath),
              NewOnlineOC("setSearchPath", 1, OnlineCode.SetSearchPath),
              NewOnlineOC("pushSilence", 0, OnlineCode.PushSilence),
              NewOnlineOC("popSilence", 0, OnlineCode.PopSilence),
              NewOnlineOC("print", 2, OnlineCode.Print),
              NewOnlineOC("printText", 1, OnlineCode.PrintText),
              NewOnlineOC("printFlush", 0, OnlineCode.PrintFlush),
              NewOnlineOC("setPrompt", 2, OnlineCode.SetPrompt)};
    packageOnline := NEW(PackageOnline, name := "sys", opCodes := opCodes,
                         scanner := NIL);
    ObLib.Register(packageOnline);
  END Setup;

PROCEDURE RegisterScanner (scanner: SynScan.T) =
  BEGIN
    packageOnline.scanner := scanner;
  END RegisterScanner;

PROCEDURE EvalOnline (                    self  : PackageOnline;
                                          opCode: ObLib.OpCode;
                      <*UNUSED*>          arity : ObLib.OpArity;
                                 READONLY args  : ObValue.ArgArray;
                      <*UNUSED*>          temp  : BOOLEAN;
                                          swr   : SynWr.T;
                                          loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR
    int1        : INTEGER;
    text1, text2: TEXT;
  BEGIN
    CASE NARROW(opCode, OnlineOpCode).code OF
    | OnlineCode.GetSearchPath =>
        RETURN ObValue.NewText(ObFrame.FmtSearchPath(ObFrame.searchPath));
    | OnlineCode.SetSearchPath =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
        END;
        ObFrame.searchPath := ObFrame.LexSearchPath(TextRd.New(text1));
        RETURN ObValue.valOk;
    | OnlineCode.Print =>
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
        END;
        ObPrintValue.PrintVal(swr, args[1], ObLib.libraries, NIL, int1);
        RETURN ObValue.valOk;
    | OnlineCode.PrintText =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
        END;
        SynWr.Text(swr, text1);
        RETURN ObValue.valOk;
    | OnlineCode.PrintFlush =>
        SynWr.Flush(swr);
        RETURN ObValue.valOk;
    | OnlineCode.PushSilence =>
        SynWr.PushSilence(swr);
        RETURN ObValue.valOk;
    | OnlineCode.PopSilence =>
        SynWr.PopSilence(swr);
        RETURN ObValue.valOk;
    | OnlineCode.SetPrompt =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
        END;
        TYPECASE args[2] OF
        | ObValue.ValText (node) => text2 := node.text;
        ELSE
          ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
        END;
        IF self.scanner # NIL THEN
          SynScan.SetPrompt(self.scanner, text1, text2);
        END;
        RETURN ObValue.valOk;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalOnline;

TYPE
  ObPackageSpecial = Pickle.Special BRANDED
        "ObLibOnline.ObPackageSpecial" OBJECT
                       OVERRIDES
                         write := WriteLib;
                         read := ReadLib;
                       END;
  
(* just use the same one locally! *)
PROCEDURE WriteLib (<*UNUSED*>ts: ObPackageSpecial; 
                    <*UNUSED*>ref: REFANY; <*UNUSED*> out: Pickle.Writer) =
  BEGIN
  END WriteLib; 

PROCEDURE ReadLib (<*UNUSED*>ts: ObPackageSpecial;
                   in: Pickle.Reader;
                   id: Pickle.RefID):REFANY =
  BEGIN
    in.noteRef(packageOnline, id);
    RETURN packageOnline;
  END ReadLib;

BEGIN
  Pickle.RegisterSpecial(NEW(ObPackageSpecial, sc := TYPECODE(PackageOnline)));
END ObLibOnline.
