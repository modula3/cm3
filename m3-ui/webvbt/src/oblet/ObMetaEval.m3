(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Dec 28 17:07:20 PST 1995 by najork                   *)
(*       Created on Thu Dec 28 15:18:59 PST 1995 by najork                   *)

MODULE ObMetaEval;

IMPORT Fmt, ObCommand, ObLib, ObLibOnline, ObValue, Obliq, ObliqParser, 
       SynLocation, SynParse, SynScan, SynWr, Text, TextRd;


CONST
  pkgname = "Meta";


TYPE 
  OpCode = ObLib.OpCode BRANDED OBJECT END;
    
  Package = ObLib.T BRANDED OBJECT 
  OVERRIDES
    Eval := DoEval;
  END;


VAR
  env    : Obliq.Env;
  parser : SynParse.T;


PROCEDURE SetupPackage () =
  BEGIN
    WITH opCodes = NEW (REF ARRAY OF ObLib.OpCode, 1) DO
      opCodes[0] := NEW (OpCode, name := "Eval", arity := 1);
      ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    END;

    ObLib.RegisterHelp (pkgname, Help);

    env    := Obliq.EmptyEnv ();
    parser := ObliqParser.New (SynWr.out);

(*    ObLibOnline.RegisterScanner (parser.Scanner ());*)
  END SetupPackage;


PROCEDURE Help (self : ObCommand.T; arg : TEXT; <* UNUSED *> data : REFANY) =
  BEGIN
    IF Text.Equal (arg, "!") THEN
      SynWr.Text (SynWr.out, 
                  "  " & Fmt.Pad (pkgname, 18, ' ', Fmt.Align.Left) & 
                  "(meta-level procedures)\n");
    ELSIF Text.Equal (arg, "?") THEN
      SynWr.Text (SynWr.out, "  Meta_Eval(code: Text): Ok");
      SynWr.NewLine (SynWr.out);
    ELSE
      SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
      SynWr.NewLine (SynWr.out);
    END;
  END Help;


PROCEDURE Load (code : TEXT): ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception } =
  VAR
    val : ObValue.Val;
  BEGIN
    SynWr.PushSilence (SynWr.out);
    ObliqParser.ReadFrom (parser, "", TextRd.New (code), TRUE, TRUE);
    LOOP
      TRY
(*        SynScan.FirstPrompt (parser.Scanner());*)
        WITH phrase = ObliqParser.ParsePhrase (parser) DO
          val := ObliqParser.EvalPhrase (parser, phrase, env);
        END;
      EXCEPT
      | ObliqParser.Eof => EXIT;
      END;
    END;
    SynWr.PopSilence (SynWr.out);

    IF val = NIL THEN
      val := Obliq.ok;
    END;

    RETURN val;
  END Load;


PROCEDURE DoEval (self         : Package; 
                  opCode       : ObLib.OpCode; 
     <* UNUSED *> arity        : ObLib.OpArity; 
                  READONLY args: ObValue.ArgArray; 
     <* UNUSED *> temp         : BOOLEAN;
                  loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    TYPECASE args[1] OF
      ObValue.ValText (node) => 
      RETURN Load (node.text);
    ELSE 
      ObValue.BadArgType (1, "text", self.name, opCode.name, loc);
      <* ASSERT FALSE *>
    END;
  END DoEval;


BEGIN
END ObMetaEval.
