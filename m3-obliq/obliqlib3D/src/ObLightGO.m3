(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:15:38 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 18:49:59 PST 1994 by najork                   *)


MODULE ObLightGO;

IMPORT LightGO, ObAux, ObBooleanProp, ObColorProp, ObCommand, ObGO, ObLib, 
       ObProp, ObProtoLoader, ObValue, SynLocation;


CONST
  pkgname = "LightGO";

(*****************************************************************************)
(* Wrapper for LightGO.T                                                     *)
(*****************************************************************************)


REVEAL
  T = ObGO.T BRANDED "ObLightGO.T" OBJECT END;


(*****************************************************************************)
(* Setup procedures                                                          *)
(*****************************************************************************)


PROCEDURE SetupPackage () =

  PROCEDURE NewOpCode (name : TEXT; arity : INTEGER; code : Code) : OpCode =
    BEGIN
      RETURN NEW (OpCode, name := name, arity := arity, code := code);
    END NewOpCode;

  TYPE 
    OpCodes = ARRAY OF ObLib.OpCode;
  VAR 
    opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW (REF OpCodes, NUMBER (Code));
    opCodes^ := 
        OpCodes {
            NewOpCode ("Color",    -1, Code.Color),
            NewOpCode ("Switch",   -1, Code.Switch),
            NewOpCode ("SetSwitch", 2, Code.SetSwitch),
            NewOpCode ("SetColor",  2, Code.SetColor)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    loader.load ("LightGO.obl");
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {Color, Switch, SetColor, SetSwitch};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;

  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;


PROCEDURE DoEval (self         : Package; 
                  opCode       : ObLib.OpCode; 
     <* UNUSED *> arity        : ObLib.OpArity; 
                  READONLY args: ObValue.ArgArray; 
     <* UNUSED *> temp         : BOOLEAN;
                  loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.Color =>
      RETURN ObProp.NameToObliq (LightGO.Colour);
    | Code.Switch =>
      RETURN ObProp.NameToObliq (LightGO.Switch);
    | Code.SetSwitch =>
      WITH go = ObGO.GetArg                   (args, 1, self, opCode, loc),
           b  = ObBooleanProp.GetOverloadedVal(args, 2, self, opCode, loc) DO
        go.setProp (LightGO.Switch.bind (b));
        RETURN ObValue.valOk;
      END;
    | Code.SetColor =>
      WITH go  = ObGO.GetArg                  (args, 1, self, opCode, loc),
           col = ObColorProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (LightGO.Colour.bind (col));
        RETURN ObValue.valOk;
      END;
    END;
  END DoEval;


(*****************************************************************************)
(* Help                                                                      *)
(*****************************************************************************)


PROCEDURE Help (self : ObCommand.T; arg : TEXT; <* UNUSED *> data : REFANY) =
  BEGIN
    ObAux.Help (self, arg, pkgname);
  END Help;


BEGIN
END ObLightGO.
