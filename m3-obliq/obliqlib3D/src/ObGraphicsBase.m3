(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Feb 16 20:46:10 PST 1995 by najork                   *)
(*       Created on Mon Mar  7 13:05:02 PST 1994 by najork                   *)


MODULE ObGraphicsBase;


IMPORT GraphicsBase, ObAux, ObCommand, ObLib, ObProtoLoader, ObProxiedObj, 
       ObValue, Obliq, SynLocation;


CONST 
  pkgname = "GraphicsBase";

(*****************************************************************************)
(* Wrapper for RootGO.T                                                      *)
(*****************************************************************************)


REVEAL
  T = ObProxiedObj.T BRANDED "ObGraphicsBase.T" OBJECT END;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : GraphicsBase.T 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | T (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetArg;


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
            NewOpCode ("Failure", -1, Code.Failure)
        };

    Failure := NEW (ObValue.ValException, name := pkgname & "_Failure");

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    loader.load ("GraphicsBase.obl");
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {Failure};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;

  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;


PROCEDURE DoEval (<* UNUSED *> self         : Package; 
                               opCode       : ObLib.OpCode; 
                  <* UNUSED *> arity        : ObLib.OpArity; 
                  <* UNUSED *> READONLY args: ObValue.ArgArray;
                  <* UNUSED *> temp         : BOOLEAN;
                  <* UNUSED *> loc          : SynLocation.T) : ObValue.Val =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
      Code.Failure => RETURN Failure;
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
END ObGraphicsBase.
