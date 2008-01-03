(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:52:38 PDT 1994 by najork                   *)
(*       Created on Wed Jul 20 09:48:11 PDT 1994 by najork                   *)


MODULE ObProxiedObj;

IMPORT ObAux, ObCommand, ObLib, ObProtoLoader, ObValue, Obliq, ProxiedObj, 
       SynLocation;


CONST
  pkgname = "ProxiedObj";


(*****************************************************************************)
(* Wrapper for ProxiedObj.T                                                  *)
(*****************************************************************************)

REVEAL 
  T = Public BRANDED "ObProxiedObj.T" OBJECT
  OVERRIDES
    Is := IsT;
  END;


PROCEDURE IsT (self: T; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF 
      T (oth) => RETURN self.po = oth.po;
    ELSE 
      RETURN FALSE 
    END;
  END IsT;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : ProxiedObj.T 
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
            NewOpCode ("Extend",   2, Code.Extend)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    loader.load ("ProxiedObj.obl");
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {Extend};

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
    | Code.Extend =>
      WITH po    = GetArg (args, 1, self, opCode, loc),
           proxy = po.proxy DO
        proxy.obj := Obliq.ObjectClone (Obliq.Vals {proxy.obj, args[2]});
        RETURN proxy.obj;
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
END ObProxiedObj.
