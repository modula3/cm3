(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:50:15 PDT 1994 by najork                   *)
(*       Created on Sat May 28 20:08:51 PDT 1994 by najork                   *)


MODULE ObAnimHandle;


IMPORT AnimHandle, AnimHandleProxy, ObAux, ObCommand, ObLib, ObProtoLoader, 
       ObProxiedObj, ObValue, Obliq, ProxiedObj, SynLocation;

CONST
  pkgname = "AnimHandle";


(*****************************************************************************)
(* Wrapper for AnimHandle.T                                                  *)
(*****************************************************************************)

TYPE
  T = ObProxiedObj.T BRANDED OBJECT END;

PROCEDURE AddTObj (ah : AnimHandle.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<an AnimHandle.T>", po := ah) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      ah.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddTObj;


PROCEDURE GetT (args    : ObValue.ArgArray; 
                idx     : INTEGER; 
                package : ObLib.T; 
                opCode  : ObLib.OpCode; 
                loc     : SynLocation.T) : AnimHandle.T 
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
  END GetT;


(*****************************************************************************)
(* Setup procedures                                                          *)
(*****************************************************************************)


PROCEDURE SetupPackage () =

  PROCEDURE NewOpCode (name: TEXT; arity: INTEGER; code: Code) : OpCode =
    BEGIN
      RETURN NEW (OpCode, name := name, arity := arity, code := code);
    END NewOpCode;

  TYPE 
    OpCodes = ARRAY OF ObLib.OpCode;
  VAR 
    opCodes := NEW (REF OpCodes, NUMBER (Code));
  BEGIN
    opCodes^ := 
        OpCodes {
            NewOpCode ("New",      0, Code.New),
            NewOpCode ("Animate",  1, Code.Animate)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR 
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** retrieve the prototype ***)
    loader.load ("AnimHandle.obl");
    TProto := loader.get ("AnimHandle_TProto");

    (*** Register the proxy makers ***)
    AnimHandleProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {New, Animate};

  OpCode = ObLib.OpCode BRANDED OBJECT
    code: Code;
  END;

  Package = ObLib.T BRANDED OBJECT
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
    | Code.New =>
      WITH val = AnimHandle.New () DO
        RETURN val.proxy.obj;
      END;
    | Code.Animate =>
      WITH ah = GetT (args, 1, self, opCode, loc) DO
        ah.animate ();
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
END ObAnimHandle.
