(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:15:46 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 19:09:02 PST 1994 by najork                   *)


MODULE ObAmbientLightGO;


IMPORT AmbientLightGO, AmbientLightGOProxy, LightGO, ObAux, ObColorProp, 
       ObCommand, ObLib, ObLightGO, ObProtoLoader, ObValue, Obliq, ProxiedObj, 
       SynLocation;


CONST 
  pkgname = "AmbientLightGO";


(*****************************************************************************)
(* Wrapper for AmbientLightGO.T                                              *)
(*****************************************************************************)


TYPE
  T = ObLightGO.T BRANDED "ObAmbientLightGO.T" OBJECT END;


PROCEDURE AddTObj (light : AmbientLightGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<an AmbientLightGO.T>", po := light) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      light.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddTObj;


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
            NewOpCode ("New", 1, Code.New)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("AmbientLightGO.obl");
    TProto := loader.get ("AmbientLightGO_TProto");

    (*** Register the proxy maker ***)
    AmbientLightGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {New};

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
    | Code.New =>
      WITH color = ObColorProp.GetOverloadedVal (args, 1, self, opCode, loc),
           light = NEW (AmbientLightGO.T).init () DO
        light.setProp (LightGO.Colour.bind (color));
        RETURN light.proxy.obj;
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
END ObAmbientLightGO.
