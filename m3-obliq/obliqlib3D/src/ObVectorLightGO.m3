(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:15:56 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 19:17:45 PST 1994 by najork                   *)


MODULE ObVectorLightGO;


IMPORT LightGO, ObAux, ObColorProp, ObCommand, ObGO, ObLib, ObLightGO,
       ObPointProp, ObProp, ObProtoLoader, ObValue, Obliq, ProxiedObj, 
       SynLocation, VectorLightGO, VectorLightGOProxy;


CONST 
  pkgname = "VectorLightGO";


(*****************************************************************************)
(* Wrapper for VectorLightGO.T                                               *)
(*****************************************************************************)


TYPE
  T = ObLightGO.T BRANDED "ObVectorLightGO.T" OBJECT END;


PROCEDURE AddTObj (light : VectorLightGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a VectorLightGO.T>", po := light) DO
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
            NewOpCode ("New",          2, Code.New),
            NewOpCode ("Direction",   -1, Code.Direction),
            NewOpCode ("SetDirection", 2, Code.SetDirection)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("VectorLightGO.obl");
    TProto := loader.get ("VectorLightGO_TProto");

    (*** Register the proxy maker ***)
    VectorLightGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {New, Direction, SetDirection};

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
           dir   = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           light = NEW (VectorLightGO.T).init () DO
        light.setProp (LightGO.Colour.bind (color));
        light.setProp (VectorLightGO.Direction.bind (dir));
        RETURN light.proxy.obj;
      END;
    | Code.Direction =>
      RETURN ObProp.NameToObliq (VectorLightGO.Direction);
    | Code.SetDirection =>
      WITH go  = ObGO.GetArg                  (args, 1, self, opCode, loc),
           dir = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (VectorLightGO.Direction.bind (dir));
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
END ObVectorLightGO.
