(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Sep 29 18:38:47 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 19:27:45 PST 1994 by najork                   *)


MODULE ObPointLightGO;


IMPORT LightGO, ObAux, ObColorProp, ObCommand, ObGO, ObLib, ObLightGO,
       ObPointProp, ObProp, ObProtoLoader, ObRealProp, ObValue, Obliq, 
       PointLightGO, PointLightGOProxy, ProxiedObj, SynLocation;


CONST 
  pkgname = "PointLightGO";


(*****************************************************************************)
(* Wrapper for PointLightGO.T                                                *)
(*****************************************************************************)


TYPE
  T = ObLightGO.T BRANDED "ObPointLightGO.T" OBJECT END;


PROCEDURE AddTObj (light : PointLightGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a PointLightGO.T>", po := light) DO
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
            NewOpCode ("New",             4, Code.New),
            NewOpCode ("Origin",         -1, Code.Origin),
            NewOpCode ("SetOrigin",       2, Code.SetOrigin),
            NewOpCode ("Attenuation0",   -1, Code.Attenuation0),
            NewOpCode ("SetAttenuation0", 2, Code.SetAttenuation0),
            NewOpCode ("Attenuation1",   -1, Code.Attenuation1),
            NewOpCode ("SetAttenuation1", 2, Code.SetAttenuation1)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("PointLightGO.obl");
    TProto := loader.get ("PointLightGO_TProto");

    (*** Register the proxy maker ***)
    PointLightGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {New, Origin, SetOrigin, Attenuation0, SetAttenuation0, 
          Attenuation1, SetAttenuation1};

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
      WITH color  = ObColorProp.GetOverloadedVal(args, 1, self, opCode, loc),
           origin = ObPointProp.GetOverloadedVal(args, 2, self, opCode, loc),
           att0   = ObRealProp.GetOverloadedVal (args, 3, self, opCode, loc),
           att1   = ObRealProp.GetOverloadedVal (args, 4, self, opCode, loc),
           light  = NEW (PointLightGO.T).init () DO
        light.setProp (LightGO.Colour.bind (color));
        light.setProp (PointLightGO.Origin.bind (origin));
        light.setProp (PointLightGO.Attenuation0.bind (att0));
        light.setProp (PointLightGO.Attenuation1.bind (att1));
        RETURN light.proxy.obj;
      END;
    | Code.Origin =>
      RETURN ObProp.NameToObliq (PointLightGO.Origin);
    | Code.SetOrigin =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (PointLightGO.Origin.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.Attenuation0 =>
      RETURN ObProp.NameToObliq (PointLightGO.Attenuation0);
    | Code.SetAttenuation0 =>
      WITH go  = ObGO.GetArg                 (args, 1, self, opCode, loc),
           att = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (PointLightGO.Attenuation0.bind (att));
        RETURN ObValue.valOk;
      END;
    | Code.Attenuation1 =>
      RETURN ObProp.NameToObliq (PointLightGO.Attenuation1);
    | Code.SetAttenuation1 =>
      WITH go  = ObGO.GetArg                 (args, 1, self, opCode, loc),
           att = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (PointLightGO.Attenuation1.bind (att));
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
END ObPointLightGO.
