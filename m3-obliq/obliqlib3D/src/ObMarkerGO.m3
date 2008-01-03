(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:18:15 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObMarkerGO;

IMPORT MarkerGO, MarkerGOProxy, ObAux, ObColorProp, ObCommand, ObGO, ObLib, 
       ObMarkerTypeProp, ObPointProp, ObProp, ObProtoLoader, ObRealProp, 
       ObValue, Obliq, ProxiedObj, SynLocation;

CONST
  pkgname = "MarkerGO";


(*****************************************************************************)
(* Wrapper for OrthoCameraGO.T                                               *)
(*****************************************************************************)


TYPE
  T = ObGO.T BRANDED "ObMarkerGO.T" OBJECT END;


PROCEDURE AddTObj (marker : MarkerGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a MarkerGO.T>", po := marker) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      marker.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddTObj;


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
    opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW (REF OpCodes, NUMBER (Code));
    opCodes^ := 
        OpCodes {
            NewOpCode ("New",       1, Code.New),
            NewOpCode ("Center",   -1, Code.Center),
            NewOpCode ("Color",    -1, Code.Color),
            NewOpCode ("Scale",    -1, Code.Scale),
            NewOpCode ("Type",     -1, Code.Type),
            NewOpCode ("SetCenter", 2, Code.SetCenter),
            NewOpCode ("SetColor",  2, Code.SetColor),
            NewOpCode ("SetScale",  2, Code.SetScale),
            NewOpCode ("SetType",   2, Code.SetType)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("MarkerGO.obl");
    TProto := loader.get ("MarkerGO_TProto");

    (*** Register the proxy maker ***)
    MarkerGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {New, Center, Color, Scale, Type, SetCenter, SetColor, SetScale, 
          SetType};

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
      WITH p = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           marker = NEW (MarkerGO.T).init () DO
        marker.setProp (MarkerGO.Center.bind (p));
        RETURN marker.proxy.obj;
      END;
    | Code.Center =>
      RETURN ObProp.NameToObliq (MarkerGO.Center);
    | Code.Color =>
      RETURN ObProp.NameToObliq (MarkerGO.Colour);
    | Code.Scale =>
      RETURN ObProp.NameToObliq (MarkerGO.Scale);
    | Code.Type =>
      RETURN ObProp.NameToObliq (MarkerGO.Type);
    | Code.SetCenter =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (MarkerGO.Center.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetColor =>
      WITH go  = ObGO.GetArg                  (args, 1, self, opCode, loc),
           col = ObColorProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (MarkerGO.Colour.bind (col));
        RETURN ObValue.valOk;
      END;
    | Code.SetScale =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           r  = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (MarkerGO.Scale.bind (r));
        RETURN ObValue.valOk;
      END;
    | Code.SetType =>
      WITH go = ObGO.GetArg (args, 1, self, opCode, loc),
           pv = ObMarkerTypeProp.GetOverloadedVal
           (args, 2, self, opCode, loc) DO
        go.setProp (MarkerGO.Type.bind (pv));
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
END ObMarkerGO.
