(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:15:31 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 20:17:39 PST 1994 by najork                   *)


MODULE ObOrthoCameraGO;

IMPORT CameraGO, ObAux, ObCameraGO, ObCommand, ObGO, ObLib, ObProp, 
       ObProtoLoader, ObPointProp, ObRealProp, ObValue, Obliq, OrthoCameraGO, 
       OrthoCameraGOProxy, ProxiedObj, SynLocation;


CONST
  pkgname = "OrthoCameraGO";


(*****************************************************************************)
(* Wrapper for OrthoCameraGO.T                                               *)
(*****************************************************************************)


TYPE
  T = ObCameraGO.T BRANDED "ObOrthoCameraGO.T" OBJECT END;


PROCEDURE AddTObj (cam : OrthoCameraGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a OrthoCameraGO.T>", po := cam) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      cam.proxy := NEW (ProxiedObj.Proxy, obj := obj);
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
            NewOpCode ("New",        4, Code.New),
            NewOpCode ("Height",    -1, Code.Height),
            NewOpCode ("SetHeight",  2, Code.SetHeight)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("OrthoCameraGO.obl");
    TProto := loader.get ("OrthoCameraGO_TProto");

    (*** Register the proxy maker ***)
    OrthoCameraGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {New, Height, SetHeight};

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
      WITH from   = ObPointProp.GetOverloadedVal(args, 1, self, opCode, loc),
           to     = ObPointProp.GetOverloadedVal(args, 2, self, opCode, loc),
           up     = ObPointProp.GetOverloadedVal(args, 3, self, opCode, loc),
           height = ObRealProp.GetOverloadedVal (args, 4, self, opCode, loc),
           cam    = NEW (OrthoCameraGO.T).init () DO
        cam.setProp (CameraGO.From.bind (from));
        cam.setProp (CameraGO.To.bind (to));
        cam.setProp (CameraGO.Up.bind (up));
        cam.setProp (OrthoCameraGO.Height.bind (height));
        RETURN cam.proxy.obj;
      END;
    | Code.Height =>
      RETURN ObProp.NameToObliq (OrthoCameraGO.Height);
    | Code.SetHeight =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal  (args, 2, self, opCode, loc) DO
        go.setProp (OrthoCameraGO.Height.bind (pv));
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
END ObOrthoCameraGO.
