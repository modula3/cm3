(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:15:22 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 20:17:39 PST 1994 by najork                   *)


MODULE ObPerspCameraGO;

IMPORT CameraGO, ObAux, ObCameraGO, ObCommand, ObGO, ObLib, ObProp, 
       ObProtoLoader, ObPointProp, ObRealProp, ObValue, Obliq, PerspCameraGO, 
       PerspCameraGOProxy, ProxiedObj, SynLocation;


CONST
  pkgname = "PerspCameraGO";


(*****************************************************************************)
(* Wrapper for PerspCameraGO.T                                               *)
(*****************************************************************************)


TYPE
  T = ObCameraGO.T BRANDED "ObPerspCameraGO.T" OBJECT END;


PROCEDURE AddTObj (cam : PerspCameraGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a PerspCameraGO.T>", po := cam) DO
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
            NewOpCode ("New",      4, Code.New),
            NewOpCode ("Fovy",    -1, Code.Fovy),
            NewOpCode ("SetFovy",  2, Code.SetFovy)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("PerspCameraGO.obl");
    TProto := loader.get ("PerspCameraGO_TProto");

    (*** Register the proxy maker ***)
    PerspCameraGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {New, Fovy, SetFovy};

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
      WITH from = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           to   = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           up   = ObPointProp.GetOverloadedVal (args, 3, self, opCode, loc),
           fovy = ObRealProp.GetOverloadedVal  (args, 4, self, opCode, loc),
           cam  = NEW (PerspCameraGO.T).init () DO
        cam.setProp (CameraGO.From.bind (from));
        cam.setProp (CameraGO.To.bind (to));
        cam.setProp (CameraGO.Up.bind (up));
        cam.setProp (PerspCameraGO.Fovy.bind (fovy));
        RETURN cam.proxy.obj;
      END;
    | Code.Fovy =>
      RETURN ObProp.NameToObliq (PerspCameraGO.Fovy);
    | Code.SetFovy =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal  (args, 2, self, opCode, loc) DO
        go.setProp (PerspCameraGO.Fovy.bind (pv));
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
END ObPerspCameraGO.
