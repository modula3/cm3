(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:31:55 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObSphereGO;

IMPORT ObAux, ObCommand, ObGO, ObInt, ObLib, ObPointProp, ObProp, 
       ObProtoLoader, ObRealProp, ObSurfaceGO, ObValue, Obliq, ProxiedObj, 
       SphereGO, SphereGOProxy, SynLocation;


CONST
  pkgname = "SphereGO";


(*****************************************************************************)
(* Wrapper for SphereGO.T                                                    *)
(*****************************************************************************)


TYPE
  T = ObSurfaceGO.T BRANDED "ObSphereGO.T" OBJECT END;


PROCEDURE AddTObj (sphere : SphereGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a SphereGO.T>", po := sphere) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      sphere.proxy := NEW (ProxiedObj.Proxy, obj := obj);
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
            NewOpCode ("New",         2, Code.New),
            NewOpCode ("NewWithPrec", 3, Code.NewWithPrec),
            NewOpCode ("Center",     -1, Code.Center),
            NewOpCode ("Radius",     -1, Code.Radius),
            NewOpCode ("SetCenter",   2, Code.SetCenter),
            NewOpCode ("SetRadius",   2, Code.SetRadius)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);

    (* DONT KNOW YET WHETHER TO INHIBIT TRANSMISSIONS ... *)

  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("SphereGO.obl");
    TProto := loader.get ("SphereGO_TProto");

    (*** Register the proxy maker ***)
    SphereGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {New, NewWithPrec, Center, Radius, SetCenter, SetRadius};

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
           r = ObRealProp.GetOverloadedVal  (args, 2, self, opCode, loc),
           sphere = NEW (SphereGO.T).init () DO
        sphere.setProp (SphereGO.Center.bind (p));
        sphere.setProp (SphereGO.Radius.bind (r));
        RETURN sphere.proxy.obj;
      END;
    | Code.NewWithPrec => 
      WITH p    = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           r    = ObRealProp.GetOverloadedVal  (args, 2, self, opCode, loc),
           prec = ObInt.GetArg                 (args, 3, self, opCode, loc),
           sphere = NEW (SphereGO.T).init (prec) DO
        sphere.setProp (SphereGO.Center.bind (p));
        sphere.setProp (SphereGO.Radius.bind (r));
        RETURN sphere.proxy.obj;
      END;
    | Code.Center =>
      RETURN ObProp.NameToObliq (SphereGO.Center);
    | Code.Radius =>
      RETURN ObProp.NameToObliq (SphereGO.Radius);
    | Code.SetCenter =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SphereGO.Center.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetRadius =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           r  = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (SphereGO.Radius.bind (r));
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
END ObSphereGO.
