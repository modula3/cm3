(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:34:27 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObCylinderGO;

IMPORT CylinderGO, CylinderGOProxy, ObAux, ObCommand, ObGO, ObInt, ObLib, 
       ObPointProp, ObProp, ObProtoLoader, ObRealProp, ObSurfaceGO, ObValue, 
       Obliq, ProxiedObj, SynLocation;


CONST
  pkgname = "CylinderGO";


(*****************************************************************************)
(* Wrapper for CylinderGO.T                                                  *)
(*****************************************************************************)


TYPE
  T = ObSurfaceGO.T BRANDED "ObCylinderGO.T" OBJECT END;


PROCEDURE AddTObj (cyl : CylinderGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a CylinderGO.T>", po := cyl) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      cyl.proxy := NEW (ProxiedObj.Proxy, obj := obj);
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
            NewOpCode ("New",         3, Code.New),
            NewOpCode ("NewWithPrec", 4, Code.NewWithPrec),
            NewOpCode ("Point1",     -1, Code.Point1),
            NewOpCode ("Point2",     -1, Code.Point2),
            NewOpCode ("Radius",     -1, Code.Radius),
            NewOpCode ("SetPoint1",   2, Code.SetPoint1),
            NewOpCode ("SetPoint2",   2, Code.SetPoint2),
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
    loader.load ("CylinderGO.obl");
    TProto := loader.get ("CylinderGO_TProto");

    (*** Register the proxy maker ***)
    CylinderGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {New, NewWithPrec, 
          Point1, SetPoint1, Point2, SetPoint2, Radius, SetRadius};

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
      WITH p1   = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           p2   = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           rad  =  ObRealProp.GetOverloadedVal (args, 3, self, opCode, loc),
           cyl  = NEW (CylinderGO.T).init () DO
        cyl.setProp (CylinderGO.Point1.bind (p1));
        cyl.setProp (CylinderGO.Point2.bind (p2));
        cyl.setProp (CylinderGO.Radius.bind (rad));
        RETURN cyl.proxy.obj;
      END;
    | Code.NewWithPrec => 
      WITH p1   = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           p2   = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           rad  = ObRealProp.GetOverloadedVal  (args, 3, self, opCode, loc),
           prec = ObInt.GetArg                 (args, 4, self, opCode, loc),
           cyl  = NEW (CylinderGO.T).init (prec) DO
        cyl.setProp (CylinderGO.Point1.bind (p1));
        cyl.setProp (CylinderGO.Point2.bind (p2));
        cyl.setProp (CylinderGO.Radius.bind (rad));
        RETURN cyl.proxy.obj;
      END;
    | Code.Point1 =>
      RETURN ObProp.NameToObliq (CylinderGO.Point1);
    | Code.Point2 =>
      RETURN ObProp.NameToObliq (CylinderGO.Point2);
    | Code.Radius =>
      RETURN ObProp.NameToObliq (CylinderGO.Radius);
    | Code.SetPoint1 =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p1 = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (CylinderGO.Point1.bind (p1));
        RETURN ObValue.valOk;
      END;
    | Code.SetPoint2 =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p2 = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (CylinderGO.Point2.bind (p2));
        RETURN ObValue.valOk;
      END;
    | Code.SetRadius =>
      WITH go  = ObGO.GetArg                 (args, 1, self, opCode, loc),
           rad = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (CylinderGO.Radius.bind (rad));
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
END ObCylinderGO.
