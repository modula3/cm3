(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:27:11 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObPolygonGO;

IMPORT ObAux, ObCommand, ObLib, ObPoint3, ObShape, ObSurfaceGO, ObPointProp, 
       ObProtoLoader, ObValue, Obliq, Point3, PointProp, PolygonGO, 
       PolygonGOProxy, ProxiedObj, SynLocation;


CONST
  pkgname = "PolygonGO";


(*****************************************************************************)
(* Wrapper for PolygonGO.T                                                   *)
(*****************************************************************************)


TYPE
  T = ObSurfaceGO.T BRANDED "ObPolygonGO.T" OBJECT END;


PROCEDURE AddTObj (pgon : PolygonGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a PolygonGO.T>", po := pgon) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      pgon.proxy := NEW (ProxiedObj.Proxy, obj := obj);
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
            NewOpCode ("New",              1, Code.New),
            NewOpCode ("NewWithShapeHint", 2, Code.NewWithShapeHint)
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
    loader.load ("PolygonGO.obl");
    TProto := loader.get ("PolygonGO_TProto");

    (*** Register the proxy maker ***)
    PolygonGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {New, NewWithShapeHint};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;
    
  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;


PROCEDURE IsPointArray (val : Obliq.Val) : BOOLEAN RAISES {ObValue.Error} =
  BEGIN
    FOR i := 0 TO Obliq.ArraySize (val) - 1 DO 
      IF NOT ISTYPE (Obliq.ArrayGet (val, i), ObPoint3.T) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END IsPointArray;


PROCEDURE GetPointArray (val : Obliq.Val) : REF ARRAY OF Point3.T 
    RAISES {ObValue.Error} =
  BEGIN
    WITH size = Obliq.ArraySize (val),
         pts  = NEW (REF ARRAY OF Point3.T, size) DO
      FOR i := 0 TO size - 1 DO
        pts[i] := ObPoint3.ObliqToM3 (Obliq.ArrayGet (val, i));
      END;
      RETURN pts;
    END;
  END GetPointArray;


PROCEDURE GetPointPropValArray (val : Obliq.Val) : REF ARRAY OF PointProp.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH size = Obliq.ArraySize (val),
         pts  = NEW (REF ARRAY OF PointProp.Val, size) DO
      FOR i := 0 TO size - 1 DO
        pts[i] := ObPointProp.ObliqToM3 (Obliq.ArrayGet (val, i));
      END;
      RETURN pts;
    END;
  END GetPointPropValArray;


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
      IF IsPointArray (args[1]) THEN
        WITH pts = GetPointArray (args[1]),
             go  = PolygonGO.NewStatic (pts^) DO
          RETURN go.proxy.obj;
        END;
      ELSE
        WITH pts = GetPointPropValArray (args[1]),
             go  = PolygonGO.New (pts^) DO
          RETURN go.proxy.obj;
        END;
      END;
    | Code.NewWithShapeHint => 
      IF IsPointArray (args[1]) THEN
        WITH pts = GetPointArray (args[1]),
             shp = ObShape.GetArg (args, 2, self, opCode, loc),
             go  = PolygonGO.NewStatic (pts^, shp) DO
          RETURN go.proxy.obj;
        END;
      ELSE
        WITH pts = GetPointPropValArray (args[1]),
             shp = ObShape.GetArg (args, 2, self, opCode, loc),
             go  = PolygonGO.New (pts^, shp) DO
          RETURN go.proxy.obj;
        END;
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
END ObPolygonGO.
