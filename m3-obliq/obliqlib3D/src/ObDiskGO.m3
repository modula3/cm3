(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:30:37 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObDiskGO;

IMPORT DiskGO, DiskGOProxy, ObAux, ObCommand, ObGO, ObInt, ObLib, ObPointProp,
       ObProp, ObProtoLoader, ObRealProp, ObSurfaceGO, ObValue, Obliq, 
       ProxiedObj, SynLocation;


CONST pkgname = "DiskGO";


(*****************************************************************************)
(* Wrapper for DiskGO.T                                                      *)
(*****************************************************************************)


TYPE
  T = ObSurfaceGO.T BRANDED "ObDiskGO.T" OBJECT END;


PROCEDURE AddTObj (disk : DiskGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a DiskGO.T>", po := disk) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      disk.proxy := NEW (ProxiedObj.Proxy, obj := obj);
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
            NewOpCode ("Center",     -1, Code.Center),
            NewOpCode ("Normal",     -1, Code.Normal),
            NewOpCode ("Radius",     -1, Code.Radius),
            NewOpCode ("SetCenter",   2, Code.SetCenter),
            NewOpCode ("SetNormal",   2, Code.SetNormal),
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
    loader.load ("DiskGO.obl");
    TProto := loader.get ("DiskGO_TProto");

    (*** Register the proxy maker ***)
    DiskGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {New, NewWithPrec, 
          Center, SetCenter, Normal, SetNormal, Radius, SetRadius};

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
      WITH c    = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           n    = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           r    = ObRealProp.GetOverloadedVal  (args, 3, self, opCode, loc),
           disk = NEW (DiskGO.T).init () DO
        disk.setProp (DiskGO.Center.bind (c));
        disk.setProp (DiskGO.Normal.bind (n));
        disk.setProp (DiskGO.Radius.bind (r));
        RETURN disk.proxy.obj;
      END;
    | Code.NewWithPrec => 
      WITH c    = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           n    = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           r    = ObRealProp.GetOverloadedVal  (args, 3, self, opCode, loc),
           prec = ObInt.GetArg                 (args, 4, self, opCode, loc),
           disk = NEW (DiskGO.T).init (prec) DO
        disk.setProp (DiskGO.Center.bind (c));
        disk.setProp (DiskGO.Normal.bind (n));
        disk.setProp (DiskGO.Radius.bind (r));
        RETURN disk.proxy.obj;
      END;
    | Code.Center =>
      RETURN ObProp.NameToObliq (DiskGO.Center);
    | Code.Normal =>
      RETURN ObProp.NameToObliq (DiskGO.Normal);
    | Code.Radius =>
      RETURN ObProp.NameToObliq (DiskGO.Radius);
    | Code.SetCenter =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (DiskGO.Center.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetNormal =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (DiskGO.Normal.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetRadius =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           r  = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (DiskGO.Radius.bind (r));
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
END ObDiskGO.
