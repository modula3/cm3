(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug 22 12:03:52 PDT 1995 by najork                   *)
(*       Created on Sat Mar  5 20:42:30 PST 1994 by najork                   *)


MODULE ObRootGO;


IMPORT GraphicsBase, ObAux, ObBooleanProp, ObCameraGO, ObColorProp, ObCommand,
       ObGO, ObGraphicsBase, ObGroupGO, ObLib, ObPoint, ObPoint3, ObProp, 
       ObProtoLoader, ObReal, ObRealProp, ObValue, Obliq, ProxiedObj, RootGO, 
       RootGOProxy, SynLocation;


CONST 
  pkgname = "RootGO";


(*****************************************************************************)
(* Wrapper for RootGO.T                                                      *)
(*****************************************************************************)


TYPE
  T = ObGroupGO.T BRANDED "ObRootGO.T" OBJECT END;


PROCEDURE AddTObj (root : RootGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a RootGO.T>", po := root) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      root.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddTObj;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : RootGO.T 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | T (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetArg;


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
            NewOpCode ("New",                   2, Code.New),
            NewOpCode ("NewStd",                0, Code.NewStd),
            NewOpCode ("NewStdWithBase",        1, Code.NewStdWithBase),
            NewOpCode ("ChangeCamera",          2, Code.ChangeCamera),
            NewOpCode ("Background",           -1, Code.Background),
            NewOpCode ("DepthcueSwitch",       -1, Code.DepthcueSwitch),
            NewOpCode ("DepthcueColor",        -1, Code.DepthcueColor),
            NewOpCode ("DepthcueFrontPlane",   -1, Code.DepthcueFrontPlane),
            NewOpCode ("DepthcueBackPlane",    -1, Code.DepthcueBackPlane),
            NewOpCode ("DepthcueFrontScale",   -1, Code.DepthcueFrontScale),
            NewOpCode ("DepthcueBackScale",    -1, Code.DepthcueBackScale),
            NewOpCode ("SetBackground",         2, Code.SetBackground),
            NewOpCode ("SetDepthcueSwitch",     2, Code.SetDepthcueSwitch),
            NewOpCode ("SetDepthcueColor",      2, Code.SetDepthcueColor),
            NewOpCode ("SetDepthcueFrontPlane", 2, Code.SetDepthcueFrontPlane),
            NewOpCode ("SetDepthcueBackPlane",  2, Code.SetDepthcueBackPlane),
            NewOpCode ("SetDepthcueFrontScale", 2, Code.SetDepthcueFrontScale),
            NewOpCode ("SetDepthcueBackScale",  2, Code.SetDepthcueBackScale),
            NewOpCode ("ScreenToWorld",         3, Code.ScreenToWorld)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("RootGO.obl");
    TProto := loader.get ("RootGO_TProto");

    (*** Register the proxy maker ***)
    RootGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {ChangeCamera, New, NewStd, NewStdWithBase,
          Background, DepthcueSwitch, DepthcueColor, DepthcueFrontPlane, 
          DepthcueBackPlane, DepthcueFrontScale, DepthcueBackScale, 
          SetBackground, SetDepthcueSwitch, SetDepthcueColor, 
          SetDepthcueFrontPlane, SetDepthcueBackPlane, SetDepthcueFrontScale, 
          SetDepthcueBackScale, ScreenToWorld};

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
      WITH cam  = ObCameraGO.GetArg     (args, 1, self, opCode, loc),
           base = ObGraphicsBase.GetArg (args, 2, self, opCode, loc),
           root = RootGO.New (cam, base) DO
        RETURN root.proxy.obj;
      END;
    | Code.NewStd =>
      TRY
        WITH root = RootGO.NewStd () DO
          RETURN root.proxy.obj;
        END;
      EXCEPT
        GraphicsBase.Failure =>
        ObValue.RaiseException (ObGraphicsBase.Failure, opCode.name, loc);
        <* ASSERT FALSE *>
      END;
    | Code.NewStdWithBase =>
      TRY
        WITH base = ObGraphicsBase.GetArg (args, 1, self, opCode, loc),
             root = RootGO.NewStd (base) DO
          RETURN root.proxy.obj;
        END;
      EXCEPT
        GraphicsBase.Failure =>
        ObValue.RaiseException (ObGraphicsBase.Failure, opCode.name, loc);
        <* ASSERT FALSE *>
      END;
    | Code.ChangeCamera =>
      WITH root = GetArg            (args, 1, self, opCode, loc),
           cam  = ObCameraGO.GetArg (args, 2, self, opCode, loc) DO
        root.changeCamera (cam);
        RETURN ObValue.valOk;
      END;
    | Code.Background =>
      RETURN ObProp.NameToObliq (RootGO.Background);
    | Code.SetBackground =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           pv = ObColorProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (RootGO.Background.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.DepthcueSwitch =>
      RETURN ObProp.NameToObliq (RootGO.DepthcueSwitch);
    | Code.SetDepthcueSwitch =>
      WITH go = ObGO.GetArg                   (args, 1, self, opCode, loc),
           pv = ObBooleanProp.GetOverloadedVal(args, 2, self, opCode, loc) DO
        go.setProp (RootGO.DepthcueSwitch.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.DepthcueColor =>
      RETURN ObProp.NameToObliq (RootGO.DepthcueColour);
    | Code.SetDepthcueColor =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           pv = ObColorProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (RootGO.DepthcueColour.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.DepthcueFrontPlane =>
      RETURN ObProp.NameToObliq (RootGO.DepthcueFrontPlane);
    | Code.SetDepthcueFrontPlane =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (RootGO.DepthcueFrontPlane.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.DepthcueBackPlane =>
      RETURN ObProp.NameToObliq (RootGO.DepthcueBackPlane);
    | Code.SetDepthcueBackPlane =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (RootGO.DepthcueBackPlane.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.DepthcueFrontScale =>
      RETURN ObProp.NameToObliq (RootGO.DepthcueFrontScale);
    | Code.SetDepthcueFrontScale =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (RootGO.DepthcueFrontScale.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.DepthcueBackScale =>
      RETURN ObProp.NameToObliq (RootGO.DepthcueBackScale);
    | Code.SetDepthcueBackScale =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           pv = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (RootGO.DepthcueBackScale.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.ScreenToWorld =>
      WITH root = GetArg         (args, 1, self, opCode, loc),
           pos  = ObPoint.GetArg (args, 2, self, opCode, loc),
           z    = ObReal.GetArg  (args, 3, self, opCode, loc) DO
        RETURN ObPoint3.M3ToObliq (root.screenToWorld (pos, z));
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
END ObRootGO.
