(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:33:18 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObConeGO;

IMPORT ConeGO, ConeGOProxy, ObAux, ObCommand, ObGO, ObInt, ObLib, ObPointProp,
       ObProp, ObProtoLoader, ObRealProp, ObSurfaceGO, ObValue, Obliq, 
       ProxiedObj, SynLocation;


CONST
  pkgname = "ConeGO";


(*****************************************************************************)
(* Wrapper for ConeGO.T                                                      *)
(*****************************************************************************)


TYPE
  T = ObSurfaceGO.T BRANDED "ObConeGO.T" OBJECT END;


PROCEDURE AddTObj (cone : ConeGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a ConeGO.T>", po := cone) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      cone.proxy := NEW (ProxiedObj.Proxy, obj := obj);
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
            NewOpCode ("Base",       -1, Code.Base),
            NewOpCode ("Tip",        -1, Code.Tip),
            NewOpCode ("Radius",     -1, Code.Radius),
            NewOpCode ("SetBase",     2, Code.SetBase),
            NewOpCode ("SetTip",      2, Code.SetTip),
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
    loader.load ("ConeGO.obl");
    TProto := loader.get ("ConeGO_TProto");

    (*** Register the proxy maker ***)
    ConeGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {New, NewWithPrec, Base, SetBase, Tip, SetTip, Radius, 
          SetRadius};

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
      WITH base = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           tip  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           rad  = ObRealProp.GetOverloadedVal  (args, 3, self, opCode, loc),
           cone = NEW (ConeGO.T).init () DO
        cone.setProp (ConeGO.Base.bind (base));
        cone.setProp (ConeGO.Tip.bind (tip));
        cone.setProp (ConeGO.Radius.bind (rad));
        RETURN cone.proxy.obj;
      END;
    | Code.NewWithPrec => 
      WITH base = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           tip  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           rad  = ObRealProp.GetOverloadedVal  (args, 3, self, opCode, loc),
           prec = ObInt.GetArg                 (args, 4, self, opCode, loc), 
           cone = NEW (ConeGO.T).init (prec) DO
        cone.setProp (ConeGO.Base.bind (base));
        cone.setProp (ConeGO.Tip.bind (tip));
        cone.setProp (ConeGO.Radius.bind (rad));
        RETURN cone.proxy.obj;
      END;
    | Code.Base =>
      RETURN ObProp.NameToObliq (ConeGO.Base);
    | Code.Tip =>
      RETURN ObProp.NameToObliq (ConeGO.Tip);
    | Code.Radius =>
      RETURN ObProp.NameToObliq (ConeGO.Radius);
    | Code.SetBase =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p1 = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (ConeGO.Base.bind (p1));
        RETURN ObValue.valOk;
      END;
    | Code.SetTip =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p2 = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (ConeGO.Tip.bind (p2));
        RETURN ObValue.valOk;
      END;
    | Code.SetRadius =>
      WITH go  = ObGO.GetArg                 (args, 1, self, opCode, loc),
           rad = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (ConeGO.Radius.bind (rad));
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
END ObConeGO.
