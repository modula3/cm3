(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:48:30 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObTorusGO;

IMPORT ObAux, ObCommand, ObGO, ObInt, ObLib, ObPointProp, ObProp, 
       ObProtoLoader, ObRealProp, ObSurfaceGO, ObValue, Obliq, ProxiedObj, 
       SynLocation, TorusGO, TorusGOProxy;


CONST
  pkgname = "TorusGO";

(*****************************************************************************)
(* Wrapper for TorusGO.T                                                     *)
(*****************************************************************************)


TYPE
  T = ObSurfaceGO.T BRANDED "ObTorusGO.T" OBJECT END;


PROCEDURE AddTObj (torus : TorusGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a TorusGO.T>", po := torus) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      torus.proxy := NEW (ProxiedObj.Proxy, obj := obj);
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
            NewOpCode ("New",         4, Code.New),
            NewOpCode ("NewWithPrec", 5, Code.NewWithPrec),
            NewOpCode ("Center",     -1, Code.Center),
            NewOpCode ("Normal",     -1, Code.Normal),
            NewOpCode ("Radius1",    -1, Code.Radius1),
            NewOpCode ("Radius2",    -1, Code.Radius2),
            NewOpCode ("SetCenter",   2, Code.SetCenter),
            NewOpCode ("SetNormal",   2, Code.SetNormal),
            NewOpCode ("SetRadius1",  2, Code.SetRadius1),
            NewOpCode ("SetRadius2",  2, Code.SetRadius2)
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
    loader.load ("TorusGO.obl");
    TProto := loader.get ("TorusGO_TProto");

    (*** Register the proxy maker ***)
    TorusGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {New, NewWithPrec, Center, Normal, Radius1, Radius2,
          SetCenter, SetNormal, SetRadius1, SetRadius2};

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
      WITH cent  = ObPointProp.GetOverloadedVal(args, 1, self, opCode, loc),
           norm  = ObPointProp.GetOverloadedVal(args, 2, self, opCode, loc),
           rad1  = ObRealProp.GetOverloadedVal (args, 3, self, opCode, loc),
           rad2  = ObRealProp.GetOverloadedVal (args, 4, self, opCode, loc),
           torus = NEW (TorusGO.T).init () DO
        torus.setProp (TorusGO.Center.bind (cent));
        torus.setProp (TorusGO.Normal.bind (norm));
        torus.setProp (TorusGO.Radius1.bind (rad1));
        torus.setProp (TorusGO.Radius2.bind (rad2));
        RETURN torus.proxy.obj;
      END;
    | Code.NewWithPrec => 
      WITH cent  = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           norm  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           rad1  = ObRealProp.GetOverloadedVal  (args, 3, self, opCode, loc),
           rad2  = ObRealProp.GetOverloadedVal  (args, 4, self, opCode, loc),
           prec  = ObInt.GetArg                 (args, 5, self, opCode, loc),
           torus = NEW (TorusGO.T).init (prec) DO
        torus.setProp (TorusGO.Center.bind (cent));
        torus.setProp (TorusGO.Normal.bind (norm));
        torus.setProp (TorusGO.Radius1.bind (rad1));
        torus.setProp (TorusGO.Radius2.bind (rad2));
        RETURN torus.proxy.obj;
      END;
    | Code.Center =>
      RETURN ObProp.NameToObliq (TorusGO.Center);
    | Code.Normal =>
      RETURN ObProp.NameToObliq (TorusGO.Normal);
    | Code.Radius1 =>
      RETURN ObProp.NameToObliq (TorusGO.Radius1);
    | Code.Radius2 =>
      RETURN ObProp.NameToObliq (TorusGO.Radius2);
    | Code.SetCenter =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (TorusGO.Center.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetNormal =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (TorusGO.Normal.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetRadius1 =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           r  = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (TorusGO.Radius1.bind (r));
        RETURN ObValue.valOk;
      END;
    | Code.SetRadius2 =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           r  = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (TorusGO.Radius2.bind (r));
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
END ObTorusGO.
