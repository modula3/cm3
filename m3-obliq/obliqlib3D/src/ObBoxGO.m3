(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:29:10 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObBoxGO;

IMPORT BoxGO, BoxGOProxy, ObAux, ObCommand, ObGO, ObLib, ObPointProp, ObProp, 
       ObProtoLoader, ObSurfaceGO, ObValue, Obliq, ProxiedObj, SynLocation;

CONST
  pkgname = "BoxGO";


(*****************************************************************************)
(* Wrapper for BoxGO.T                                                       *)
(*****************************************************************************)


TYPE
  T = ObSurfaceGO.T BRANDED "ObBoxGO.T" OBJECT END;


PROCEDURE AddTObj (box : BoxGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a BoxGO.T>", po := box) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      box.proxy := NEW (ProxiedObj.Proxy, obj := obj);
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
            NewOpCode ("New",        2, Code.New),
            NewOpCode ("Corner1",   -1, Code.Corner1),
            NewOpCode ("Corner2",   -1, Code.Corner2),
            NewOpCode ("SetCorner1", 2, Code.SetCorner1),
            NewOpCode ("SetCorner2", 2, Code.SetCorner2)
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
    loader.load ("BoxGO.obl");
    TProto := loader.get ("BoxGO_TProto");

    (*** Register the proxy maker ***)
    BoxGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {New, Corner1, Corner2, SetCorner1, SetCorner2};

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
      WITH a   = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           b   = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc), 
           box = NEW (BoxGO.T).init () DO
        box.setProp (BoxGO.Corner1.bind (a));
        box.setProp (BoxGO.Corner2.bind (b));
        RETURN box.proxy.obj;
      END;
    | Code.Corner1 =>
      RETURN ObProp.NameToObliq (BoxGO.Corner1);
    | Code.Corner2 =>
      RETURN ObProp.NameToObliq (BoxGO.Corner2);
    | Code.SetCorner1 =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (BoxGO.Corner1.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetCorner2 =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (BoxGO.Corner2.bind (p));
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
END ObBoxGO.
