(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:19:45 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObLineGO;

IMPORT LineGO, LineGOProxy, ObAux, ObColorProp, ObCommand, ObGO, ObLib, 
       ObLineTypeProp, ObPointProp, ObProp, ObProtoLoader, ObRealProp, ObValue,
       Obliq, ProxiedObj, SynLocation;


CONST
  pkgname = "LineGO";


(*****************************************************************************)
(* Wrapper for LineGO.T                                                      *)
(*****************************************************************************)


TYPE
  T = ObGO.T BRANDED "ObLineGO.T" OBJECT END;


PROCEDURE AddTObj (line : LineGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a LineGO.T>", po := line) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      line.proxy := NEW (ProxiedObj.Proxy, obj := obj);
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
            NewOpCode ("New",       2, Code.New),
            NewOpCode ("Color",    -1, Code.Color),
            NewOpCode ("Width",    -1, Code.Width),
            NewOpCode ("Type",     -1, Code.Type),
            NewOpCode ("Point1",   -1, Code.Point1),
            NewOpCode ("Point2",   -1, Code.Point2),
            NewOpCode ("SetColor",  2, Code.SetColor),
            NewOpCode ("SetWidth",  2, Code.SetWidth),
            NewOpCode ("SetType",   2, Code.SetType),
            NewOpCode ("SetPoint1", 2, Code.SetPoint1),
            NewOpCode ("SetPoint2", 2, Code.SetPoint2)
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
    loader.load ("LineGO.obl");
    TProto := loader.get ("LineGO_TProto");

    (*** Register the proxy maker ***)
    LineGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {New, Color, Width, Type, Point1, Point2, 
          SetColor, SetWidth, SetType, SetPoint1, SetPoint2};

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
      WITH p1 = ObPointProp.GetOverloadedVal (args, 1, self, opCode, loc),
           p2 = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc),
           line = NEW (LineGO.T).init () DO
        line.setProp (LineGO.Point1.bind (p1));
        line.setProp (LineGO.Point2.bind (p2));
        RETURN line.proxy.obj;
      END;
    | Code.Color =>
      RETURN ObProp.NameToObliq (LineGO.Colour);
    | Code.Width =>
      RETURN ObProp.NameToObliq (LineGO.Width);
    | Code.Type =>
      RETURN ObProp.NameToObliq (LineGO.Type);
    | Code.Point1 =>
      RETURN ObProp.NameToObliq (LineGO.Point1);
    | Code.Point2 =>
      RETURN ObProp.NameToObliq (LineGO.Point2);
    | Code.SetColor =>
      WITH go  = ObGO.GetArg (args, 1, self, opCode, loc),
           col = ObColorProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (LineGO.Colour.bind (col));
        RETURN ObValue.valOk;
      END;
    | Code.SetWidth =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           r  = ObRealProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (LineGO.Width.bind (r));
        RETURN ObValue.valOk;
      END;
    | Code.SetType =>
      WITH go= ObGO.GetArg                    (args, 1, self, opCode, loc),
           k = ObLineTypeProp.GetOverloadedVal(args, 2, self, opCode, loc) DO
        go.setProp (LineGO.Type.bind (k));
        RETURN ObValue.valOk;
      END;
    | Code.SetPoint1 =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (LineGO.Point1.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetPoint2 =>
      WITH go = ObGO.GetArg                  (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal (args, 2, self, opCode, loc) DO
        go.setProp (LineGO.Point2.bind (p));
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
END ObLineGO.
