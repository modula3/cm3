(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Sep 30 13:27:36 PDT 1994 by najork                   *)
(*       Created on Sat Mar  5 20:17:15 PST 1994 by najork                   *)


MODULE ObCameraGO;

IMPORT CameraGO, ObAux, ObCommand, ObGO, ObLib, ObPointProp, ObProp, 
       ObProtoLoader, ObRealProp, ObValue, Obliq, SynLocation;


CONST
  pkgname = "CameraGO";

(*****************************************************************************)
(* Wrapper for CameraGO.T                                                    *)
(*****************************************************************************)


REVEAL
  T = ObGO.T BRANDED "ObCameraGO.T" OBJECT END;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : CameraGO.T 
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
            NewOpCode ("From",     -1, Code.From),
            NewOpCode ("To",       -1, Code.To),
            NewOpCode ("Up",       -1, Code.Up),
            NewOpCode ("Aspect",   -1, Code.Aspect),
            NewOpCode ("SetFrom",   2, Code.SetFrom),
            NewOpCode ("SetTo",     2, Code.SetTo),
            NewOpCode ("SetUp",     2, Code.SetUp),
            NewOpCode ("SetAspect", 2, Code.SetAspect)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    loader.load ("CameraGO.obl");
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {From, To, Up, Aspect, SetFrom, SetTo, SetUp, SetAspect};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;

  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;


PROCEDURE DoEval (             self         : Package; 
                               opCode       : ObLib.OpCode; 
                  <* UNUSED *> arity        : ObLib.OpArity; 
                               READONLY args: ObValue.ArgArray; 
                  <* UNUSED *> tmp          : BOOLEAN;
                               loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.From   => RETURN ObProp.NameToObliq (CameraGO.From);
    | Code.To     => RETURN ObProp.NameToObliq (CameraGO.To);
    | Code.Up     => RETURN ObProp.NameToObliq (CameraGO.Up);
    | Code.Aspect => RETURN ObProp.NameToObliq (CameraGO.Aspect);
    | Code.SetFrom =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal(args, 2, self, opCode, loc) DO
        go.setProp (CameraGO.From.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetTo =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal(args, 2, self, opCode, loc) DO
        go.setProp (CameraGO.To.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetUp =>
      WITH go = ObGO.GetArg                 (args, 1, self, opCode, loc),
           p  = ObPointProp.GetOverloadedVal(args, 2, self, opCode, loc) DO
        go.setProp (CameraGO.Up.bind (p));
        RETURN ObValue.valOk;
      END;
    | Code.SetAspect =>
      WITH go = ObGO.GetArg                (args, 1, self, opCode, loc),
           r  = ObRealProp.GetOverloadedVal(args, 2, self, opCode, loc) DO
        go.setProp (CameraGO.Aspect.bind (r));
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
END ObCameraGO.
