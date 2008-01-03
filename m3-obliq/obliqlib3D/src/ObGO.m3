(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:14:46 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)


MODULE ObGO;

IMPORT GO, ObAux, ObCommand, ObKeyCB, ObLib, ObMouseCB, ObPositionCB, ObProp,
       ObProtoLoader, ObProxiedObj, ObText, ObTransformProp, ObValue, Obliq, 
       SynLocation;


CONST
  pkgname = "GO";


(*****************************************************************************)
(* Wrapper for GO.T                                                          *)
(*****************************************************************************)

REVEAL 
  T = ObProxiedObj.T BRANDED "ObGO.T" OBJECT END;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : GO.T 
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
            NewOpCode ("PropUndefined",   -1, Code.PropUndefined),
            NewOpCode ("StackError",      -1, Code.StackError),
            NewOpCode ("SetProp",          2, Code.SetProp),
            NewOpCode ("UnsetProp",        2, Code.UnsetProp),
            NewOpCode ("GetProp",          2, Code.GetProp),
            NewOpCode ("SetName",          2, Code.SetName),
            NewOpCode ("GetName",          1, Code.GetName),
            NewOpCode ("FindName",         2, Code.FindName),
            NewOpCode ("Transform",       -1, Code.Transform),
            NewOpCode ("SetTransform",     2, Code.SetTransform),
            NewOpCode ("GetTransform",     1, Code.GetTransform),
            NewOpCode ("PushMouseCB",      2, Code.PushMouseCB),
            NewOpCode ("PopMouseCB",       1, Code.PopMouseCB),
            NewOpCode ("RemoveMouseCB",    2, Code.RemoveMouseCB),
            NewOpCode ("InvokeMouseCB",    2, Code.InvokeMouseCB),
            NewOpCode ("PushPositionCB",   2, Code.PushPositionCB),
            NewOpCode ("PopPositionCB",    1, Code.PopPositionCB),
            NewOpCode ("RemovePositionCB", 2, Code.RemovePositionCB),
            NewOpCode ("InvokePositionCB", 2, Code.InvokePositionCB),
            NewOpCode ("PushKeyCB",        2, Code.PushKeyCB),
            NewOpCode ("PopKeyCB",         1, Code.PopKeyCB),
            NewOpCode ("RemoveKeyCB",      2, Code.RemoveKeyCB),
            NewOpCode ("InvokeKeyCB",      2, Code.InvokeKeyCB)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);

    PropUndefined := 
        NEW (ObValue.ValException, name := pkgname & "_PropUndefined");
    StackError := 
        NEW (ObValue.ValException, name := pkgname & "_StackError");
  END SetupPackage;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    loader.load ("GO.obl");
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {PropUndefined, StackError, SetProp, UnsetProp, GetProp, 
          SetName, GetName, FindName, 
          Transform, SetTransform, GetTransform,
          PushMouseCB, PopMouseCB, RemoveMouseCB, InvokeMouseCB,
          PushPositionCB, PopPositionCB, RemovePositionCB, InvokePositionCB,
          PushKeyCB, PopKeyCB, RemoveKeyCB, InvokeKeyCB};

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
    | Code.PropUndefined => 
      RETURN PropUndefined;
    | Code.StackError => 
      RETURN StackError;
    | Code.SetProp =>
      WITH go   = GetArg      (args, 1, self, opCode, loc),
           prop = ObProp.GetT (args, 2, self, opCode, loc) DO
        go.setProp (prop);
        RETURN ObValue.valOk;
      END;
    | Code.UnsetProp =>
      WITH go = GetArg         (args, 1, self, opCode, loc),
           pn = ObProp.GetName (args, 2, self, opCode, loc) DO
        TRY
          go.unsetProp (pn);
        EXCEPT
          GO.PropUndefined =>
          ObValue.RaiseException (PropUndefined, opCode.name, loc);
        END;
        RETURN ObValue.valOk;      
      END;
    | Code.GetProp =>
      WITH go = GetArg         (args, 1, self, opCode, loc),
           pn = ObProp.GetName (args, 2, self, opCode, loc) DO
        TRY
          RETURN go.getProp (pn).proxy.obj;
        EXCEPT
          GO.PropUndefined =>
          ObValue.RaiseException (PropUndefined, opCode.name, loc);
          RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
        END;
      END;
    | Code.SetName =>
      WITH go   = GetArg        (args, 1, self, opCode, loc),
           name = ObText.GetArg (args, 2, self, opCode, loc) DO
        go.setName (name);
        RETURN ObValue.valOk;
      END;
    | Code.GetName =>
      WITH go = GetArg (args, 1, self, opCode, loc),
           name = go.getName () DO
        IF name = NIL THEN
          RETURN ObValue.valOk;
        ELSE
          RETURN Obliq.NewText (name);
        END;
      END;
    | Code.FindName =>
      WITH go   = GetArg        (args, 1, self, opCode, loc),
           name = ObText.GetArg (args, 2, self, opCode, loc),
           res  = go.findName (name) DO
        IF res = NIL THEN
          RETURN ObValue.valOk;
        ELSE
          RETURN res.proxy.obj;
        END;
      END;
    | Code.Transform =>
      RETURN ObProp.NameToObliq (GO.Transform);
    | Code.SetTransform =>
      WITH go = GetArg (args, 1, self, opCode, loc),
           pv = ObTransformProp.GetOverloadedVal 
           (args, 2, self, opCode, loc) DO
        go.setProp (GO.Transform.bind (pv));
        RETURN ObValue.valOk;
      END;
    | Code.GetTransform =>
      WITH go = GetArg (args, 1, self, opCode, loc) DO
        TRY
          RETURN GO.GetTransform (go).proxy.obj;
        EXCEPT
        | GO.PropUndefined =>
          ObValue.RaiseException (PropUndefined, opCode.name, loc);
          RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
        END;
      END;
    | Code.PushMouseCB =>
      WITH go = GetArg         (args, 1, self, opCode, loc),
           cb = ObMouseCB.GetT (args, 2, self, opCode, loc) DO
        go.pushMouseCB (cb);
        RETURN ObValue.valOk;
      END;
    | Code.PopMouseCB =>
      TRY
        WITH go = GetArg (args, 1, self, opCode, loc) DO
          go.popMouseCB ();
        END;
      EXCEPT
      | GO.StackError => Obliq.RaiseException (StackError, opCode.name, loc);
      END;
      RETURN ObValue.valOk;
    | Code.RemoveMouseCB =>
      TRY
        WITH go = GetArg         (args, 1, self, opCode, loc),
             cb = ObMouseCB.GetT (args, 2, self, opCode, loc) DO
          go.removeMouseCB (cb);
        END;
      EXCEPT
      | GO.StackError => Obliq.RaiseException (StackError, opCode.name, loc);
      END;
      RETURN ObValue.valOk;
    | Code.InvokeMouseCB =>
      WITH go = GetArg           (args, 1, self, opCode, loc),
           mr = ObMouseCB.GetRec (args, 2, self, opCode, loc) DO
        go.invokeMouseCB (mr);
        RETURN ObValue.valOk;
      END;
    | Code.PushPositionCB =>
      WITH go = GetArg            (args, 1, self, opCode, loc),
           cb = ObPositionCB.GetT (args, 2, self, opCode, loc) DO
        go.pushPositionCB (cb);
        RETURN ObValue.valOk;
      END;
    | Code.PopPositionCB =>
      TRY
        WITH go = GetArg (args, 1, self, opCode, loc) DO
          go.popPositionCB ();
        END;
      EXCEPT
      | GO.StackError => Obliq.RaiseException (StackError, opCode.name, loc);
      END;
      RETURN ObValue.valOk;
    | Code.RemovePositionCB =>
      TRY
        WITH go = GetArg            (args, 1, self, opCode, loc),
             cb = ObPositionCB.GetT (args, 2, self, opCode, loc) DO
          go.removePositionCB (cb);
        END;
      EXCEPT
      | GO.StackError => Obliq.RaiseException (StackError, opCode.name, loc);
      END;
      RETURN ObValue.valOk;
    | Code.InvokePositionCB =>
      WITH go = GetArg              (args, 1, self, opCode, loc),
           pr = ObPositionCB.GetRec (args, 2, self, opCode, loc) DO
        go.invokePositionCB (pr);
        RETURN ObValue.valOk;
      END;
    | Code.PushKeyCB =>
      WITH go = GetArg       (args, 1, self, opCode, loc),
           cb = ObKeyCB.GetT (args, 2, self, opCode, loc) DO
        go.pushKeyCB (cb);
        RETURN ObValue.valOk;
      END;
    | Code.PopKeyCB =>
      TRY
        WITH go = GetArg (args, 1, self, opCode, loc) DO
          go.popKeyCB ();
        END;
      EXCEPT
      | GO.StackError => Obliq.RaiseException (StackError, opCode.name, loc);
      END;
      RETURN ObValue.valOk;
    | Code.RemoveKeyCB =>
      TRY
        WITH go = GetArg       (args, 1, self, opCode, loc),
             cb = ObKeyCB.GetT (args, 2, self, opCode, loc) DO
          go.removeKeyCB (cb);
        END;
      EXCEPT
      | GO.StackError => Obliq.RaiseException (StackError, opCode.name, loc);
      END;
      RETURN ObValue.valOk;
    | Code.InvokeKeyCB =>
      WITH go = GetArg         (args, 1, self, opCode, loc),
           kr = ObKeyCB.GetRec (args, 2, self, opCode, loc) DO
        go.invokeKeyCB (kr);
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
END ObGO.
