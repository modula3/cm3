(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Sep 27 14:22:18 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)

MODULE ObGroupGO;

IMPORT GroupGO, GroupGOProxy, ObAux, ObCommand, ObGO, ObInt, ObLib, 
       ObProtoLoader, ObValue, Obliq, ProxiedObj, SynLocation;


CONST
  pkgname = "GroupGO";


(*****************************************************************************)
(* Wrapper for GroupGO.T                                                     *)
(*****************************************************************************)


REVEAL
  T = ObGO.T BRANDED "ObGroupGO.T" OBJECT END;


PROCEDURE AddTObj (group : GroupGO.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a GroupGO.T>", po := group) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      group.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddTObj;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : GroupGO.T 
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
            NewOpCode ("BadElement",     -1, Code.BadElement),
            NewOpCode ("New",             0, Code.New),
            NewOpCode ("NewWithSizeHint", 1, Code.NewWithSizeHint),
            NewOpCode ("Add",             2, Code.Add),
            NewOpCode ("Remove",          2, Code.Remove),
            NewOpCode ("Flush",           1, Code.Flush),
            NewOpCode ("Content",         1, Code.Content)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);

    BadElement := NEW (ObValue.ValException, name := pkgname & "_BadElement");
  END SetupPackage;


VAR
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** Retrieve the prototype ***)
    loader.load ("GroupGO.obl");
    TProto := loader.get ("GroupGO_TProto");

    (*** Register the proxy maker ***)
    GroupGOProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {BadElement, New, NewWithSizeHint, Add, Remove, Flush, Content};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;
    
  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;

VAR
  BadElement : ObValue.ValException;


PROCEDURE DoEval (self         : Package; 
                  opCode       : ObLib.OpCode; 
     <* UNUSED *> arity        : ObLib.OpArity; 
                  READONLY args: ObValue.ArgArray; 
     <* UNUSED *> temp         : BOOLEAN;
                  loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.BadElement => 
      RETURN BadElement;
    | Code.New => 
      WITH group = NEW (GroupGO.T).init () DO
        RETURN group.proxy.obj;
      END;
    | Code.NewWithSizeHint => 
      WITH size  = ObInt.GetArg (args, 1, self, opCode, loc),
           group = NEW (GroupGO.T).init (size) DO
        RETURN group.proxy.obj;
      END;
    | Code.Add =>
      WITH group =      GetArg (args, 1, self, opCode, loc),
           go    = ObGO.GetArg (args, 2, self, opCode, loc) DO
        group.add (go);
        RETURN ObValue.valOk;
      END;
    | Code.Remove =>
      WITH group = GetArg (args, 1, self, opCode, loc),
           go    = ObGO.GetArg (args, 2, self, opCode, loc) DO
        TRY
          group.remove (go);
        EXCEPT
        | GroupGO.BadElement => 
          ObValue.RaiseException (BadElement, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.Flush =>
      WITH group = GetArg (args, 1, self, opCode, loc) DO
        group.flush ();
        RETURN ObValue.valOk;
      END;
    | Code.Content =>
      WITH group = GetArg (args, 1, self, opCode, loc) DO
        WITH cont = group.content()^,
             vals = NEW (REF ARRAY OF Obliq.Val, NUMBER (cont)) DO
          FOR i := FIRST (cont) TO LAST (cont) DO
            vals[i] := cont[i].proxy.obj;
          END;
          RETURN Obliq.NewArray (vals^);
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
END ObGroupGO.
