(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:44:02 PDT 1994 by najork                   *)
(*       Created on Sat May 28 17:58:15 PDT 1994 by najork                   *)


MODULE ObProp;

IMPORT ObAux, ObCommand, ObLib, ObProtoLoader, ObReal, ObProxiedObj, ObValue, 
       Obliq, Prop, PropProxy, ProxiedObj, SynLocation;

CONST
  pkgname = "Prop";


(*****************************************************************************)
(* Wrapper for Prop.T                                                        *)
(*****************************************************************************)

TYPE T = ObProxiedObj.T BRANDED "ObProp.T" OBJECT END;


PROCEDURE AddTObj (prop : Prop.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a Prop.T>", po := prop) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      prop.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddTObj;


PROCEDURE GetT (args    : ObValue.ArgArray; 
                idx     : INTEGER; 
                package : ObLib.T; 
                opCode  : ObLib.OpCode; 
                loc     : SynLocation.T) : Prop.T 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
        T (node) => RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetT;


(*****************************************************************************)
(* Wrapper for Prop.Name                                                     *)
(*****************************************************************************)


REVEAL Name = ObProxiedObj.T BRANDED "ObProp.Name" OBJECT END;
    

PROCEDURE GetName (args    : ObValue.ArgArray; 
                   idx     : INTEGER; 
                   package : ObLib.T; 
                   opCode  : ObLib.OpCode; 
                   loc     : SynLocation.T) : Prop.Name 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
        Name (node) => RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetName;


PROCEDURE NameToObliq (pn : Prop.Name) : ObValue.Val =
  BEGIN
    IF pn.proxy = NIL THEN
      pn.makeProxy ();
    END;
    <* ASSERT pn.proxy # NIL *>
    RETURN pn.proxy.obj;
  END NameToObliq;


(*****************************************************************************)
(* Wrapper for Prop.Val                                                      *)
(*****************************************************************************)


REVEAL Val = ObProxiedObj.T BRANDED "ObProp.Val" OBJECT END;
    

(*****************************************************************************)
(* Wrapper for Prop.Beh                                                      *)
(*****************************************************************************)


REVEAL Beh = ObProxiedObj.T BRANDED "ObProp.Beh" OBJECT END;
    

(*****************************************************************************)
(* Wrapper for Prop.Request                                                  *)
(*****************************************************************************)


REVEAL Request = ObProxiedObj.T BRANDED "ObProp.Request" OBJECT END;
    

PROCEDURE GetRequest (args    : ObValue.ArgArray; 
                      idx     : INTEGER; 
                      package : ObLib.T; 
                      opCode  : ObLib.OpCode; 
                      loc     : SynLocation.T) : Prop.Request 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
        Request (node) => RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetRequest;


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
    opCodes := NEW (REF OpCodes, NUMBER (Code));
  BEGIN
    opCodes^ := 
        OpCodes {
            NewOpCode ("BadMethod",   -1, Code.BadMethod),
            NewOpCode ("BadInterval", -1, Code.BadInterval),
            NewOpCode ("RequestStart", 1, Code.RequestStart),
            NewOpCode ("RequestDur",   1, Code.RequestDur)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));

    BadMethod := 
        NEW (ObValue.ValException, name := pkgname & "_BadMethod");
    BadInterval := 
        NEW (ObValue.ValException, name := pkgname & "_BadInterval");

    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR 
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** retrieve the prototype ***)
    loader.load ("Prop.obl");
    TProto := loader.get ("Prop_TProto");

    (*** Register the proxy makers ***)
    PropProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE
  Code = {BadMethod, BadInterval, RequestStart, RequestDur};

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
    | Code.BadMethod => 
      RETURN BadMethod;
    | Code.BadInterval =>
      RETURN BadInterval;
    | Code.RequestStart =>
      WITH req = GetRequest (args, 1, self, opCode, loc) DO
        RETURN ObReal.M3ToObliq (req.start);
      END;
    | Code.RequestDur =>
      WITH req = GetRequest (args, 1, self, opCode, loc) DO
        RETURN ObReal.M3ToObliq (req.dur);
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
END ObProp.
