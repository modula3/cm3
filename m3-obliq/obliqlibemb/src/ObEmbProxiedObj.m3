(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sun Aug 13 12:51:44 EDT 1995 by macintyre                *)
(*      modified on Mon Sep 26 21:52:38 PDT 1994 by najork                   *)
(*       Created on Wed Jul 20 09:48:11 PDT 1994 by najork                   *)
(*                                                                           *)
(* Used to be ObProxiedObj in Anim3D, but I don't want everything            *)
(* depending on Anim3D so I made a separate "embedded language" package      *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Apr  6 22:35:52 1998
 *)

MODULE ObEmbProxiedObj;

IMPORT EmbProxiedObj, ObLoader, ObCommand, ObLib, ObValue, Obliq,
       SynLocation, SynWr; 

CONST
  pkgname = "EmbProxiedObj";

(*****************************************************************************)
(* Wrapper for EmbProxiedObj.T                                               *)
(*****************************************************************************)

REVEAL 
  T = Public BRANDED "ObEmbProxiedObj.T" OBJECT
  OVERRIDES
    Is := IsT;
  END;

PROCEDURE IsT (self: T; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF 
      T (oth) => RETURN self.po = oth.po;
    ELSE 
      RETURN FALSE 
    END;
  END IsT;

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : EmbProxiedObj.T 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw", Obliq.Console()) DO
      TYPECASE raw OF
      | T (node) =>
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetArg;

PROCEDURE Extend(READONLY objs: Obliq.Vals): Obliq.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    RETURN Obliq.ObjectInvoke(objs[0], "extend", objs, Obliq.Console());
  END Extend;

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
            NewOpCode ("Error",   -1, Code.Error),
            NewOpCode ("Extend",   2, Code.Extend)
        };

    errorException := Obliq.NewException("EmbProxiedObj_Error");
    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;

PROCEDURE SetupModule (loader: ObLoader.T) =
  BEGIN
    pkgloader := loader;
    pkgloader.load ("EmbProxiedObj.obl");
  END SetupModule;

(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)

VAR errorException: ObValue.Val;

TYPE
  Code = {Extend, Error};

  OpCode = ObLib.OpCode BRANDED "ObEmbProxiedObj.OpCode" OBJECT
    code: Code;
  END;
    
  Package = ObLib.T BRANDED "ObEmbProxiedObj.Package" OBJECT
  OVERRIDES
    Eval := DoEval;
  END;

PROCEDURE DoEval (self         : Package; 
                  opCode       : ObLib.OpCode; 
     <* UNUSED *> arity        : ObLib.OpArity; 
                  READONLY args: ObValue.ArgArray; 
     <* UNUSED *> temp         : BOOLEAN;
     <* UNUSED *> swr          : SynWr.T;
                  loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.Error =>
      RETURN errorException;
    | Code.Extend =>
      WITH po    = GetArg (args, 1, self, opCode, loc),
           proxy = po.proxy DO
        TRY
          proxy.changeProxy(Obliq.ObjectClone (Obliq.Vals {args[1], args[2]},
                                               self := args[1]));
          RETURN proxy.obj;
        EXCEPT
        | EmbProxiedObj.Error (msg) =>
          ObValue.RaiseException(errorException, 
                                 self.name & "_" & opCode.name & ": " & msg,
                                 loc);
          <*ASSERT FALSE*>(* to shut up warning in compiler *)
        END;
      END;
    END;
  END DoEval;

(*****************************************************************************)
(* Help                                                                      *)
(*****************************************************************************)

PROCEDURE Help (wr: SynWr.T; self : ObCommand.T; arg : TEXT; 
                <* UNUSED *> data : REFANY) =
  BEGIN
    IF pkgloader # NIL THEN
      pkgloader.help (wr, self, arg, pkgname);
    END;
  END Help;

VAR pkgloader: ObLoader.T := NIL;

BEGIN
END ObEmbProxiedObj.
