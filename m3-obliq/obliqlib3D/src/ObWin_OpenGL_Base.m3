(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Oct  9 13:31:54 PDT 1995 by najork                   *)
(*       Created on Mon Mar  7 13:05:12 PST 1994 by najork                   *)


MODULE ObWin_OpenGL_Base;


IMPORT GraphicsBase, ObAux, ObCommand, ObGraphicsBase, ObInt, ObLib, 
       ObProtoLoader, ObText, ObValue, Obliq, ProxiedObj, SynLocation, 
       Win_OpenGL_Base, Win_OpenGL_BaseProxy;


CONST 
  pkgname = "Win`OpenGL`Base";


(*****************************************************************************)
(* Wrapper for RootGO.T                                                      *)
(*****************************************************************************)


TYPE
  T = ObGraphicsBase.T BRANDED "ObWin_OpenGL_Base.T" OBJECT END;


PROCEDURE AddTObj (base : Win_OpenGL_Base.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a Win_OpenGL_Base.T>", po := base) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      base.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddTObj;


PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : Win_OpenGL_Base.T 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | T (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
        <* ASSERT FALSE *>
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
            NewOpCode ("New",         5, Code.New),
            NewOpCode ("NewStd",      0, Code.NewStd),
            NewOpCode ("ChangeTitle", 2, Code.ChangeTitle),
            NewOpCode ("AwaitDelete", 1, Code.AwaitDelete),
            NewOpCode ("Destroy",     1, Code.Destroy)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR 
  TProto : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** retrieve the prototype ***)
    loader.load ("Win_OpenGL_Base.obl");
    TProto := loader.get ("Win`OpenGL`Base_TProto");

    (*** Register the proxy maker ***)
    Win_OpenGL_BaseProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {New, NewStd, ChangeTitle, AwaitDelete, Destroy};

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
      TRY
        WITH  title = ObText.GetArg (args, 1, self, opCode, loc),
              win_x = ObInt.GetArg  (args, 2, self, opCode, loc),
              win_y = ObInt.GetArg  (args, 3, self, opCode, loc),
              win_w = ObInt.GetArg  (args, 4, self, opCode, loc),
              win_h = ObInt.GetArg  (args, 5, self, opCode, loc),
              base  = NEW (Win_OpenGL_Base.T).init (title, win_x, win_y, 
                                                  win_w, win_h) DO
          RETURN base.proxy.obj;
        END;
      EXCEPT
      | GraphicsBase.Failure =>
        ObValue.RaiseException (ObGraphicsBase.Failure, opCode.name, loc);
        <* ASSERT FALSE *>
      END;
    | Code.NewStd =>
      TRY
        WITH base = NEW (Win_OpenGL_Base.T).init ("Anim3D Viewer") DO
          RETURN base.proxy.obj;
        END;
      EXCEPT
      | GraphicsBase.Failure =>
        ObValue.RaiseException (ObGraphicsBase.Failure, opCode.name, loc);
        <* ASSERT FALSE *>
      END;
    | Code.ChangeTitle =>
      WITH base  = GetArg        (args, 1, self, opCode, loc),
           title = ObText.GetArg (args, 2, self, opCode, loc) DO
        base.changeTitle (title);
        RETURN ObValue.valOk;
      END;
    | Code.AwaitDelete =>
      WITH base  = GetArg (args, 1, self, opCode, loc) DO
        base.awaitDelete ();
        RETURN ObValue.valOk;
      END;
    | Code.Destroy =>
      WITH base  = GetArg (args, 1, self, opCode, loc) DO
        base.destroy ();
        RETURN ObValue.valOk;
      END;
    END;
  END DoEval;


(*****************************************************************************)
(* Help                                                                      *)
(*****************************************************************************)


PROCEDURE Help (self : ObCommand.T; arg : TEXT; <* UNUSED *> data : REFANY) =
  BEGIN
    ObAux.Help (self, arg, pkgname, "Win_OpenGL_Base");
  END Help;


BEGIN
END ObWin_OpenGL_Base.
