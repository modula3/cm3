(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:57:06 PDT 1994 by najork                   *)
(*       Created on Mon May 30 09:59:32 PDT 1994 by najork                   *)


MODULE ObBooleanProp;

IMPORT BooleanProp, BooleanPropProxy, ObAnimHandle, ObAux, ObBool, ObCommand, 
       ObLib, ObLongReal, ObProp, ObProtoLoader, ObReal, ObValue, Obliq, Prop,
       ProxiedObj, SynLocation;


(*****************************************************************************)
(* Wrapper for BooleanProp.Name                                              *)
(*****************************************************************************)

TYPE 
  Name = ObProp.Name BRANDED "ObBooleanProp.Name" OBJECT END;


PROCEDURE AddNameObj (pn : BooleanProp.Name) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {NameProto}),
         raw = NEW (Name, what := "<a BooleanProp.Name>", po := pn) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      pn.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddNameObj;


PROCEDURE GetName (args    : ObValue.ArgArray; 
                   idx     : INTEGER; 
                   package : ObLib.T; 
                   opCode  : ObLib.OpCode; 
                   loc     : SynLocation.T) : BooleanProp.Name 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | Name (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetName;


(*****************************************************************************)
(* Wrapper for BooleanProp.Val                                               *)
(*****************************************************************************)


TYPE 
  Val = ObProp.Val BRANDED "ObBooleanProp.Val" OBJECT END;


PROCEDURE AddValObj (pv : BooleanProp.Val) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {ValProto}),
         raw = NEW (Val, what := "<a BooleanProp.Val>", po := pv) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      pv.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddValObj;


PROCEDURE GetVal (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : BooleanProp.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | Val (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetVal;


PROCEDURE GetOverloadedVal (args    : ObValue.ArgArray; 
                            idx     : INTEGER; 
                            package : ObLib.T; 
                            opCode  : ObLib.OpCode; 
                            loc     : SynLocation.T) : BooleanProp.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    TYPECASE args[idx] OF 
    | ObValue.ValBool (node) => 
      RETURN BooleanProp.NewConst (node.bool);
    | ObValue.ValObj =>
      WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
        TYPECASE raw OF 
        | Val (node) => 
          RETURN node.po;
        ELSE 
          ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
          RETURN NIL;      (* ... only to suppress compiler warning *)
        END;
      END;
    ELSE
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
      RETURN NIL;   (* ... only to suppress compiler warning *)
    END;
  END GetOverloadedVal;


(*****************************************************************************)
(* Wrapper for BooleanProp.Beh                                               *)
(*****************************************************************************)

TYPE 
  Beh = ObProp.Beh BRANDED "ObBooleanProp.Beh" OBJECT END;


PROCEDURE GetBeh (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : BooleanProp.Beh 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | Beh (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetBeh;


(*****************************************************************************)
(* Wrapper for BooleanProp.ConstBeh                                          *)
(*****************************************************************************)


TYPE 
  ConstBeh = Beh BRANDED "ObBooleanProp.ConstBeh" OBJECT END;


PROCEDURE AddConstBehObj (beh : BooleanProp.ConstBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {ConstBehProto}),
         raw = NEW (ConstBeh, what := "<a BooleanProp.ConstBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddConstBehObj;


PROCEDURE GetConstBeh (args    : ObValue.ArgArray; 
                       idx     : INTEGER; 
                       package : ObLib.T; 
                       opCode  : ObLib.OpCode; 
                       loc     : SynLocation.T) : BooleanProp.ConstBeh 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | ConstBeh (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetConstBeh;


(*****************************************************************************)
(* Wrapper for BooleanProp.SyncBeh                                           *)
(*****************************************************************************)


TYPE 
  SyncBeh = Beh BRANDED "ObBooleanProp.SyncBeh" OBJECT END;


PROCEDURE AddSyncBehObj (beh : BooleanProp.SyncBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {SyncBehProto}),
         raw = NEW (SyncBeh, what := "<a BooleanProp.SyncBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddSyncBehObj;


PROCEDURE GetSyncBeh (args    : ObValue.ArgArray; 
                       idx     : INTEGER; 
                       package : ObLib.T; 
                       opCode  : ObLib.OpCode; 
                       loc     : SynLocation.T) : BooleanProp.SyncBeh 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | SyncBeh (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetSyncBeh;


(*****************************************************************************)
(* Wrapper for BooleanProp.AsyncBeh                                          *)
(*****************************************************************************)


TYPE 
  AsyncBeh = Beh BRANDED "ObBooleanProp.AsyncBeh" OBJECT END;


PROCEDURE AddAsyncBehObj (beh : BooleanProp.AsyncBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {AsyncBehProto}),
         raw = NEW (AsyncBeh, what := "<a BooleanProp.AsyncBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (AsyncBehProxy, obj := obj);
    END;
  END AddAsyncBehObj;


TYPE
  AsyncBehProxy = BooleanPropProxy.AsyncBehProxy BRANDED OBJECT
  OVERRIDES
    compute := AsyncBehCompute;
  END;


PROCEDURE AsyncBehCompute (proxy : AsyncBehProxy; time : LONGREAL) : BOOLEAN 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {Obliq.NewReal (time)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "compute", args) DO
        RETURN Obliq.ToBool (res);
      END;
    EXCEPT
    | ObValue.Error (packet) =>
      RAISE Prop.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE Prop.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END AsyncBehCompute;


(*****************************************************************************)
(* Wrapper for BooleanProp.DepBeh                                            *)
(*****************************************************************************)


TYPE 
  DepBeh = Beh BRANDED "ObBooleanProp.DepBeh" OBJECT END;


PROCEDURE AddDepBehObj (beh : BooleanProp.DepBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {DepBehProto}),
         raw = NEW (DepBeh, what := "<a BooleanProp.DepBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (DepBehProxy, obj := obj);
    END;
  END AddDepBehObj;


TYPE
  DepBehProxy = BooleanPropProxy.DepBehProxy BRANDED OBJECT
  OVERRIDES
    compute := DepBehCompute;
  END;


PROCEDURE DepBehCompute (proxy : DepBehProxy; time : LONGREAL) : BOOLEAN 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {Obliq.NewReal (time)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "compute", args) DO
        RETURN Obliq.ToBool (res);
      END;
    EXCEPT
    | ObValue.Error (packet) =>
      RAISE Prop.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE Prop.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END DepBehCompute;


(*****************************************************************************)
(* Wrapper for BooleanProp.Request                                           *)
(*****************************************************************************)


TYPE
  Request = ObProp.Request BRANDED "ObBooleanProp.Request" OBJECT END;


PROCEDURE AddRequestObj (req : BooleanProp.Request) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {RequestProto}),
         raw = NEW (Request, what := "<a BooleanProp.Request>", po := req) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      req.proxy := NEW (RequestProxy, obj := obj);
    END;
  END AddRequestObj;


PROCEDURE GetRequest (args    : ObValue.ArgArray; 
                      idx     : INTEGER; 
                      package : ObLib.T; 
                      opCode  : ObLib.OpCode; 
                      loc     : SynLocation.T) : BooleanProp.Request 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | Request (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetRequest;


TYPE
  RequestProxy = BooleanPropProxy.RequestProxy BRANDED OBJECT
  OVERRIDES
    value := RequestValue;
  END;


PROCEDURE RequestValue (proxy    : RequestProxy; 
                        startval : BOOLEAN; 
                        reltime  : REAL) : BOOLEAN 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {Obliq.NewBool (startval),
                              ObReal.M3ToObliq (reltime)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "value", args) DO
        RETURN Obliq.ToBool (res);
      END;
    EXCEPT
    | ObValue.Error (packet) =>
      RAISE Prop.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE Prop.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END RequestValue;


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
            NewOpCode ("NameBind",           2, Code.NameBind),
            NewOpCode ("ValGetBeh",          1, Code.ValGetBeh),
            NewOpCode ("ValSetBeh",          2, Code.ValSetBeh),
            NewOpCode ("ValGet",             1, Code.ValGet),
            NewOpCode ("ValValue",           2, Code.ValValue),
            NewOpCode ("NewConst",           1, Code.NewConst),
            NewOpCode ("NewSync",            2, Code.NewSync),
            NewOpCode ("NewAsync",           1, Code.NewAsync),
            NewOpCode ("NewDep",             1, Code.NewDep),
            NewOpCode ("ConstBehSet",        2, Code.ConstBehSet),
            NewOpCode ("NewConstBeh",        1, Code.NewConstBeh),
            NewOpCode ("SyncBehAddRequest",  2, Code.SyncBehAddRequest),
            NewOpCode ("SyncBehChange",      3, Code.SyncBehChange),
            NewOpCode ("NewSyncBeh",         2, Code.NewSyncBeh),
            NewOpCode ("NewAsyncBeh",        1, Code.NewAsyncBeh),
            NewOpCode ("NewDepBeh",          1, Code.NewDepBeh),
            NewOpCode ("NewRequest",         3, Code.NewRequest)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR
  NameProto     : ObValue.Val;
  ValProto      : ObValue.Val;
  ConstBehProto : ObValue.Val;
  SyncBehProto  : ObValue.Val;
  AsyncBehProto : ObValue.Val;
  DepBehProto   : ObValue.Val;
  RequestProto  : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** retrieve the prototypes ***)
    loader.load ("BooleanProp.obl");
    NameProto     := loader.get ("BooleanProp_NameProto");
    ValProto      := loader.get ("BooleanProp_ValProto");
    ConstBehProto := loader.get ("BooleanProp_ConstBehProto");
    SyncBehProto  := loader.get ("BooleanProp_SyncBehProto");
    AsyncBehProto := loader.get ("BooleanProp_AsyncBehProto");
    DepBehProto   := loader.get ("BooleanProp_DepBehProto");
    RequestProto  := loader.get ("BooleanProp_RequestProto");

    (*** Register the proxy makers ***)
    BooleanPropProxy.NamePM     := AddNameObj;
    BooleanPropProxy.ValPM      := AddValObj;
    BooleanPropProxy.ConstBehPM := AddConstBehObj;
    BooleanPropProxy.SyncBehPM  := AddSyncBehObj;
    BooleanPropProxy.AsyncBehPM := AddAsyncBehObj;
    BooleanPropProxy.DepBehPM   := AddDepBehObj;
    BooleanPropProxy.RequestPM  := AddRequestObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {NameBind, 
          ValGetBeh, ValSetBeh, ValGet, ValValue, 
             NewConst, NewSync, NewAsync, NewDep,
          ConstBehSet, NewConstBeh, 
          SyncBehAddRequest, SyncBehChange, NewSyncBeh,
          NewAsyncBeh,
          NewDepBeh,
          NewRequest};

  OpCode = ObLib.OpCode BRANDED OBJECT
    code: Code;
  END;

  Package = ObLib.T BRANDED OBJECT
  OVERRIDES
    Eval := DoEval;
  END;

CONST 
  pkgname = "BooleanProp";



PROCEDURE DoEval (self         : Package; 
                  opCode       : ObLib.OpCode; 
     <* UNUSED *> arity        : ObLib.OpArity; 
                  READONLY args: ObValue.ArgArray; 
     <* UNUSED *> temp         : BOOLEAN;
                  loc          : SynLocation.T) : ObValue.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.NameBind =>
      WITH pn   = GetName (args, 1, self, opCode, loc),
           pv   = GetVal  (args, 2, self, opCode, loc),
           prop = pn.bind (pv) DO
        RETURN prop.proxy.obj;
      END;
    | Code.ValGetBeh =>
      WITH pv = GetVal (args, 1, self, opCode, loc) DO
        RETURN pv.beh.proxy.obj;
      END;
    | Code.ValSetBeh =>
      WITH pv  = GetVal (args, 1, self, opCode, loc),
           beh = GetBeh (args, 2, self, opCode, loc) DO
        pv.beh := beh;
        RETURN ObValue.valOk;
      END;
    | Code.ValGet =>
      WITH pv = GetVal (args, 1, self, opCode, loc) DO
        TRY
          RETURN Obliq.NewBool (pv.get ());
        EXCEPT
        | Prop.BadMethod =>
          ObValue.RaiseException (ObProp.BadMethod, opCode.name, loc);
          RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
        END;
      END;
    | Code.ValValue =>
      WITH pv   = GetVal (args, 1, self, opCode, loc),
           time = ObLongReal.GetArg (args, 2, self, opCode, loc) DO
        TRY
          RETURN Obliq.NewBool (pv.value (time));
        EXCEPT
        | Prop.BadMethod =>
          ObValue.RaiseException (ObProp.BadMethod, opCode.name, loc);
          RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
        END;
      END;
    | Code.NewConst =>
      (*** AddValObj creates the actual Obliq object ***)
      WITH r   = ObBool.GetArg (args, 1, self, opCode, loc),
           val = BooleanProp.NewConst (r) DO
        RETURN val.proxy.obj;
      END;
    | Code.NewSync =>
      WITH ah  = ObAnimHandle.GetT (args, 1, self, opCode, loc),
           r   = ObBool.GetArg (args, 2, self, opCode, loc),
           val = BooleanProp.NewSync (ah, r) DO
        RETURN val.proxy.obj;
      END;
    | Code.NewAsync =>
      WITH beh = NEW (BooleanProp.AsyncBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        WITH val = BooleanProp.NewAsync (beh) DO
          RETURN val.proxy.obj;
        END;
      END;
    | Code.NewDep =>
      WITH beh = NEW (BooleanProp.DepBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        WITH val = BooleanProp.NewDep (beh) DO
          RETURN val.proxy.obj;
        END;
      END;
    | Code.ConstBehSet =>
      WITH beh = GetConstBeh (args, 1, self, opCode, loc),
           r   = ObBool.GetArg (args, 2, self, opCode, loc) DO
        beh.set (r);
        RETURN ObValue.valOk;
      END;
    | Code.NewConstBeh =>
      WITH r     = ObBool.GetArg (args, 1, self, opCode, loc),
           beh   = NEW (BooleanProp.ConstBeh).init (r) DO
        RETURN beh.proxy.obj;
      END;
    | Code.SyncBehAddRequest =>
      WITH beh = GetSyncBeh (args, 1, self, opCode, loc),
           req = GetRequest (args, 2, self, opCode, loc) DO
        TRY
          beh.addRequest (req);
        EXCEPT
        | Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.SyncBehChange =>
      WITH beh   = GetSyncBeh (args, 1, self, opCode, loc),
           r     = ObBool.GetArg (args, 2, self, opCode, loc),
           start = ObReal.GetArg (args, 3, self, opCode, loc) DO
        TRY
          beh.change (r, start);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.NewSyncBeh =>
      WITH ah    = ObAnimHandle.GetT (args, 1, self, opCode, loc),
           r     = ObBool.GetArg (args, 2, self, opCode, loc),
           beh   = NEW (BooleanProp.SyncBeh).init (ah, r) DO
        RETURN beh.proxy.obj;
      END;
    | Code.NewAsyncBeh =>
      WITH beh = NEW (BooleanProp.AsyncBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        RETURN obj;
      END;
    | Code.NewDepBeh =>
      WITH beh = NEW (BooleanProp.DepBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        RETURN obj;
      END;
    | Code.NewRequest =>
      WITH start = ObReal.GetArg (args, 1, self, opCode, loc),
           dur   = ObReal.GetArg (args, 2, self, opCode, loc),
           req   = NEW (BooleanProp.Request).init (start, dur),
           obj   = NARROW (req.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "value", args[3]);
        RETURN obj;
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
END ObBooleanProp.
