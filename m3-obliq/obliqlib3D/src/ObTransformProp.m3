(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:59:46 PDT 1994 by najork                   *)
(*       Created on Sat May 28 11:15:36 PDT 1994 by najork                   *)


MODULE ObTransformProp;

IMPORT Matrix4, ObAnimHandle, ObAux, ObCommand, ObLib, ObLongReal, ObMatrix4, 
       ObProp, ObProtoLoader, ObReal, ObValue, Obliq, Prop, ProxiedObj, 
       SynLocation, TransformProp, TransformPropProxy;


(*****************************************************************************)
(* Wrapper for TransformProp.Name                                            *)
(*****************************************************************************)

TYPE 
  Name = ObProp.Name BRANDED "ObTransformProp.Name" OBJECT END;


PROCEDURE AddNameObj (pn : TransformProp.Name) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {NameProto}),
         raw = NEW (Name, what := "<a TransformProp.Name>", po := pn) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      pn.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddNameObj;


PROCEDURE GetName (args    : ObValue.ArgArray; 
                   idx     : INTEGER; 
                   package : ObLib.T; 
                   opCode  : ObLib.OpCode; 
                   loc     : SynLocation.T) : TransformProp.Name 
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
(* Wrapper for TransformProp.Val                                             *)
(*****************************************************************************)


TYPE 
  Val = ObProp.Val BRANDED "ObTransformProp.Val" OBJECT END;


PROCEDURE AddValObj (pv : TransformProp.Val) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {ValProto}),
         raw = NEW (Val, what := "<a TransformProp.Val>", po := pv) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      pv.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddValObj;


PROCEDURE GetVal (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : TransformProp.Val 
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
                            loc     : SynLocation.T) : TransformProp.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    TYPECASE args[idx] OF 
    | ObMatrix4.T (node) => 
      RETURN TransformProp.NewConst (ObMatrix4.ObliqToM3 (node));
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
(* Wrapper for TransformProp.Beh                                             *)
(*****************************************************************************)

TYPE 
  Beh = ObProp.Beh BRANDED "ObTransformProp.Beh" OBJECT END;


PROCEDURE GetBeh (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : TransformProp.Beh 
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
(* Wrapper for TransformProp.ConstBeh                                        *)
(*****************************************************************************)


TYPE 
  ConstBeh = Beh BRANDED "ObTransformProp.ConstBeh" OBJECT END;


PROCEDURE AddConstBehObj (beh : TransformProp.ConstBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {ConstBehProto}),
         raw = NEW (ConstBeh, what := "<a TransformProp.ConstBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddConstBehObj;


PROCEDURE GetConstBeh (args    : ObValue.ArgArray; 
                       idx     : INTEGER; 
                       package : ObLib.T; 
                       opCode  : ObLib.OpCode; 
                       loc     : SynLocation.T) : TransformProp.ConstBeh 
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
(* Wrapper for TransformProp.SyncBeh                                         *)
(*****************************************************************************)


TYPE 
  SyncBeh = Beh BRANDED "ObTransformProp.SyncBeh" OBJECT END;


PROCEDURE AddSyncBehObj (beh : TransformProp.SyncBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {SyncBehProto}),
         raw = NEW (SyncBeh, what := "<a TransformProp.SyncBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddSyncBehObj;


PROCEDURE GetSyncBeh (args    : ObValue.ArgArray; 
                       idx     : INTEGER; 
                       package : ObLib.T; 
                       opCode  : ObLib.OpCode; 
                       loc     : SynLocation.T) : TransformProp.SyncBeh 
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
(* Wrapper for TransformProp.AsyncBeh                                        *)
(*****************************************************************************)


TYPE 
  AsyncBeh = Beh BRANDED "ObTransformProp.AsyncBeh" OBJECT END;


PROCEDURE AddAsyncBehObj (beh : TransformProp.AsyncBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {AsyncBehProto}),
         raw = NEW (AsyncBeh, what := "<a TransformProp.AsyncBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (AsyncBehProxy, obj := obj);
    END;
  END AddAsyncBehObj;


TYPE
  AsyncBehProxy = TransformPropProxy.AsyncBehProxy BRANDED OBJECT
  OVERRIDES
    compute := AsyncBehCompute;
  END;


PROCEDURE AsyncBehCompute (proxy : AsyncBehProxy; time : LONGREAL) : Matrix4.T 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {Obliq.NewReal (time)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "compute", args) DO
        RETURN ObMatrix4.ObliqToM3 (res);
      END;
    EXCEPT
    | ObValue.Error (packet) =>
      RAISE Prop.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE Prop.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END AsyncBehCompute;


(*****************************************************************************)
(* Wrapper for TransformProp.DepBeh                                          *)
(*****************************************************************************)


TYPE 
  DepBeh = Beh BRANDED "ObTransformProp.DepBeh" OBJECT END;


PROCEDURE AddDepBehObj (beh : TransformProp.DepBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {DepBehProto}),
         raw = NEW (DepBeh, what := "<a TransformProp.DepBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (DepBehProxy, obj := obj);
    END;
  END AddDepBehObj;


TYPE
  DepBehProxy = TransformPropProxy.DepBehProxy BRANDED OBJECT
  OVERRIDES
    compute := DepBehCompute;
  END;


PROCEDURE DepBehCompute (proxy : DepBehProxy; time : LONGREAL) : Matrix4.T
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {Obliq.NewReal (time)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "compute", args) DO
        RETURN ObMatrix4.ObliqToM3 (res);
      END;
    EXCEPT
    | ObValue.Error (packet) =>
      RAISE Prop.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE Prop.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END DepBehCompute;


(*****************************************************************************)
(* Wrapper for TransformProp.Request                                         *)
(*****************************************************************************)


TYPE
  Request = ObProp.Request BRANDED "ObTransformProp.Request" OBJECT END;


PROCEDURE AddRequestObj (req : TransformProp.Request) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {RequestProto}),
         raw = NEW (Request, what := "<a TransformProp.Request>", po := req) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      req.proxy := NEW (RequestProxy, obj := obj);
    END;
  END AddRequestObj;


PROCEDURE GetRequest (args    : ObValue.ArgArray; 
                      idx     : INTEGER; 
                      package : ObLib.T; 
                      opCode  : ObLib.OpCode; 
                      loc     : SynLocation.T) : TransformProp.Request 
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
  RequestProxy = TransformPropProxy.RequestProxy BRANDED OBJECT
  OVERRIDES
    value := RequestValue;
  END;


PROCEDURE RequestValue (proxy             : RequestProxy; 
                        READONLY startval : Matrix4.T; 
                        reltime           : REAL) : Matrix4.T 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {ObMatrix4.M3ToObliq (startval),
                              ObReal.M3ToObliq (reltime)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "value", args) DO
        RETURN ObMatrix4.ObliqToM3 (res);
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
            NewOpCode ("NameBind",            2, Code.NameBind),
            NewOpCode ("ValGetBeh",           1, Code.ValGetBeh),
            NewOpCode ("ValSetBeh",           2, Code.ValSetBeh),
            NewOpCode ("ValGet",              1, Code.ValGet),
            NewOpCode ("ValValue",            2, Code.ValValue),
            NewOpCode ("NewConst",            1, Code.NewConst),
            NewOpCode ("NewSync",             2, Code.NewSync),
            NewOpCode ("NewAsync",            1, Code.NewAsync),
            NewOpCode ("NewDep",              1, Code.NewDep),
            NewOpCode ("ConstBehSet",         2, Code.ConstBehSet),
            NewOpCode ("ConstBehCompose",     2, Code.ConstBehCompose),
            NewOpCode ("ConstBehReset",       1, Code.ConstBehReset),
            NewOpCode ("ConstBehTranslate",   4, Code.ConstBehTranslate),
            NewOpCode ("ConstBehScale",       4, Code.ConstBehScale),
            NewOpCode ("ConstBehRotateX",     2, Code.ConstBehRotateX),
            NewOpCode ("ConstBehRotateY",     2, Code.ConstBehRotateY),
            NewOpCode ("ConstBehRotateZ",     2, Code.ConstBehRotateZ),
            NewOpCode ("NewConstBeh",         1, Code.NewConstBeh),
            NewOpCode ("SyncBehAddRequest",   2, Code.SyncBehAddRequest),
            NewOpCode ("SyncBehReset",        2, Code.SyncBehReset),
            NewOpCode ("SyncBehChangeTo",     4, Code.SyncBehChangeTo),
            NewOpCode ("SyncBehTranslate",    6, Code.SyncBehTranslate),
            NewOpCode ("SyncBehScale",        6, Code.SyncBehScale),
            NewOpCode ("SyncBehRotateX",      4, Code.SyncBehRotateX),
            NewOpCode ("SyncBehRotateY",      4, Code.SyncBehRotateY),
            NewOpCode ("SyncBehRotateZ",      4, Code.SyncBehRotateZ),
            NewOpCode ("NewSyncBeh",          2, Code.NewSyncBeh),
            NewOpCode ("NewAsyncBeh",         1, Code.NewAsyncBeh),
            NewOpCode ("NewDepBeh",           1, Code.NewDepBeh),
            NewOpCode ("NewRequest",          3, Code.NewRequest)
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
    loader.load ("TransformProp.obl");
    NameProto     := loader.get ("TransformProp_NameProto");
    ValProto      := loader.get ("TransformProp_ValProto");
    ConstBehProto := loader.get ("TransformProp_ConstBehProto");
    SyncBehProto  := loader.get ("TransformProp_SyncBehProto");
    AsyncBehProto := loader.get ("TransformProp_AsyncBehProto");
    DepBehProto   := loader.get ("TransformProp_DepBehProto");
    RequestProto  := loader.get ("TransformProp_RequestProto");

    (*** Register the proxy makers ***)
    TransformPropProxy.NamePM     := AddNameObj;
    TransformPropProxy.ValPM      := AddValObj;
    TransformPropProxy.ConstBehPM := AddConstBehObj;
    TransformPropProxy.SyncBehPM  := AddSyncBehObj;
    TransformPropProxy.AsyncBehPM := AddAsyncBehObj;
    TransformPropProxy.DepBehPM   := AddDepBehObj;
    TransformPropProxy.RequestPM  := AddRequestObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {NameBind, 
          ValGetBeh, ValSetBeh, ValGet, ValValue, 
             NewConst, NewSync, NewAsync, NewDep,
          ConstBehSet, ConstBehCompose, ConstBehReset, ConstBehTranslate,
             ConstBehScale, ConstBehRotateX, ConstBehRotateY, ConstBehRotateZ, 
             NewConstBeh, 
          SyncBehAddRequest, SyncBehReset, SyncBehChangeTo, SyncBehTranslate, 
             SyncBehScale, SyncBehRotateX, SyncBehRotateY, SyncBehRotateZ, 
             NewSyncBeh,
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
  pkgname = "TransformProp";


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
          RETURN ObMatrix4.M3ToObliq (pv.get ());
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
          RETURN ObMatrix4.M3ToObliq (pv.value (time));
        EXCEPT
        | Prop.BadMethod =>
          ObValue.RaiseException (ObProp.BadMethod, opCode.name, loc);
          RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
        END;
      END;
    | Code.NewConst =>
      (*** AddValObj creates the actual Obliq object ***)
      WITH r   = ObMatrix4.GetArg (args, 1, self, opCode, loc),
           val = TransformProp.NewConst (r) DO
        RETURN val.proxy.obj;
      END;
    | Code.NewSync =>
      WITH ah  = ObAnimHandle.GetT (args, 1, self, opCode, loc),
           r   = ObMatrix4.GetArg (args, 2, self, opCode, loc),
           val = TransformProp.NewSync (ah, r) DO
        RETURN val.proxy.obj;
      END;
    | Code.NewAsync =>
      WITH beh = NEW (TransformProp.AsyncBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        WITH val  = TransformProp.NewAsync (beh) DO
          RETURN val.proxy.obj;
        END;
      END;
    | Code.NewDep =>
      WITH beh = NEW (TransformProp.DepBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        WITH val = TransformProp.NewDep (beh) DO
          RETURN val.proxy.obj;
        END;
      END;
    | Code.ConstBehSet =>
      WITH beh = GetConstBeh (args, 1, self, opCode, loc),
           r   = ObMatrix4.GetArg (args, 2, self, opCode, loc) DO
        beh.set (r);
        RETURN ObValue.valOk;
      END;
    | Code.ConstBehCompose =>
      WITH beh = GetConstBeh (args, 1, self, opCode, loc),
           r   = ObMatrix4.GetArg (args, 2, self, opCode, loc) DO
        beh.compose (r);
        RETURN ObValue.valOk;
      END;
    | Code.ConstBehReset =>
      WITH beh = GetConstBeh (args, 1, self, opCode, loc) DO
        beh.reset ();
        RETURN ObValue.valOk;
      END;
    | Code.ConstBehTranslate =>
      WITH beh = GetConstBeh   (args, 1, self, opCode, loc),
           x   = ObReal.GetArg (args, 2, self, opCode, loc),
           y   = ObReal.GetArg (args, 3, self, opCode, loc),
           z   = ObReal.GetArg (args, 4, self, opCode, loc) DO
        beh.translate (x, y, z);
        RETURN ObValue.valOk;
      END;
    | Code.ConstBehScale =>
      WITH beh = GetConstBeh   (args, 1, self, opCode, loc),
           x   = ObReal.GetArg (args, 2, self, opCode, loc),
           y   = ObReal.GetArg (args, 3, self, opCode, loc),
           z   = ObReal.GetArg (args, 4, self, opCode, loc) DO
        beh.scale (x, y, z);
        RETURN ObValue.valOk;
      END;
    | Code.ConstBehRotateX =>
      WITH beh = GetConstBeh   (args, 1, self, opCode, loc),
           a   = ObReal.GetArg (args, 2, self, opCode, loc) DO
        beh.rotateX (a);
        RETURN ObValue.valOk;
      END;
    | Code.ConstBehRotateY =>
      WITH beh = GetConstBeh   (args, 1, self, opCode, loc),
           a   = ObReal.GetArg (args, 2, self, opCode, loc) DO
        beh.rotateY (a);
        RETURN ObValue.valOk;
      END;
    | Code.ConstBehRotateZ =>
      WITH beh = GetConstBeh   (args, 1, self, opCode, loc),
           a   = ObReal.GetArg (args, 2, self, opCode, loc) DO
        beh.rotateZ (a);
        RETURN ObValue.valOk;
      END;
    | Code.NewConstBeh =>
      WITH m   = ObMatrix4.GetArg (args, 1, self, opCode, loc),
           beh = NEW (TransformProp.ConstBeh).init (m) DO
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
    | Code.SyncBehReset =>
      WITH beh   = GetSyncBeh    (args, 1, self, opCode, loc),
           start = ObReal.GetArg (args, 2, self, opCode, loc) DO
        TRY
          beh.reset (start);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.SyncBehChangeTo =>
      WITH beh   = GetSyncBeh       (args, 1, self, opCode, loc),
           m     = ObMatrix4.GetArg (args, 2, self, opCode, loc),
           start = ObReal.GetArg    (args, 3, self, opCode, loc),
           dur   = ObReal.GetArg    (args, 4, self, opCode, loc) DO
        TRY
          beh.changeTo (m, start, dur);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.SyncBehTranslate =>
      WITH beh   = GetSyncBeh    (args, 1, self, opCode, loc),
           x     = ObReal.GetArg (args, 2, self, opCode, loc),
           y     = ObReal.GetArg (args, 3, self, opCode, loc),
           z     = ObReal.GetArg (args, 4, self, opCode, loc),
           start = ObReal.GetArg (args, 5, self, opCode, loc),
           dur   = ObReal.GetArg (args, 6, self, opCode, loc) DO
        TRY
          beh.translate (x, y, z, start, dur);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.SyncBehScale =>
      WITH beh   = GetSyncBeh    (args, 1, self, opCode, loc),
           x     = ObReal.GetArg (args, 2, self, opCode, loc),
           y     = ObReal.GetArg (args, 3, self, opCode, loc),
           z     = ObReal.GetArg (args, 4, self, opCode, loc),
           start = ObReal.GetArg (args, 5, self, opCode, loc),
           dur   = ObReal.GetArg (args, 6, self, opCode, loc) DO
        TRY
          beh.scale (x, y, z, start, dur);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.SyncBehRotateX =>
      WITH beh   = GetSyncBeh    (args, 1, self, opCode, loc),
           a     = ObReal.GetArg (args, 2, self, opCode, loc),
           start = ObReal.GetArg (args, 3, self, opCode, loc),
           dur   = ObReal.GetArg (args, 4, self, opCode, loc) DO
        TRY
          beh.rotateX (a, start, dur);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.SyncBehRotateY =>
      WITH beh   = GetSyncBeh    (args, 1, self, opCode, loc),
           a     = ObReal.GetArg (args, 2, self, opCode, loc),
           start = ObReal.GetArg (args, 3, self, opCode, loc),
           dur   = ObReal.GetArg (args, 4, self, opCode, loc) DO
        TRY
          beh.rotateY (a, start, dur);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.SyncBehRotateZ =>
      WITH beh   = GetSyncBeh    (args, 1, self, opCode, loc),
           a     = ObReal.GetArg (args, 2, self, opCode, loc),
           start = ObReal.GetArg (args, 3, self, opCode, loc),
           dur   = ObReal.GetArg (args, 4, self, opCode, loc) DO
        TRY
          beh.rotateZ (a, start, dur);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.NewSyncBeh =>
      WITH ah  = ObAnimHandle.GetT (args, 1, self, opCode, loc),
           m   = ObMatrix4.GetArg (args, 2, self, opCode, loc),
           beh = NEW (TransformProp.SyncBeh).init (ah, m) DO
        RETURN beh.proxy.obj;
      END;
    | Code.NewAsyncBeh =>
      WITH beh = NEW (TransformProp.AsyncBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        RETURN obj;
      END;
    | Code.NewDepBeh =>
      WITH beh   = NEW (TransformProp.DepBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        RETURN obj;
      END;
    | Code.NewRequest =>
      WITH start = ObReal.GetArg (args, 1, self, opCode, loc),
           dur   = ObReal.GetArg (args, 2, self, opCode, loc),
           req   = NEW (TransformProp.Request).init (start, dur),
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
END ObTransformProp.
