(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 21:59:08 PDT 1994 by najork                   *)
(*       Created on Sat May 28 11:15:36 PDT 1994 by najork                   *)


MODULE ObPointProp;

IMPORT ObAnimHandle, ObAux, ObCommand, ObLib, ObLongReal, ObPoint3, ObProp, 
       ObProtoLoader, ObReal, ObValue, Obliq, Point3, PointProp, 
       PointPropProxy, Prop, ProxiedObj, SynLocation;

(*****************************************************************************)
(* Wrapper for PointProp.Name                                                *)
(*****************************************************************************)

TYPE 
  Name = ObProp.Name BRANDED "ObPointProp.Name" OBJECT END;


PROCEDURE AddNameObj (pn : PointProp.Name) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {NameProto}),
         raw = NEW (Name, what := "<a PointProp.Name>", po := pn) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      pn.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddNameObj;


PROCEDURE GetName (args    : ObValue.ArgArray; 
                   idx     : INTEGER; 
                   package : ObLib.T; 
                   opCode  : ObLib.OpCode; 
                   loc     : SynLocation.T) : PointProp.Name 
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
(* Wrapper for PointProp.Val                                                 *)
(*****************************************************************************)


TYPE 
  Val = ObProp.Val BRANDED "ObPointProp.Val" OBJECT END;


PROCEDURE AddValObj (pv : PointProp.Val) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {ValProto}),
         raw = NEW (Val, what := "<a PointProp.Val>", po := pv) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      pv.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddValObj;


PROCEDURE GetVal (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : PointProp.Val 
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
                            loc     : SynLocation.T) : PointProp.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    vals : ARRAY [0 .. 2] OF Obliq.Val;
  BEGIN
    TYPECASE args[idx] OF 
    | ObValue.ValArray =>
      TRY
        Obliq.ToArray (args[idx], vals);
        WITH x = ObReal.ObliqToM3 (vals[0]),
             y = ObReal.ObliqToM3 (vals[1]),
             z = ObReal.ObliqToM3 (vals[2]),
             p = Point3.T {x, y, z} DO
          RETURN PointProp.NewConst (p);
        END;
      EXCEPT
      | ObValue.Error =>
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
        RETURN NIL;   (* ... only to suppress compiler warning *)
      END;
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


PROCEDURE ObliqToM3 (val : Obliq.Val) : PointProp.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    vals : ARRAY [0 .. 2] OF Obliq.Val;
  BEGIN
    TYPECASE val OF 
    | ObValue.ValArray =>
      Obliq.ToArray (val, vals);
      WITH x = ObReal.ObliqToM3 (vals[0]),
           y = ObReal.ObliqToM3 (vals[1]),
           z = ObReal.ObliqToM3 (vals[2]),
           p = Point3.T {x, y, z} DO
        RETURN PointProp.NewConst (p);
      END;
    | ObValue.ValObj =>
      WITH raw = Obliq.ObjectSelect (val, "raw") DO
        TYPECASE raw OF 
        | Val (node) => 
          RETURN node.po;
        ELSE 
          Obliq.RaiseError ("Expected (Point3 + PointPropVal)");
          RETURN NIL;      (* ... only to suppress compiler warning *)
        END;
      END;
    ELSE
      Obliq.RaiseError ("Expected (Point3 + PointPropVal)");
      RETURN NIL;   (* ... only to suppress compiler warning *)
    END;
  END ObliqToM3;


(*****************************************************************************)
(* Wrapper for PointProp.Beh                                                 *)
(*****************************************************************************)

TYPE 
  Beh = ObProp.Beh BRANDED "ObPointProp.Beh" OBJECT END;


PROCEDURE GetBeh (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : PointProp.Beh 
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
(* Wrapper for PointProp.ConstBeh                                            *)
(*****************************************************************************)


TYPE 
  ConstBeh = Beh BRANDED "ObPointProp.ConstBeh" OBJECT END;


PROCEDURE AddConstBehObj (beh : PointProp.ConstBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {ConstBehProto}),
         raw = NEW (ConstBeh, what := "<a PointProp.ConstBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddConstBehObj;


PROCEDURE GetConstBeh (args    : ObValue.ArgArray; 
                       idx     : INTEGER; 
                       package : ObLib.T; 
                       opCode  : ObLib.OpCode; 
                       loc     : SynLocation.T) : PointProp.ConstBeh 
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
(* Wrapper for PointProp.SyncBeh                                             *)
(*****************************************************************************)


TYPE 
  SyncBeh = Beh BRANDED "ObPointProp.SyncBeh" OBJECT END;


PROCEDURE AddSyncBehObj (beh : PointProp.SyncBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {SyncBehProto}),
         raw = NEW (SyncBeh, what := "<a PointProp.SyncBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddSyncBehObj;


PROCEDURE GetSyncBeh (args    : ObValue.ArgArray; 
                       idx     : INTEGER; 
                       package : ObLib.T; 
                       opCode  : ObLib.OpCode; 
                       loc     : SynLocation.T) : PointProp.SyncBeh 
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
(* Wrapper for PointProp.AsyncBeh                                            *)
(*****************************************************************************)


TYPE 
  AsyncBeh = Beh BRANDED "ObPointProp.AsyncBeh" OBJECT END;


PROCEDURE AddAsyncBehObj (beh : PointProp.AsyncBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {AsyncBehProto}),
         raw = NEW (AsyncBeh, what := "<a PointProp.AsyncBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (AsyncBehProxy, obj := obj);
    END;
  END AddAsyncBehObj;


TYPE
  AsyncBehProxy = PointPropProxy.AsyncBehProxy BRANDED OBJECT
  OVERRIDES
    compute := AsyncBehCompute;
  END;


PROCEDURE AsyncBehCompute (proxy : AsyncBehProxy; time : LONGREAL) : Point3.T 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {Obliq.NewReal (time)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "compute", args) DO
        RETURN ObPoint3.ObliqToM3 (res);
      END;
    EXCEPT
    | ObValue.Error (packet) =>
      RAISE Prop.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE Prop.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END AsyncBehCompute;


(*****************************************************************************)
(* Wrapper for PointProp.DepBeh                                              *)
(*****************************************************************************)


TYPE 
  DepBeh = Beh BRANDED "ObPointProp.DepBeh" OBJECT END;


PROCEDURE AddDepBehObj (beh : PointProp.DepBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {DepBehProto}),
         raw = NEW (DepBeh, what := "<a PointProp.DepBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (DepBehProxy, obj := obj);
    END;
  END AddDepBehObj;


TYPE
  DepBehProxy = PointPropProxy.DepBehProxy BRANDED OBJECT
  OVERRIDES
    compute := DepBehCompute;
  END;


PROCEDURE DepBehCompute (proxy : DepBehProxy; time : LONGREAL) : Point3.T
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {Obliq.NewReal (time)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "compute", args) DO
        RETURN ObPoint3.ObliqToM3 (res);
      END;
    EXCEPT
    | ObValue.Error (packet) =>
      RAISE Prop.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE Prop.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END DepBehCompute;


(*****************************************************************************)
(* Wrapper for PointProp.Request                                             *)
(*****************************************************************************)


TYPE
  Request = ObProp.Request BRANDED "ObPointProp.Request" OBJECT END;


PROCEDURE AddRequestObj (req : PointProp.Request) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {RequestProto}),
         raw = NEW (Request, what := "<a PointProp.Request>", po := req) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      req.proxy := NEW (RequestProxy, obj := obj);
    END;
  END AddRequestObj;


PROCEDURE GetRequest (args    : ObValue.ArgArray; 
                      idx     : INTEGER; 
                      package : ObLib.T; 
                      opCode  : ObLib.OpCode; 
                      loc     : SynLocation.T) : PointProp.Request 
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
  RequestProxy = PointPropProxy.RequestProxy BRANDED OBJECT
  OVERRIDES
    value := RequestValue;
  END;


PROCEDURE RequestValue (proxy    : RequestProxy; 
                        startval : Point3.T; 
                        reltime  : REAL) : Point3.T 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {ObPoint3.M3ToObliq (startval),
                              ObReal.M3ToObliq (reltime)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "value", args) DO
        RETURN ObPoint3.ObliqToM3 (res);
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
            NewOpCode ("SyncBehLinMoveTo",   4, Code.SyncBehLinMoveTo),
            NewOpCode ("SyncBehLinMoveBy",   4, Code.SyncBehLinMoveBy),
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
    loader.load ("PointProp.obl");
    NameProto     := loader.get ("PointProp_NameProto");
    ValProto      := loader.get ("PointProp_ValProto");
    ConstBehProto := loader.get ("PointProp_ConstBehProto");
    SyncBehProto  := loader.get ("PointProp_SyncBehProto");
    AsyncBehProto := loader.get ("PointProp_AsyncBehProto");
    DepBehProto   := loader.get ("PointProp_DepBehProto");
    RequestProto  := loader.get ("PointProp_RequestProto");

    (*** Register the proxy makers ***)
    PointPropProxy.NamePM     := AddNameObj;
    PointPropProxy.ValPM      := AddValObj;
    PointPropProxy.ConstBehPM := AddConstBehObj;
    PointPropProxy.SyncBehPM  := AddSyncBehObj;
    PointPropProxy.AsyncBehPM := AddAsyncBehObj;
    PointPropProxy.DepBehPM   := AddDepBehObj;
    PointPropProxy.RequestPM  := AddRequestObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {NameBind, 
          ValGetBeh, ValSetBeh, ValGet, ValValue, 
             NewConst, NewSync, NewAsync, NewDep,
          ConstBehSet, NewConstBeh, 
          SyncBehAddRequest, SyncBehLinMoveTo, SyncBehLinMoveBy, 
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
  pkgname = "PointProp";


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
          RETURN ObPoint3.M3ToObliq (pv.get ());
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
          RETURN ObPoint3.M3ToObliq (pv.value (time));
        EXCEPT
        | Prop.BadMethod =>
          ObValue.RaiseException (ObProp.BadMethod, opCode.name, loc);
          RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
        END;
      END;
    | Code.NewConst =>
      (*** AddValObj creates the actual Obliq object ***)
      WITH r   = ObPoint3.GetArg (args, 1, self, opCode, loc),
           val = PointProp.NewConst (r) DO
        RETURN val.proxy.obj;
      END;
    | Code.NewSync =>
      WITH ah  = ObAnimHandle.GetT (args, 1, self, opCode, loc),
           r   = ObPoint3.GetArg (args, 2, self, opCode, loc),
           val = PointProp.NewSync (ah, r) DO
        RETURN val.proxy.obj;
      END;
    | Code.NewAsync =>
      WITH beh = NEW (PointProp.AsyncBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        WITH val = PointProp.NewAsync (beh) DO
          RETURN val.proxy.obj;
        END;
      END;
    | Code.NewDep =>
      WITH beh = NEW (PointProp.DepBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        WITH val = PointProp.NewDep (beh) DO
          RETURN val.proxy.obj;
        END;
      END;
    | Code.ConstBehSet =>
      WITH beh = GetConstBeh (args, 1, self, opCode, loc),
           r   = ObPoint3.GetArg (args, 2, self, opCode, loc) DO
        beh.set (r);
        RETURN ObValue.valOk;
      END;
    | Code.NewConstBeh =>
      WITH r   = ObPoint3.GetArg (args, 1, self, opCode, loc),
           beh = NEW (PointProp.ConstBeh).init (r) DO
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
    | Code.SyncBehLinMoveTo =>
      WITH beh   = GetSyncBeh (args, 1, self, opCode, loc),
           r     = ObPoint3.GetArg (args, 2, self, opCode, loc),
           start = ObReal.GetArg (args, 3, self, opCode, loc),
           dur   = ObReal.GetArg (args, 4, self, opCode, loc) DO
        TRY
          beh.linMoveTo (r, start, dur);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.SyncBehLinMoveBy =>
      WITH beh   = GetSyncBeh (args, 1, self, opCode, loc),
           r     = ObPoint3.GetArg (args, 2, self, opCode, loc),
           start = ObReal.GetArg (args, 3, self, opCode, loc),
           dur   = ObReal.GetArg (args, 4, self, opCode, loc) DO
        TRY
          beh.linMoveBy (r, start, dur);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.NewSyncBeh =>
      WITH ah  = ObAnimHandle.GetT (args, 1, self, opCode, loc),
           r   = ObPoint3.GetArg (args, 2, self, opCode, loc),
           beh = NEW (PointProp.SyncBeh).init (ah, r) DO
        RETURN beh.proxy.obj;
      END;
    | Code.NewAsyncBeh =>
      WITH beh = NEW (PointProp.AsyncBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        RETURN obj;
      END;
    | Code.NewDepBeh =>
      WITH beh = NEW (PointProp.DepBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        RETURN obj;
      END;
    | Code.NewRequest =>
      WITH start = ObReal.GetArg (args, 1, self, opCode, loc),
           dur   = ObReal.GetArg (args, 2, self, opCode, loc),
           req   = NEW (PointProp.Request).init (start, dur),
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
END ObPointProp.
