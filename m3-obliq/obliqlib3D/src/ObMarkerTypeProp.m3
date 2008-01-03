(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep 26 22:09:18 PDT 1994 by najork                   *)
(*       Created on Mon May 30 09:59:32 PDT 1994 by najork                   *)


MODULE ObMarkerTypeProp;

IMPORT MarkerTypeProp, MarkerTypePropProxy, ObAnimHandle, ObAux, ObCommand, 
       ObLib, ObLongReal, ObProp, ObProtoLoader, ObReal, ObValue, Obliq, Prop,
       ProxiedObj, SynLocation, Text;


(*****************************************************************************)
(* Wrapper for MarkerTypeProp.Kind                                           *)
(*****************************************************************************)


PROCEDURE NewKind (kind : MarkerTypeProp.Kind) : ObValue.Val =
  BEGIN
    CASE kind OF
    | MarkerTypeProp.Kind.Dot      => RETURN Obliq.NewText ("Dot");
    | MarkerTypeProp.Kind.Cross    => RETURN Obliq.NewText ("Cross");
    | MarkerTypeProp.Kind.Asterisk => RETURN Obliq.NewText ("Asterisk");
    | MarkerTypeProp.Kind.Circle   => RETURN Obliq.NewText ("Circle");
    | MarkerTypeProp.Kind.X        => RETURN Obliq.NewText ("X");
    END;
  END NewKind;


EXCEPTION BadKind(TEXT);


PROCEDURE KindToM3 (val : ObValue.Val) : MarkerTypeProp.Kind RAISES {BadKind} =
  BEGIN
    TYPECASE val OF 
    | ObValue.ValText (node) => 
      IF Text.Equal (node.text, "Dot") THEN
        RETURN MarkerTypeProp.Kind.Dot;
      ELSIF Text.Equal (node.text, "Cross") THEN
        RETURN MarkerTypeProp.Kind.Cross;
      ELSIF Text.Equal (node.text, "Asterisk") THEN
        RETURN MarkerTypeProp.Kind.Asterisk;
      ELSIF Text.Equal (node.text, "Circle") THEN
        RETURN MarkerTypeProp.Kind.Circle;
      ELSIF Text.Equal (node.text, "X") THEN
        RETURN MarkerTypeProp.Kind.X;
      ELSE
        RAISE BadKind ("Unknown MarkerType");
      END;
    ELSE 
      RAISE BadKind ("MarkerType must be a Text");
    END;
  END KindToM3;


PROCEDURE GetKind (args    : ObValue.ArgArray; 
                   idx     : INTEGER; 
                   package : ObLib.T; 
                   opCode  : ObLib.OpCode; 
                   loc     : SynLocation.T) : MarkerTypeProp.Kind 
    RAISES {ObValue.Error} =
  BEGIN
    TRY
      RETURN KindToM3 (args[idx]);
    EXCEPT
    | BadKind =>
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
      RETURN MarkerTypeProp.Kind.Dot; (* only to suppress compiler warning *)
    END;
  END GetKind;


(*****************************************************************************)
(* Wrapper for MarkerTypeProp.Name                                           *)
(*****************************************************************************)

TYPE 
  Name = ObProp.Name BRANDED "ObMarkerTypeProp.Name" OBJECT END;


PROCEDURE AddNameObj (pn : MarkerTypeProp.Name) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {NameProto}),
         raw = NEW (Name, what := "<a MarkerTypeProp.Name>", po := pn) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      pn.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddNameObj;


PROCEDURE GetName (args    : ObValue.ArgArray; 
                   idx     : INTEGER; 
                   package : ObLib.T; 
                   opCode  : ObLib.OpCode; 
                   loc     : SynLocation.T) : MarkerTypeProp.Name 
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
(* Wrapper for MarkerTypeProp.Val                                            *)
(*****************************************************************************)


TYPE 
  Val = ObProp.Val BRANDED "ObMarkerTypeProp.Val" OBJECT END;


PROCEDURE AddValObj (pv : MarkerTypeProp.Val) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {ValProto}),
         raw = NEW (Val, what := "<a MarkerTypeProp.Val>", po := pv) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      pv.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddValObj;


PROCEDURE GetVal (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : MarkerTypeProp.Val 
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
                            loc     : SynLocation.T) : MarkerTypeProp.Val 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    TYPECASE args[idx] OF 
    | ObValue.ValText (node) => 
      IF Text.Equal (node.text, "Dot") THEN
        RETURN MarkerTypeProp.NewConst (MarkerTypeProp.Kind.Dot);
      ELSIF Text.Equal (node.text, "Cross") THEN
        RETURN MarkerTypeProp.NewConst (MarkerTypeProp.Kind.Cross);
      ELSIF Text.Equal (node.text, "Asterisk") THEN
        RETURN MarkerTypeProp.NewConst (MarkerTypeProp.Kind.Asterisk);
      ELSIF Text.Equal (node.text, "Circle") THEN
        RETURN MarkerTypeProp.NewConst (MarkerTypeProp.Kind.Circle);
      ELSIF Text.Equal (node.text, "X") THEN
        RETURN MarkerTypeProp.NewConst (MarkerTypeProp.Kind.X);
      ELSE
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
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


(*****************************************************************************)
(* Wrapper for MarkerTypeProp.Beh                                            *)
(*****************************************************************************)

TYPE 
  Beh = ObProp.Beh BRANDED "ObMarkerTypeProp.Beh" OBJECT END;


PROCEDURE GetBeh (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : MarkerTypeProp.Beh 
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
(* Wrapper for MarkerTypeProp.ConstBeh                                       *)
(*****************************************************************************)


TYPE 
  ConstBeh = Beh BRANDED "ObMarkerTypeProp.ConstBeh" OBJECT END;


PROCEDURE AddConstBehObj (beh : MarkerTypeProp.ConstBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {ConstBehProto}),
         raw = NEW (ConstBeh, what := "<a MarkerTypeProp.ConstBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddConstBehObj;


PROCEDURE GetConstBeh (args    : ObValue.ArgArray; 
                       idx     : INTEGER; 
                       package : ObLib.T; 
                       opCode  : ObLib.OpCode; 
                       loc     : SynLocation.T) : MarkerTypeProp.ConstBeh 
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
(* Wrapper for MarkerTypeProp.SyncBeh                                        *)
(*****************************************************************************)


TYPE 
  SyncBeh = Beh BRANDED "ObMarkerTypeProp.SyncBeh" OBJECT END;


PROCEDURE AddSyncBehObj (beh : MarkerTypeProp.SyncBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {SyncBehProto}),
         raw = NEW (SyncBeh, what := "<a MarkerTypeProp.SyncBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (ProxiedObj.Proxy, obj := obj);
    END;
  END AddSyncBehObj;


PROCEDURE GetSyncBeh (args    : ObValue.ArgArray; 
                       idx     : INTEGER; 
                       package : ObLib.T; 
                       opCode  : ObLib.OpCode; 
                       loc     : SynLocation.T) : MarkerTypeProp.SyncBeh 
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
(* Wrapper for MarkerTypeProp.AsyncBeh                                       *)
(*****************************************************************************)


TYPE 
  AsyncBeh = Beh BRANDED "ObMarkerTypeProp.AsyncBeh" OBJECT END;


PROCEDURE AddAsyncBehObj (beh : MarkerTypeProp.AsyncBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {AsyncBehProto}),
         raw = NEW (AsyncBeh, what := "<a MarkerTypeProp.AsyncBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (AsyncBehProxy, obj := obj);
    END;
  END AddAsyncBehObj;


TYPE
  AsyncBehProxy = MarkerTypePropProxy.AsyncBehProxy BRANDED OBJECT
  OVERRIDES
    compute := AsyncBehCompute;
  END;


PROCEDURE AsyncBehCompute (proxy : AsyncBehProxy; 
                           time  : LONGREAL) : MarkerTypeProp.Kind 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {Obliq.NewReal (time)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "compute", args) DO
        RETURN KindToM3 (res);
      END;
    EXCEPT
    | BadKind (msg) => 
      RAISE Prop.BadMethod (msg);
    | ObValue.Error (packet) =>
      RAISE Prop.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE Prop.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END AsyncBehCompute;


(*****************************************************************************)
(* Wrapper for MarkerTypeProp.DepBeh                                         *)
(*****************************************************************************)


TYPE 
  DepBeh = Beh BRANDED "ObMarkerTypeProp.DepBeh" OBJECT END;


PROCEDURE AddDepBehObj (beh : MarkerTypeProp.DepBeh) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {DepBehProto}),
         raw = NEW (DepBeh, what := "<a MarkerTypeProp.DepBeh>", po := beh) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      beh.proxy := NEW (DepBehProxy, obj := obj);
    END;
  END AddDepBehObj;


TYPE
  DepBehProxy = MarkerTypePropProxy.DepBehProxy BRANDED OBJECT
  OVERRIDES
    compute := DepBehCompute;
  END;


PROCEDURE DepBehCompute (proxy : DepBehProxy; 
                         time  : LONGREAL) : MarkerTypeProp.Kind 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {Obliq.NewReal (time)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "compute", args) DO
        RETURN KindToM3 (res);
      END;
    EXCEPT
    | BadKind (msg) => 
      RAISE Prop.BadMethod (msg);
    | ObValue.Error (packet) =>
      RAISE Prop.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE Prop.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END DepBehCompute;


(*****************************************************************************)
(* Wrapper for MarkerTypeProp.Request                                        *)
(*****************************************************************************)


TYPE
  Request = ObProp.Request BRANDED "ObMarkerTypeProp.Return" OBJECT END;


PROCEDURE AddRequestObj (req : MarkerTypeProp.Request) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {RequestProto}),
         raw = NEW (Request, what := "<a MarkerTypeProp.Request>", po := req) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      req.proxy := NEW (RequestProxy, obj := obj);
    END;
  END AddRequestObj;


PROCEDURE GetRequest (args    : ObValue.ArgArray; 
                      idx     : INTEGER; 
                      package : ObLib.T; 
                      opCode  : ObLib.OpCode; 
                      loc     : SynLocation.T) : MarkerTypeProp.Request 
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
  RequestProxy = MarkerTypePropProxy.RequestProxy BRANDED OBJECT
  OVERRIDES
    value := RequestValue;
  END;


PROCEDURE RequestValue (proxy    : RequestProxy; 
                        startval : MarkerTypeProp.Kind; 
                        reltime  : REAL) : MarkerTypeProp.Kind 
    RAISES {Prop.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {NewKind (startval), ObReal.M3ToObliq (reltime)},
           obj  = NARROW (proxy.obj, Obliq.Val),
           res  = Obliq.ObjectInvoke (obj, "value", args) DO
        RETURN KindToM3 (res);
      END;
    EXCEPT
    | BadKind (msg) => 
      RAISE Prop.BadMethod (msg);
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
    loader.load ("MarkerTypeProp.obl");
    NameProto     := loader.get ("MarkerTypeProp_NameProto");
    ValProto      := loader.get ("MarkerTypeProp_ValProto");
    ConstBehProto := loader.get ("MarkerTypeProp_ConstBehProto");
    SyncBehProto  := loader.get ("MarkerTypeProp_SyncBehProto");
    AsyncBehProto := loader.get ("MarkerTypeProp_AsyncBehProto");
    DepBehProto   := loader.get ("MarkerTypeProp_DepBehProto");
    RequestProto  := loader.get ("MarkerTypeProp_RequestProto");

    (*** Register the proxy makers ***)
    MarkerTypePropProxy.NamePM     := AddNameObj;
    MarkerTypePropProxy.ValPM      := AddValObj;
    MarkerTypePropProxy.ConstBehPM := AddConstBehObj;
    MarkerTypePropProxy.SyncBehPM  := AddSyncBehObj;
    MarkerTypePropProxy.AsyncBehPM := AddAsyncBehObj;
    MarkerTypePropProxy.DepBehPM   := AddDepBehObj;
    MarkerTypePropProxy.RequestPM  := AddRequestObj;
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
  pkgname = "MarkerTypeProp";


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
          RETURN NewKind (pv.get ());
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
          RETURN NewKind (pv.value (time));
        EXCEPT
        | Prop.BadMethod =>
          ObValue.RaiseException (ObProp.BadMethod, opCode.name, loc);
          RETURN ObValue.valOk;   (* ... only to suppress compiler warning *)
        END;
      END;
    | Code.NewConst =>
      (*** AddValObj creates the actual Obliq object ***)
      WITH lt  = GetKind (args, 1, self, opCode, loc),
           val = MarkerTypeProp.NewConst (lt) DO
        RETURN val.proxy.obj;
      END;
    | Code.NewSync =>
      WITH ah  = ObAnimHandle.GetT (args, 1, self, opCode, loc),
           lt  = GetKind (args, 2, self, opCode, loc),
           val = MarkerTypeProp.NewSync (ah, lt) DO
        RETURN val.proxy.obj;
      END;
    | Code.NewAsync =>
      WITH beh = NEW (MarkerTypeProp.AsyncBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        WITH val = MarkerTypeProp.NewAsync (beh) DO
          RETURN val.proxy.obj;
        END;
      END;
    | Code.NewDep =>
      WITH beh = NEW (MarkerTypeProp.DepBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        WITH val = MarkerTypeProp.NewDep (beh) DO
          RETURN val.proxy.obj;
        END;
      END;
    | Code.ConstBehSet =>
      WITH beh = GetConstBeh (args, 1, self, opCode, loc),
           lt  = GetKind     (args, 2, self, opCode, loc) DO
        beh.set (lt);
        RETURN ObValue.valOk;
      END;
    | Code.NewConstBeh =>
      WITH lt    = GetKind (args, 1, self, opCode, loc),
           beh   = NEW (MarkerTypeProp.ConstBeh).init (lt) DO
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
      WITH beh   = GetSyncBeh    (args, 1, self, opCode, loc),
           lt    = GetKind       (args, 2, self, opCode, loc),
           start = ObReal.GetArg (args, 3, self, opCode, loc) DO
        TRY
          beh.change (lt, start);
        EXCEPT
          Prop.BadInterval => 
          ObValue.RaiseException (ObProp.BadInterval, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
      END;
    | Code.NewSyncBeh =>
      WITH ah    = ObAnimHandle.GetT (args, 1, self, opCode, loc),
           lt    = GetKind           (args, 2, self, opCode, loc),
           beh   = NEW (MarkerTypeProp.SyncBeh).init (ah, lt) DO
        RETURN beh.proxy.obj;
      END;
    | Code.NewAsyncBeh =>
      WITH beh = NEW (MarkerTypeProp.AsyncBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        RETURN obj;
      END;
    | Code.NewDepBeh =>
      WITH beh   = NEW (MarkerTypeProp.DepBeh).init (),
           obj = NARROW (beh.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "compute", args[1]);
        RETURN obj;
      END;
    | Code.NewRequest =>
      WITH start = ObReal.GetArg (args, 1, self, opCode, loc),
           dur   = ObReal.GetArg (args, 2, self, opCode, loc),
           req   = NEW (MarkerTypeProp.Request).init (start, dur),
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
END ObMarkerTypeProp.
