MODULE ObRefShSeqCB;

IMPORT ObCommand, ObLib, ObLoader, ObValue, Obliq, ObEmbProxiedObj,
       SynLocation;
IMPORT RefShSeq, RefShSeqCB, ObRefShSeq, RefShSeqProxy;

(*******************************************************************)
(* Wrapper for RefShSeqCB.T                                        *)
(*******************************************************************)

TYPE CBT = ObEmbProxiedObj.T BRANDED "ObRefShSeqCB.T" OBJECT END; 

PROCEDURE AddTObj (pn : RefShSeqCB.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    IF pn.proxy # NIL THEN RETURN END;
    WITH raw = NEW (CBT, what := "<a RefShSeqCB.T>", po := pn),
         args = Obliq.Vals {ObValue.valOk, raw},
         obj = Obliq.ObjectInvoke (TProto, "extend", args) DO
      pn.proxy := NEW (CallbackProxy, obj := obj);
    END;
  END AddTObj;

TYPE
  CallbackProxy = RefShSeqProxy.CallbackProxy BRANDED OBJECT
  OVERRIDES
    pre_init := pre_Init;
    pre_fromArray := pre_FromArray;
    pre_addhi := pre_Addhi;
    pre_addlo := pre_Addlo;
    pre_remhi := pre_Remhi;
    pre_remlo := pre_Remlo;
    pre_put := pre_Put;
    pre_size := pre_Size;
    pre_gethi := pre_Gethi;
    pre_getlo := pre_Getlo;
    pre_get := pre_Get;
    pre_cat := pre_Cat;
    pre_sub := pre_Sub;
    pre_anyChange := Pre_anyChange;

    post_init := post_Init;
    post_fromArray := post_FromArray;
    post_addhi := post_Addhi;
    post_addlo := post_Addlo;
    post_remhi := post_Remhi;
    post_remlo := post_Remlo;
    post_put := post_Put;
    post_size := post_Size;
    post_gethi := post_Gethi;
    post_getlo := post_Getlo;
    post_get := post_Get;
    post_cat := post_Cat;
    post_sub := post_Sub;
    post_anyChange := Post_anyChange;
  END;

PROCEDURE Pre_init(self: CallbackProxy; 
                   READONLY obj: RefShSeq.T; 
                   READONLY sizeHint: CARDINAL := 5): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Pre_init;

PROCEDURE Pre_fromArray(self: CallbackProxy; 
                        READONLY obj: RefShSeq.T; 
                        READONLY a: ARRAY OF Refany.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`fromArray") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Pre_fromArray;

PROCEDURE Pre_addhi(self: CallbackProxy; 
                    READONLY obj: RefShSeq.T; READONLY x: Refany.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Pre_addhi;

PROCEDURE Pre_addlo(self: CallbackProxy; 
                    READONLY obj: RefShSeq.T; READONLY x: Refany.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Pre_addlo;

PROCEDURE Pre_remhi(self: CallbackProxy; 
                    READONLY obj: RefShSeq.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Pre_remhi;

PROCEDURE Pre_remlo(self: CallbackProxy; 
                    READONLY obj: RefShSeq.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Pre_remlo;

PROCEDURE Pre_put(self: CallbackProxy; 
                  READONLY obj: RefShSeq.T; READONLY i: CARDINAL; 
                  READONLY x: Refany.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Pre_put;

PROCEDURE Post_init(self: CallbackProxy; 
                    READONLY obj: RefShSeq.T; 
                    READONLY sizeHint: CARDINAL := 5): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Post_init;

PROCEDURE Post_fromArray(self: CallbackProxy; 
                         READONLY obj: RefShSeq.T; 
                         READONLY a: ARRAY OF Refany.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Post_fromArray;

PROCEDURE Post_addhi(self: CallbackProxy; 
                     READONLY obj: RefShSeq.T; READONLY x: Refany.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Post_addhi;

PROCEDURE Post_addlo(self: CallbackProxy; 
                     READONLY obj: RefShSeq.T; READONLY x: Refany.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Post_addlo;

PROCEDURE Post_remhi(self: CallbackProxy; 
                     READONLY obj: RefShSeq.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Post_remhi;

PROCEDURE Post_remlo(self: CallbackProxy; 
                     READONLY obj: RefShSeq.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Post_remlo;

PROCEDURE Post_put(self: CallbackProxy; 
                   READONLY obj: RefShSeq.T; READONLY i: CARDINAL; 
                   READONLY x: Refany.T): BOOLEAN =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF NOT Obliq.ObjectHas(obliqCallbackObj, "pre`init") THEN
        RETURN FALSE;
      END;
      WITH args = Obliq.Vals {obliqSharedObj, Obliq.NewInt(sizeHint)} DO
        RETURN NARROW(Obliq.ObjectInvoke (obliqCallbackObj, "pre`init", args), 
                      ObValue.ValBool).bool;
      END;
    END;
  END Post_put;

PROCEDURE Pre_anyChange (self : CallbackProxy; 
                         READONLY obj: RefShSeq.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF Obliq.ObjectHas(obliqCallbackObj, "pre`anyChange") THEN
        WITH args = Obliq.Vals {obliqSharedObj} DO
          EVAL NARROW(Obliq.ObjectInvoke (obliqCallbackObj,
                                          "pre`anyChange", args),
                      ObValue.ValBool).bool;
        END;
      END;
    END;
  END Pre_anyChange; 

PROCEDURE Post_anyChange (self : CallbackProxy; 
                          READONLY obj: RefShSeq.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obliqCallbackObj = NARROW (self.obj, Obliq.Val),
         obliqSharedObj   = NARROW (obj.proxy.obj, Obliq.Val) DO
      IF Obliq.ObjectHas(obliqCallbackObj, "post`anyChange") THEN
        WITH args = Obliq.Vals {obliqSharedObj} DO
          EVAL NARROW(Obliq.ObjectInvoke (obliqCallbackObj,
                                          "post`anyChange", args),
                      ObValue.ValBool).bool;
        END;
      END;
    END;
  END Post_anyChange; 

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : RefShSeqCB.T 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    TYPECASE args[idx] OF 
    | ObValue.ValObj =>
      WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
        TYPECASE raw OF 
        | CBT(node) =>  RETURN node.po;
        ELSE 
          ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
          RETURN NIL;      (* ... only to suppress compiler warning *)
        END;
      END;
    ELSE
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
      RETURN NIL;   (* ... only to suppress compiler warning *)
    END;
  END GetArg; 

(*****************************************************************************)
(* Setup procedures                                                          *)
(*****************************************************************************)

TYPE
  Code = {New, init, fromArray, addhi, addlo, remhi, remlo, put };

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;

  Package = ObLib.T OBJECT OVERRIDES
    Eval := Eval;
  END;

CONST
  pkgname = "RefShSeqCB";

PROCEDURE SetupPackage() =
  PROCEDURE NewOpCode (name: TEXT; arity: INTEGER; code: Code) : OpCode =
    BEGIN
      RETURN NEW (OpCode, name := name, arity := arity, code := code);
    END NewOpCode;

  VAR opCodes: REF ObLib.OpCodes;
  BEGIN
    opCodes := NEW(REF ObLib.OpCodes, NUMBER(Code));
    opCodes^ :=
        ObLib.OpCodes{
          NewOpCode ("New",             2, Code.New)
        };
    ObLib.Register(NEW(Package, name:=pkgname, opCodes:=opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;

VAR
  TProto        : ObValue.Val;
  EmptyObjProto : ObValue.Val;

PROCEDURE SetupModule (loader: ObLoader.T) =
  BEGIN
    pkgloader := loader;
    (*** retrieve the prototypes ***)
    pkgloader.load ("RefShSeqCB.obl");
    TProto        := pkgloader.get ("RefShSeqCB_TProto");
    EmptyObjProto := pkgloader.get ("{}");

    (*** Register the proxy makers ***)
    RefShSeqProxy.MkProxyCB := AddTObj;
  END SetupModule;

PROCEDURE Eval(self: Package; opCode: ObLib.OpCode;
               <* UNUSED *> arity        : ObLib.OpArity; 
               READONLY args: ObValue.ArgArray; 
               <* UNUSED *> temp: BOOLEAN; loc: SynLocation.T): ObValue.Val
  RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW(opCode, OpCode).code OF
    | Code.New =>
      WITH obj = ObRefShSeq.GetArg(args, 1, self, opCode, loc),
           raw = NEW (CBT, what := "<a RefShSeqCB.T>"),
           args = Obliq.Vals {args[2], raw},
           cbobj = Obliq.ObjectInvoke (TProto, "extend", args),
           val = NEW(RefShSeqCB.T) DO
        val.proxy := NEW (CallbackProxy, obj := cbobj);
        raw.po := val.init(obj);
        RETURN cbobj;
      END;
    END;
  END Eval;
  
(*****************************************************************************)
(* Help                                                                      *)
(*****************************************************************************)

PROCEDURE Help (self : ObCommand.T; arg : TEXT; <* UNUSED *> data : REFANY) =
  BEGIN
    IF pkgloader # NIL THEN
      pkgloader.help (self, arg, pkgname);
    END;
  END Help;

VAR 
  pkgloader: ObLoader.T := NIL;

BEGIN
END ObRefShSeqCB. 
