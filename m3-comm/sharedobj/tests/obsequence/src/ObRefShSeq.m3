(* TODO: create picklers that will not duplicate the obliq object when
   passed to a host more than once.  Simply send a receive the real
   object first.  If it already exists, there will be a proxy, which
   can just be used. *)

MODULE ObRefShSeq;

IMPORT ObCommand, ObLib, ObLoader, ObValue, Obliq, EmbProxiedObj,
       SynLocation, ObSharedObj, SharedObj, Thread, ObError;
IMPORT RefShSeq, RefShSeqProxy;

TYPE 
  RefShSeqT = ObSharedObj.T BRANDED "ObRefShSeq.T" OBJECT END;

PROCEDURE AddTObj (pn : RefShSeq.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    IF pn.proxy # NIL THEN RETURN END;
    WITH raw = NEW (RefShSeqT, what := "<a RefShSeq.T>", po:=pn),
         args = Obliq.Vals {ObValue.valOk, raw},
         obj = Obliq.ObjectInvoke (TProto, "extend", args) DO
      pn.proxy := NEW (EmbProxiedObj.Proxy, obj := obj);
    END;
  END AddTObj;

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : RefShSeq.T 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    TYPECASE args[idx] OF 
    | ObValue.ValObj =>
      WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
        TYPECASE raw OF 
        | RefShSeqT(node) => 
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
  END GetArg; 

(*****************************************************************************)
(* Setup procedures                                                          *)
(*****************************************************************************)

TYPE
  Code = {init, fromArray, addhi, addlo, remhi, remlo, put, New};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;

  Package = ObLib.T OBJECT OVERRIDES
    Eval := DoEval;
  END;

CONST
  pkgname = "RefShSeq";

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
          NewOpCode ("New",             0, Code.New),
          NewOpCode ("init",            2, Code.init),
          NewOpCode ("fromArray",       2, Code.fromArray),
          NewOpCode ("addhi",           2, Code.get),
          NewOpCode ("addlo",           2, Code.get),
          NewOpCode ("remhi",           1, Code.get),
          NewOpCode ("remlo",           1, Code.get),
          NewOpCode ("put",             3, Code.get)
        };
    ObLib.Register(NEW(Package, name:=pkgname, opCodes:=opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;

VAR
  TProto     : ObValue.Val;

PROCEDURE SetupModule (loader: ObLoader.T) =
  BEGIN
    pkgloader := loader;
    (*** retrieve the prototypes ***)
    pkgloader.load ("RefShSeq.obl");
    TProto     := pkgloader.get ("RefShSeq_TProto");

    (*** Register the proxy makers ***)
    RefShSeqProxy.MkProxyT := AddTObj;
  END SetupModule;

PROCEDURE DoEval(self: Package; 
                 opCode: ObLib.OpCode;
                 <* UNUSED *> arity        : ObLib.OpArity; 
                 READONLY args: ObValue.ArgArray; 
                 <* UNUSED *> temp: BOOLEAN; 
                 loc: SynLocation.T) : ObValue.Val
  RAISES {ObValue.Error, ObValue.Exception} =
  VAR arg1: RefShSeq.Data;
  BEGIN
    TRY
      CASE NARROW(opCode, OpCode).code OF
      | Code.New =>
        WITH val = NEW(RefShSeq.T).init() DO
          RETURN val.proxy.obj;
        END;
      | Code.init =>
        WITH obj = GetArg(args, 1, self, opCode, loc) DO
          arg1 := ObRefShSeqData.GetArg(args, 2, self, opCode, loc);
          obj.set(arg1);
        END;
        RETURN ObValue.valOk;
      | Code.get =>
        WITH obj = GetArg(args, 1, self, opCode, loc) DO
          RETURN ObRefShSeqData.M3ToObliq(obj.get());
        END;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc);
        RETURN NIL; (* to shut up warning in compiler about no return value *)
      END;
    EXCEPT
    | SharedObj.Error(ec) =>
      ObValue.RaiseException(ObSharedObj.errorException, 
                             self.name & "_" & opCode.name & 
                             ": " & ObError.AtomListToText(ec), loc);
      RETURN NIL; (* to shut up warning in compiler about no return value *)
    | SharedObj.Fatal(ec) =>
      ObValue.RaiseException(ObSharedObj.fatalException, 
                             self.name & "_" & opCode.name & ": " & 
                             ObError.AtomListToText(ec), loc);
      RETURN NIL; (* to shut up warning in compiler about no return value *)
    | Thread.Alerted =>
      ObValue.RaiseException(ObValue.threadAlerted, 
                             self.name & "_" & opCode.name & ": ", loc);
      RETURN NIL; (* to shut up warning in compiler about no return value *)
    END;
  END DoEval;
  
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
END ObRefShSeq. 
