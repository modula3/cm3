(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Apr 12 15:16:18 PDT 1996 by najork                   *)
(*       Created on Fri Jul 29 14:25:38 PDT 1994 by najork                   *)


MODULE ObAnim3D;


IMPORT Anim3D, Clock, ObAux, ObBuiltIn, ObCommand, ObLib, ObValue, Obliq,
       RTCollector, SynLocation;


TYPE
  (*** Hack for Lyle's video ***)
  Code = {lock, ChangeClock, Collect};

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;

  Package = ObLib.T OBJECT
  OVERRIDES
    Eval := DoEval;
  END;

CONST
  pkgname = "Anim3D";


PROCEDURE SetupPackage () =

  PROCEDURE NewOpCode (name: TEXT; arity: INTEGER; code: Code) : OpCode =
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
            NewOpCode ("lock",       -1, Code.lock),
            NewOpCode ("ChangeClock", 1, Code.ChangeClock),
           (*** Hack for Lyle's video ***)
            NewOpCode ("Collect",     0, Code.Collect)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


PROCEDURE DoEval (             self         : Package; 
                               opCode       : ObLib.OpCode; 
                  <* UNUSED *> arity        : ObLib.OpArity; 
                               READONLY args: ObValue.ArgArray; 
                  <* UNUSED *> temp         : BOOLEAN;
                               loc          : SynLocation.T) : ObValue.Val
  RAISES {ObValue.Error} =
  BEGIN
    CASE NARROW (opCode, OpCode).code OF
    | Code.lock =>
      RETURN NEW (ObBuiltIn.ValMutex, 
                  what  := "<a Thread.Mutex>", 
                  mutex := Anim3D.lock);
    | Code.ChangeClock =>
      TYPECASE args[1] OF
      | ObValue.ValOk =>
        Anim3D.ChangeClock (NEW (Clock.T).init ());
        RETURN Obliq.ok;
      | ObValue.ValFun (p) =>
        Anim3D.ChangeClock (NEW (MyClock, p:= p).init ());
        RETURN Obliq.ok;
      ELSE
        ObValue.BadArgType(1, "Clock", self.name, opCode.name, loc);
        <* ASSERT FALSE *>
      END;
    (*** Hack for Lyle's video ***)
    | Code.Collect =>
      RTCollector.Enable();
      RTCollector.Collect();
      RTCollector.Collect();
      RTCollector.Disable();
      RETURN ObValue.valOk;
    END;
  END DoEval;


TYPE 
  MyClock = Clock.T BRANDED OBJECT
    p: Obliq.Val;
  OVERRIDES
    time := Time;
  END;

PROCEDURE Time (self: MyClock): LONGREAL =
  BEGIN
    TRY
      TYPECASE Obliq.Call (self.p, Obliq.Vals {}) OF
      | ObValue.ValReal (r) =>
        RETURN r.real;
      | ObValue.ValInt (i) =>
        RETURN FLOAT (i.int, LONGREAL);
      ELSE
                  RETURN 0.0d0;
      END;
    EXCEPT
      ObValue.Error, ObValue.Exception =>
      RETURN 0.0d0;
    END;
  END Time;


(*****************************************************************************)
(* Help                                                                      *)
(*****************************************************************************)


PROCEDURE Help (self : ObCommand.T; arg : TEXT; <* UNUSED *> data : REFANY) =
  BEGIN
    ObAux.Help (self, arg, pkgname);
  END Help;


BEGIN
END ObAnim3D.
