(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul  7 10:59:50 PDT 1995 by najork                   *)
(*       Created on Fri Jul 22 19:41:47 PDT 1994 by najork                   *)


MODULE ObPositionCB;


IMPORT CB, ObAux, ObCB, ObCommand, ObLib, ObProtoLoader, ObPoint, ObProxiedObj,
       ObValue, Obliq, Point, PositionCB, PositionCBProxy, SynLocation, Text, 
       VBT;

CONST
  pkgname = "PositionCB";


(*****************************************************************************)
(* Wrapper for PositionCB.T                                                  *)
(*****************************************************************************)

TYPE
  T = ObProxiedObj.T BRANDED OBJECT END;

PROCEDURE AddTObj (cb : PositionCB.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a PositionCB.T>", po := cb) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      cb.proxy := NEW (Proxy, obj := obj);
    END;
  END AddTObj;


PROCEDURE GetT (args    : ObValue.ArgArray; 
                idx     : INTEGER; 
                package : ObLib.T; 
                opCode  : ObLib.OpCode; 
                loc     : SynLocation.T) : PositionCB.T 
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF 
      | T (node) => 
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetT;


TYPE
  Proxy = PositionCBProxy.Proxy BRANDED OBJECT
  OVERRIDES
    invoke := Invoke;
  END;


PROCEDURE Invoke (proxy : Proxy; mr : PositionCB.Rec) RAISES {CB.BadMethod} =
  BEGIN
    TRY
      WITH args = Obliq.Vals {RecToObliq (mr)},
           obj  = NARROW (proxy.obj, Obliq.Val) DO
        EVAL Obliq.ObjectInvoke (obj, "invoke", args);
      END;
    EXCEPT
    | ObValue.Error (packet) =>
      RAISE CB.BadMethod (ObAux.ErrorToText (packet));
    | ObValue.Exception (packet) =>
      RAISE CB.BadMethod (ObAux.ExceptionToText (packet));
    END;
  END Invoke;


(*****************************************************************************)
(* Wrapper for PositionCB.Rec                                                *)
(*****************************************************************************)


EXCEPTION FatalError;  <* FATAL FatalError *>

PROCEDURE GetRec (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : PositionCB.Rec
    RAISES {ObValue.Error} =
  BEGIN
    TRY
      WITH obj  = args[idx],
           pos  = ObPoint.ObliqToM3 (Obliq.ObjectSelect (obj, "pos")),
           mods = ObliqToModifiers  (Obliq.ObjectSelect (obj, "modifiers")) DO
        RETURN PositionCB.Rec {pos, mods}
      END;
    EXCEPT
    | ObValue.Error, ObValue.Exception =>
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
      RETURN PositionCB.Rec {Point.Origin, VBT.Modifiers {}};
          (* ... only to suppress compiler warning *)
    END;
  END GetRec;


PROCEDURE RecToObliq (mr : PositionCB.Rec) : Obliq.Val =
  TYPE 
    Field = RECORD label : TEXT; field : Obliq.Val END;
  BEGIN
    RETURN Obliq.NewObject (Obliq.Fields {
                    Field {"pos",       ObPoint.M3ToObliq (mr.pos2D)},
                    Field {"modifiers", ModifiersToObliq  (mr.modifiers)}});
  END RecToObliq;


PROCEDURE ModifierToObliq (m : VBT.Modifier) : Obliq.Val =
  BEGIN
    CASE m OF
    | VBT.Modifier.Shift   => RETURN Obliq.NewText ("Shift");
    | VBT.Modifier.Lock    => RETURN Obliq.NewText ("Lock");
    | VBT.Modifier.Control => RETURN Obliq.NewText ("Control");
    | VBT.Modifier.Option  => RETURN Obliq.NewText ("Option");
    | VBT.Modifier.MouseL  => RETURN Obliq.NewText ("Left");
    | VBT.Modifier.MouseM  => RETURN Obliq.NewText ("Middle");
    | VBT.Modifier.MouseR  => RETURN Obliq.NewText ("Right");
    ELSE
      RAISE FatalError;
    END;
  END ModifierToObliq;


PROCEDURE ObliqToModifier (val : Obliq.Val) : VBT.Modifier 
    RAISES {ObValue.Error} =
  BEGIN
    WITH t = Obliq.ToText (val) DO
      IF Text.Equal (t, "Shift") THEN
        RETURN VBT.Modifier.Shift;
      ELSIF Text.Equal (t, "Lock") THEN
        RETURN VBT.Modifier.Lock;
      ELSIF Text.Equal (t, "Control") THEN
        RETURN VBT.Modifier.Control;
      ELSIF Text.Equal (t, "Option") THEN
        RETURN VBT.Modifier.Option;
      ELSIF Text.Equal (t, "Left") THEN
        RETURN VBT.Modifier.MouseL;
      ELSIF Text.Equal (t, "Middle") THEN
        RETURN VBT.Modifier.MouseM;
      ELSIF Text.Equal (t, "Right") THEN
        RETURN VBT.Modifier.MouseR;
      ELSE
        Obliq.RaiseError ("Not a valid Modifier");
        RETURN VBT.Modifier.MouseL; (* ... only to suppress compiler warning *)
      END;
    END;
  END ObliqToModifier;


PROCEDURE ModifiersToObliq (modifiers : VBT.Modifiers) : Obliq.Val =
  VAR
    mods : ARRAY [0 .. ORD (LAST (VBT.Modifier))] OF Obliq.Val;
    size := 0;
  BEGIN
    FOR m := FIRST (VBT.Modifier) TO LAST (VBT.Modifier) DO
      IF m IN modifiers THEN
        mods[size] := ModifierToObliq (m);
        INC (size);
      END;
    END;
    RETURN Obliq.NewArray (SUBARRAY (mods, 0, size));
  END ModifiersToObliq;


PROCEDURE ObliqToModifiers (val : Obliq.Val) : VBT.Modifiers 
    RAISES {ObValue.Error} =
  VAR
    mods := VBT.Modifiers {};
  BEGIN
    FOR i := 0 TO Obliq.ArraySize (val) - 1 DO
      mods := mods + VBT.Modifiers {ObliqToModifier (Obliq.ArrayGet (val, i))};
    END;
    RETURN mods;
  END ObliqToModifiers;


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
            NewOpCode ("New",      1, Code.New),
            NewOpCode ("Invoke",   2, Code.Invoke)
        };

    ObLib.Register (NEW (Package, name := pkgname, opCodes := opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;


VAR 
  TProto  : ObValue.Val;


PROCEDURE SetupModule (loader : ObProtoLoader.T) =
  BEGIN
    (*** retrieve the prototype ***)
    loader.load ("PositionCB.obl");
    TProto   := loader.get ("PositionCB_TProto");

    (*** Register the proxy makers ***)
    PositionCBProxy.MkProxyT := AddTObj;
  END SetupModule;


(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)


TYPE 
  Code = {New, Invoke};

  OpCode = ObLib.OpCode BRANDED OBJECT
    code: Code;
  END;

  Package = ObLib.T BRANDED OBJECT
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
      WITH cb  = NEW (PositionCB.T).init (),
           obj = NARROW (cb.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "invoke", args[1]);
        RETURN obj;
      END;
    | Code.Invoke =>
      TRY
        WITH cb = GetT   (args, 1, self, opCode, loc),
             pr = GetRec (args, 2, self, opCode, loc) DO
          cb.invoke (pr);
        END;
      EXCEPT
      | CB.BadMethod => 
        Obliq.RaiseException (ObCB.BadMethod, opCode.name, loc);
      END;
      RETURN ObValue.valOk;      
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
END ObPositionCB.
