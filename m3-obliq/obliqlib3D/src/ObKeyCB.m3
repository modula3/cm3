(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Feb 16 20:47:18 PST 1995 by najork                   *)
(*       Created on Fri Jul 22 19:52:26 PDT 1994 by najork                   *)


MODULE ObKeyCB;


IMPORT CB, KeyCB, KeyCBProxy, ObAux, ObCB, ObCommand, ObKeySym, ObLib, 
       ObProtoLoader, ObProxiedObj, ObValue, Obliq, SynLocation, Text, VBT;

CONST
  pkgname = "KeyCB";


(*****************************************************************************)
(* Wrapper for KeyCB.T                                                       *)
(*****************************************************************************)

TYPE
  T = ObProxiedObj.T BRANDED OBJECT END;

PROCEDURE AddTObj (cb : KeyCB.T) =
  <* FATAL ObValue.Error, ObValue.Exception *>
  BEGIN
    WITH obj = Obliq.ObjectClone (Obliq.Vals {TProto}),
         raw = NEW (T, what := "<a KeyCB.T>", po := cb) DO
      Obliq.ObjectUpdate (obj, "raw", raw);
      cb.proxy := NEW (Proxy, obj := obj);
    END;
  END AddTObj;


PROCEDURE GetT (args    : ObValue.ArgArray; 
                idx     : INTEGER; 
                package : ObLib.T; 
                opCode  : ObLib.OpCode; 
                loc     : SynLocation.T) : KeyCB.T 
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
  Proxy = KeyCBProxy.Proxy BRANDED OBJECT
  OVERRIDES
    invoke := Invoke;
  END;


PROCEDURE Invoke (proxy : Proxy; mr : KeyCB.Rec) RAISES {CB.BadMethod} =
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
(* Wrapper for KeyCB.Rec                                                     *)
(*****************************************************************************)


EXCEPTION FatalError;  <* FATAL FatalError *>


PROCEDURE GetRec (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : KeyCB.Rec
    RAISES {ObValue.Error} =
  BEGIN
    TRY
      WITH obj    = args[idx],
           change = ObliqToKeySym    (Obliq.ObjectSelect (obj, "change")),
           down   = Obliq.ToBool     (Obliq.ObjectSelect (obj, "wentDown")),
           mods   = ObliqToModifiers (Obliq.ObjectSelect (obj, "modifiers")) DO
        RETURN KeyCB.Rec {change, down, mods}
      END;
    EXCEPT
    | ObValue.Error, ObValue.Exception =>
      ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc); 
      RETURN KeyCB.Rec {VBT.NoKey, FALSE, VBT.Modifiers {}};
          (* ... only to suppress compiler warning *)
    END;
  END GetRec;


PROCEDURE RecToObliq (mr : KeyCB.Rec) : Obliq.Val =
  TYPE 
    Field = RECORD label : TEXT; field : Obliq.Val END;
  BEGIN
    RETURN Obliq.NewObject (Obliq.Fields {
                    Field {"change",    KeySymToObliq (mr.whatChanged)},
                    Field {"wentDown",  Obliq.NewBool (mr.wentDown)},
                    Field {"modifiers", ModifiersToObliq (mr.modifiers)}});
  END RecToObliq;


PROCEDURE KeySymToObliq (k : VBT.KeySym) : Obliq.Val =
  BEGIN
    RETURN ObKeySym.M3ToObliq (k);
  END KeySymToObliq;


PROCEDURE ObliqToKeySym (val : Obliq.Val) : VBT.KeySym RAISES {ObValue.Error} =
  BEGIN
    RETURN ObKeySym.ObliqToM3 (val);
  END ObliqToKeySym;


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
    loader.load ("KeyCB.obl");
    TProto   := loader.get ("KeyCB_TProto");

    (*** Register the proxy makers ***)
    KeyCBProxy.MkProxyT := AddTObj;
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
      WITH cb  = NEW (KeyCB.T).init (),
           obj = NARROW (cb.proxy.obj, Obliq.Val) DO
        Obliq.ObjectUpdate (obj, "invoke", args[1]);
        RETURN obj;
      END;
    | Code.Invoke =>
      TRY
        WITH cb = GetT   (args, 1, self, opCode, loc),
             kr = GetRec (args, 2, self, opCode, loc) DO
          cb.invoke (kr);
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
END ObKeyCB.
