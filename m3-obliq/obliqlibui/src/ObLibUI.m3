(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 15:04:25 1997
 *)

UNSAFE MODULE ObLibUI;

IMPORT Color, ColorName, FormsVBT, FVTypes, MultiFilter, MultiSplit, Obliq, 
       ObBuiltIn, ObEval, ObLib, ObValue, Rd, Rect, SourceVBT, Split, 
       SynLocation, SynWr, Text, Thread, TranslateVBT, Trestle, TrestleComm, 
       VBT, VBTClass, ZSplit;

VAR setupDone := FALSE;

  PROCEDURE PackageSetup() =
  BEGIN
    IF NOT setupDone THEN
      setupDone := TRUE;
      Setup();
    END;
  END PackageSetup;

  PROCEDURE Setup() =
  BEGIN
    SetupColor();
    SetupVBT();
    SetupZSplit ();
    SetupForm();
  END Setup;


(* ============ "color" package ============ *)

TYPE

  ColorCode = {Named, RGB, HSV, R, G, B, H, S, V, Brightness};

  ColorOpCode = ObLib.OpCode OBJECT code: ColorCode;  END;

  PackageColor = ObLib.T OBJECT OVERRIDES Eval := EvalColor; END;

PROCEDURE IsColor (self: ValColor; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValColor (oth) => RETURN self.color = oth.color;
    ELSE
      RETURN FALSE
    END;
  END IsColor;

PROCEDURE CopyColor (           self: ObValue.ValAnything;
                     <*UNUSED*> tbl : ObValue.Tbl;
                     <*UNUSED*> loc : SynLocation.T        ):
  ObValue.ValAnything =
  BEGIN
    RETURN self;
  END CopyColor;

PROCEDURE NewColorOC (name: TEXT; arity: INTEGER; code: ColorCode):
  ColorOpCode =
  BEGIN
    RETURN NEW(ColorOpCode, name := name, arity := arity, code := code);
  END NewColorOC;

PROCEDURE SetupColor () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(ColorCode));
    opCodes^ :=
      OpCodes{
        NewColorOC("named", 1, ColorCode.Named),
        NewColorOC("rgb", 3, ColorCode.RGB),
        NewColorOC("hsv", 3, ColorCode.HSV),
        NewColorOC("r", 1, ColorCode.R), NewColorOC("g", 1, ColorCode.G),
        NewColorOC("b", 1, ColorCode.B), NewColorOC("h", 1, ColorCode.H),
        NewColorOC("s", 1, ColorCode.S), NewColorOC("v", 1, ColorCode.V),
        NewColorOC("brightness", 1, ColorCode.Brightness)};
    ObLib.Register(NEW(PackageColor, name := "color", opCodes := opCodes));
  END SetupColor;

PROCEDURE EvalColor (                    self  : PackageColor;
                                         opCode: ObLib.OpCode;
                     <*UNUSED*>          arity : ObLib.OpArity;
                                READONLY args  : ObValue.ArgArray;
                                         temp  : BOOLEAN;
                     <*UNUSED*>          swr   : SynWr.T;
                                         loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR
    real1, real2, real3: LONGREAL;
    rgb1               : Color.T;
    hsv1               : Color.HSV;
    text1              : TEXT;
  BEGIN
    CASE NARROW(opCode, ColorOpCode).code OF
    | ColorCode.Named =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
        END;
        TRY
          rgb1 := ColorName.ToRGB(text1);
        EXCEPT
          ColorName.NotFound => rgb1 := Color.Black;
        END;
        RETURN NEW(ValColor, what := "<a Color.T>", picklable := TRUE,
                   tag := "Color`T", color := rgb1);
    | ColorCode.RGB =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValReal (node) => real2 := node.real;
        ELSE
          ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[3] OF
        | ObValue.ValReal (node) => real3 := node.real;
        ELSE
          ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        IF (real1 < 0.0d0) OR (real1 > 1.0d0) THEN
          ObValue.BadArgVal(1, "in range", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        IF (real2 < 0.0d0) OR (real2 > 1.0d0) THEN
          ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        IF (real3 < 0.0d0) OR (real3 > 1.0d0) THEN
          ObValue.BadArgVal(3, "in range", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        rgb1 :=
          Color.T{r := FLOAT(real1), g := FLOAT(real2), b := FLOAT(real3)};
        RETURN NEW(ValColor, what := "<a Color.T>", picklable := TRUE,
                   tag := "Color`T", color := rgb1);
    | ColorCode.HSV =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValReal (node) => real2 := node.real;
        ELSE
          ObValue.BadArgType(2, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        TYPECASE args[3] OF
        | ObValue.ValReal (node) => real3 := node.real;
        ELSE
          ObValue.BadArgType(3, "real", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        IF (real1 < 0.0d0) OR (real1 > 1.0d0) THEN
          ObValue.BadArgVal(1, "in range", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        IF (real2 < 0.0d0) OR (real2 > 1.0d0) THEN
          ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        IF (real3 < 0.0d0) OR (real3 > 1.0d0) THEN
          ObValue.BadArgVal(3, "in range", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        rgb1 := Color.FromHSV(Color.HSV{h := FLOAT(real1), s :=
                                        FLOAT(real2), v := FLOAT(real3)});
        RETURN NEW(ValColor, what := "<a Color.T>", tag := "Color`T",
                   picklable := TRUE, color := rgb1);
    | ColorCode.R =>
        TYPECASE args[1] OF
        | ValColor (node) => rgb1 := node.color;
        ELSE
          ObValue.BadArgType(1, "Color`T", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN NEW(ObValue.ValReal, real := FLOAT(rgb1.r, LONGREAL),
                   temp := temp);
    | ColorCode.G =>
        TYPECASE args[1] OF
        | ValColor (node) => rgb1 := node.color;
        ELSE
          ObValue.BadArgType(1, "Color`T", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN NEW(ObValue.ValReal, real := FLOAT(rgb1.g, LONGREAL),
                   temp := temp);
    | ColorCode.B =>
        TYPECASE args[1] OF
        | ValColor (node) => rgb1 := node.color;
        ELSE
          ObValue.BadArgType(1, "Color`T", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN NEW(ObValue.ValReal, real := FLOAT(rgb1.b, LONGREAL),
                   temp := temp);
    | ColorCode.H =>
        TYPECASE args[1] OF
        | ValColor (node) => rgb1 := node.color;
        ELSE
          ObValue.BadArgType(1, "Color`T", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        hsv1 := Color.ToHSV(rgb1);
        RETURN NEW(ObValue.ValReal, real := FLOAT(hsv1.h, LONGREAL),
                   temp := temp);
    | ColorCode.S =>
        TYPECASE args[1] OF
        | ValColor (node) => rgb1 := node.color;
        ELSE
          ObValue.BadArgType(1, "Color`T", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        hsv1 := Color.ToHSV(rgb1);
        RETURN NEW(ObValue.ValReal, real := FLOAT(hsv1.s, LONGREAL),
                   temp := temp);
    | ColorCode.V =>
        TYPECASE args[1] OF
        | ValColor (node) => rgb1 := node.color;
        ELSE
          ObValue.BadArgType(1, "Color`T", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        hsv1 := Color.ToHSV(rgb1);
        RETURN NEW(ObValue.ValReal, real := FLOAT(hsv1.v, LONGREAL),
                   temp := temp);
    | ColorCode.Brightness =>
        TYPECASE args[1] OF
        | ValColor (node) => rgb1 := node.color;
        ELSE
          ObValue.BadArgType(1, "Color`T", self.name, opCode.name, loc); <*ASSERT FALSE*>
        END;
        RETURN NEW(ObValue.ValReal,
                   real := FLOAT(Color.Brightness(rgb1), LONGREAL),
                   temp := temp);
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
    END;
  END EvalColor;


(* ============ "vbt" package ============= *)

TYPE
  VBTCode = {failure, Lock, Show, Domain};

  VBTOpCode = ObLib.OpCode OBJECT
    code: VBTCode;
  END;
    
  PackageVBT = ObLib.T OBJECT
  OVERRIDES
    Eval := EvalVBT;
  END;

VAR 
  vbt_mu: ObValue.Val;

VAR vbtException: ObValue.ValException;

PROCEDURE SetupVBT () =

  PROCEDURE NewOpCode (name: TEXT; arity: INTEGER; code: VBTCode): VBTOpCode =
    BEGIN
      RETURN NEW (VBTOpCode, name := name, arity := arity, code := code);
    END NewOpCode;

  PROCEDURE NewMutex (mu: MUTEX): ObValue.Val =
    BEGIN
      RETURN NEW (ObBuiltIn.ValMutex, 
                  what := "<a Thread.Mutex>", 
                  tag := "Thread`Mutex",
                  picklable := FALSE,
                  mutex := mu);
    END NewMutex;

  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW (REF OpCodes, NUMBER (VBTCode));
    opCodes^ := OpCodes{NewOpCode ("mu",      -1, VBTCode.Lock),
                        NewOpCode ("failure", -1, VBTCode.failure),
                        NewOpCode ("show",     1, VBTCode.Show),
                        NewOpCode ("domain",   1, VBTCode.Domain)
                       };

    vbtException := NEW(ObValue.ValException, name:="vbt_failure");
    vbt_mu := NewMutex (VBT.mu);
    ObLib.Register (NEW (PackageVBT, name := "vbt", opCodes := opCodes));
  END SetupVBT;


PROCEDURE EvalVBT (                    self  : PackageVBT;
                                       opCode: ObLib.OpCode;
                   <*UNUSED*>          arity : ObLib.OpArity;
                              READONLY args  : ObValue.ArgArray;
                   <*UNUSED*>          temp  : BOOLEAN;
                   <*UNUSED*>          swr   : SynWr.T;
                                       loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR vbt: VBT.T;
  BEGIN
    CASE NARROW(opCode, VBTOpCode).code OF
    | VBTCode.failure => RETURN vbtException;
    | VBTCode.Lock => RETURN vbt_mu;
    | VBTCode.Show =>
        TYPECASE args[1] OF
        | ValVBT (node) => vbt := node.vbt;
        ELSE
          ObValue.BadArgType(1, "vbt", self.name, opCode.name, loc);
        END;
        TRY
          Trestle.Install(vbt);
        EXCEPT
        | TrestleComm.Failure =>
            ObValue.RaiseException(vbtException, opCode.name, loc);
        END;
        RETURN ObValue.valOk;
    | VBTCode.Domain =>
        TYPECASE args[1] OF
        | ValVBT (node) => vbt := node.vbt;
        ELSE
          ObValue.BadArgType(1, "vbt", self.name, opCode.name, loc);
        END;
        WITH dom   = VBT.Domain(vbt),
             west  = Obliq.NewInt(dom.west),
             east  = Obliq.NewInt(dom.east),
             north = Obliq.NewInt(dom.north),
             south = Obliq.NewInt(dom.south)  DO
          RETURN Obliq.NewArray(Obliq.Vals{west, east, north, south});
        END;
    END;
  END EvalVBT;

PROCEDURE IsVBT(self: ValVBT; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF ValVBT(oth)=> RETURN self.vbt = oth.vbt;
    ELSE RETURN FALSE END;
  END IsVBT;

PROCEDURE CopyVBT(<*UNUSED*>self: ObValue.ValAnything; <*UNUSED*>tbl: ObValue.Tbl;
    loc: SynLocation.T): ObValue.ValAnything RAISES {ObValue.Error} =
  BEGIN
    ObValue.RaiseError("Cannot copy vbts", loc);<*ASSERT FALSE*> 
  END CopyVBT;

(* ============ "zsplit" package ============ *)

TYPE
  ZSplitCode = {Move};

  ZSplitOpCode = ObLib.OpCode OBJECT
    code: ZSplitCode;
  END;
    
  PackageZSplit = ObLib.T OBJECT
  OVERRIDES
    Eval := EvalZSplit;
  END;


PROCEDURE SetupZSplit () =

  PROCEDURE NewOpCode (name: TEXT; arity: INTEGER; code: ZSplitCode): ZSplitOpCode =
    BEGIN
      RETURN NEW (ZSplitOpCode, name := name, arity := arity, code := code);
    END NewOpCode;

  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW (REF OpCodes, NUMBER (ZSplitCode));
    opCodes^ := OpCodes{NewOpCode ("move", 2, ZSplitCode.Move)
                       };

    ObLib.Register (NEW (PackageZSplit, name := "zsplit", opCodes := opCodes));
  END SetupZSplit;


PROCEDURE EvalZSplit (                    self  : PackageZSplit;
                                          opCode: ObLib.OpCode;
                      <*UNUSED*>          arity : ObLib.OpArity;
                                 READONLY args  : ObValue.ArgArray;
                      <*UNUSED*>          temp  : BOOLEAN;
                      <*UNUSED*>          swr   : SynWr.T;
                                          loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR
    vbt : VBT.T;
    rect: Rect.T;
  BEGIN
    CASE NARROW(opCode, ZSplitOpCode).code OF
    | ZSplitCode.Move =>
        TYPECASE args[1] OF
        | ValVBT (node) => vbt := node.vbt;
        ELSE
          ObValue.BadArgType(1, "vbt", self.name, opCode.name, loc);
        END;
        TYPECASE args[2] OF
        | ObValue.ValArray (node) =>
            IF Obliq.ArraySize(node) = 4 THEN
              TYPECASE Obliq.ArrayGet(node, 0) OF
              | ObValue.ValInt (node) => rect.west := node.int;
              ELSE
                ObValue.BadArgType(
                  2, "[4*Int]", self.name, opCode.name, loc);
              END;
              TYPECASE Obliq.ArrayGet(node, 1) OF
              | ObValue.ValInt (node) => rect.east := node.int;
              ELSE
                ObValue.BadArgType(
                  2, "[4*Int]", self.name, opCode.name, loc);
              END;
              TYPECASE Obliq.ArrayGet(node, 2) OF
              | ObValue.ValInt (node) => rect.north := node.int;
              ELSE
                ObValue.BadArgType(
                  2, "[4*Int]", self.name, opCode.name, loc);
              END;
              TYPECASE Obliq.ArrayGet(node, 3) OF
              | ObValue.ValInt (node) => rect.south := node.int;
              ELSE
                ObValue.BadArgType(
                  2, "[4*Int]", self.name, opCode.name, loc);
              END;
            ELSE
              ObValue.BadArgType(2, "[4*Int]", self.name, opCode.name, loc);
            END;
        ELSE
          ObValue.BadArgType(2, "[4*Int]", self.name, opCode.name, loc);
        END;

        (* Make sure that the parent of "vbt" is a ZSplit.T. *)
        TYPECASE VBT.Parent(vbt) OF
        | NULL =>
            ObValue.BadArgType(1, "vbt", self.name, opCode.name, loc);
        | ZSplit.T =>            (* everything is fine. *)
        | VBT.Split =>
            ObValue.BadArgType(1, "vbt", self.name, opCode.name, loc);
        END;

        ZSplit.Move(vbt, rect);
        RETURN ObValue.valOk;
    END;
  END EvalZSplit;


(* ============ "form" package ============ *)

TYPE

  FormCode = {Error, New, FromFile, FromURL, Attach, PutGeneric, PutColor,
    GetBool, PutBool, GetInt, PutInt, GetText, PutText, 
    GetBoolean, PutBoolean, GetChoice, PutChoice, TakeFocus,
    GetReactivity, PutReactivity, PopUp, PopDown, 
    Insert, InsertVBT, Move, Delete, DeleteRange, DeleteVBT,
    ChildIndex, Child, NumOfChildren,
    ShowAt, Show, Hide, Lift, DetachGarnish, 
    BeTarget, SetTargetValue, AttachTargetHit, AttachTargetDrop};

  FormOpCode =  
    ObLib.OpCode OBJECT
        code: FormCode;
      END;
    
  PackageForm = 
    ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalForm;
      END;

TYPE 
  Form = FormsVBT.T BRANDED "ObLibUI.Form" OBJECT
    swr: SynWr.T;
  OVERRIDES
    realize := Realize;
  END;

PROCEDURE Realize (form: Form; type, name: TEXT) : VBT.T 
    RAISES {FormsVBT.Error} =
  BEGIN
    IF Text.Equal (type, "Source") THEN
      RETURN NEW (Source, swr := form.swr);
    ELSIF Text.Equal (type, "Target") THEN
      RETURN NEW (Target, val := Obliq.ok);
    ELSE
      RETURN FormsVBT.T.realize (form, type, name);
    END;
  END Realize;

TYPE 
  Source = FVTypes.FVSource BRANDED "ObLibUI.Source" OBJECT
    hitProc : Obliq.Val := NIL;
    dropProc: Obliq.Val := NIL;
    swr     : SynWr.T;
  OVERRIDES
    hit      := SourceHit;
    callback := SourceCallback;
  END;

TYPE 
  Target = FVTypes.FVTarget BRANDED "ObLibUI.Target" OBJECT 
    val: Obliq.Val;   (* initialized to "Obliq.ok" *)
  END;

PROCEDURE SourceHit (                      s : Source;
                                           t : VBT.T;
                     <* UNUSED *> READONLY cd: VBT.PositionRec): BOOLEAN =
  VAR
    source: Source := s;
    target: Target := t;
  BEGIN
    (* If there is a "hitProc" procedure attached to the source, call it,
       passing it the value attached to the target (default is "ok"). *)
    IF source.hitProc # NIL THEN
      TRY
        WITH result = Obliq.Call(source.hitProc, Obliq.Vals{target.val},
                                 s.swr) DO
          RETURN Obliq.ToBool(result);
        END;
      EXCEPT
      | ObValue.Error, ObValue.Exception =>
          (* we should report an error (by printing out a message) *)
          RETURN TRUE;
      END;
    ELSE
      RETURN TRUE;
    END;
  END SourceHit;


PROCEDURE SourceCallback (self: Source; READONLY cd: VBT.MouseRec) =
  VAR target := NARROW(SourceVBT.GetTarget(self), Target);
  BEGIN
    (* If there is a "dropProc" procedure attached to the source, call it,
       passing it the value attached to the target (default is "ok"). *)
    IF self.dropProc # NIL THEN
      TRY
        WITH pt = Obliq.NewArray(Obliq.Vals{Obliq.NewInt(cd.cp.pt.h),
                                            Obliq.NewInt(cd.cp.pt.v)}) DO
          EVAL Obliq.Call(self.dropProc, Obliq.Vals{target.val, pt},
                          self.swr);
        END;
      EXCEPT
      | ObValue.Error, ObValue.Exception =>
        (* we should report an error (by printing out a message) *)
      END;
    END;
  END SourceCallback;

TYPE
  FormClosure = FormsVBT.Closure OBJECT
                  fun     : ObValue.ValFun;
                  fv      : ObValue.Val;
                  location: SynLocation.T;
                  swr     : SynWr.T;
                OVERRIDES
                  apply := ApplyFormClosure;
                END;

PROCEDURE ApplyFormClosure (           self: FormClosure;
                            <*UNUSED*> fv  : FormsVBT.T;
                            <*UNUSED*> name: TEXT;
                            <*UNUSED*> time: VBT.TimeStamp) RAISES {} =
  VAR args: ARRAY [0 .. 0] OF ObValue.Val;
  BEGIN
    TRY
      args[0] := self.fv;
      EVAL ObEval.Call(self.fun, args, self.swr, self.location);
    EXCEPT
    | ObValue.Error (packet) =>
        SynWr.Text(self.swr,
          "*** A Modula3 callback to Obliq caused an Obliq error: ***\n");
        ObValue.ErrorMsg(self.swr, packet);
        SynWr.Flush(self.swr);
    | ObValue.Exception (packet) =>
        SynWr.Text(self.swr,
          "*** A Modula3 callback to Obliq caused an Obliq exception: ***\n");
        ObValue.ExceptionMsg(self.swr, packet);
        SynWr.Flush(self.swr);
    END;
  END ApplyFormClosure;

VAR formException: ObValue.ValException;

PROCEDURE NewFormOC (name: TEXT; arity: INTEGER; code: FormCode):
  FormOpCode =
  BEGIN
    RETURN NEW(FormOpCode, name := name, arity := arity, code := code);
  END NewFormOC;

PROCEDURE SetupForm () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(FormCode));
    opCodes^ :=
      OpCodes{NewFormOC("failure", -1, FormCode.Error),
              NewFormOC("new", 1, FormCode.New),
              NewFormOC("fromFile", 1, FormCode.FromFile),
              NewFormOC("fromURL", 1, FormCode.FromURL),
              NewFormOC("attach", 3, FormCode.Attach),
              NewFormOC("putGeneric", 3, FormCode.PutGeneric),
              NewFormOC("putColor", 4, FormCode.PutColor),
              NewFormOC("getBool", 3, FormCode.GetBool),
              NewFormOC("putBool", 4, FormCode.PutBool),
              NewFormOC("getInt", 3, FormCode.GetInt),
              NewFormOC("putInt", 4, FormCode.PutInt),
              NewFormOC("getText", 3, FormCode.GetText),
              NewFormOC("putText", 5, FormCode.PutText),
              NewFormOC("getBoolean", 2, FormCode.GetBoolean),
              NewFormOC("putBoolean", 3, FormCode.PutBoolean),
              NewFormOC("getChoice", 2, FormCode.GetChoice),
              NewFormOC("putChoice", 3, FormCode.PutChoice),
              NewFormOC("takeFocus", 3, FormCode.TakeFocus),
              NewFormOC("getReactivity", 2, FormCode.GetReactivity),
              NewFormOC("putReactivity", 3, FormCode.PutReactivity),
              NewFormOC("popUp", 2, FormCode.PopUp),
              NewFormOC("popDown", 2, FormCode.PopDown),
              NewFormOC("insert", 4, FormCode.Insert),
              NewFormOC("insertVBT", 4, FormCode.InsertVBT),
              NewFormOC("move", 5, FormCode.Move),
              NewFormOC("delete", 3, FormCode.Delete),
              NewFormOC("deleteVBT", 3, FormCode.DeleteVBT),
              NewFormOC("deleteRange", 4, FormCode.DeleteRange),
              NewFormOC("childIndex", 3, FormCode.ChildIndex),
              NewFormOC("child", 3, FormCode.Child),
              NewFormOC("numOfChildren", 2, FormCode.NumOfChildren),
              NewFormOC("showAt", 3, FormCode.ShowAt),
              NewFormOC("show", 1, FormCode.Show),
              NewFormOC("hide", 1, FormCode.Hide),
              NewFormOC("lift", 2, FormCode.Lift),
              NewFormOC("detachGarnish", 1, FormCode.DetachGarnish),
              NewFormOC("beTarget", 3, FormCode.BeTarget),
              NewFormOC("setTargetValue", 3, FormCode.SetTargetValue),
              NewFormOC("attachTargetHit", 3, FormCode.AttachTargetHit),
              NewFormOC("attachTargetDrop", 3, FormCode.AttachTargetDrop)};
    ObLib.Register(NEW(PackageForm, name := "form", opCodes := opCodes));
    formException := NEW(ObValue.ValException, name := "form_failure");
    ObValue.InhibitTransmission(
      TYPECODE(ValForm), "forms cannot be transmitted/duplicated");
  END SetupForm;

PROCEDURE EvalForm (                    self  : PackageForm;
                                        opCode: ObLib.OpCode;
                    <*UNUSED*>          arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                    <*UNUSED*>          temp  : BOOLEAN;
                                        swr   : SynWr.T;
                                        loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    text1, text2, text3: TEXT;
    fv1                : FormsVBT.T;
    bool1              : BOOLEAN;
    int1, int2, index  : INTEGER;
    fun1               : ObValue.Val;
    vbt1               : VBT.T;
    color1             : Color.T;
    ch, toCh, p        : VBT.T;
  BEGIN
    TRY
      CASE NARROW(opCode, FormOpCode).code OF
      | FormCode.Error => RETURN formException;
      | FormCode.New =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          fv1 := NEW(Form, swr := swr).init(text1);
          RETURN NEW(ValForm, what := "<a FormsVBT.T>",
                     tag := "FormsVBT`T", picklable := FALSE, vbt := fv1);
      | FormCode.FromFile =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TRY
            fv1 := NEW(Form, swr := swr).initFromFile(text1);
          EXCEPT
          | Rd.Failure =>
              ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN NEW(ValForm, what := "<a FormsVBT.T>",
                     tag := "FormsVBT`T", picklable := FALSE, vbt := fv1);
      | FormCode.FromURL =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TRY
            fv1 := NEW(Form, swr := swr).initFromURL(text1);
          EXCEPT
          | Rd.Failure =>
              ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN NEW(ValForm, what := "<a FormsVBT.T>", picklable := FALSE,
                     tag := "FormsVBT`T", vbt := fv1);
      | FormCode.Attach =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValFun (node) => fun1 := node;
          ELSE
            ObValue.BadArgType(3, "procedure", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.Attach(
            fv1, text1, NEW(FormClosure, fun := fun1, swr := swr,
                            fv := args[1], location := loc));
          RETURN ObValue.valOk;
      | FormCode.PutGeneric =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ValVBT (node) => vbt1 := node.vbt;
          ELSE
            ObValue.BadArgType(3, "vbt", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE vbt1.parent OF
          | NULL =>
              FormsVBT.PutGeneric(fv1, text1, TranslateVBT.New(vbt1));
          | TranslateVBT.T (tv) => FormsVBT.PutGeneric(fv1, text1, tv);
          ELSE                   <*ASSERT FALSE*>(* shouldn't happen *)
          END;
          RETURN ObValue.valOk;
      | FormCode.PutColor =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ValColor (node) => color1 := node.color;
          ELSE
            ObValue.BadArgType(4, "color", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.PutColorProperty(fv1, text1, text2, color1);
          RETURN ObValue.valOk;
      | FormCode.GetBool =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          bool1 := FormsVBT.GetBooleanProperty(fv1, text1, text2);
          RETURN NEW(ObValue.ValBool, bool := bool1);
      | FormCode.PutBool =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(4, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.PutBooleanProperty(fv1, text1, text2, bool1);
          RETURN ObValue.valOk;
      | FormCode.GetInt =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Empty(text2) THEN
            int1 := FormsVBT.GetInteger(fv1, text1);
          ELSE
            int1 := FormsVBT.GetIntegerProperty(fv1, text1, text2);
          END;
          RETURN NEW(ObValue.ValInt, int := int1);
      | FormCode.PutInt =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(4, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Empty(text2) THEN
            FormsVBT.PutInteger(fv1, text1, int1);
          ELSE
            FormsVBT.PutIntegerProperty(fv1, text1, text2, int1);
          END;
          RETURN ObValue.valOk;
      | FormCode.GetText =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Empty(text2) THEN
            text3 := FormsVBT.GetText(fv1, text1);
          ELSE
            text3 := FormsVBT.GetTextProperty(fv1, text1, text2);
          END;
          RETURN ObValue.NewText(text3);
      | FormCode.PutText =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValText (node) => text3 := node.text;
          ELSE
            ObValue.BadArgType(4, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(5, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Empty(text2) THEN
            FormsVBT.PutText(fv1, text1, text3, bool1);
          ELSE
            FormsVBT.PutTextProperty(fv1, text1, text2, text3);
          END;
          RETURN ObValue.valOk;
      | FormCode.GetBoolean =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          bool1 := FormsVBT.GetBoolean(fv1, text1);
          RETURN NEW(ObValue.ValBool, bool := bool1);
      | FormCode.PutBoolean =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(3, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.PutBoolean(fv1, text1, bool1);
          RETURN ObValue.valOk;
      | FormCode.GetChoice =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          text2 := FormsVBT.GetChoice(fv1, text1);
          RETURN ObValue.NewText(text2);
      | FormCode.PutChoice =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Empty(text2) THEN
            FormsVBT.PutChoice(fv1, text1, NIL);
          ELSE
            FormsVBT.PutChoice(fv1, text1, text2);
          END;
          RETURN ObValue.valOk;
      | FormCode.GetReactivity =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF FormsVBT.IsActive(fv1, text1) THEN
            RETURN ObValue.NewText("active");
          ELSIF FormsVBT.IsPassive(fv1, text1) THEN
            RETURN ObValue.NewText("passive");
          ELSIF FormsVBT.IsDormant(fv1, text1) THEN
            RETURN ObValue.NewText("dormant");
          ELSIF FormsVBT.IsVanished(fv1, text1) THEN
            RETURN ObValue.NewText("vanished");
          ELSE
            RETURN ObValue.NewText("");
          END;
      | FormCode.PutReactivity =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Equal(text2, "active") THEN
            FormsVBT.MakeActive(fv1, text1);
          ELSIF Text.Equal(text2, "passive") THEN
            FormsVBT.MakePassive(fv1, text1);
          ELSIF Text.Equal(text2, "dormant") THEN
            FormsVBT.MakeDormant(fv1, text1);
          ELSIF Text.Equal(text2, "vanished") THEN
            FormsVBT.MakeVanish(fv1, text1);
          ELSE
            ObValue.BadArgVal(
              3, "a valid reactivity", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN ObValue.valOk;
      | FormCode.TakeFocus =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(3, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.TakeFocus(
            fv1, text1, FormsVBT.GetTheEventTime(fv1), bool1);
          RETURN ObValue.valOk;
      | FormCode.PopUp =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.PopUp(fv1, text1);
          RETURN ObValue.valOk;
      | FormCode.PopDown =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.PopDown(fv1, text1);
          RETURN ObValue.valOk;
      | FormCode.Insert =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(4, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              4, "non-negative", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          EVAL FormsVBT.Insert(fv1, text1, text2, int1);
          RETURN ObValue.valOk;
      | FormCode.InsertVBT =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ValVBT (node) => vbt1 := node.vbt;
          ELSE
            ObValue.BadArgType(3, "vbt", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(4, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              4, "non-negative", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.InsertVBT(fv1, text1, vbt1, int1);
          RETURN ObValue.valOk;
      | FormCode.ChildIndex =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          p := FormsVBT.GetVBT(fv1, text1);
          ch := FormsVBT.GetVBT(fv1, text2);
          IF (p = NIL) OR (ch = NIL) THEN
            ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TRY
            int1 := MultiSplit.Index(p, ch);
          EXCEPT
            MultiSplit.NotAChild =>
              ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValInt, int := int1);
      | FormCode.Child =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              3, "non-negative", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          p := FormsVBT.GetVBT(fv1, text1);
          ch := MultiSplit.Nth(p, int1);
          IF (p = NIL) OR (ch = NIL) THEN
            ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TRY
            text2 := FormsVBT.GetName(ch);
          EXCEPT
            FormsVBT.Error =>
              ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN ObValue.NewText(text2);
      | FormCode.NumOfChildren =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          p := FormsVBT.GetVBT(fv1, text1);
          IF p = NIL THEN
            ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TRY
            int1 := MultiSplit.NumChildren(p);
          EXCEPT
            MultiSplit.NotAChild =>
              ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValInt, int := int1);
      | FormCode.Move =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValText (node) => text3 := node.text;
          ELSE
            ObValue.BadArgType(4, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ObValue.ValBool (node) => bool1 := node.bool;
          ELSE
            ObValue.BadArgType(5, "bool", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Equal(text2, text3) THEN RETURN ObValue.valOk END;
          p := FormsVBT.GetVBT(fv1, text1);
          ch := FormsVBT.GetVBT(fv1, text2);
          IF Text.Empty(text3) THEN
            toCh := NIL
          ELSE
            toCh := FormsVBT.GetVBT(fv1, text3);
          END;
          IF (p = NIL) OR (ch = NIL)
               OR ((NOT Text.Empty(text3)) AND (toCh = NIL)) THEN
            ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TRY
            IF bool1 THEN toCh := MultiSplit.Pred(p, toCh) END;
            MultiSplit.Move(p, toCh, ch);
          EXCEPT
            MultiSplit.NotAChild =>
              ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          RETURN ObValue.valOk;
      | FormCode.Delete =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          p := FormsVBT.GetVBT(fv1, text1);
          ch := FormsVBT.GetVBT(fv1, text2);
          IF (p = NIL) OR (ch = NIL) THEN
            ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TRY
            index := MultiSplit.Index(p, ch);
          EXCEPT
            MultiSplit.NotAChild =>
              ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.Delete(fv1, text1, index, 1);
          RETURN ObValue.valOk;
      | FormCode.DeleteVBT =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ValVBT (node) => vbt1 := node.vbt;
          ELSE
            ObValue.BadArgType(3, "vbt", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          p := FormsVBT.GetVBT(fv1, text1);
          IF (p = NIL) OR (vbt1 = NIL) THEN
            ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TRY
            MultiSplit.Delete(p, vbt1);
          EXCEPT
            MultiSplit.NotAChild =>
              ObValue.RaiseException(formException, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN ObValue.valOk;
      | FormCode.DeleteRange =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValInt (node) => int2 := node.int;
          ELSE
            ObValue.BadArgType(4, "int", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              3, "non-negative", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF int2 < 0 THEN
            ObValue.BadArgVal(
              4, "non-negative", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          FormsVBT.Delete(fv1, text1, int1, int2);
          RETURN ObValue.valOk;
      | FormCode.Show =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Trestle.Install(fv1);
          RETURN ObValue.valOk;
      | FormCode.ShowAt =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          IF Text.Empty(text1) THEN
            Trestle.Install(fv1);
          ELSE
            Trestle.Install(v := fv1, trsl := Trestle.Connect(text1),
                            windowTitle := text2, iconTitle := text2);
          END;
          RETURN ObValue.valOk;
      | FormCode.Hide =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          Trestle.Delete(fv1);
          RETURN ObValue.valOk;
      | FormCode.Lift =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          ch := FormsVBT.GetVBT(fv1, text1);
          IF ch = NIL THEN
            ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
          END;
          ZSplit.Lift(ch);
          RETURN ObValue.valOk;
      | FormCode.DetachGarnish =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          WITH v = MultiFilter.Child(fv1) DO
            TRY
              Split.Delete(v.parent, v);
            EXCEPT
              Split.NotAChild =>
                ObValue.RaiseException(formException, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
            RETURN NEW(ValVBT, what := "<a VBT.T>", picklable := FALSE,
                       tag := "VBT`T", vbt := v);
          END;
      | FormCode.BeTarget =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          vbt1 := FormsVBT.GetVBT(fv1, text1);
          IF vbt1 = NIL THEN
            ObValue.RaiseException(formException, opCode.name, loc);
            <* ASSERT FALSE *>
          END;
          IF Text.Equal(text2, "invert") THEN
            SourceVBT.BeTarget(vbt1, SourceVBT.NewTarget());
            RETURN ObValue.valOk;
          ELSIF Text.Equal(text2, "grid") THEN
            SourceVBT.BeTarget(vbt1, SourceVBT.NewSwapTarget());
            RETURN ObValue.valOk;
          ELSE
            ObValue.BadArgType(
              3, "\"invert\" or \"grid \"", self.name, opCode.name, loc);
            <* ASSERT FALSE *>
          END;
      | FormCode.SetTargetValue =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE FormsVBT.GetVBT(fv1, text1) OF
          | NULL =>
              ObValue.RaiseException(formException, opCode.name, loc);
            <* ASSERT FALSE *>
          | Target (target) => target.val := args[3]; RETURN ObValue.valOk;
          ELSE
            ObValue.RaiseException(formException, opCode.name, loc);
            <* ASSERT FALSE *>
          END;
      | FormCode.AttachTargetHit =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE FormsVBT.GetVBT(fv1, text1) OF
          | NULL =>
              ObValue.RaiseException(formException, opCode.name, loc);
            <* ASSERT FALSE *>
          | Source (source) =>
              source.hitProc := args[3];
              RETURN ObValue.valOk;
          ELSE
            ObValue.RaiseException(formException, opCode.name, loc);
            <* ASSERT FALSE *>
          END;
      | FormCode.AttachTargetDrop =>
          TYPECASE args[1] OF
          | ValForm (node) => fv1 := node.vbt;
          ELSE
            ObValue.BadArgType(1, "form", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc); <*ASSERT FALSE*>
          END;
          TYPECASE FormsVBT.GetVBT(fv1, text1) OF
          | NULL =>
              ObValue.RaiseException(formException, opCode.name, loc);
            <* ASSERT FALSE *>
          | Source (source) =>
              source.dropProc := args[3];
              RETURN ObValue.valOk;
          ELSE
            ObValue.RaiseException(formException, opCode.name, loc);
            <* ASSERT FALSE *>
          END;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc); <*ASSERT FALSE*>
      END;
    EXCEPT
    | FormsVBT.Error, FormsVBT.Unimplemented, TrestleComm.Failure =>
        ObValue.RaiseException(formException, opCode.name, loc); <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc); <*ASSERT FALSE*>
    END;
  END EvalForm;

BEGIN
END ObLibUI.
