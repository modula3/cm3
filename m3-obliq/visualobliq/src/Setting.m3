(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Aug 24 15:27:30 PDT 1994 by bharat *)
(*      modified on Thu Mar 10 12:52:29 PST 1994 by mhb    *)
<* PRAGMA LL *>

MODULE Setting;

IMPORT Attributes, FormsVBT, Fmt, NodeVBT, VBT,Rd, Wr, RW;

REVEAL
  NumericNode = NodeVBT.Widget BRANDED "VO-NumericNode" OBJECT
                  numAllowEmpty : BOOLEAN := FALSE;
                  numHideButtons: BOOLEAN := FALSE;
                  Min           : INTEGER := -100;
                  Max           : INTEGER := 100;
                  Val           : INTEGER := 0;
                  (* constraint is Min <= Val <= Max *)
                  (* This imposes the dynamic constraints : *)
                  (* numMin.Max = numVal.Val = numMax.Min *)
                  (* numVal.Min = numMin.Val *)
                  (* numVal.Max = numMax.Val *)
                  (* When *.Val changes ranges are affected *)
                OVERRIDES
                  loadAttributes  := NumLoadAttributes;
                  applyAttributes := NumApplyAttributes;
                  computeSX := NumComputeSX;
                  save                       := NumSave;
                  load                       := NumLoad;
                  initObliqAttrs       := NumObAttrs;
                END;

  ScrollerNode = NodeVBT.Widget BRANDED "VO-ScrollerNode" OBJECT
                   Min  : INTEGER  := -50;
                   Max  : INTEGER  := 50;
                   Val  : INTEGER  := 0;
                   Thumb: CARDINAL := 0;
                   Step : CARDINAL := 1;
                   (* Dynamic Constraints : *)
                   (* scrMin.Max = scrVal.Val *)
                   (* scrMax.Min = scrVal.Val + scrThu.Val *)
                   (* scrVal.Min = scrMin.Val *)
                   (* scrVal.Max = scrMax.Val - scrThu.Val *)
                   (* scrThu.Max = scrMax.Val - scrMin.Val *)

                 OVERRIDES
                   loadAttributes  := ScrLoadAttributes;
                   applyAttributes := ScrApplyAttributes;
                   computeSX       := ScrComputeSX;
                   save                       := ScrSave;
                   load                       := ScrLoad;
                   initObliqAttrs       := ScrObAttrs;
                 END;

  HScrollerNode = ScrollerNode BRANDED "VO-HScrollerNode" OBJECT END;

  VScrollerNode = ScrollerNode BRANDED "VO-VScrollerNode" OBJECT END;

<* FATAL FormsVBT.Error,FormsVBT.Unimplemented*>

PROCEDURE NumericConstructor (): NodeVBT.T =
  BEGIN
    RETURN
      NEW(NumericNode, BgColor := "Grey85", FgColor := "Black",
          Rim := 0, Border := 0,
          Font := "-*-helvetica-bold-*R-*120-*", width := 70,
          height := 30, Embellishment := "Flat",
          ResizeModel := "CenterPin");
  END NumericConstructor;

PROCEDURE HScrollerConstructor (): NodeVBT.T =
  BEGIN
    RETURN NEW(HScrollerNode, BgColor := "PaleGray", FgColor := "Black",
               Rim := 0, Border := 1, width := 30, height := 15,
               Embellishment := "Lowered", ResizeModel := "HScaled");
  END HScrollerConstructor;

PROCEDURE VScrollerConstructor (): NodeVBT.T =
  BEGIN
    RETURN NEW(VScrollerNode, BgColor := "PaleGray", FgColor := "Black",
               Rim := 0, Border := 1, width := 15, height := 30,
               Embellishment := "Lowered", ResizeModel := "VScaled");
  END VScrollerConstructor;

(* Numeric support procs *************)

PROCEDURE EnforceConstraints (min, val, max: INTEGER; as: FormsVBT.T) =
  BEGIN
    FormsVBT.PutIntegerProperty(as, "numMin", "Max", val);
    FormsVBT.PutIntegerProperty(as, "numMax", "Min", val);
    FormsVBT.PutIntegerProperty(as, "numVal", "Min", min);
    FormsVBT.PutIntegerProperty(as, "numVal", "Max", max);
  END EnforceConstraints;

PROCEDURE NumLoadAttributes (nv: NumericNode; as: FormsVBT.T) =
  BEGIN
    FormsVBT.PutBoolean(as, "numAllowEmpty", nv.numAllowEmpty);
    FormsVBT.PutBoolean(as, "numHideButtons", nv.numHideButtons);

    FormsVBT.PutInteger(as, "numMin", nv.Min);
    FormsVBT.PutInteger(as, "numVal", nv.Val);
    FormsVBT.PutInteger(as, "numMax", nv.Max);

    (* enforce constraints *)
    FormsVBT.PutIntegerProperty(as, "numMin", "Min", FIRST(INTEGER));
    FormsVBT.PutIntegerProperty(as, "numMax", "Max", LAST(INTEGER));
    EnforceConstraints(nv.Min, nv.Val, nv.Max, as);
    NodeVBT.T.loadAttributes(nv, as);

  END NumLoadAttributes;

PROCEDURE NumApplyAttributes (nv: NumericNode; as: FormsVBT.T) =
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);
    nv.numAllowEmpty := FormsVBT.GetBoolean(as, "numAllowEmpty");
    nv.numHideButtons := FormsVBT.GetBoolean(as, "numHideButtons");
    nv.Min := FormsVBT.GetInteger(as, "numMin");
    nv.Val := FormsVBT.GetInteger(as, "numVal");
    nv.Max := FormsVBT.GetInteger(as, "numMax");
  END NumApplyAttributes;

PROCEDURE  NumObAttrs (nv: NumericNode): TEXT =
VAR code := "";
  BEGIN
    code := NodeVBT.BoolAttr("AllowEmpty", nv.numAllowEmpty) &
                NodeVBT.BoolAttr("HideButtons", nv.numHideButtons) &
                NodeVBT.IntAttr("Min", nv.Min) &
                NodeVBT.IntAttr("Max", nv.Max) &
                NodeVBT.IntAttr("Val", nv.Val);
    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END NumObAttrs;

PROCEDURE numProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                afv : FormsVBT.T;
                   <* UNUSED *> name: TEXT;
                   <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    WITH min = FormsVBT.GetInteger(afv, "numMin"),
         val = FormsVBT.GetInteger(afv, "numVal"),
         max = FormsVBT.GetInteger(afv, "numMax")  DO
      EnforceConstraints(min, val, max, afv);
    END
  END numProc;


PROCEDURE NumComputeSX (nv: NumericNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Value", Fmt.Int(nv.Val));
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Min", Fmt.Int(nv.Min));
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Max", Fmt.Int(nv.Max));
    IF nv.numAllowEmpty THEN
      nv.DialogSX :=
          NodeVBT.FindAndReplace(nv.DialogSX, "AllowEmpty", "AllowEmpty")
    ELSE
      nv.DialogSX :=
          NodeVBT.FindAndReplace(nv.DialogSX, "AllowEmpty", "")
    END;
    IF nv.numHideButtons THEN
      nv.DialogSX :=
          NodeVBT.FindAndReplace(nv.DialogSX, "HideButtons", "HideButtons")
    ELSE
      nv.DialogSX :=
          NodeVBT.FindAndReplace(nv.DialogSX, "HideButtons", "")
    END;
    RETURN NodeVBT.T.computeSX(nv, Final);
  END NumComputeSX;

(* Scroller support procs *************)

PROCEDURE EnforceConstraints2 (min, val, max, thumb: INTEGER;
                               as                  : FormsVBT.T) =
  BEGIN
    FormsVBT.PutIntegerProperty(as, "scrMin", "Max", val);
    FormsVBT.PutIntegerProperty(as, "scrMax", "Min", val + thumb);
    FormsVBT.PutIntegerProperty(as, "scrVal", "Min", min);
    FormsVBT.PutIntegerProperty(as, "scrVal", "Max", max - thumb);
    FormsVBT.PutIntegerProperty(as, "scrThu", "Max", max - min);
  END EnforceConstraints2;

PROCEDURE ScrLoadAttributes (nv: ScrollerNode; as: FormsVBT.T) =
  BEGIN

    FormsVBT.PutInteger(as, "scrMin", nv.Min);
    FormsVBT.PutInteger(as, "scrVal", nv.Val);
    FormsVBT.PutInteger(as, "scrMax", nv.Max);

    FormsVBT.PutInteger(as, "scrThu", nv.Thumb);
    FormsVBT.PutInteger(as, "scrStep", nv.Step);

    (* enforce constraints *)
    FormsVBT.PutIntegerProperty(as, "scrMin", "Min", FIRST(INTEGER));
    FormsVBT.PutIntegerProperty(as, "scrMax", "Max", LAST(INTEGER));
    FormsVBT.PutIntegerProperty(as, "scrThu", "Min", 0);
    FormsVBT.PutIntegerProperty(as, "scrStep", "Min", 0);
    FormsVBT.PutIntegerProperty(as, "scrStep", "Max", LAST(INTEGER));

    EnforceConstraints2(nv.Min, nv.Val, nv.Max, nv.Thumb, as);
    NodeVBT.T.loadAttributes(nv, as);

  END ScrLoadAttributes;

PROCEDURE ScrApplyAttributes (nv: ScrollerNode; as: FormsVBT.T) =
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);


    nv.Min := FormsVBT.GetInteger(as, "scrMin");
    nv.Val := FormsVBT.GetInteger(as, "scrVal");
    nv.Max := FormsVBT.GetInteger(as, "scrMax");
    nv.Thumb := FormsVBT.GetInteger(as, "scrThu");
    nv.Step := FormsVBT.GetInteger(as, "scrStep");

  END ScrApplyAttributes;

PROCEDURE  ScrObAttrs (nv: ScrollerNode): TEXT =
VAR code := "";
  BEGIN
    code :=  NodeVBT.IntAttr("Thumb", nv.Thumb) &
                NodeVBT.IntAttr("Step", nv.Step) &
                NodeVBT.IntAttr("Min", nv.Min) &
                NodeVBT.IntAttr("Max", nv.Max) &
                NodeVBT.IntAttr("Val", nv.Val);
    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END ScrObAttrs;


PROCEDURE ScrComputeSX (nv: ScrollerNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN

    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Value", Fmt.Int(nv.Val));
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Min", Fmt.Int(nv.Min));
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Max", Fmt.Int(nv.Max));
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Thumb", Fmt.Int(nv.Thumb));
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Step", Fmt.Int(nv.Step));
    RETURN NodeVBT.T.computeSX(nv, Final);
  END ScrComputeSX;

PROCEDURE scrProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                afv : FormsVBT.T;
                   <* UNUSED *> name: TEXT;
                   <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    WITH min   = FormsVBT.GetInteger(afv, "scrMin"),
         val   = FormsVBT.GetInteger(afv, "scrVal"),
         max   = FormsVBT.GetInteger(afv, "scrMax"),
         thumb = FormsVBT.GetInteger(afv, "scrThu")  DO
      EnforceConstraints2(min, val, max, thumb, afv);
    END
  END scrProc;

PROCEDURE NumSave (nv: NumericNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wbool(s, nv.numAllowEmpty);
    RW.wbool(s, nv.numHideButtons);
    RW.wint(s, nv.Min);
    RW.wint(s, nv.Max);
    RW.wint(s, nv.Val);
  END NumSave;

PROCEDURE NumLoad (nv: NumericNode; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rbool(s, nv.numAllowEmpty);
    RW.rbool(s, nv.numHideButtons);
    RW.rint(s, nv.Min);
    RW.rint(s, nv.Max);
    RW.rint(s, nv.Val);
  END NumLoad;

PROCEDURE ScrSave (nv: ScrollerNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wint(s, nv.Min);
    RW.wint(s, nv.Max);
    RW.wint(s, nv.Val);
    RW.wcard(s, nv.Thumb);
    RW.wcard(s, nv.Step);
  END ScrSave;

PROCEDURE ScrLoad (nv: ScrollerNode; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rint(s, nv.Min);
    RW.rint(s, nv.Max);
    RW.rint(s, nv.Val);
    RW.rcard(s, nv.Thumb);
    RW.rcard(s, nv.Step);
  END ScrLoad;

PROCEDURE Initialize () =
  BEGIN
    EVAL NodeVBT.Register("numeric", NumericConstructor);
    EVAL
      NodeVBT.Register(
        "hscroll", HScrollerConstructor, attrsheetName := "scrolleratt");
    EVAL
      NodeVBT.Register(
        "vscroll", VScrollerConstructor, attrsheetName := "scrolleratt");

    (* numeric attachments *)
    WITH                         (* to enforce constraints *)
      numclosure = NEW(FormsVBT.Closure, apply := numProc) DO
      FormsVBT.Attach(Attributes.afv, "numMin", numclosure);
      FormsVBT.Attach(Attributes.afv, "numMax", numclosure);
      FormsVBT.Attach(Attributes.afv, "numVal", numclosure);
    END;

    (* scroller attachments *)
    WITH                         (* to enforce constraints *)
      scrclosure = NEW(FormsVBT.Closure, apply := scrProc) DO
      FormsVBT.Attach(Attributes.afv, "scrMin", scrclosure);
      FormsVBT.Attach(Attributes.afv, "scrMax", scrclosure);
      FormsVBT.Attach(Attributes.afv, "scrVal", scrclosure);
      FormsVBT.Attach(Attributes.afv, "scrThu", scrclosure);
    END;

  END Initialize;

BEGIN
END Setting.






