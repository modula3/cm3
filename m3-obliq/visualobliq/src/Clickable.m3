(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Aug 24 13:15:07 PDT 1994 by bharat *)
(*      modified on Tue Oct 26 01:56:06 PDT 1993 by mhb    *)
<* PRAGMA LL *>

MODULE Clickable;

IMPORT FormsVBT, Split, NodeVBT, Text, VBT, Rd, Wr, RW;

REVEAL
  ButtonNode = NodeVBT.Widget BRANDED "VO-ButtonNode" OBJECT
                 textLabel: BOOLEAN := TRUE;
                 text     : TEXT    := "Label";
                 pixmap   : TEXT    := "";
                 Guard    : BOOLEAN := FALSE;
                 Trill    : BOOLEAN := FALSE;
               OVERRIDES
                 loadAttributes := BuLoadAttributes;
                 (* checkAttributes := BuCheckAttributes;*)
                 applyAttributes := BuApplyAttributes;
                 computeSX       := BuComputeSX;
                 save                       := BuSave;
                 load                       := BuLoad;
                 initObliqAttrs       := BuObAttrs;
               END;


  BooleanNode = NodeVBT.Widget BRANDED "VO-BooleanNode" OBJECT
                  textLabel    : BOOLEAN := TRUE;
                  text         : TEXT    := "Label";
                  pixmap       : TEXT    := "";
                  initialValue : BOOLEAN := FALSE;
                  feedbackStyle: TEXT    := "CheckBox";
                  (* may be "CheckBox", "CheckMark", "Inverted" *)
                OVERRIDES
                  loadAttributes  := BoLoadAttributes;
                  applyAttributes := BoApplyAttributes;
                  computeSX       := BoComputeSX;
                  save                       := BoSave;
                  load                       := BoLoad;
                  initObliqAttrs       := BoObAttrs;
                END;

  ChoiceNode = NodeVBT.Widget BRANDED "VO-ChoiceNode" OBJECT
                 textLabel    : BOOLEAN := TRUE;
                 text         : TEXT    := "Label";
                 pixmap       : TEXT    := "";
                 initialValue : BOOLEAN := FALSE;
                 feedbackStyle: TEXT    := "CheckBox";
                 (* may be "CheckBox", "CheckMark", "Inverted" *)
               OVERRIDES
                 loadAttributes  := ChLoadAttributes;
                 applyAttributes := ChApplyAttributes;
                 computeSX       := ChComputeSX;
                 save                       := ChSave;
                 load                       := ChLoad;
                 initObliqAttrs       := ChObAttrs;
               END;

<* FATAL FormsVBT.Error,FormsVBT.Unimplemented, Split.NotAChild *>

PROCEDURE ButtonConstructor (): NodeVBT.T =
  BEGIN
    RETURN
      NEW(ButtonNode, BgColor := "RatherLightYellow", FgColor := "Black",
          Rim := 0, Border := 1, Font := "-*-helvetica-bold-*R-*120-*",
          width := 50, height := 20, Embellishment := "None",
          ResizeModel := "CenterPin");
  END ButtonConstructor;

PROCEDURE BooleanConstructor (): NodeVBT.T =
  BEGIN
    RETURN
      NEW(BooleanNode, BgColor := "Grey90", FgColor := "Black",
          Rim := 0, Border := 1, Font := "-*-helvetica-bold-*R-*120-*",
          width := 50, height := 20, Embellishment := "None",
          ResizeModel := "CenterPin");
  END BooleanConstructor;

PROCEDURE ChoiceConstructor (): NodeVBT.T =
  BEGIN
    RETURN
      NEW(ChoiceNode, BgColor := "Grey90", FgColor := "Black",
          Rim := 0, Border := 0, Font := "-*-helvetica-bold-*R-*120-*",
          width := 50, height := 20, Embellishment := "None",
          ResizeModel := "CenterPin");
  END ChoiceConstructor;

(* Button support procs *)

PROCEDURE BuLoadAttributes (nv: ButtonNode; as: FormsVBT.T) =
  BEGIN
    IF nv.textLabel THEN
      FormsVBT.PutChoice(as, "buRadio", "buTChoice")
    ELSE
      FormsVBT.PutChoice(as, "buRadio", "buPChoice")
    END;
    FormsVBT.PutText(as, "buText", nv.text, FALSE);
    FormsVBT.PutText(as, "buPix", nv.pixmap, FALSE);
    FormsVBT.PutBoolean(as, "buGuard", nv.Guard);
    FormsVBT.PutBoolean(as, "buTrill", nv.Trill);
    NodeVBT.T.loadAttributes(nv, as);
  END BuLoadAttributes;

(*
PROCEDURE BuCheckAttributes (    nv   : ButtonNode;
                                 as   : FormsVBT.T;
                             VAR error: TEXT        ): BOOLEAN =
  BEGIN
    IF NOT NodeVBT.T.checkAttributes(nv, as, error) THEN RETURN FALSE END;

    IF Text.Equal(FormsVBT.GetChoice(as, "buRadio"), "buPChoice") THEN
      (* check pixmap *)
      WITH pixmap = FormsVBT.GetText(as, "buPix") DO
        TRY
          EVAL GetRawImage(pixmap)
        EXCEPT
          Error (msg) => error := "Invalid Pixmap" & msg; RETURN FALSE
        ELSE
        END
      END
    END;
    RETURN TRUE;
  END BuCheckAttributes;
*)
PROCEDURE BuApplyAttributes (nv: ButtonNode; as: FormsVBT.T) =
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);
    nv.text := FormsVBT.GetText(as, "buText");
    nv.pixmap := FormsVBT.GetText(as, "buPix");
    nv.textLabel :=
      Text.Equal(FormsVBT.GetChoice(as, "buRadio"), "buTChoice");
    nv.Guard := FormsVBT.GetBoolean(as, "buGuard");
    nv.Trill := FormsVBT.GetBoolean(as, "buTrill");
  END BuApplyAttributes;

PROCEDURE BuComputeSX (nv: ButtonNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN
    IF nv.Guard THEN
      nv.DialogSX :=
        NodeVBT.FindAndReplace(NodeVBT.FindAndReplace(
                                 nv.DialogSX, "GuardHeader",
                                 "(Guard %@guard "), "GuardFooter", ")");
    ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(NodeVBT.FindAndReplace(
                                              nv.DialogSX, "GuardHeader",
                                              ""), "GuardFooter", "");
    END;

    IF nv.Trill THEN
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Trill", "Trill")
    ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Trill", "")
    END;

    IF nv.textLabel THEN
      nv.DialogSX :=
        NodeVBT.FindAndReplace(
          nv.DialogSX, "ButtonContents", "Text %@text \"" & nv.text & "\"")
    ELSE
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "ButtonContents",
                               "Pixmap Accurate %@pixmap \"" & nv.pixmap & "\"")
    END;

    RETURN NodeVBT.T.computeSX(nv, Final);

  END BuComputeSX;

PROCEDURE  BuObAttrs (nv: ButtonNode): TEXT =
  VAR code := "";
  BEGIN
    code := NodeVBT.BoolAttr("textLabel", nv.textLabel) &
                NodeVBT.TextAttr("textString", nv.text) &
                NodeVBT.TextAttr("pixmap", nv.pixmap) &
                NodeVBT.BoolAttr("Guard", nv.Guard) &
                NodeVBT.BoolAttr("Trill", nv.Trill);
    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END BuObAttrs; 

(* Boolean support procs *)


PROCEDURE BoLoadAttributes (nv: BooleanNode; as: FormsVBT.T) =
  BEGIN
    NodeVBT.T.loadAttributes(nv, as);
    FormsVBT.PutBoolean(as, "boInitial", nv.initialValue);
    FormsVBT.PutChoice(as, "Booleanfbs", "B" & nv.feedbackStyle);
    IF nv.textLabel THEN
      FormsVBT.PutChoice(as, "boRadio", "boTChoice")
    ELSE
      FormsVBT.PutChoice(as, "boRadio", "boPChoice")
    END;
    FormsVBT.PutText(as, "boText", nv.text, FALSE);
    FormsVBT.PutText(as, "boPix", nv.pixmap, FALSE);
    nv.text := FormsVBT.GetText(as, "boText");
    nv.pixmap := FormsVBT.GetText(as, "boPix");
    nv.textLabel :=
      Text.Equal(FormsVBT.GetChoice(as, "boRadio"), "boTChoice");
  END BoLoadAttributes;

PROCEDURE BoApplyAttributes (nv: BooleanNode; as: FormsVBT.T) =
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);
    nv.initialValue := FormsVBT.GetBoolean(as, "boInitial");
    nv.feedbackStyle := Text.Sub(FormsVBT.GetChoice(as, "Booleanfbs"), 1);
    nv.text := FormsVBT.GetText(as, "boText");
    nv.pixmap := FormsVBT.GetText(as, "boPix");
    nv.textLabel :=
      Text.Equal(FormsVBT.GetChoice(as, "boRadio"), "boTChoice");

  END BoApplyAttributes;

PROCEDURE BoComputeSX (nv: BooleanNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "fbstyle", nv.feedbackStyle);
    IF nv.initialValue THEN
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "initValue", "TRUE")
    ELSE
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "initValue", "FALSE")
    END;

    IF nv.textLabel THEN
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "BooleanContents",
                               "Text %@text \"" & nv.text & "\"")
    ELSE
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "BooleanContents",
                               "Pixmap %@pixmap \"" & nv.pixmap & "\"")
    END;

    RETURN NodeVBT.T.computeSX(nv, Final);

  END BoComputeSX;

PROCEDURE  BoObAttrs(nv: BooleanNode) : TEXT =
  VAR code := "";
  BEGIN
    code := NodeVBT.BoolAttr("textLabel", nv.textLabel) &
                NodeVBT.TextAttr("textString", nv.text) &
                NodeVBT.TextAttr("feedbackStyle", nv.feedbackStyle) &
                NodeVBT.BoolAttr("initialValue", nv.initialValue);
    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END BoObAttrs;

(* Choice support procs *)

PROCEDURE ChLoadAttributes (nv: ChoiceNode; as: FormsVBT.T) =
  BEGIN
    NodeVBT.T.loadAttributes(nv, as);
    FormsVBT.PutBoolean(as, "chInitial", nv.initialValue);
    FormsVBT.PutChoice(as, "Choicefbs", "C" & nv.feedbackStyle);
    IF nv.textLabel THEN
      FormsVBT.PutChoice(as, "chRadio", "chTChoice")
    ELSE
      FormsVBT.PutChoice(as, "chRadio", "chPChoice")
    END;
    FormsVBT.PutText(as, "chText", nv.text, FALSE);
    FormsVBT.PutText(as, "chPix", nv.pixmap, FALSE);
  END ChLoadAttributes;

PROCEDURE ChApplyAttributes (nv: ChoiceNode; as: FormsVBT.T) =
  VAR child: VBT.T;
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);
    nv.initialValue := FormsVBT.GetBoolean(as, "chInitial");
    nv.feedbackStyle := Text.Sub(FormsVBT.GetChoice(as, "Choicefbs"), 1);
    IF nv.initialValue THEN
      (* Since this choice is being set turn off all other siblings *)
      WITH parent = nv.parent DO (* turn off all siblings *)
        (* parent is a NodeVBT.TYPE and hence a ZHandleVBT.T and hence a
           ZSplit.T *)
        child := Split.Succ(parent, NIL); (* Top *)
        WHILE child # NIL DO
          IF ISTYPE(child, ChoiceNode) THEN
            WITH kid   = NARROW(child, ChoiceNode),
                 sonny = NARROW(kid.getchild(), FormsVBT.T) DO
              FormsVBT.PutBoolean(sonny, kid.name, FALSE);
              kid.initialValue := FALSE;
            END
          END;
          child := Split.Succ(parent, child);
        END
      END;
      nv.initialValue := TRUE;   (*since we would have turned this off as
                                    well*)
    END;
    nv.text := FormsVBT.GetText(as, "chText");
    nv.pixmap := FormsVBT.GetText(as, "chPix");
    nv.textLabel :=
      Text.Equal(FormsVBT.GetChoice(as, "chRadio"), "chTChoice");
  END ChApplyAttributes;

PROCEDURE ChComputeSX (nv: ChoiceNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN
    IF NOT Final THEN
      nv.DialogSX := "(Radio \n" & nv.DialogSX & ")\n"
    END;
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "fbstyle", nv.feedbackStyle);

    IF nv.initialValue THEN
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "initValue", "TRUE")
    ELSE
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "initValue", "FALSE")
    END;

    IF nv.textLabel THEN
      nv.DialogSX :=
        NodeVBT.FindAndReplace(
          nv.DialogSX, "ChoiceContents", "Text %@text \"" & nv.text & "\"")
    ELSE
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "ChoiceContents",
                               "Pixmap %@pixmap \"" & nv.pixmap & "\"")
    END;


    RETURN NodeVBT.T.computeSX(nv, Final);

  END ChComputeSX;

PROCEDURE  ChObAttrs(nv: ChoiceNode) : TEXT =
  VAR code := "";
  BEGIN
    code := NodeVBT.BoolAttr("textLabel", nv.textLabel) &
                NodeVBT.TextAttr("textString", nv.text) &
                NodeVBT.TextAttr("pixmap", nv.pixmap) &
                NodeVBT.TextAttr("feedbackStyle", nv.feedbackStyle) &
                NodeVBT.BoolAttr("initialValue", nv.initialValue);
    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END ChObAttrs;

PROCEDURE BuSave (nv: ButtonNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wbool(s, nv.textLabel);
    RW.wtext(s, nv.text);
    RW.wtext(s, nv.pixmap);
    RW.wbool(s, nv.Guard);
    RW.wbool(s, nv.Trill);
  END BuSave;
PROCEDURE BuLoad (nv: ButtonNode; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rbool(s, nv.textLabel);
    RW.rtext(s, nv.text);
    RW.rtext(s, nv.pixmap);
    RW.rbool(s, nv.Guard);
    RW.rbool(s, nv.Trill);
  END BuLoad;

PROCEDURE BoSave(nv: BooleanNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wbool(s, nv.textLabel);
    RW.wtext(s, nv.text);
    RW.wtext(s, nv.pixmap);
    RW.wbool(s, nv.initialValue);
    RW.wtext(s, nv.feedbackStyle);
  END BoSave;
PROCEDURE BoLoad(nv: BooleanNode; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rbool(s, nv.textLabel);
    RW.rtext(s, nv.text);
    RW.rtext(s, nv.pixmap);
    RW.rbool(s, nv.initialValue);
    RW.rtext(s, nv.feedbackStyle);
  END BoLoad;

PROCEDURE ChSave(nv: ChoiceNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wbool(s, nv.textLabel);
    RW.wtext(s, nv.text);
    RW.wtext(s, nv.pixmap);
    RW.wbool(s, nv.initialValue);
    RW.wtext(s, nv.feedbackStyle);
  END ChSave;
PROCEDURE ChLoad(nv: ChoiceNode; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rbool(s, nv.textLabel);
    RW.rtext(s, nv.text);
    RW.rtext(s, nv.pixmap);
    RW.rbool(s, nv.initialValue);
    RW.rtext(s, nv.feedbackStyle);
  END ChLoad;

PROCEDURE Initialize () =
  BEGIN
    EVAL NodeVBT.Register("button", ButtonConstructor);
    EVAL NodeVBT.Register("boolean", BooleanConstructor);
    EVAL NodeVBT.Register("choice", ChoiceConstructor);
  END Initialize;

BEGIN
END Clickable.





