(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Feb  1 09:44:45 PST 1995 by kalsow *)
(*      modified on Wed Aug 24 14:47:56 PDT 1994 by bharat *)
(*      modified on Tue Oct 26 02:24:47 PDT 1993 by mhb    *)
<* PRAGMA LL *>

MODULE Textual;

IMPORT Attributes, FormsVBT, NodeVBT, Text, VBT, Rd, Wr, RW;

REVEAL
  TextEditNode = NodeVBT.Widget BRANDED "VO-TextEditNode" OBJECT
                   teReadOnly    : BOOLEAN := FALSE;
                   teClip        : BOOLEAN := FALSE;
                   teHasScrollbar: BOOLEAN := TRUE;
                   teContents    : TEXT    := "";
                   teFromFile    : TEXT    := "";
                   getFromFile   : BOOLEAN := FALSE;
                 OVERRIDES
                   loadAttributes  := TeLoadAttributes;
                   applyAttributes := TeApplyAttributes;
                   computeSX       := TeComputeSX;
                   save                       := TeSave;
                   load                       := TeLoad;
                   initObliqAttrs       := TeObAttrs;
                 END;

  TypeInNode = NodeVBT.Widget BRANDED "VO-TypeInNode" OBJECT
                 tyReadOnly: BOOLEAN := FALSE;
                 tyExpand  : BOOLEAN := FALSE;
                 tyInit    : TEXT    := "";
               OVERRIDES
                 loadAttributes  := TyLoadAttributes;
                 applyAttributes := TyApplyAttributes;
                 computeSX       := TyComputeSX;
                 save                       := TySave;
                 load                       := TyLoad;
                 initObliqAttrs       := TyObAttrs;
               END;

  TextNode = NodeVBT.Widget BRANDED "VO-TextNode" OBJECT
               tOrientation: TEXT := "Center";
               tVal        : TEXT := "Text";
             OVERRIDES
               loadAttributes  := TLoadAttributes;
               applyAttributes := TApplyAttributes;
               computeSX       := TComputeSX;
               save                       := TSave;
               load                       := TLoad;
               initObliqAttrs       := TObAttrs;
             END;

<* FATAL FormsVBT.Error,FormsVBT.Unimplemented *>

PROCEDURE TextConstructor (): NodeVBT.T =
  BEGIN
    RETURN NEW(TextNode, BgColor := "Grey75",
               FgColor := "Black", 
               Rim := 0,
               Border := 0, Font := "-*-helvetica-bold-*R-*120-*",
               width := 50, height := 20,
               Embellishment := "Flat");
  END TextConstructor;

PROCEDURE TextEditConstructor (): NodeVBT.T =
  BEGIN
    RETURN NEW(TextEditNode, BgColor := "VeryLightYellow", FgColor := "Black",
               Rim := 0, Border :=1,
               Font := "-*-helvetica-bold-*R-*120-*", width := 50,
               height := 50, Embellishment := "Lowered");
  END TextEditConstructor;

PROCEDURE TypeInConstructor (): NodeVBT.T =
  BEGIN
    RETURN NEW(TypeInNode, BgColor := "LightGrey75", FgColor := "Black",
               Rim := 0, Border := 0, Font := "-*-helvetica-bold-*R-*120-*",
               width := 50,
               height := 20, Embellishment := "Lowered",
               ResizeModel := "HScaled");
  END TypeInConstructor;



(* TextEdit support procs *)

PROCEDURE TeLoadAttributes (nv: TextEditNode; as: FormsVBT.T) =
  BEGIN
    FormsVBT.PutBoolean(as, "teReadOnly", nv.teReadOnly);
    FormsVBT.PutBoolean(as, "teClip", nv.teClip);
    FormsVBT.PutBoolean(as, "teHasScrollbar", nv.teHasScrollbar);
    FormsVBT.PutBoolean(as, "teToggle", nv.getFromFile);
    SetTeState(as, nv.getFromFile);
    FormsVBT.PutText(as, "teInitial", nv.teContents, FALSE);
    FormsVBT.PutText(as, "teFrom", nv.teFromFile, FALSE);
    NodeVBT.T.loadAttributes(nv, as);
  END TeLoadAttributes;

PROCEDURE TeApplyAttributes (nv: TextEditNode; as: FormsVBT.T) =
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);
    nv.teReadOnly := FormsVBT.GetBoolean(as, "teReadOnly");
    nv.teClip := FormsVBT.GetBoolean(as, "teClip");
    nv.teHasScrollbar := FormsVBT.GetBoolean(as, "teHasScrollbar");
    nv.getFromFile := FormsVBT.GetBoolean(as, "teToggle");
    nv.teContents := FormsVBT.GetText(as, "teInitial");
    nv.teFromFile := FormsVBT.GetText(as, "teFrom");

  END TeApplyAttributes;

PROCEDURE SetTeState (afv: FormsVBT.T; state: BOOLEAN) =
  BEGIN
    IF state THEN
      FormsVBT.MakeDormant(afv, "teFilter2");
      FormsVBT.MakeActive(afv, "teFilter1");
    ELSE
      FormsVBT.MakeDormant(afv, "teFilter1");
      FormsVBT.MakeActive(afv, "teFilter2");
    END
  END SetTeState;

PROCEDURE teProc (<* UNUSED *> cl  : FormsVBT.Closure;
                               afv : FormsVBT.T;
                  <* UNUSED *> name: TEXT;
                  <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    SetTeState(afv, FormsVBT.GetBoolean(afv, "teToggle"));
  END teProc;


PROCEDURE DecodeText (input: TEXT): TEXT =
  (* So that it will write "ab\nc" instead of "ab c" *)
  VAR
    first        := Text.FindChar(input, '\n');
    prefix: TEXT;
  BEGIN
    IF first = -1 THEN
      RETURN input
    ELSE
      prefix := Text.Sub(input, 0, first) & "\\n";
      IF first + 1 < Text.Length(input) THEN
        RETURN prefix & DecodeText(Text.Sub(input, first + 1))
      ELSE
        RETURN prefix
      END
    END
  END DecodeText;

PROCEDURE TeComputeSX (nv: TextEditNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN

    IF nv.teReadOnly THEN
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "ReadOnly", "ReadOnly")
    ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "ReadOnly", "")
    END;
    IF nv.teClip THEN
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Clip", "Clip")
    ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Clip", "")
    END;
    IF nv.teHasScrollbar THEN
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "NoScrollbar", "")
    ELSE
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "NoScrollbar", "NoScrollbar")
    END;
    IF nv.getFromFile THEN
      nv.DialogSX :=
        NodeVBT.FindAndReplace(
          nv.DialogSX, "Initial", "From \"" & nv.teFromFile & "\"")
    ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "Initial",
                       "Value \"" & DecodeText(nv.teContents) & "\"")
    END;
    RETURN NodeVBT.T.computeSX(nv, Final);
  END TeComputeSX;

(* TypeIn support procs *************)

PROCEDURE TyLoadAttributes (nv: TypeInNode; as: FormsVBT.T) =
  BEGIN
    FormsVBT.PutBoolean(as, "tyReadOnly", nv.tyReadOnly);
    FormsVBT.PutBoolean(as, "tyExpand", nv.tyExpand);
    FormsVBT.PutText(as, "tyInit", nv.tyInit, FALSE);
    NodeVBT.T.loadAttributes(nv, as);
  END TyLoadAttributes;

PROCEDURE TyApplyAttributes (nv: TypeInNode; as: FormsVBT.T) =
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);
    nv.tyReadOnly := FormsVBT.GetBoolean(as, "tyReadOnly");
    nv.tyExpand := FormsVBT.GetBoolean(as, "tyExpand");
    nv.tyInit := FormsVBT.GetText(as, "tyInit");
  END TyApplyAttributes;

PROCEDURE TyComputeSX (nv: TypeInNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN
    IF nv.tyReadOnly THEN
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "ReadOnly", "ReadOnly")
    ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "ReadOnly", "")
    END;
    IF nv.tyExpand THEN
      nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "ExpandOnDemand", "ExpandOnDemand")
    ELSE
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "ExpandOnDemand", "")
    END;
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Initial", nv.tyInit);
    RETURN NodeVBT.T.computeSX(nv, Final);
  END TyComputeSX;

(* Text support procs *************)

PROCEDURE TLoadAttributes (nv: TextNode; as: FormsVBT.T) =
  BEGIN
    FormsVBT.PutChoice(as, "tOrientation", nv.tOrientation);
    FormsVBT.PutText(as, "tVal", nv.tVal, FALSE);
    NodeVBT.T.loadAttributes(nv, as);
  END TLoadAttributes;

PROCEDURE TApplyAttributes (nv: TextNode; as: FormsVBT.T) =
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);
    nv.tOrientation := FormsVBT.GetChoice(as, "tOrientation");
    nv.tVal := FormsVBT.GetText(as, "tVal");

  END TApplyAttributes;

PROCEDURE TComputeSX (nv: TextNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN
    nv.DialogSX :=
      NodeVBT.FindAndReplace(nv.DialogSX, "Alignment", nv.tOrientation);

    nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Initial", nv.tVal);

    RETURN NodeVBT.T.computeSX(nv, Final);
  END TComputeSX;

PROCEDURE TeSave (nv: TextEditNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wbool(s, nv.teReadOnly);
    RW.wbool(s, nv.teClip);
    RW.wbool(s, nv.teHasScrollbar);
    RW.wtext(s, nv.teContents);
    RW.wtext(s, nv.teFromFile);
    RW.wbool(s, nv.getFromFile);

  END TeSave;

PROCEDURE TeLoad (nv: TextEditNode ; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rbool(s, nv.teReadOnly);
    RW.rbool(s, nv.teClip);
    RW.rbool(s, nv.teHasScrollbar);
    RW.rtext(s, nv.teContents);
    RW.rtext(s, nv.teFromFile);
    RW.rbool(s, nv.getFromFile);
  END TeLoad;

PROCEDURE  TeObAttrs (nv: TextEditNode): TEXT =
VAR code := "";
  BEGIN
    code := NodeVBT.BoolAttr("teReadOnly", nv.teReadOnly) &
                NodeVBT.BoolAttr("teClip", nv.teClip) &
                NodeVBT.BoolAttr("teHasScrollbar", nv.teHasScrollbar) &
                NodeVBT.TextAttr("teContents", nv.teContents) &
                NodeVBT.TextAttr("teFromFile", nv.teFromFile) &
                NodeVBT.BoolAttr("getFromFile", nv.getFromFile);

    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END TeObAttrs;

PROCEDURE TySave (nv: TypeInNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wbool(s, nv.tyReadOnly);
    RW.wbool(s, nv.tyExpand);
    RW.wtext(s, nv.tyInit);
  END TySave;

PROCEDURE TyLoad (nv: TypeInNode; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rbool(s, nv.tyReadOnly);
    RW.rbool(s, nv.tyExpand);
    RW.rtext(s, nv.tyInit);
  END TyLoad;

PROCEDURE  TyObAttrs (nv: TypeInNode): TEXT =
VAR code := "";
  BEGIN
    code := NodeVBT.BoolAttr("tyReadOnly", nv.tyReadOnly) &
                NodeVBT.BoolAttr("tyExpand", nv.tyExpand) &
                NodeVBT.TextAttr("tyInit", nv.tyInit);

    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END TyObAttrs;

PROCEDURE TSave (nv: TextNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wtext(s, nv.tOrientation);
    RW.wtext(s, nv.tVal);
  END TSave;

PROCEDURE TLoad (nv: TextNode; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rtext(s, nv.tOrientation);
    RW.rtext(s, nv.tVal);
  END TLoad;

PROCEDURE  TObAttrs (nv: TextNode): TEXT =
VAR code := "";
  BEGIN
    code := NodeVBT.TextAttr("tOrientation", nv.tOrientation) &
                NodeVBT.TextAttr("tVal", nv.tVal);
    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END TObAttrs;


PROCEDURE Initialize () =
  BEGIN
    EVAL NodeVBT.Register("text", TextConstructor);
    EVAL NodeVBT.Register("textedit", TextEditConstructor);
    EVAL NodeVBT.Register("typein", TypeInConstructor);

    (* textedit attachments *)
    WITH teclosure = NEW(FormsVBT.Closure, apply := teProc) DO
      FormsVBT.Attach(Attributes.afv, "teToggle", teclosure);
    END;

  END Initialize;

BEGIN

END Textual.





