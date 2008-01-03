(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Tue Jan 16 13:08:17 PST 1996 by heydon *)
(*      modified on Wed Feb  1 09:40:44 PST 1995 by kalsow *)
(*      modified on Wed Sep  7 18:48:22 PDT 1994 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>

MODULE Browser;

IMPORT Attributes, FormsVBT,Fmt,  ListVBT, NodeVBT, Split, Text, TSplit, VBT,
       Rd, Wr, RW;

REVEAL
  BrowserNode = NodeVBT.Widget BRANDED "VO-BrowserNode" OBJECT END;

  Browser = BrowserNode BRANDED "VO-Browser" OBJECT
              Quick       : BOOLEAN := FALSE;
              Multiplicity: BOOLEAN := FALSE;

              Contents: REF ARRAY OF TEXT := NIL;
              Cix     : CARDINAL          := 0;
              (* multibrowser *)
              Selections: REF ARRAY OF CARDINAL := NIL;
              Six       : CARDINAL              := 0;
              (* unibrowser *)
              Selection: INTEGER := -1; (* not selected *)
            OVERRIDES
              loadAttributes  := BroLoadAttributes;
              applyAttributes := BroApplyAttributes;
              computeSX       := BroComputeSX;
              save                       := BroSave;
              load                       := BroLoad;
              initObliqAttrs       := BroObAttrs;
            END;

  FileBrowser = BrowserNode BRANDED "VO-FileBrowser" OBJECT
                  LabelFont: TEXT := "-*-helvetica-bold-*R-*120-*"; 
                  ReadOnly: BOOLEAN           := FALSE;
                  Value   : TEXT              := ".";
                  Suffixes: REF ARRAY OF TEXT := NIL;
                  ActionLabel : TEXT := "Open";
                OVERRIDES
                  loadAttributes  := FbLoadAttributes;
                  applyAttributes := FbApplyAttributes;
                  generateAttachments := FbGenerateAttachments;
                  computeSX := FbComputeSX;
                  save                       := FbSave;
                  load                       := FbLoad;
                  initObliqAttrs       := FbObAttrs;
                END;

  <* FATAL FormsVBT.Error,FormsVBT.Unimplemented, Split.NotAChild *>

PROCEDURE BrowserConstructor (): NodeVBT.T =
  BEGIN
    RETURN NEW(Browser, BgColor := "SlightlyYellowishGrey90",
               FgColor := "Black",
               Rim := 0,
               Border := 0, Font := "-*-helvetica-bold-*R-*120-*",
               width := 100, height := 100, Embellishment := "Lowered" );
  END BrowserConstructor;

PROCEDURE FileBrowserConstructor (): NodeVBT.T =
  BEGIN
    RETURN NEW(FileBrowser, BgColor := "PaleGray", FgColor := "Black",
               Rim := 0,
               Border := 0, Font := "-*-helvetica-bold-*R-*120-*",
               width := 150, height := 150, Embellishment := "Lowered");
  END FileBrowserConstructor;

PROCEDURE BroComputeSX (nv: Browser; Final: BOOLEAN := FALSE): TEXT =
  VAR
    items : TEXT := "";
    values: TEXT := "";
  BEGIN
    IF nv.Multiplicity THEN
      nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "typeOfBrowser", "MultiBrowser");
      IF nv.Six = 0 THEN
        nv.DialogSX :=
          NodeVBT.FindAndReplace(nv.DialogSX, "ValueList", "");
      ELSE
        FOR i := FIRST(nv.Selections^)
            TO FIRST(nv.Selections^) + nv.Six - 1 DO
          values := values & Fmt.Int(nv.Selections[i]) & " ";
        END;
        nv.DialogSX :=
          NodeVBT.FindAndReplace(nv.DialogSX, "ValueList", 
                                 "(Value " & values  & " )" );
      END
    ELSE
      nv.DialogSX :=
        NodeVBT.FindAndReplace(nv.DialogSX, "typeOfBrowser", "Browser");
      IF nv.Selection = -1 THEN
        nv.DialogSX :=
          NodeVBT.FindAndReplace(nv.DialogSX, "ValueList", "");
      ELSE
        nv.DialogSX := NodeVBT.FindAndReplace(
                         nv.DialogSX, "ValueList",
                         "(Value  " &  Fmt.Int(nv.Selection) & " )");
      END
    END;

    IF nv.Quick THEN
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "quick", "Quick")
    ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "quick", "")
    END;
    IF nv.Cix = 0 THEN
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Items", "")
    ELSE
      FOR i := FIRST(nv.Contents^) TO LAST(nv.Contents^) DO
        items := items & "\"" & nv.Contents[i] &  "\" ";
      END;
      nv.DialogSX := NodeVBT.FindAndReplace(nv.DialogSX, "Items", items);
    END;
    RETURN NodeVBT.T.computeSX(nv, Final);
  END BroComputeSX;

(* Browser Support Procs *)
PROCEDURE BroLoadAttributes (nv: Browser; as: FormsVBT.T) =
  BEGIN

    FormsVBT.PutBoolean(as, "brQuick", nv.Quick);
    FormsVBT.PutBoolean(as, "brMultiplicity", nv.Multiplicity);
    FormsVBT.PutChoice(as, "brModes", "brEditMode"); (* always start in
                                                        edit mode *)


    WITH                         (* set all the t-splits *)
      tsp1 = NARROW(FormsVBT.GetVBT(as, "brSelectionMode"), TSplit.T),
      tsp2 = NARROW(FormsVBT.GetVBT(as, "brBrowsers"), TSplit.T),
      tsp3 = NARROW(FormsVBT.GetVBT(as, "brNestedTS"), TSplit.T)       DO
      TSplit.SetCurrent(tsp1, FormsVBT.GetVBT(as, "page1"));
      TSplit.SetCurrent(tsp2, FormsVBT.GetVBT(as, "brEdit"));
      IF nv.Multiplicity THEN
        TSplit.SetCurrent(tsp3, FormsVBT.GetVBT(as, "brMulti"))
      ELSE
        TSplit.SetCurrent(tsp3, FormsVBT.GetVBT(as, "brUni"))
      END
    END;

    (* load contents into browsers *)
    WITH editBrowser = NARROW(FormsVBT.GetVBT(as, "brInitial"), ListVBT.T),
         uniBrowser  = NARROW(FormsVBT.GetVBT(as, "brSingle"), ListVBT.T),
         multiBrowser = NARROW(
                          FormsVBT.GetVBT(as, "brMultiple"), ListVBT.T) DO
      Attributes.LVFlush(editBrowser);
      Attributes.LVFlush(uniBrowser);
      Attributes.LVFlush(multiBrowser);

      IF nv.Cix = 0 THEN         (* no contents yet so disable "add next"
                                    "delete" *)
        FormsVBT.MakeDormant(as, "brFilter")
      ELSE
        FormsVBT.MakeActive(as, "brFilter");
        FOR i := FIRST(nv.Contents^) TO LAST(nv.Contents^) DO
          Attributes.LVAppendText(editBrowser, nv.Contents[i] & "\n");
          Attributes.LVAppendText(uniBrowser, nv.Contents[i] & "\n");
          Attributes.LVAppendText(multiBrowser, nv.Contents[i] & "\n");
        END;
        IF nv.Selection # -1 THEN uniBrowser.selectOnly(nv.Selection) END;
        multiBrowser.selectNone();
        IF nv.Six # 0 THEN
          FOR i := FIRST(nv.Selections^)
              TO FIRST(nv.Selections^) + nv.Six - 1 DO
            multiBrowser.select(nv.Selections[i], TRUE);
          END
        END
      END;
    END;
    NodeVBT.T.loadAttributes(nv, as);
  END BroLoadAttributes;


PROCEDURE BroObAttrs (nv : Browser): TEXT =
  VAR code : TEXT;
  BEGIN
    code := NodeVBT.BoolAttr("Quick", nv.Quick) &
                NodeVBT.BoolAttr("Multiplicity", nv.Multiplicity);
 
    IF nv.Cix > 0 THEN 
      code := code & "\ttemp.Contents := [";
      FOR i:= 0 TO nv.Cix - 1 DO
        code := code & "\"" &  nv.Contents[FIRST(nv.Contents^) + i] &
                    "\",";
      END;
      code := code & "];\n";
    END;

    IF nv.Six > 0 THEN 
      code := code & "\ttemp.Selections := [";
      FOR i:= 0 TO nv.Six - 1 DO
        code := code &  Fmt.Int(nv.Selections[FIRST(nv.Selections^) + i]) &
                    ",";
      END;
      code := code & "];\n";
    END;

    code := code & NodeVBT.IntAttr("Selection", nv.Selection);

    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END BroObAttrs;


PROCEDURE BroApplyAttributes (nv: Browser; as: FormsVBT.T) =
  VAR
    ctr  : CARDINAL;
    index: INTEGER;
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);
    nv.Quick := FormsVBT.GetBoolean(as, "brQuick");
    nv.Multiplicity := FormsVBT.GetBoolean(as, "brMultiplicity");
    WITH uniBrowser = NARROW(FormsVBT.GetVBT(as, "brSingle"), ListVBT.T),
         multiBrowser = NARROW(
                          FormsVBT.GetVBT(as, "brMultiple"), ListVBT.T) DO
      nv.Cix := uniBrowser.count();
      nv.Contents := NEW(REF ARRAY OF TEXT, nv.Cix);
      nv.Selections := NEW(REF ARRAY OF CARDINAL, nv.Cix);
      nv.Six := 0;
      ctr := 0;
      index := FIRST(nv.Selections^);
      FOR i := FIRST(nv.Contents^) TO LAST(nv.Contents^) DO
        nv.Contents[i] := uniBrowser.getValue(ctr);
        IF multiBrowser.isSelected(ctr) THEN
          INC(nv.Six);
          nv.Selections[index] := ctr;
          INC(index);
        END;
        INC(ctr);
      END;
      IF NOT uniBrowser.getFirstSelected(nv.Selection) THEN
        nv.Selection := -1
      END;
    END
  END BroApplyAttributes;
  
PROCEDURE addProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                afv : FormsVBT.T;
                                name: TEXT;
                   <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    editBrowser  := NARROW(FormsVBT.GetVBT(afv, "brInitial"), ListVBT.T);
    uniBrowser   := NARROW(FormsVBT.GetVBT(afv, "brSingle"), ListVBT.T);
    multiBrowser := NARROW(FormsVBT.GetVBT(afv, "brMultiple"), ListVBT.T);
    item         := FormsVBT.GetText(afv, "brItem");
    pos: INTEGER;
  PROCEDURE insertAt (n: CARDINAL) =
    BEGIN
      editBrowser.insertCells(n, 1);
      editBrowser.setValue(n, item);
      uniBrowser.insertCells(n, 1);
      uniBrowser.setValue(n, item);
      multiBrowser.insertCells(n, 1);
      multiBrowser.setValue(n, item);
      editBrowser.selectOnly(n);
    END insertAt;
  BEGIN
    IF Text.Equal(name, "brAddFirst") THEN
      insertAt(0);
    ELSIF editBrowser.getFirstSelected(pos) THEN
      insertAt(pos + 1);
    ELSE
      insertAt(0)
    END;
    FormsVBT.MakeActive(afv, "brFilter");
  END addProc;

PROCEDURE delProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                afv : FormsVBT.T;
                   <* UNUSED *> name: TEXT;
                   <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    editBrowser  := NARROW(FormsVBT.GetVBT(afv, "brInitial"), ListVBT.T);
    uniBrowser   := NARROW(FormsVBT.GetVBT(afv, "brSingle"), ListVBT.T);
    multiBrowser := NARROW(FormsVBT.GetVBT(afv, "brMultiple"), ListVBT.T);
    pos: INTEGER;
  BEGIN
    IF editBrowser.getFirstSelected(pos) THEN
      editBrowser.removeCells(pos, 1);
      uniBrowser.removeCells(pos, 1);
      multiBrowser.removeCells(pos, 1);
      editBrowser.selectOnly(pos);
    END;
    IF editBrowser.count() = 0 THEN
      FormsVBT.MakeDormant(afv, "brFilter");
    END
  END delProc;

PROCEDURE selProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                afv : FormsVBT.T;
                   <* UNUSED *> name: TEXT;
                   <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    (* load the currently selected item into the typein for copy-editing*)
    WITH z = FormsVBT.GetTextProperty(afv, "brInitial", "Select") DO
      FormsVBT.PutText(afv, "brItem", z, FALSE);
    END
  END selProc;

(* FileBrowser Support Procs ******************************************)

PROCEDURE FbLoadAttributes (nv: FileBrowser; as: FormsVBT.T) =
  BEGIN
    FormsVBT.PutText(as, "fbLFtypein", nv.LabelFont, FALSE);
    FormsVBT.PutBoolean(as, "fbReadOnly", nv.ReadOnly);
    FormsVBT.PutText(as, "fbVal", nv.Value, FALSE);
    FormsVBT.PutText(as, "fbActionLabel", nv.ActionLabel, FALSE);
    WITH                         (* set all the t-split & the browser *)
      tsp2 = NARROW(FormsVBT.GetVBT(as, "fbEditSuffixes"), TSplit.T),
      tsp3 = NARROW(FormsVBT.GetVBT(as, "fbSuffixesToolkit"), TSplit.T),
      suffixBrowser = NARROW(FormsVBT.GetVBT(as, "fbSuffixes"), ListVBT.T) DO
      Attributes.LVFlush(suffixBrowser);
      IF nv.Suffixes = NIL THEN
        FormsVBT.PutChoice(as, "fbModes", "fbNoSuffixes");
        TSplit.SetCurrent(tsp2, FormsVBT.GetVBT(as, "fbBlank"));
        TSplit.SetCurrent(tsp3, FormsVBT.GetVBT(as, "fbPage2"));
        FormsVBT.MakeDormant(as, "fbFilter");
      ELSE
        FormsVBT.PutChoice(as, "fbModes", "fbAddSuffixes");
        TSplit.SetCurrent(tsp2, FormsVBT.GetVBT(as, "fbEdit"));
        TSplit.SetCurrent(tsp3, FormsVBT.GetVBT(as, "fbPage1"));
        FormsVBT.MakeActive(as, "fbFilter");
        FOR i := FIRST(nv.Suffixes^) TO LAST(nv.Suffixes^) DO
          Attributes.LVAppendText(suffixBrowser, nv.Suffixes[i] & "\n")
        END;
      END;
    END;
    NodeVBT.T.loadAttributes(nv, as);
  END FbLoadAttributes;

PROCEDURE FbApplyAttributes (nv: FileBrowser; as: FormsVBT.T) =
  VAR ctr := 0;
  BEGIN
    NodeVBT.T.applyAttributes(nv, as);
    nv.LabelFont := FormsVBT.GetText(as, "fbLFtypein");
    nv.ReadOnly := FormsVBT.GetBoolean(as, "fbReadOnly");
    nv.Value := FormsVBT.GetText(as, "fbVal");
    nv.ActionLabel := FormsVBT.GetText(as, "fbActionLabel");
    WITH suffixBrowser = NARROW(
                           FormsVBT.GetVBT(as, "fbSuffixes"), ListVBT.T),
         ct          = suffixBrowser.count(),
         currentMode = FormsVBT.GetChoice(as, "fbModes") DO
      IF ct = 0 OR Text.Equal(currentMode, "fbNoSuffixes") THEN
        nv.Suffixes := NIL
      ELSE
        nv.Suffixes := NEW(REF ARRAY OF TEXT, ct);
        FOR i := FIRST(nv.Suffixes^) TO LAST(nv.Suffixes^) DO
          nv.Suffixes[i] := suffixBrowser.getValue(ctr);
          INC(ctr);
        END;
      END;
    END
  END FbApplyAttributes;

PROCEDURE fbAddProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                  afv : FormsVBT.T;
                                  name: TEXT;
                     <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    suffixBrowser := NARROW(FormsVBT.GetVBT(afv, "fbSuffixes"), ListVBT.T);
    item          := FormsVBT.GetText(afv, "fbItem");
    pos: INTEGER;
  BEGIN
    IF Text.Equal(name, "fbAddFirst") THEN
      suffixBrowser.insertCells(0, 1);
      suffixBrowser.setValue(0, item);
      suffixBrowser.selectOnly(0);
    ELSIF suffixBrowser.getFirstSelected(pos) THEN
      suffixBrowser.insertCells(pos + 1, 1);
      suffixBrowser.setValue(pos + 1, item);
      suffixBrowser.selectOnly(pos + 1);
    ELSE
      suffixBrowser.insertCells(0, 1);
      suffixBrowser.setValue(0, item);
      suffixBrowser.selectOnly(0);
    END;
    FormsVBT.MakeActive(afv, "fbFilter");
  END fbAddProc;

PROCEDURE fbDelProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                  afv : FormsVBT.T;
                     <* UNUSED *> name: TEXT;
                     <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    suffixBrowser := NARROW(FormsVBT.GetVBT(afv, "fbSuffixes"), ListVBT.T);
    pos: INTEGER;
  BEGIN
    IF suffixBrowser.getFirstSelected(pos) THEN
      suffixBrowser.removeCells(pos, 1);
      IF suffixBrowser.count() > pos THEN
        suffixBrowser.selectOnly(pos)
      ELSIF pos > 0 THEN
        suffixBrowser.selectOnly(pos - 1)
      END
    END;
    IF suffixBrowser.count() = 0 THEN
      FormsVBT.MakeDormant(afv, "fbFilter");
    END
  END fbDelProc;

PROCEDURE FbGenerateAttachments (nv: FileBrowser): TEXT =
  BEGIN
    IF NodeVBT.AllWhitespace(nv.Code) THEN RETURN ""; END;
    RETURN "form_attach(SELF.FORM,  SELF." & nv.name & ".name & \"action\", SELF." & nv.name
             & "Proc);\n" & NodeVBT.T.generateAttachments(nv);
  END FbGenerateAttachments;


PROCEDURE FbComputeSX (nv: FileBrowser; Final: BOOLEAN := FALSE): TEXT =
  VAR 
    suffixList := "";
  BEGIN
    IF nv.ReadOnly THEN
      nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "ReadOnly", "ReadOnly")
    ELSE
      nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "ReadOnly", "")
    END;
    (* two occurrences *)
     nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "LabelFont", nv.LabelFont);
     nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "LabelFont", nv.LabelFont);
    nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "Initial", nv.Value);
    nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "fbName1", nv.name);
    nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "fbName2", nv.name);
    nv.DialogSX := NodeVBT.FindAndReplace(
                    nv.DialogSX, "ActionLabel", nv.ActionLabel);
    IF nv.Suffixes = NIL THEN
      nv.DialogSX := NodeVBT.FindAndReplace(
                       nv.DialogSX, "Suffixes", "\"\"")
    ELSE
      FOR i:= FIRST(nv.Suffixes^) TO LAST(nv.Suffixes^) DO
        suffixList := suffixList & " \"" & nv.Suffixes[i] &
                          "\" "
      END;
      nv.DialogSX := NodeVBT.FindAndReplace(
                         nv.DialogSX, "Suffixes", suffixList)
    END;
    RETURN NodeVBT.T.computeSX(nv, Final);
  END FbComputeSX;

PROCEDURE FbObAttrs (nv : FileBrowser): TEXT =
  VAR code : TEXT;
  BEGIN
    code := NodeVBT.TextAttr("LabelFont", nv.LabelFont) &
                NodeVBT.BoolAttr("ReadOnly", nv.ReadOnly) &
                NodeVBT.TextAttr("Value", nv.Value);
     IF  nv.Suffixes # NIL THEN
       code := code & "\ttemp.Suffixes := [";
        FOR i := FIRST(nv.Suffixes^) TO LAST(nv.Suffixes^) DO
          code := code & "\"" &  nv.Suffixes[i]  & "\",";
      END;
           code := code & "];\n";
     END;
     code := code & NodeVBT.TextAttr("ActionLabel", nv.ActionLabel);
    RETURN NodeVBT.T.initObliqAttrs(nv) & code ;
  END FbObAttrs;

PROCEDURE BroSave (nv: Browser; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
    RW.wbool(s, nv.Quick);
    RW.wbool(s, nv.Multiplicity);

    RW.wcard(s, nv.Cix);
    IF nv.Cix > 0 THEN 
      FOR i:= 0 TO nv.Cix - 1 DO
        RW.wtext(s, nv.Contents[FIRST(nv.Contents^) + i]);
      END
    END;

    RW.wcard(s, nv.Six);
    IF nv.Six > 0 THEN 
      FOR i:= 0 TO nv.Six - 1 DO
        RW.wcard(s, nv.Selections[FIRST(nv.Selections^) + i]);
      END
    END;
    
    RW.wint(s, nv.Selection);
    
  END BroSave;

PROCEDURE BroLoad (nv: Browser ; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rbool(s, nv.Quick);
    RW.rbool(s, nv.Multiplicity);

    RW.rcard(s, nv.Cix);
    IF nv.Cix > 0 THEN
      nv.Contents := NEW(REF ARRAY OF TEXT, nv.Cix);
      FOR i:= 0 TO nv.Cix - 1 DO
        RW.rtext(s, nv.Contents[FIRST(nv.Contents^) + i]);
      END
    ELSE 
      nv.Contents := NIL
    END;

    RW.rcard(s, nv.Six);
    IF nv.Six > 0 THEN 
      nv.Selections := NEW(REF ARRAY OF CARDINAL, nv.Six);
      FOR i:= 0 TO nv.Six - 1 DO
        RW.rcard(s, nv.Selections[FIRST(nv.Selections^) + i]);
      END
    ELSE
      nv.Selections := NIL
    END;
    
    RW.rint(s, nv.Selection);
    
  END BroLoad;

PROCEDURE FbSave (nv: FileBrowser ; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    NodeVBT.T.save(nv, fv, s);
 
    RW.wtext(s, nv.LabelFont);
    RW.wbool(s, nv.ReadOnly);
    RW.wtext(s, nv.Value);
   
    IF  nv.Suffixes # NIL THEN
      RW.wcard(s, NUMBER(nv.Suffixes^));
      FOR i := FIRST(nv.Suffixes^) TO LAST(nv.Suffixes^) DO
        RW.wtext(s, nv.Suffixes[i]);
      END
    ELSE
      RW.wcard(s, 0)
    END;
    RW.wtext(s, nv.ActionLabel);
  END FbSave;

PROCEDURE FbLoad (nv: FileBrowser ; fv: FormsVBT.T; s: Rd.T) =
  VAR size : CARDINAL;
  BEGIN
    NodeVBT.T.load(nv, fv, s);
    RW.rtext(s, nv.LabelFont);
    RW.rbool(s, nv.ReadOnly);
    RW.rtext(s, nv.Value);
    RW.rcard(s, size);
    IF size > 0 THEN
      nv.Suffixes := NEW(REF ARRAY OF TEXT, size);
      FOR i := FIRST(nv.Suffixes^) TO LAST(nv.Suffixes^) DO
        RW.rtext(s, nv.Suffixes[i]);
      END
    ELSE
      nv.Suffixes := NIL;
    END;
    RW.rtext(s, nv.ActionLabel);
  END FbLoad;

PROCEDURE Initialize () =
  BEGIN

    EVAL NodeVBT.Register("browser", BrowserConstructor);
    EVAL NodeVBT.Register("filebrowser", FileBrowserConstructor);
    WITH addclosure = NEW(FormsVBT.Closure, apply := addProc),
         delclosure = NEW(FormsVBT.Closure, apply := delProc),
         selclosure = NEW(FormsVBT.Closure, apply := selProc)  DO
      FormsVBT.Attach(Attributes.afv, "brAddFirst", addclosure);
      FormsVBT.Attach(Attributes.afv, "brAddAfter", addclosure);
      FormsVBT.Attach(Attributes.afv, "brDelete", delclosure);
      FormsVBT.Attach(Attributes.afv, "brItem", addclosure);
      FormsVBT.Attach(Attributes.afv, "brInitial", selclosure);
    END;

    WITH addclosure = NEW(FormsVBT.Closure, apply := fbAddProc),
         delclosure = NEW(FormsVBT.Closure, apply := fbDelProc)
         DO
          FormsVBT.Attach(Attributes.afv, "fbLF", addclosure);
          FormsVBT.Attach(Attributes.afv, "fbAddFirst", addclosure);
          FormsVBT.Attach(Attributes.afv, "fbAddAfter", addclosure);
          FormsVBT.Attach(Attributes.afv, "fbDelete", delclosure);
          FormsVBT.Attach(Attributes.afv, "fbItem", addclosure);
        END;

  END Initialize;

BEGIN
END Browser.











