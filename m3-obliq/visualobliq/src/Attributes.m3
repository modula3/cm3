(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Nov 17 15:55:22 PST 1993 by mhb    *)
(*      modified on Thu Sep  9 15:58:11 PDT 1993 by bharat *)

MODULE Attributes;

IMPORT Axis, ColorName, Dialog, FlexVBT, FormsVBT, Fmt, ISOChar, ListVBT,
       NodeVBT, Rd, Rect, Region, Rsrc,
       Stdio, Split, Text, TextList,
       Thread, Trestle, TrestleComm, TSplit, VBT, Wr,  ZSplit;

<* FATAL Rd.Failure, Thread.Alerted, FormsVBT.Error, TrestleComm.Failure,
         FormsVBT.Unimplemented, Split.NotAChild, Wr.Failure *>
REVEAL
  T =
    Public BRANDED "VO-Attributes"
    OBJECT installed := FALSE;  OVERRIDES init := Init; END;

VAR
  colorTypein: TEXT;
  fontTypein: TEXT;
  nodev      : NodeVBT.T;



PROCEDURE Init (v: T): T =
  <* FATAL Rsrc.NotFound *>
  VAR
    Acl          := NEW(FormsVBT.Closure, apply := ApplyProc);
    Scl          := NEW(FormsVBT.Closure, apply := SelectColorProc);
    colorclosure := NEW(FormsVBT.Closure, apply := ColorProc);
    fontclosure  := NEW(FormsVBT.Closure, apply := FontProc);
    Fcl          := NEW(FormsVBT.Closure, apply := SelectFontProc);
    Ccl          := NEW(FormsVBT.Closure, apply := ShowColorProc);
  BEGIN
    (* afv is the attribute sheet fv *)
    afv := FormsVBT.T.initFromRsrc(v, "attributes.fv", Dialog.rsrcPath);
    FormsVBT.Attach(afv, "apply", Acl);

    (* load color browsers *)
    WITH mod1     = NARROW(FormsVBT.GetVBT(afv, "modifier1"), ListVBT.T),
         mod2     = NARROW(FormsVBT.GetVBT(afv, "modifier2"), ListVBT.T),
         clr      = NARROW(FormsVBT.GetVBT(afv, "colorlist"), ListVBT.T),
         namelist = ColorName.NameList()                                  DO
      LVAppendText(
        mod1,
        " \nLight\nDark\nDull\nBright\nReddish\nGreenish\nBluish\nYellowish\n");
      LVAppendText(
        mod2,
        " \nVeryVerySlightly\nVerySlightly\nSlightly\nSomewhat\nRather\nQuite\nVery\nVeryVery\nVeryVeryVery\n");

      FOR i := 0 TO TextList.Length(namelist) - 1 DO
        WITH theName = TextList.Nth(namelist, i) DO
          LVAppendText(
            clr, Text.FromChar(ISOChar.Upper[Text.GetChar(theName, 0)])
                   & Text.Sub(theName, 1, Text.Length(theName) - 1) & "\n")
        END
      END;

    END;
    (* attach common color-popup-helper-buttons *)
    FormsVBT.Attach(afv, "bgc", colorclosure);
    FormsVBT.Attach(afv, "fgc", colorclosure);
    
    (* attach common font-popup-helper-button *)
    FormsVBT.Attach(afv, "font", fontclosure);

    (* attach Inherit and Apply buttons of the color popup *)
    FormsVBT.Attach(afv, "inheritcolor", Scl);
    FormsVBT.Attach(afv, "applycolor", Scl);

    (* attach Inherit and Apply buttons of the font popup *)
    FormsVBT.Attach(afv, "inheritfont", Fcl);
    FormsVBT.Attach(afv, "fixedfont",   Fcl);
    FormsVBT.Attach(afv, "applyfont", Fcl);

    FormsVBT.Attach(afv, "modifier1", Ccl);
    FormsVBT.Attach(afv, "modifier2", Ccl);
    FormsVBT.Attach(afv, "colorlist", Ccl);
    RETURN afv;
  END Init;


PROCEDURE ApplyProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                  afv : FormsVBT.T;
                     <* UNUSED *> name: TEXT;
                     <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR error: TEXT;
      dom  := VBT.Domain(nodev);
      intestmode := FALSE;
  BEGIN

    FOR i:= 1 TO Dialog.screens DO
     intestmode := intestmode OR Dialog.screen[i].TestMode;
    END;

    IF intestmode THEN RETURN; (* avoid potential problems *)
    END;

    IF NOT nodev.checkAttributes(afv, error) THEN
      Dialog.message(afv, error)
    ELSE
      nodev.applyAttributes(afv);
      (* this causes node entries to be updated *)
        
      nodev.DialogSX := nodev.SXTemplate(); (* copy over *)
      TRY
        WITH newSX = nodev.computeSX(),
             replacementForm = NEW(FormsVBT.T).init(nodev.DialogSX) DO
          (* this causes the s-expression corresponding to the current
             state to be generated *)
          Wr.PutText(
              Stdio.stdout, "Creating fv for \n" & newSX &  "\n");
          Wr.Flush(Stdio.stdout);
          
          (* the delicate operation of deleting the existing fv and 
             creating a new one in its place ... *)
          nodev.replaceChild(replacementForm);
        END
      EXCEPT ELSE 
        Dialog.message(afv, "Could not create interface - Please Check Attributes");
      END;
     
      WITH 
           nv        = NARROW(nodev.getchild(), FormsVBT.T),
           sh        = NARROW(FormsVBT.GetVBT(nv, nodev.name & "shape"), FlexVBT.T),
           vpixpermm = VBT.MMToPixels(sh, 1.0, Axis.T.Ver),
           hpixpermm = VBT.MMToPixels(sh, 1.0, Axis.T.Hor)              DO
   
        FlexVBT.SetRange(
            sh, Axis.T.Hor,
            FlexVBT.SizeRange{FLOAT(Rect.HorSize(dom)) / hpixpermm,
                              FLOAT(Rect.HorSize(dom)) / hpixpermm, 
                              FlexVBT.Infinity});
        FlexVBT.SetRange(
            sh, Axis.T.Ver,
            FlexVBT.SizeRange{FLOAT(Rect.VerSize(dom)) / vpixpermm,
                              FLOAT(Rect.VerSize(dom)) / vpixpermm,
                              FlexVBT.Infinity});
      END;
        (* this may have erased knobs if present - so *)
        (* make the widget visible *)
      ZSplit.Lift(nodev);
      VBT.ForceRepaint(nodev, Region.FromRect(dom));
        

      WITH
        dom = ZSplit.GetDomain(nodev) DO
        Wr.PutText(
          Stdio.stdout, "Final Dimensions = " & Fmt.Int(Rect.HorSize(dom))
                          & " X " & Fmt.Int(Rect.VerSize(dom)) & "\n");
        Wr.Flush(Stdio.stdout);
      END

    END
  END ApplyProc;

PROCEDURE ColorProc (cl  : FormsVBT.Closure;
                     afv : FormsVBT.T;
                     name: TEXT;
                     time: VBT.TimeStamp     ) =
  BEGIN
    (* save the name of the corresponding typein field *)
    ShowColorProc(cl, afv, name, time); (* call the other callback *)
    colorTypein := name & "typein";
  END ColorProc;

PROCEDURE FontProc ( <* UNUSED *> cl  : FormsVBT.Closure;
                     <* UNUSED *> afv : FormsVBT.T;
                     name: TEXT;
                     <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    (* save the name of the corresponding typein field *)
  (*  ShowColorProc(cl, afv, name, time); (* call the other callback *) *)

    fontTypein := name & "typein";
  END FontProc;

PROCEDURE ShowColorProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                      afv : FormsVBT.T;
                         <* UNUSED *> name: TEXT;
                         <* UNUSED *> time: VBT.TimeStamp     ) =
  (* this provides feedback by changing the bgcolor of the feedback
     block *)

  BEGIN
    WITH mod1      = FormsVBT.GetText(afv, "modifier1"),
         mod2      = FormsVBT.GetText(afv, "modifier2"),
         colorlist = FormsVBT.GetText(afv, "colorlist"),
         composite = mod2 & mod1 & colorlist             DO
      TRY
        FormsVBT.PutColorProperty(
          afv, "showcolor", "BgColor", ColorName.ToRGB(composite));
      EXCEPT
      ELSE
        RETURN;
      END;                       (* may not be a legit color - dont worry
                                    at this stage *)
    END
  END ShowColorProc;

PROCEDURE SelectColorProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                        afv : FormsVBT.T;
                                        name: TEXT;
                           <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR mod1, mod2, colorlist, composite: TEXT;
  BEGIN

    IF Text.Equal(name, "inheritcolor") THEN
      FormsVBT.PutText(afv, colorTypein, "Inherit", FALSE)
    ELSE                         (* applycolor *)
      mod1 := FormsVBT.GetText(afv, "modifier1");

      (* ensure that an invalid color has not been chosen *)
      (* only case we have to look out for is when modifier 1 is empty *)
      (* modifier 2 should also be empty *)
      IF Text.Equal(mod1, " ") THEN
        FormsVBT.PutTextProperty(afv, "modifier2", "Select", " ")
      END;

      mod2 := FormsVBT.GetText(afv, "modifier2");
      colorlist := FormsVBT.GetText(afv, "colorlist");
      composite := mod2 & mod1 & colorlist;
      FormsVBT.PutText(afv, colorTypein, composite, FALSE)
    END
  END SelectColorProc;

PROCEDURE SelectFontProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                        afv : FormsVBT.T;
                                        name: TEXT;
                           <* UNUSED *> time: VBT.TimeStamp     ) =
 VAR
   composite : TEXT;
  BEGIN

    IF Text.Equal(name, "inheritfont") THEN
      FormsVBT.PutText(afv, fontTypein, "Inherit", FALSE)
    ELSIF Text.Equal(name, "fixedfont") THEN
      FormsVBT.PutText(afv, fontTypein, "Fixed", FALSE)
    ELSE
      (* create composite font name from radio-name*)
     WITH
       fnm  = FormsVBT.GetChoice(afv, "fontradio"),
       fsz = FormsVBT.GetChoice(afv, "sizeradio")
       DO

        CASE Text.GetChar(fnm, 0) OF
          't' => composite := "-*-times-"
        | 'h' => composite := "-*-helvetica-"
        | 'c' => composite := "-*-courier-"
        ELSE
        END;

        CASE Text.GetChar(fnm, 1) OF
          'm' => composite := composite & "medium-*"
        | 'b' => composite := composite & "bold-*"
        ELSE
        END;
        
        composite := composite & Text.Sub(fnm, 2, 1) &
                         "-*" & Text.Sub(fsz, 2) & "-*";
        (* e.g. "-*-times-medium-R-*80-*" *)
        (* phew *)
         FormsVBT.PutText(afv, fontTypein, composite, FALSE)
      END
    END
  END SelectFontProc;

PROCEDURE Invoke (v: T; nv: NodeVBT.T) =
  BEGIN
    nodev := nv;
    nv.loadAttributes(v);        (* loads object attributes*)
    SetPage(v, nv);              (* sets the appropriate page *)

    IF ISTYPE(nv, NodeVBT.SplitNode) THEN
      FormsVBT.PutText(
        v, "codetype", "Initialization Code", FALSE);
    ELSE
      FormsVBT.PutText(v, "codetype", "Callback", FALSE);
    END;

    IF NOT v.installed THEN
      (* install *)
      Trestle.Install(
        v, applName := "VisualObliq", inst := "VO-Attributes",
        windowTitle := "Visual Obliq Attribute Sheet");
      v.installed := TRUE;
    ELSE
      (* activate and deiconize *)
      FormsVBT.MakeActive(v, "attrfilter");
      Trestle.MoveNear(v, NIL);
    END
  END Invoke;

PROCEDURE Iconize (v: T) =
  BEGIN
    FormsVBT.MakeDormant(v, "attrfilter");
    Trestle.Iconize(v);
  END Iconize;

PROCEDURE SetPage (v: T; nv: NodeVBT.T) =
  BEGIN
    WITH tsplit    = FormsVBT.GetVBT(v, "jeff"),
         attrsheet = NodeVBT.GetAttributeSheetName(nv),
         widget    = FormsVBT.GetVBT(v, attrsheet),
         tsplit2    = FormsVBT.GetVBT(v, "wholepage"),
         attrmain = FormsVBT.GetVBT(v, "attrmain")
         DO
      TSplit.SetCurrent(tsplit, widget);
      TSplit.SetCurrent(tsplit2, attrmain);
    END
  END SetPage;

PROCEDURE LVFlush (v: ListVBT.T) =
  BEGIN
    v.removeCells(0, v.count());
  END LVFlush;

PROCEDURE LVAppendText (v: ListVBT.T; t: TEXT) =
  VAR
    indx := Text.FindChar(t, '\n', 0);
    from := 0;
    ct   := v.count();
  BEGIN
    WHILE indx # -1 DO
      v.insertCells(ct, 1);
      v.setValue(ct, Text.Sub(t, from, indx - from));
      from := indx + 1;
      INC(ct);
      IF from < Text.Length(t) THEN
        indx := Text.FindChar(t, '\n', from);
      ELSE
        indx := -1
      END
    END;
  END LVAppendText;

BEGIN
END Attributes.

