(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Feb  1 09:45:44 PST 1995 by kalsow *)
(*      modified on Tue Sep  6 16:20:21 PDT 1994 by bharat *)
(*      modified on Sat Oct 23 23:43:23 PDT 1993 by mhb    *)
<* PRAGMA LL *>

MODULE DialogMenu;

IMPORT Attributes, Dialog, Fmt, FormsVBT, GenerateObliq, ListVBT,
       NodeVBT, Rd, Rsrc, Text, VBT, ZHandleVBT;

<* FATAL FormsVBT.Error,FormsVBT.Unimplemented *>

VAR
  M : ARRAY [0 .. 100] OF T;     (* working copy of menu structure *)
  MC: CARDINAL;                  (* counter for M *)
  ParentForm: NodeVBT.FormNode;
  MenuTemplate: TEXT;            (* read from menuTEMPLATE.fv on
                                    initialization *)


PROCEDURE LoadAttributes (n: ZHandleVBT.T) =
  VAR nv := NARROW(n, NodeVBT.FormNode);
  BEGIN
    ParentForm := nv;

    (* we don't check the HasMenu field here *)
    FormsVBT.PutText(Attributes.afv, "mbgctypein", nv.MenuBgColor, FALSE);
    FormsVBT.PutText(Attributes.afv, "mfgctypein", nv.MenuFgColor, FALSE);
    FormsVBT.PutText(Attributes.afv, "mfonttypein", nv.MenuFont, FALSE);

    (* clean up *)
    FormsVBT.PutText(Attributes.afv, "mlabel", "", FALSE);
    FormsVBT.PutText(Attributes.afv, "mname", "", FALSE);
    FormsVBT.MakeDormant(Attributes.afv, "mItemFilter");
    FormsVBT.PutBoolean(Attributes.afv, "mActive", TRUE);
    FormsVBT.PutText(Attributes.afv, "menucallback", "", FALSE);
    FormsVBT.PutChoice(Attributes.afv, "mexechow", "mForeground");
    FormsVBT.PutChoice(Attributes.afv, "mexecwhere", "mLocal");
    FormsVBT.MakeDormant(Attributes.afv, "mremFilter");
    FormsVBT.PutText(Attributes.afv, "mLocation", "", FALSE);
    FormsVBT.PutText(Attributes.afv, "menucallback", "", FALSE);
    WITH brow = NARROW(FormsVBT.GetVBT(Attributes.afv, "menubrowser"),
                       ListVBT.T) DO
      Attributes.LVFlush(brow);
      MC := 0;
      IF nv.Menu # NIL THEN
        (* copy to working storage *)
        FOR i := FIRST(nv.Menu^) TO LAST(nv.Menu^) DO
          M[MC] := nv.Menu[i];
          InsertInBrowser(MC, M[MC]); (* insert into the browser *)
          INC(MC);
        END;
        IF MC > 0 THEN
          FormsVBT.MakeActive(Attributes.afv, "mItemFilter")
        END
      END;
    END
  END LoadAttributes;

PROCEDURE ComputeMenuObjDefs (n: ZHandleVBT.T): TEXT =
  (* no state information needs to be recorded so we dont need the
     template *)
  VAR
    menudef := "";
    fn      := NARROW(n, NodeVBT.FormNode);
    menu    := fn.Menu;
  BEGIN
    IF menu # NIL THEN
      FOR i := FIRST(menu^) TO LAST(menu^) DO
        IF menu[i].Level < 2 AND NOT Text.Equal(menu[i].Name, "RIDGE") THEN
          menudef := menudef & menu[i].Name
                         & " => ( let temp = LOCAL.menuItemNew( \""
                         & menu[i].Label & "\", \""
                         & menu[i].Name & "\", " &
                         Fmt.Int(menu[i].Level) & ", \"" &
                         menu[i].initialState  & "\");\n" &
                         "\ttemp.form := meth(s) SELF.FORM end;\ntemp),\n";   
                                                    

        END
      END (* FOR *);
      menudef := menudef & "RIDGE => LOCAL.menuItemNew(" &
                     "\"RIDGE\", \"RIDGE\", 1, \"\"),\n";
    END;
    RETURN menudef
  END ComputeMenuObjDefs;

PROCEDURE ComputeMenuCallbacks (n: ZHandleVBT.T): TEXT =
  VAR
    menucbacks       := "";
    fn               := NARROW(n, NodeVBT.FormNode);
    menu             := fn.Menu;
    micb      : TEXT;
  BEGIN
    IF menu # NIL THEN
      FOR i := FIRST(menu^) TO LAST(menu^) DO
        IF menu[i].Level < 2 AND NOT Text.Equal(menu[i].Name, "RIDGE") 
          AND NOT NodeVBT.AllWhitespace(menu[i].callback) THEN
          micb := GenerateObliq.callbackTemplate;
          micb := NodeVBT.FindAndReplace(micb, "objname", menu[i].Name);

          IF menu[i].inForeGround THEN
            micb := NodeVBT.FindAndReplace(micb, "bgHeader", "");
            micb := NodeVBT.FindAndReplace(micb, "bgFooter", "")
          ELSE
            micb := NodeVBT.FindAndReplace(
                      micb, "bgHeader", "thread_fork(proc()\n");
            micb :=
              NodeVBT.FindAndReplace(micb, "bgFooter", "\nend, 10000)\n")
          END;

          IF menu[i].isLocal THEN
            micb := NodeVBT.FindAndReplace(micb, "remoteHeader", "");
            micb := NodeVBT.FindAndReplace(micb, "remoteFooter", "")
          ELSE
            micb :=
              NodeVBT.FindAndReplace(
                micb, "remoteHeader",
                "let VODest = \n(*----------------------------------------*)\n"
                  & menu[i].executeAt & ";\n"
                  & "(*----------------------------------------*)\n"
                  & "VODest.VOCompute( proc(REMOTE) \n");

            micb := NodeVBT.FindAndReplace(
                      micb, "remoteFooter", "\n ok \n end )\n");
          END;
         menucbacks := menucbacks &  NodeVBT.FindAndReplace
                           (micb, "usercode", "(* Menu Callback for "
                           & menu[i].Name  & "*)\n" & menu[i].callback)
                           & "\n";
        END
      END
    END;
    RETURN menucbacks;
  END ComputeMenuCallbacks;

PROCEDURE ComputeMenuAttachments (n: ZHandleVBT.T): TEXT =
  VAR
    mats       := "";
    fn               := NARROW(n, NodeVBT.FormNode);
    menu             := fn.Menu;
   
  BEGIN
    IF menu # NIL THEN
      FOR i := FIRST(menu^) TO LAST(menu^) DO
        IF menu[i].Level < 2 AND NOT Text.Equal(menu[i].Name, "RIDGE") 
          AND NOT NodeVBT.AllWhitespace(menu[i].callback) THEN
          mats := mats & "form_attach(SELF.FORM,  SELF." &
                      menu[i].Name & ".name, SELF." & menu[i].Name &
                      "Proc);\n";
        END
      END
    END;
    RETURN mats;
  END ComputeMenuAttachments;

PROCEDURE ComputeMenuSX (n: ZHandleVBT.T): TEXT =
  (* modify MenuTemplate using current state information *)
  VAR
    currentSX           : TEXT;
    fn                            := NARROW(n, NodeVBT.FormNode);
    menuDef, pulldownDef: TEXT;
    i, ix, ctr          : INTEGER;
    menu                          := fn.Menu;
  BEGIN
    IF menu = NIL THEN
      RETURN ""
    ELSE
      currentSX := MenuTemplate;
      currentSX :=
        NodeVBT.FindAndReplace(currentSX, "MenuBgColor", fn.MenuBgColor);
      currentSX :=
        NodeVBT.FindAndReplace(currentSX, "MenuFgColor", fn.MenuFgColor);
      currentSX :=
        NodeVBT.FindAndReplace(currentSX, "MenuFont", fn.MenuFont);
      menuDef := "";

      i := FIRST(menu^);
      WHILE i <= LAST(menu^) DO  (* i counts menu items *)
        IF menu[i].Level = 0 AND NOT Text.Equal(menu[i].Name, "RIDGE") THEN
          ctr := 0;
          ix := i + 1;           (* See if it is a pulldownmenuitem *)
          pulldownDef := "(VBox \n"; (* start creation of pulldown *)
          IF ix <= LAST(menu^) THEN (* if this is the end then it cant be a
                                       pulldown *)

            WHILE ix <= LAST(menu^) AND menu[ix].Level > 0 DO (* until next
                                                                 level 0
                                                                 item *)
              IF menu[ix].Level = 1 THEN (* only 2 levels in this
                                            implementation *)
                IF Text.Equal(menu[ix].Name, "RIDGE") THEN
                  pulldownDef := pulldownDef & "Ridge\n";
                ELSE
                  pulldownDef :=
                    pulldownDef & "(MenuButton " & menu[ix].Name
                      & "Filter " & menu[ix].initialState & " "
                      & menu[ix].Name & " " & menu[ix].Name & "Label "
                      & "\"" & menu[ix].Label & "\")\n"
                END;
                INC(ctr);
              END;
              INC(ix)
            END;
            pulldownDef := pulldownDef & ")\n"; (* close VBox *)
          END;                   (* of IF ix <= LAST(menu^) *)

          (* that should have computed pulldownDef *)

          IF (ctr > 0) THEN      (* this is indeed a pulldown *)
            menuDef :=
              menuDef & "(PullDownMenuItem " & menu[i].Name & "Filter "
                & menu[i].initialState & " " & menu[i].Name & " "
                & menu[i].Name & "Label " & "\"" & menu[i].Label & "\"\n"
                & pulldownDef & ")\n";
          ELSE
            menuDef :=
              menuDef & "(MenuItem " & menu[i].Name & "Filter "
                & menu[i].initialState & " " & menu[i].Name & " "
                & menu[i].Name & "Label " & "\"" & menu[i].Label & "\")\n";
          END;

          (* assert : ix is either beyond the end of the list or points to
             next level 0 item *)
          (* now we can move i forward *)
          i := ix - 1;           (* it will be incremented in any case *)

        END;                     (* of IF menu[i].Level = 0... *)
        INC(i)
      END;                       (* of WHILE i... *)
      currentSX := NodeVBT.FindAndReplace(currentSX, "MenuItems", menuDef);
      RETURN currentSX;
    END
  END ComputeMenuSX;

PROCEDURE InsertInBrowser (at: CARDINAL; item: T) =
  VAR
    offset       := "";
    text  : TEXT;
  BEGIN
    FOR i := 1 TO item.Level DO offset := offset & "..." END;
    IF Text.Equal(item.Name, "RIDGE") THEN
      text := offset & "-------------------------"
    ELSE
      text := offset & item.Label;
    END;
    WITH brow = NARROW(FormsVBT.GetVBT(Attributes.afv, "menubrowser"),
                       ListVBT.T) DO
      brow.insertCells(at, 1);
      brow.setValue(at, text);
      brow.selectOnly(at);
    END;
  END InsertInBrowser;


PROCEDURE ApplyProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                  afv : FormsVBT.T;
                     <* UNUSED *> name: TEXT;
                     <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    noo := NEW(REF ARRAY OF T, MC);
    ctr := 0;
  BEGIN
    (* make the changes happen on the parent form *)
    FOR i := FIRST(noo^) TO LAST(noo^) DO noo[i] := M[ctr]; INC(ctr) END;
    ParentForm.Menu := noo;
    WITH bgc  = FormsVBT.GetText(afv, "mbgctypein"),
         fgc  = FormsVBT.GetText(afv, "mfgctypein"),
         font = FormsVBT.GetText(afv, "mfonttypein") DO
      IF Text.Equal(bgc, "Inherit") THEN
        FormsVBT.PutText(afv, "mbgctypein", ParentForm.BgColor, FALSE);
        ParentForm.MenuBgColor := ParentForm.BgColor
      ELSE
        ParentForm.MenuBgColor := bgc
      END;
      IF Text.Equal(fgc, "Inherit") THEN
        FormsVBT.PutText(afv, "mfgctypein", ParentForm.FgColor, FALSE);
        ParentForm.MenuFgColor := ParentForm.FgColor
      ELSE
        ParentForm.MenuFgColor := fgc
      END;
      IF Text.Equal(font, "Inherit") THEN
        FormsVBT.PutText(afv, "mfonttypein", ParentForm.Font, FALSE);
        ParentForm.MenuFont := ParentForm.Font
      ELSE
        ParentForm.MenuFont := font
      END
    END;

    (* now make these changes happen on the actual menu bar *)
    (* note - the visibility of the menubar is controlled by the form attr
       sheet *)

    (* RenderMenuBar(ParentForm); *)

  END ApplyProc;

PROCEDURE LoadProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                 afv : FormsVBT.T;
                                 name: TEXT;
                    <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    (* if a doubleclick was involved *)
    IF Text.Equal(name, "menubrowser") THEN
      WITH sel = FormsVBT.GetInteger(afv, "menubrowser") DO
        FormsVBT.PutText(afv, "mlabel", M[sel].Label, FALSE);
        FormsVBT.PutText(afv, "mname", M[sel].Name, FALSE);
        FormsVBT.PutChoice(afv, "mInitialState", "m" & M[sel].initialState);
        IF M[sel].inForeGround THEN
          FormsVBT.PutChoice(afv, "mexechow", "mForeground")
        ELSE
          FormsVBT.PutChoice(afv, "mexechow", "mBackground")
        END;
        FormsVBT.PutText(afv, "menucallback", M[sel].callback, FALSE);
        FormsVBT.PutText(afv, "mLocation", M[sel].executeAt, FALSE);
        IF M[sel].isLocal THEN
          FormsVBT.PutChoice(afv, "mexecwhere", "mLocal");
          FormsVBT.MakeDormant(afv, "mremFilter");
        ELSE
          FormsVBT.PutChoice(afv, "mexecwhere", "mRemote");
          FormsVBT.MakeActive(afv, "mremFilter");
        END
      END
    ELSE
      (* must be ridge button *)
      FormsVBT.PutText(afv, "mlabel", "------------------------", FALSE);
      FormsVBT.PutText(afv, "mname", "RIDGE", FALSE);
      FormsVBT.PutChoice(afv, "mInitialState", "mActive");
      FormsVBT.PutChoice(afv, "mexechow", "mForeground");
      FormsVBT.PutText(afv, "menucallback", "", FALSE);
      FormsVBT.PutText(afv, "mLocation", "", FALSE);
      FormsVBT.PutChoice(afv, "mexecwhere", "mLocal");
      FormsVBT.MakeDormant(afv, "mremFilter");
    END
  END LoadProc;


PROCEDURE AddProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                afv : FormsVBT.T;
                                name: TEXT;
                   <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    level, pos: CARDINAL;
    sel       : INTEGER;
  BEGIN
    WITH label        = FormsVBT.GetText(afv, "mlabel"),
         nom          = FormsVBT.GetText(afv, "mname"),
         choice       = FormsVBT.GetChoice(afv, "mInitialState"),
         initialState = Text.Sub(choice, 1),
         infg = Text.Equal(
                  FormsVBT.GetChoice(afv, "mexechow"), "mForeground"),
         isloc = Text.Equal(
                   FormsVBT.GetChoice(afv, "mexecwhere"), "mLocal"),
         location = FormsVBT.GetText(afv, "mLocation"),
         callback = FormsVBT.GetText(afv, "menucallback"),
         brow     = NARROW(FormsVBT.GetVBT(afv, "menubrowser"), ListVBT.T) DO
      IF Text.Equal(name, "maddfirst") THEN
        level := 0;
        pos := 0;
        FormsVBT.MakeActive(afv, "mItemFilter");
      ELSIF brow.getFirstSelected(sel) THEN
        IF Text.Equal(name, "maddafter") OR Text.Equal(name, "mlabel") THEN
          level := M[sel].Level;
          (* pos = MIN(MC, next occurrence of a level <= this level) *)
          pos := sel + 1;
          WHILE pos < MC AND M[pos].Level > level DO INC(pos) END;
        ELSE
          (* "maddunder" *)
          level := M[sel].Level + 1;
          pos := sel + 1;
        END
      ELSE
        level := 0;              (* hitting enter without selection = add
                                    first *)
        pos := 0;
        FormsVBT.MakeActive(afv, "mItemFilter");
      END;
      (* insert cell in M at pos *)
      FOR ix := MC TO pos + 1 BY -1 DO M[ix] := M[ix - 1]; END;
      INC(MC);
      M[pos].Level := level;
      M[pos].Label := label;
      IF Text.Equal(nom, "") THEN
        M[pos].Name := label & "MenuItem";
        FormsVBT.PutText(afv, "mname", M[pos].Name, FALSE);
      ELSE
        M[pos].Name := nom
      END;
      M[pos].inForeGround := infg;
      M[pos].isLocal := isloc;
      M[pos].initialState := initialState;
      M[pos].executeAt := location;
      M[pos].callback := callback;
      InsertInBrowser(pos, M[pos]);
    END
  END AddProc;

PROCEDURE DelProc (             cl  : FormsVBT.Closure;
                                afv : FormsVBT.T;
                   <* UNUSED *> name: TEXT;
                                time: VBT.TimeStamp     ) =
  VAR
    sel := FormsVBT.GetInteger(afv, "menubrowser");
    start, finplusone, nentries: CARDINAL;
  BEGIN
    (* load attributes into form fields before deleting - so that the user
       gets feedback on what was deleted *)
    LoadProc(cl, afv, "menubrowser", time);

    (* compute how many entries need to be deleted as a result of this *)
    WITH level = M[sel].Level DO
      start := sel;
      finplusone := sel + 1;
      WHILE finplusone < MC AND M[finplusone].Level > level DO
        INC(finplusone)
      END
    END;
    nentries := finplusone - start;
    (* delete selection and update working store *)
    FOR i := finplusone TO MC - 1 DO M[i - nentries] := M[i]; END;
    MC := MC - nentries;
    WITH brow = NARROW(FormsVBT.GetVBT(afv, "menubrowser"), ListVBT.T) DO
      brow.removeCells(start, nentries);
      IF MC = 0 THEN
        FormsVBT.MakeDormant(afv, "mItemFilter")
      ELSIF MC > sel THEN
        brow.selectOnly(sel)
      ELSE
        brow.selectOnly(MC - 1)
      END
    END;
  END DelProc;

PROCEDURE ReplaceProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                    afv : FormsVBT.T;
                       <* UNUSED *> name: TEXT;
                       <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR pos: INTEGER;
  BEGIN
    WITH label        = FormsVBT.GetText(afv, "mlabel"),
         nom          = FormsVBT.GetText(afv, "mname"),
         choice       = FormsVBT.GetChoice(afv, "mInitialState"),
         initialState = Text.Sub(choice, 1),
         infg = Text.Equal(
                  FormsVBT.GetChoice(afv, "mexechow"), "mForeground"),
         isloc = Text.Equal(
                   FormsVBT.GetChoice(afv, "mexecwhere"), "mLocal"),
         location = FormsVBT.GetText(afv, "mLocation"),
         callback = FormsVBT.GetText(afv, "menucallback"),
         brow     = NARROW(FormsVBT.GetVBT(afv, "menubrowser"), ListVBT.T) DO
      EVAL brow.getFirstSelected(pos);
      M[pos].Label := label;
      IF Text.Equal(nom, "") THEN
        M[pos].Name := label & "MenuItem";
        FormsVBT.PutText(afv, "mname", M[pos].Name, FALSE);
      ELSE
        M[pos].Name := nom
      END;
      M[pos].inForeGround := infg;
      M[pos].isLocal := isloc;
      M[pos].initialState := initialState;
      M[pos].executeAt := location;
      M[pos].callback := callback;
    END
  END ReplaceProc;

PROCEDURE LRProc (<* UNUSED *> cl  : FormsVBT.Closure;
                               afv : FormsVBT.T;
                               name: TEXT;
                  <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    IF Text.Equal(name, "Local") THEN
      FormsVBT.MakeDormant(afv, "mremFilter")
    ELSE
      FormsVBT.MakeActive(afv, "mremFilter")
    END
  END LRProc;

PROCEDURE Initialize () =
  BEGIN

    WITH colorclosure = NEW(
                          FormsVBT.Closure, apply := Attributes.ColorProc),
         fontclosure = NEW(FormsVBT.Closure, apply := Attributes.FontProc) DO
      (* attach menu-attribute sheet color-popup-helper-buttons and
         font-helper-popup-buttons *)
      FormsVBT.Attach(Attributes.afv, "mbgc", colorclosure);
      FormsVBT.Attach(Attributes.afv, "mfgc", colorclosure);
      FormsVBT.Attach(Attributes.afv, "mfont", fontclosure);
    END;


    (* Attach the apply button *)
    WITH applyclosure = NEW(FormsVBT.Closure, apply := ApplyProc) DO
      (* attach apply button *)
      FormsVBT.Attach(Attributes.afv, "mapply", applyclosure);
    END;

    WITH loadclosure = NEW(FormsVBT.Closure, apply := LoadProc) DO
      (* attach browser dbl clk and ridge button - load stuff *)
      FormsVBT.Attach(Attributes.afv, "menubrowser", loadclosure);
      FormsVBT.Attach(Attributes.afv, "mridge", loadclosure);
    END;


    WITH addclosure = NEW(FormsVBT.Closure, apply := AddProc) DO
      (* attach addfirst, addafter, addunder buttons *)
      FormsVBT.Attach(Attributes.afv, "maddfirst", addclosure);
      FormsVBT.Attach(Attributes.afv, "maddafter", addclosure);
      FormsVBT.Attach(Attributes.afv, "maddunder", addclosure);
      FormsVBT.Attach(Attributes.afv, "mlabel", addclosure);
    END;

    WITH delclosure = NEW(FormsVBT.Closure, apply := DelProc) DO
      (* attach delete button *)
      FormsVBT.Attach(Attributes.afv, "mdelete", delclosure);
    END;

    WITH replaceclosure = NEW(FormsVBT.Closure, apply := ReplaceProc) DO
      (* attach delete button *)
      FormsVBT.Attach(Attributes.afv, "mreplace", replaceclosure);
    END;

    WITH lrclosure = NEW(FormsVBT.Closure, apply := LRProc) DO
      (* attach local and remote choices *)
      FormsVBT.Attach(Attributes.afv, "mLocal", lrclosure);
      FormsVBT.Attach(Attributes.afv, "mRemote", lrclosure);
    END;
    TRY
      WITH reader = Rsrc.Open("menuTEMPLATE.fv", Dialog.rsrcPath) DO
        MenuTemplate := Rd.GetText(reader, LAST(CARDINAL));
        Rd.Close(reader);
      END
    EXCEPT
    ELSE
      Dialog.message(Dialog.screen[1], "Error Loading : menuTEMPLATE.fv");
    END;
  END Initialize;

BEGIN
END DialogMenu.
