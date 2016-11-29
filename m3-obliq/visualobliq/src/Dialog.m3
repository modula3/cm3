(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)


MODULE Dialog EXPORTS Main, Dialog;

IMPORT Attributes, Axis,  DialogBundle, FileBrowserVBT, FileRd,
       FileWr, Fmt, FormsVBT, FVTypes, GenerateObliq, NodeVBT,
       ObliqRuntime, Pathname, Point, Pts, RW, Rd, Rect, Rsrc, Split,
       Text, Thread, Trestle, TrestleComm, TSplit, VBT,
       VBTClass, Wr, ZHandleVBT, ZSplit,


       (* Import interfaces of extensions *)

       Browser, Clickable, DialogMenu, Textual, Setting, VideoWidget;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
<* FATAL Rd.Failure, Rsrc.NotFound, Thread.Alerted, TrestleComm.Failure *>
<* FATAL Wr.Failure *>
<* FATAL NodeVBT.InstanceListFull, NodeVBT.InvalidObjectName, Split.NotAChild *>


REVEAL
  T = Public BRANDED "VO-Dialog" OBJECT
      METHODS
        initSelection () := InitSelection;
      OVERRIDES
        realize := Realize;
      END;

TYPE

  Blocker = FVTypes.FVFilter OBJECT
            OVERRIDES
              mouse    := BlockMouse;
              position := BlockPosition;
            END;

CodeType = { None, Global, ServerSide, SessionConstructor };

CONST myVerNum = 30; (* 3.0 *)

VAR
  filename              := "<unnamed>";
  delta                 := 0;    (* global insertion offset *)
  testModeCtr      : INTEGER;
  testModeList     : ARRAY [1 .. 100] OF VBT.T;
  progname := "";
  cutnode : NodeVBT.T := NIL; (* result of last cut operation *)
  loadingVerNum := 0;
  SaveCodeIn : CodeType;

PROCEDURE InitSelection (fv: T) =
  BEGIN
    fv.selection := NEW(ZHandleVBT.Selection);
    fv.selection.init(50, TRUE, fv);
  END InitSelection;

PROCEDURE NewDialog (): T =
  VAR
    fv      := NEW(T).initFromRsrc("dialog.fv", rsrcPath);
    ccl     := NEW(FormsVBT.Closure, apply := CreateProc);
    modecl  := NEW(FormsVBT.Closure, apply := ModesProc);
    alncl    := NEW(FormsVBT.Closure, apply := AlignProc);
    shcl := NEW(FormsVBT.Closure, apply := ShapeProc);
    discl := NEW(FormsVBT.Closure, apply := DistributeProc);
    sxcl    := NEW(FormsVBT.Closure, apply := SXProc);
    sccl    := NEW(FormsVBT.Closure, apply := SCProc);
    svcl :=  NEW(FormsVBT.Closure, apply := SVRProc);
    gccl    := NEW(FormsVBT.Closure, apply := GCProc);
    buildcl := NEW(FormsVBT.Closure, apply := buildProc);
    cutcl   := NEW(FormsVBT.Closure, apply := CutProc);
    pastecl := NEW(FormsVBT.Closure, apply := PasteProc);
  BEGIN
    InitSelection(fv);

    FormsVBT.AttachProc (fv, "invite1", InviteProc);
    FormsVBT.AttachProc (fv, "invite2", InviteProc);
    FormsVBT.AttachProc (fv, "run", RunProc);
    FormsVBT.AttachProc (fv, "viewmethods", MethodsProc);
    FormsVBT.AttachProc (fv, "codeapply", ApplyCodeProc);

    FormsVBT.Attach(fv, "form", ccl);
    FormsVBT.Attach(fv, "frame", ccl);
    FormsVBT.Attach(fv, "button", ccl);
    FormsVBT.Attach(fv, "choice", ccl);
    FormsVBT.Attach(fv, "hscroll", ccl);
    FormsVBT.Attach(fv, "vscroll", ccl);
    FormsVBT.Attach(fv, "boolean", ccl);
    FormsVBT.Attach(fv, "numeric", ccl);
    FormsVBT.Attach(fv, "text", ccl);
    FormsVBT.Attach(fv, "textedit", ccl);
    FormsVBT.Attach(fv, "typein", ccl);
    FormsVBT.Attach(fv, "browser", ccl);
    FormsVBT.Attach(fv, "filebrowser", ccl);
    FormsVBT.Attach(fv, "video", ccl);

    FormsVBT.Attach(fv, "AlignNorth", alncl);
    FormsVBT.Attach(fv, "AlignSouth", alncl);
    FormsVBT.Attach(fv, "AlignEast", alncl);
    FormsVBT.Attach(fv, "AlignWest", alncl);
    FormsVBT.Attach(fv, "AlignHoriz", alncl);
    FormsVBT.Attach(fv, "AlignVert", alncl);  
    FormsVBT.Attach(fv, "AlignHoriz", alncl);
    FormsVBT.Attach(fv, "AlignCenVert", alncl);  
    FormsVBT.Attach(fv, "AlignCenHoriz", alncl); 

    FormsVBT.Attach(fv, "EqualWidth", shcl);
    FormsVBT.Attach(fv, "EqualHt", shcl);
    FormsVBT.Attach(fv, "EqualDim", shcl);

    FormsVBT.Attach(fv, "DistHoriz", discl);
    FormsVBT.Attach(fv, "DistVert", discl);
    FormsVBT.Attach(fv, "DistBoth", discl);

    FormsVBT.Attach(fv, "gensx", sxcl);
    FormsVBT.Attach(fv, "sxtypein", sxcl);
    FormsVBT.Attach(fv, "sxbutton", sxcl);

  
    FormsVBT.Attach(fv, "editsc", sccl);
    FormsVBT.Attach(fv, "editgc", gccl);
    FormsVBT.Attach(fv, "editsvr", svcl);

    FormsVBT.Attach(fv, "buildbtn", buildcl);
    FormsVBT.Attach(fv, "build", buildcl);

    (* editing actions *)
    FormsVBT.Attach(fv, "cut", cutcl);
    FormsVBT.Attach(fv, "delete", cutcl);
    FormsVBT.Attach(fv, "paste", pastecl);

    (* local settings *)
    FormsVBT.Attach(fv, "testbild", modecl);

    (* global settings *)
    FormsVBT.AttachProc (fv, "showsettings", PopSettingsProc);
    FormsVBT.AttachProc (fv, "oksettings", OKSettingsProc);

    FormsVBT.AttachProc (fv, "open", OpenProc);
    FormsVBT.AttachProc (fv, "openbtn", OpenProc);
    FormsVBT.AttachProc (fv, "openSuffixes", ChangeSuffixesProc);

    FormsVBT.AttachProc (fv, "saveas", SaveAsProc);
    FormsVBT.AttachProc (fv, "saveasbtn", SaveAsProc);

    FormsVBT.AttachProc (fv, "clearworkspace", ClearWorkspaceProc);
    FormsVBT.AttachProc (fv, "addscreen", AddScreenProc);
    FormsVBT.AttachProc (fv, "quit", QuitProc);

    RETURN fv;
  END NewDialog;


(* now the rigmarole to make the widgets in the menu inactive *)
PROCEDURE Realize (fv: T; type: TEXT; name: TEXT): VBT.T
  RAISES {FormsVBT.Error} =
  BEGIN
    IF Text.Equal(type, "Filter") THEN
      RETURN NEW(Blocker)
    ELSE
      RETURN FormsVBT.T.realize(fv, type, name);
    END;
  END Realize;

PROCEDURE BlockMouse (<* UNUSED *>          v : Blocker;
                      <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END BlockMouse;

PROCEDURE BlockPosition (<* UNUSED *>          v : Blocker;
                         <* UNUSED *> READONLY cd: VBT.PositionRec) =
  BEGIN
  END BlockPosition;

PROCEDURE message (fv: FormsVBT.T; txt: TEXT) =
  BEGIN
    FormsVBT.PutText(fv, "msg", txt, FALSE);
    WITH msgbox = FormsVBT.GetVBT(fv, "msgbox") DO
      ZSplit.Map(msgbox);
      ZSplit.Lift(msgbox)
    END
  END message;


PROCEDURE QuitProc (<* UNUSED *> fv  : FormsVBT.T;
                    <* UNUSED *> name: TEXT;
                    <* UNUSED *> data: REFANY;
                    <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    FOR i := 1 TO screens DO
      IF screen[i] # NIL THEN
        Trestle.Delete(screen[i]);
        screen[i] := NIL;
      END
    END
  END QuitProc;

PROCEDURE PasteProc(<* UNUSED *> cl  : FormsVBT.Closure;
                    fv  : FormsVBT.T;
                    <* UNUSED *> name: TEXT;
                    <* UNUSED *> time: VBT.TimeStamp) =
VAR
  dialog := NARROW(fv, T);
  n      := dialog.selection.getSelectionSize();
  zsplit  : VBT.T;
  BEGIN

    IF cutnode = NIL THEN RETURN; END;

    PixelsPerPtHor := Pts.ToPixels(fv, 1.0, Axis.T.Hor);
    PixelsPerPtVer := Pts.ToPixels(fv, 1.0, Axis.T.Ver);

    IF ISTYPE(cutnode, NodeVBT.FormNode) THEN
      WITH f = NARROW(cutnode, NodeVBT.FormNode) DO
        f.Screen := dialog.screenindex;
        f.ParentForm := NIL;
        zsplit := FormsVBT.GetVBT(fv, "topZSplit");
        ZSplit.InsertAt(NARROW(zsplit, ZSplit.T), cutnode,
                        Point.Add(Rect.NorthWest(VBT.Domain(zsplit)),
                                  Point.T{30, 30}));
        WITH 
          width  = ROUND(PixelsPerPtHor * FLOAT(cutnode.width)),
          height = ROUND(PixelsPerPtVer * FLOAT(cutnode.height)) DO
          ZSplit.Move(cutnode,
            Rect.FromCorners(Point.Add(Rect.NorthWest(VBT.Domain(zsplit)),
                                                 Point.T{30,30}),
                               Point.Add(Rect.NorthWest(VBT.Domain(zsplit)),
                                         Point.T{30+width,30+height})));
        END;
      END;
    ELSIF n < 1 THEN
      message(fv, "You need to select an object first"); RETURN;
    ELSIF n > 1 THEN
      message(fv, "You need to select a single object, a form or a frame");
      RETURN;
    ELSE
      WITH cso = dialog.selection.getSelection(1) DO
        IF NOT ISTYPE(cso, NodeVBT.SplitNode) THEN
          message(fv, "You can only insert in a form or a frame"); RETURN;
        END;
        WITH csn = NARROW(cso, NodeVBT.SplitNode) DO
          INC(csn.nc);
          csn.children[csn.nc] := cutnode;
          ZSplit.InsertAt(
              csn, cutnode, Point.Add(Rect.NorthWest(VBT.Domain(csn)),
                                   Point.T{10,10}));
          WITH 
            width  = ROUND(PixelsPerPtHor * FLOAT(cutnode.width)),
            height = ROUND(PixelsPerPtVer * FLOAT(cutnode.height)) DO
            ZSplit.Move(cutnode,
              Rect.FromCorners(Point.Add(Rect.NorthWest(VBT.Domain(csn)),
                                         Point.T{10,10}),
                               Point.Add(Rect.NorthWest(VBT.Domain(csn)),
                                         Point.T{10+width,10+height})));
          END;
          cutnode.parent := csn;

        END 
      END;
    END;
    NodeVBT.RecursivelyInsertInTables(cutnode, dialog.selection);
    cutnode := NIL; (* only once *)
  END PasteProc;

PROCEDURE CutProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                fv  : FormsVBT.T;
                                name: TEXT;
                   <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    moveback := FALSE;
    current  : NodeVBT.T;
  BEGIN
    WITH dialog = NARROW(fv, T),
         n      = dialog.selection.getSelectionSize() DO
      IF n < 1 THEN
        message(fv, "You need to select an object first")
      ELSIF n > 1 THEN
        message(fv, "You may only " & name & " single objects, not groups")
      ELSE
       
        WITH cso = dialog.selection.getSelection(1),
             csn = NARROW(cso, NodeVBT.T),
             par = csn.parent  DO
          IF Text.Equal(name, "cut") THEN
            cutnode := csn;
            NodeVBT.ComputeDimensions(csn);
          END;
          IF ISTYPE(csn, NodeVBT.FormNode) THEN
            (* disconnect all anchored forms *)
            WITH formclass = NodeVBT.NameToIndex("form"),
                 n         = NodeVBT.NoOfObjects(formclass) DO
              FOR i := 0 TO n - 1 DO
                IF i = 0 THEN
                  current := NodeVBT.GetFirst(formclass);
                ELSE
                  current := NodeVBT.GetNext(formclass);
                END;
                WITH cur = NARROW(current, NodeVBT.FormNode) DO
                  IF cur.ParentForm = csn THEN cur.ParentForm := NIL; END
                END
              END;
              WITH zsplit = FormsVBT.GetVBT(fv, "topZSplit"),
                   s      = NARROW(zsplit, Split.T)           DO
                Split.Delete(s, csn);
              END
            END;
          ELSE
            (* assert : csn is a member of the parent's children array *)
            WITH p = NARROW(par, NodeVBT.SplitNode) DO
              FOR i := 1 TO p.nc DO
                IF moveback THEN
                  p.children[i - 1] := p.children[i]
                ELSE
                  moveback := (p.children[i] = csn);
                END
              END;
              (* assert : moveback has to be true *)
              DEC(p.nc);
              Split.Delete(p, csn);
            END
          END;
          NodeVBT.RecursivelyDeleteFromTables(csn);
        END
      END;
    END
  END CutProc;


PROCEDURE ClearWorkspaceProc (<* UNUSED *> fv  : FormsVBT.T;
                              <* UNUSED *> name: TEXT;
                              <* UNUSED *> data: REFANY;
                              <* UNUSED *> time: VBT.TimeStamp) =
  VAR last := screen[1];         (* because of AwaitDelete *)
  BEGIN

    (* Get PixelsPerPt info *)

    PixelsPerPtHor := Pts.ToPixels(last, 1.0, Axis.T.Hor);
    PixelsPerPtVer := Pts.ToPixels(last, 1.0, Axis.T.Ver);

    FOR i := 2 TO screens DO
      IF screen[i] # NIL THEN
        Trestle.Delete(screen[i]);
        screen[i] := NIL;
      END
    END;
    screens := 1;
    filename := "<unnamed>";
    WITH z = NewDialog() DO
      Trestle.Install(z, applName := "VisualObliq",
                      inst := Fmt.Int(1),
                      windowTitle :=
                        "Visual Obliq Editor(" & Fmt.Int(1)
                          & ") - " & filename);
      screen[1] := z;
      z.screenindex := 1;
    END;
    IF last # NIL THEN Trestle.Delete(last); END;
    NodeVBT.ResetTables();
    NodeVBT.ReloadSExpressions();
    GenerateObliq.sessionConstructor := "CreateEachFormOnce(LOCAL);\n";
    GenerateObliq.globalCode  := "";
    GenerateObliq.serverSideCode  := "";
  END ClearWorkspaceProc;


PROCEDURE OpenProc (             fv  : FormsVBT.T;
                    <* UNUSED *> name: TEXT;
                    <* UNUSED *> data: REFANY;
                    <* UNUSED *> time: VBT.TimeStamp) =
  VAR
    s       : Rd.T;
    f, title: TEXT;
    last    : T;
  BEGIN
    f := FormsVBT.GetText(fv, "open");
    TRY
      s := FileRd.Open(f)
    EXCEPT
    ELSE
      message(fv, "Couldn't open " & f);
      RETURN;
    END;
    filename := Pathname.Last(f);
    RW.rtext(s, title);

    IF Text.Equal(title, "Visual Obliq File") THEN
      loadingVerNum := 25; (* version 2.5 *)      
    ELSIF Text.Equal(title, "Visual Obliq File, Version =") THEN
       RW.rint(s, loadingVerNum);         (* no of screens *)
       IF loadingVerNum > myVerNum THEN
         message(fv, "File has newer format than supported!");
         RETURN;
       END
    ELSE
      message(fv, "Illegal File Format : " & f);
      RETURN;
    END;

    last := screen[1];           (* because of AwaitDelete *)

    (* Get PixelsPerPt info *)

    PixelsPerPtHor := Pts.ToPixels(last, 1.0, Axis.T.Hor);
    PixelsPerPtVer := Pts.ToPixels(last, 1.0, Axis.T.Ver);

    FOR i := 2 TO screens DO
      IF screen[i] # NIL THEN
        Trestle.Delete(screen[i]);
        screen[i] := NIL;
      END
    END;

    RW.rint(s, screens);         (* no of screens *)

    IF screens < 1 THEN screens := 1 END;
    (* at all costs must have at least one screen !  0 screens =>
       error in file *)

    FOR i := 1 TO screens DO     (* make sure all the screens are
                                    present *)
      WITH z = NewDialog() DO
        Trestle.Install(
          z, applName := "VisualObliq", inst := Fmt.Int(i),
          windowTitle := "Visual Obliq Editor(" & Fmt.Int(i)
                           & ") - " & filename);
        screen[i] := z;
        z.screenindex := i;
      END
    END;

    IF last # NIL THEN Trestle.Delete(last); END;

    NodeVBT.LoadFromFile(screen[1], s);

    IF loadingVerNum >= 30 THEN (* only for backward compatibility *)
      (* load the session constructor code - version 3.0 and above *)
      RW.rtext(s, GenerateObliq.sessionConstructor);
      (* load the global code - version 3.0 and above *)
      RW.rtext(s, GenerateObliq.globalCode);
      (* load the server side code - version 3.0 and above *)
      RW.rtext(s, GenerateObliq.serverSideCode);
    END;

    FormsVBT.PopDown (fv, "open");
  END OpenProc;


PROCEDURE SaveAsProc (             fv  : FormsVBT.T;
                      <* UNUSED *> name: TEXT;
                      <* UNUSED *> data: REFANY;
                      <* UNUSED *> time: VBT.TimeStamp) =
  VAR
    f: TEXT;
    s: Wr.T;
  BEGIN
    f := FormsVBT.GetText(fv, "saveas");
    TRY
      s := FileWr.Open(f)
    EXCEPT
    ELSE
      message(fv, "Couldn't open " & f);
      RETURN;
    END;
    filename := Pathname.Last(f);
    RW.wtext(s,  "Visual Obliq File, Version =");
    RW.wint(s, myVerNum);
    RW.wint(s, screens);         (* no of screens *)
    NodeVBT.SaveToFile(fv, s);

    (* save the session constructor code - version 3.0 and above *)
    RW.wtext(s, GenerateObliq.sessionConstructor);
    (* save the global code - version 3.0 and above *)
    RW.wtext(s, GenerateObliq.globalCode);
    (* save the server side code - version 3.0 and above *)
    RW.wtext(s, GenerateObliq.serverSideCode);

    Wr.Close(s);
    FormsVBT.PopDown (fv, "saveas");
  END SaveAsProc;

PROCEDURE ChangeSuffixesProc (             fv  : FormsVBT.T;
                              <* UNUSED *> name: TEXT;
                              <* UNUSED *> data: REFANY;
                              <* UNUSED *> time: VBT.TimeStamp) =
  VAR fb: FileBrowserVBT.T;
  BEGIN
    TRY
      fb := FormsVBT.GetVBT(fv, "open");
      IF FormsVBT.GetBoolean(fv, "openSuffixes") THEN
        FileBrowserVBT.SetSuffixes(fb, "vo")
      ELSE
        FileBrowserVBT.SetSuffixes(fb, "")
      END
    EXCEPT
    | FormsVBT.Error => message(fv, "Couldn't change suffixes");
    END
  END ChangeSuffixesProc;


PROCEDURE RunProc (             fv  : FormsVBT.T;
                   <* UNUSED *> name: TEXT;
                   <* UNUSED *> data: REFANY;
                   <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
   
    ObliqRuntime.Do( GenerateObliq.GenerateCode(fv));
   
  END RunProc;

PROCEDURE ApplyCodeProc (             fv  : FormsVBT.T;
                   <* UNUSED *> name: TEXT;
                   <* UNUSED *> data: REFANY;
                   <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    IF SaveCodeIn = CodeType.Global THEN
      GenerateObliq.globalCode := FormsVBT.GetText(fv, "codeview");
    ELSIF SaveCodeIn = CodeType.ServerSide THEN
      GenerateObliq.serverSideCode := FormsVBT.GetText(fv, "codeview");
    ELSIF SaveCodeIn = CodeType.SessionConstructor THEN
      GenerateObliq.sessionConstructor := FormsVBT.GetText(fv, "codeview");
    END (* IF *)
  END ApplyCodeProc;
 
PROCEDURE MethodsProc (             fv  : FormsVBT.T;
                   <* UNUSED *> name: TEXT;
                   <* UNUSED *> data: REFANY;
                   <* UNUSED *> time: VBT.TimeStamp) =
VAR 
  parentage:= "";
  curparent : NodeVBT.T;
  par : NodeVBT.FormNode;

  BEGIN

    WITH dialog = NARROW(fv, T),
         n      = dialog.selection.getSelectionSize() DO
      IF n < 1 THEN
        message(fv, "You need to select an object first")
      ELSIF n > 1 THEN
        message( fv,  "Methods can be displayed only  for single objects, not for groups")
      ELSE
        WITH cso = dialog.selection.getSelection(1),
             csn = NARROW(cso, NodeVBT.T),
             classname  = NodeVBT.IndexToName(csn.classIndex),
             inforef = NodeVBT.GetInfo(classname)
         DO

          (* Change the title of the code viewer appropriately *)
          FormsVBT.PutText(fv, "cvtitle", csn.name &" - Interface");

          FormsVBT.PutText(fv, "codeTitle", "");
          SaveCodeIn := CodeType.None;
      

          (* get help on topic *)
          IF inforef = NIL THEN
            message( fv,  "Could not access method definition")
          ELSE

            (* Ascertain parentage of this widget *)
            IF ISTYPE(csn, NodeVBT.FormNode) THEN
              parentage := parentage & "The form-widget, " & csn.name & ", is also part of a form" &
                               " by the same name";
              curparent := csn;
            ELSE
              curparent := csn.parent;
              WHILE NOT ISTYPE(curparent, NodeVBT.FormNode) DO
                 curparent := curparent.parent;
              END;
              parentage := parentage &  csn.name & " is part of the form, " &
                               curparent.name;
            END;                
            
            (* If curparent is not the top then we list the anchorage *)
            par :=  NARROW(curparent, NodeVBT.FormNode).ParentForm;
            WHILE par # NIL DO
              curparent := par;
              parentage := parentage & ", which is anchored to " & par.name; 
              par := par.ParentForm;
            END;
            parentage := parentage & ". Within an instance of " & curparent.name &
                             " you may refer to the " & csn.name & " widget within it as SELF." &
                             csn.name & ". In other cases you would refer to this widget as " &
                             curparent.name & "[<index>]." & csn.name & " instead. Here " &
                             "<index> refers to the instance of " & curparent.name &
                             " containing the widget.\n\n";

            FormsVBT.PutText(fv, "cvinstns", "The selected object, " & csn.name &
              ", is an object of class, " & classname & ", and can be manipulated using " &
              "the methods shown below.\n\n" & 
              "Usage\n-----\n" & parentage & 
              "Also shown below are the methods in the Visual Obliq library. These may be refered to " &
              "in general as volibLocal.<method-name>. However callbacks may at times get executed " &
              "remotely. Hence, if you are within a callback it may be better to use  " &
              "LOCAL.<method-name> instead.\n");
            FormsVBT.PutText(fv, "codeview", inforef.info  & NodeVBT.GetInfo("Local").info);
          END
        END
      END

    END
    
   
  END MethodsProc;

PROCEDURE InviteProc (             fv  : FormsVBT.T;
                       <* UNUSED *> name: TEXT;
                       <* UNUSED *> data: REFANY;
                       <* UNUSED *> time: VBT.TimeStamp) =
  VAR where := FormsVBT.GetText(fv, "invite2");
  BEGIN
    FormsVBT.PutText(fv, "invite2", "");
    ObliqRuntime.Do("installAt (\"" & where & "\");");
  END InviteProc;

PROCEDURE buildProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                  fv  : FormsVBT.T;
                     <* UNUSED *> name: TEXT;
                     <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN

    progname := FormsVBT.GetText(fv, "build");   
    EVAL GenerateObliq.GenerateCode(fv, progname,FormsVBT.GetBoolean(fv, "singleFile"));
    FormsVBT.PopDown (fv, "buildDialog");
  END buildProc;

PROCEDURE ShapeProc (<* UNUSED *> cl  : FormsVBT.Closure;
  fv  : FormsVBT.T;
  name: TEXT;
  <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR   
  modeOfAlignment := FormsVBT.GetChoice(fv, "shapeMode");
  BEGIN 
    (* get hold of the selection *)
    WITH dfv = NARROW(fv, T) DO
      IF Text.Equal(name, "EqualDim") THEN
        dfv.selection.shapeSelectedObjects("EqualWidth", modeOfAlignment);
        dfv.selection.shapeSelectedObjects("EqualHt", modeOfAlignment);
      ELSE
        dfv.selection.shapeSelectedObjects(name, modeOfAlignment);
      END
    END (* WITH *)
  END ShapeProc; 

PROCEDURE DistributeProc (<* UNUSED *> cl  : FormsVBT.Closure;
  fv  : FormsVBT.T;
  name: TEXT;
  <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN 
    (* get hold of the selection *)
    WITH dfv = NARROW(fv, T) DO
      IF Text.Equal(name, "DistBoth") THEN
        dfv.selection.distributeSelectedObjects("DistHoriz");
        dfv.selection.distributeSelectedObjects("DistVert");
      ELSE
        dfv.selection.distributeSelectedObjects(name);
      END
    END (* WITH *)
  END DistributeProc; 


PROCEDURE AlignProc (<* UNUSED *> cl  : FormsVBT.Closure;
  fv  : FormsVBT.T;
  name: TEXT;
  <* UNUSED *> time: VBT.TimeStamp     ) =
VAR   
  modeOfAlignment := FormsVBT.GetChoice(fv, "alignMode");
  stretchAlign := FormsVBT.GetBoolean(fv, "stretchAlign");
  BEGIN
    (* get hold of the selection *)
    WITH dfv = NARROW(fv, T) DO
      IF Text.Equal(name, "AlignHoriz") THEN
        dfv.selection.alignSelectedObjects("AlignNorth", modeOfAlignment);
        dfv.selection.alignSelectedObjects("AlignSouth", modeOfAlignment);
          (* The next command may seem redundant but it is not 
             since the previous command moved the south
             edges there may be some alignments that are
            now legal - so we do the north again*) 
        dfv.selection.alignSelectedObjects("AlignNorth", modeOfAlignment);
      ELSIF Text.Equal(name, "AlignVert") THEN

        dfv.selection.alignSelectedObjects("AlignEast", modeOfAlignment);
        dfv.selection.alignSelectedObjects("AlignWest", modeOfAlignment);

        (* The next command may seem redundant but it is not 
            since the previous command moved the west
            edges there may be some alignments that are
            now legal - so we do the east again *)
        dfv.selection.alignSelectedObjects("AlignEast", modeOfAlignment);
      ELSE
      dfv.selection.alignSelectedObjects(name, modeOfAlignment, NOT stretchAlign);
      END
    END (* WITH *)
  END AlignProc;

PROCEDURE SXProc (<* UNUSED *> cl  : FormsVBT.Closure;
                               fv  : FormsVBT.T;
                               name: TEXT;
                  <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    filename          := FormsVBT.GetText(fv, "sxtypein");
    writer  : Wr.T;
  BEGIN
    IF Text.Equal(name, "gensx") THEN
      WITH dialog = NARROW(fv, T),
           n      = dialog.selection.getSelectionSize() DO
        IF n < 1 THEN
          message(fv, "You need to select an object first")
        ELSIF n > 1 THEN
          message( fv,   "You can generate the SX only for single objects, not for groups")
        ELSE
          WITH cso = dialog.selection.getSelection(1),
               csn = NARROW(cso, NodeVBT.T)            DO
            TRY
              NodeVBT.ComputeDimensions(csn);
              csn.DialogSX := csn.SXTemplate();
              FormsVBT.PutText(fv, "sxview", csn.computeSX(TRUE));
              FormsVBT.PopUp(fv, "sxviewer");
            EXCEPT
            ELSE
              message(fv, "Unable to generate SX for " & csn.name);
            END;
          END
        END
      END;
      RETURN;
    END;

    TRY
      writer := FileWr.Open(filename);
      Wr.PutText(writer, FormsVBT.GetText(fv, "sxview"));
      Wr.Close(writer);
    EXCEPT
    ELSE
      message(fv, "Unable to save to " & filename);
    END
  END SXProc;

PROCEDURE SCProc (<* UNUSED *> cl  : FormsVBT.Closure;
                               fv  : FormsVBT.T;
                   <* UNUSED *>            name: TEXT;
                  <* UNUSED *> time: VBT.TimeStamp     ) =
 
  BEGIN
    FormsVBT.PutText(fv, "cvtitle", "Edit Session Constructor");
   
    WITH t = Rsrc.Open("sessionConstructor.help", rsrcPath),
         contents = Rd.GetText(t, LAST(CARDINAL)) DO
      FormsVBT.PutText(fv, "cvinstns", contents & NodeVBT.GetInfo("Local").info);
      Rd.Close(t)
    END;

    
    FormsVBT.PutText(fv, "codeview",  GenerateObliq.sessionConstructor);
    SaveCodeIn := CodeType.SessionConstructor;
    FormsVBT.PutText(fv, "codeTitle", "Save Session Constructor");
 
  END SCProc;

PROCEDURE GCProc (<* UNUSED *> cl  : FormsVBT.Closure;
                               fv  : FormsVBT.T;
                     <* UNUSED *>          name: TEXT;
                  <* UNUSED *> time: VBT.TimeStamp     ) =
 
  BEGIN
    FormsVBT.PutText(fv, "cvtitle", "Edit Global Code");
   
    FormsVBT.PutText(fv, "cvinstns",
        "\t\tGlobal Code \n" 
    &   "\t\t***********\n\n"
    & "The code and data  you put in here will be shared by all members of the session. "
    & "Any code that is placed here will get executed at the server site  before the session "
    & "starts up. volibLocal will refer to the Visual Obliq library at the server. If you would "
    & "like the procedures and methods you define here to use the local instance of the "
    & "Visual Obliq library, you need to pass it in as a parameter.\n\n"
    & "This is a convenient place to keep synchronization variables, and status information that "
    & "new forms will need to bring shared widgets up to speed.\n\n"
    & NodeVBT.GetInfo("Local").info
      );
    FormsVBT.PutText(fv, "codeview",  GenerateObliq.globalCode);
    SaveCodeIn := CodeType.Global;
   FormsVBT.PutText(fv, "codeTitle", "Save Global Code");
  END GCProc;

PROCEDURE SVRProc (<* UNUSED *> cl  : FormsVBT.Closure;
                               fv  : FormsVBT.T;
                  <* UNUSED *>           name: TEXT;
                  <* UNUSED *> time: VBT.TimeStamp     ) =
 
  BEGIN
    FormsVBT.PutText(fv, "cvtitle", "Edit Server Side Code");
   
    FormsVBT.PutText(fv, "cvinstns", 
        "\t\tServer Side Code \n" 
    &   "\t\t****************\n\n"
    & "The code you place here will get executed at the server after the session comes up. "
    & "It will not be accessible to other hosts\n\n"
    & NodeVBT.GetInfo("Local").info
    );
    FormsVBT.PutText(fv, "codeview",  GenerateObliq.serverSideCode);
    SaveCodeIn :=  CodeType.ServerSide;
    FormsVBT.PutText(fv, "codeTitle", "Save Server Side Code");
  END SVRProc;



PROCEDURE ModesProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                  fv  : FormsVBT.T;
                     <* UNUSED *> name: TEXT;
                     <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    dialog               := NARROW(fv, T);
    formclass            := NodeVBT.NameToIndex("form");
    n                    := NodeVBT.NoOfObjects(formclass);
    current  : NodeVBT.T;
    offset := Rect.NorthWest(VBT.Domain(FormsVBT.GetVBT(fv, "topZSplit")));
  BEGIN
    TRY
      IF Text.Equal(FormsVBT.GetChoice(fv, "testbild"), "testmode") THEN
        (* create a ZChassis for all root forms and introduce them into the
           testZSplit *)
        dialog.TestMode := TRUE;
        testModeCtr := 0;
        NodeVBT.ComputeAnchoredFormTree();

        PixelsPerPtHor := Pts.ToPixels(fv, 1.0, Axis.T.Hor);
        PixelsPerPtVer := Pts.ToPixels(fv, 1.0, Axis.T.Ver);

        (* pass 1 - compute all the s-expressions and dimensions of root
           forms *)
        FOR i := 0 TO n - 1 DO
          IF i = 0 THEN
            current := NodeVBT.GetFirst(formclass);
          ELSE
            current := NodeVBT.GetNext(formclass);
          END;
          IF NARROW(current, NodeVBT.FormNode).ParentForm = NIL THEN
            (* root form *)
            NodeVBT.ComputeDimensions(current);
            current.DialogSX := current.SXTemplate();
            EVAL current.computeSX(TRUE);
          END
        END;


        (* now switch screens *)
        WITH tsplit = FormsVBT.GetVBT(fv, "testAndbild"),
             testbd = FormsVBT.GetVBT(fv, "testZSplit")   DO
          TSplit.SetCurrent(tsplit, testbd);
          VBTClass.Redisplay(tsplit);
        END;

        (* pass 2 - insert root forms into the zsplit *)
        FOR i := 0 TO n - 1 DO
          IF i = 0 THEN
            current := NodeVBT.GetFirst(formclass);
          ELSE
            current := NodeVBT.GetNext(formclass);
          END;
          IF NARROW(current, NodeVBT.FormNode).ParentForm = NIL THEN
            (* root form *)
            WITH newfv  = NEW(FormsVBT.T).init( "(ZChassis Open NoClose (Title \"\") " & 
                 current.DialogSX & ")"),
                (*  c = Filter.Replace(newfv, NIL), *)
                 zsplit = FormsVBT.GetVBT(fv, "testZSplit"),
                 z      = NARROW(zsplit, FVTypes.FVZSplit),
                 x      = ROUND(PixelsPerPtHor * FLOAT(current.x)),
                 y      = ROUND(PixelsPerPtVer * FLOAT(current.y)),
                 width  = ROUND(PixelsPerPtHor * FLOAT(current.width)),
                 height = ROUND(PixelsPerPtVer * FLOAT(current.height)),
                 nw     = Point.Add(offset, Point.T{x, y})               DO
              NodeVBT.print(
                "Inserting " & current.name & " at " & Fmt.Int(nw.h) & ","
                  & Fmt.Int(nw.v) & ":" & Fmt.Int(width) & ","
                  & Fmt.Int(height) & "\n");
              
              ZSplit.InsertAt(z, newfv, nw);
              (* ZSplit.InsertAt(z, c, nw);
                 ZChildVBT.Inserted(c); *)
              INC(testModeCtr);
              testModeList[testModeCtr] := newfv
            END
          END
        END
      ELSE
        dialog.TestMode := FALSE;
        FOR i := 1 TO testModeCtr DO
          WITH zsplit = FormsVBT.GetVBT(fv, "testZSplit"),
               s      = NARROW(zsplit, Split.T)            DO
            Split.Delete(s, testModeList[i]);
            testModeList[i] := NIL;
          END
        END;
        testModeCtr := 0;
        (* now switch screens *)
        WITH tsplit    = FormsVBT.GetVBT(fv, "testAndbild"),
             topzsplit = FormsVBT.GetVBT(fv, "topZSplit")    DO
          TSplit.SetCurrent(tsplit, topzsplit);
        END;

      END
    EXCEPT
    ELSE
      message(fv, "Trouble Switching Modes");
    END
  END ModesProc;

PROCEDURE AddScreenProc (<* UNUSED *> fv  : FormsVBT.T;
                         <* UNUSED *> name: TEXT;
                         <* UNUSED *> data: REFANY;
                         <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    WITH z = NewDialog() DO
      INC(screens);
      Trestle.Install(
        z, applName := "VisualObliq", inst := Fmt.Int(screens),
        windowTitle := "Visual Obliq Editor(" & Fmt.Int(screens)
                         & ") - " & filename);
      screen[screens] := z;
      z.screenindex := screens;
    END
  END AddScreenProc;

<* UNUSED *>
PROCEDURE AttributesProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                       fv  : FormsVBT.T;
                          <* UNUSED *> name: TEXT;
                          <* UNUSED *> data: REFANY;
                          <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    WITH dialog = NARROW(fv, T),
         n      = dialog.selection.getSelectionSize() DO
      IF n < 1 THEN
        message(fv, "You need to select an object first")
      ELSIF n > 1 THEN
        message(
          fv,
          "Attributes may be set only for single objects, not for groups")
      ELSE
        WITH cso = dialog.selection.getSelection(1),
             csn = NARROW(cso, NodeVBT.T)            DO
          Attributes.Invoke(attributes, csn)
        END
      END

    END
  END AttributesProc;

(* Settings are global - and apply to all screens *)
(* the widgets are explicitly synchronized *)
PROCEDURE OKSettingsProc (             fv  : FormsVBT.T;
                          <* UNUSED *> name: TEXT;
                          <* UNUSED *> data: REFANY;
                          <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    WITH dialog = NARROW(fv, T) DO
      dialog.grid := FormsVBT.GetInteger(fv, "grid")
    END;
  
    SetGlobalBg(FormsVBT.GetText(fv, "bgcolor"));
    SetGlobalFg(FormsVBT.GetText(fv, "fgcolor"));
    SetGlobalFont(FormsVBT.GetText(fv, "font"));
    SetDefaultName(FormsVBT.GetText(fv, "defname"));

    SetEditingFont(FormsVBT.GetBoolean(fv, "blowFont"));

  END OKSettingsProc;

PROCEDURE PopSettingsProc (             fv  : FormsVBT.T;
                           <* UNUSED *> name: TEXT;
                           <* UNUSED *> data: REFANY;
                           <* UNUSED *> time: VBT.TimeStamp) =
  BEGIN
    WITH dialog = NARROW(fv, T) DO
      FormsVBT.PutInteger(fv, "grid", dialog.grid)
    END;
    FormsVBT.PutText(
      fv, "bgcolor", NodeVBT.defaultBgColor, FALSE);
    FormsVBT.PutText(fv, "font", NodeVBT.defaultFont, FALSE);
    FormsVBT.PutText(
      fv, "fgcolor", NodeVBT.defaultFgColor, FALSE);
    FormsVBT.PutBoolean(
      fv, "blowFont", NodeVBT.blowEditingFont);
  END PopSettingsProc;

PROCEDURE SetEditingFont (blowit: BOOLEAN) =
  VAR font := "-*-courier-bold-*R-*120-*";
  BEGIN
    (* Do all the editors in the attr sheet *)
    IF blowit THEN
      font := "-*-helvetica-bold-*R-*180-*";
    END;

    WITH afv = Attributes.afv DO
      FormsVBT.PutTextProperty(afv, "supportCodeEditor", "Font",
                               font);
      FormsVBT.PutTextProperty(afv, "CallbackEditor", "Font",
                               font);
     (* FormsVBT.PutTextProperty(afv, "menubrowser", "Font",
                         font);*)
      FormsVBT.PutTextProperty(afv, "menucallback", "Font",
                          font);
    END;

    TRY
      FOR i := 1 TO screens DO
        WITH fv = screen[i] DO
          FormsVBT.PutBoolean(fv, "blowFont", blowit)
        END
      END
    EXCEPT
    ELSE
    END;
    NodeVBT.blowEditingFont := blowit;
  END SetEditingFont;


PROCEDURE SetGlobalBg (n: TEXT) =
  BEGIN
    TRY
      FOR i := 1 TO screens DO
        WITH fv = screen[i] DO
          FormsVBT.PutText(fv, "bgcolor", n, FALSE)
        END
      END
    EXCEPT
    ELSE
    END;
    NodeVBT.defaultBgColor := n;
  END SetGlobalBg;

PROCEDURE SetGlobalFg (n: TEXT) =
  BEGIN
    TRY
      FOR i := 1 TO screens DO
        WITH fv = screen[i] DO
          FormsVBT.PutText(fv, "fgcolor", n, FALSE)
        END
      END;
    EXCEPT
    ELSE
    END;
    NodeVBT.defaultFgColor := n;
  END SetGlobalFg;

PROCEDURE SetGlobalFont (n: TEXT) =
  BEGIN
    TRY
      FOR i := 1 TO screens DO
        WITH fv = screen[i] DO FormsVBT.PutText(fv, "font", n, FALSE) END
      END
    EXCEPT
    ELSE
    END;
    NodeVBT.defaultFont := n;
  END SetGlobalFont;

PROCEDURE SetDefaultName (n: TEXT) =
  BEGIN
    TRY
      FOR i := 1 TO screens DO
        WITH fv = screen[i] DO
          FormsVBT.PutText(fv, "defname", n, FALSE)
        END
      END
    EXCEPT
    ELSE
    END;
    NodeVBT.defaultName := n;
  END SetDefaultName; 

PROCEDURE CreateProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                   fv  : FormsVBT.T;
                                   name: TEXT;
                      <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR

    zsplit             : VBT.T;
    dialog                            := NARROW(fv, T);
    selsize            : CARDINAL;
    pardom             : Rect.T;
    parent             : ZHandleVBT.T;
    newnode            : NodeVBT.T;
    newform            : FormsVBT.T;
    minParWid, minParHt: INTEGER;
  BEGIN
    IF dialog.TestMode THEN RETURN; END;

    (* If this is a valid insertion ... *)
    (* i.e if it is a form elsif a single parent has been selected (a form
       or a frame) and there is enough space to insert the object { When
       you register a new widget you may specify how big the parent should
       be.  Pick a size at least 50 points larger than the default size of
       the widget, in each dimension.  Default min-size of parent = 100X100
       }

       *)
    selsize := dialog.selection.getSelectionSize();
    IF NOT Text.Equal(name, "form") THEN
      IF selsize < 1 THEN
        message(
          fv,
          "You need to select a form or a frame before inserting this object");
        RETURN;
      ELSIF selsize > 1 THEN
        message(
          fv,
          "At most one object may be selected before inserting such objects");
        RETURN;
      ELSE
        parent := dialog.selection.getSelection(1);
        IF parent = NIL THEN
          message(fv, "Selection Error");
          RETURN;
        ELSE 
          WHILE NOT ISTYPE(parent, NodeVBT.SplitNode) DO
            parent := parent.parent;
          END;
        END;

        pardom := parent.getDomain();
        TRY
          NodeVBT.GetMinParentDimensions(name, minParWid, minParHt);
        EXCEPT
        | NodeVBT.InvalidObjectName (foo) =>
            message(fv, foo & "has not been implemented");
        END;
        IF Rect.HorSize(pardom) < minParWid
          OR Rect.VerSize(pardom) < minParHt THEN
          message(
              fv,
              "There isn't enough space to insert this object. Resize parent.");
          RETURN;
        END
      END 
      END;
   

    (* all checks have been completed *)

    (* NodeVBT.NewObject is used to create a new instance *)
    (* which is then inserted into the hierarchy *)
    (* Only insertion related issues are addressed here *)
    (* insertion of forms is at 100+delta, 100+delta insertion of other
       objects is at 30+delta, 30+delta delta varies from 0 to 10 in steps
       of 2 *)

    IF Text.Equal(name, "form") THEN
      newnode := NodeVBT.NewObject(dialog, "form", NIL);
      NARROW(newnode, NodeVBT.FormNode).Screen := dialog.screenindex;
    ELSE
      newnode := NodeVBT.NewObject(dialog, name, NARROW(parent, NodeVBT.T))
    END;
    IF newnode = NIL THEN RETURN; END;
    TRY
      newform := NEW(FormsVBT.T).init(newnode.DialogSX)
    EXCEPT
    ELSE
      message(fv, "Error in Default S-Expression - Please Check "
                    & name & "TEMPLATE.fv");
      RETURN;
    END;
    EVAL ZHandleVBT.T.init(newnode, newform, dialog.selection);

    IF (Text.Equal(name, "form")) THEN
      zsplit := FormsVBT.GetVBT(fv, "topZSplit");
      ZSplit.InsertAt(NARROW(zsplit, ZSplit.T), newnode,
                      Point.Add(Rect.NorthWest(VBT.Domain(zsplit)),
                                Point.T{50 + delta, 50 + delta}))
    ELSE
      (* parent and pardom would already be set *)
      (* parent is a zsplit *)
      WITH dad = NARROW(parent, NodeVBT.SplitNode) DO
        INC(dad.nc);
        dad.children[dad.nc] := newnode;
      END;
      ZSplit.InsertAt(
        parent, newnode, Point.Add(Rect.NorthWest(VBT.Domain(parent)),
                                   Point.T{30 + delta, 30 + delta}))
    END;
    delta := (delta + 2) MOD 12;
 
  END CreateProc;

BEGIN
(*  Thread.IncDefaultStackSize(16*1024); *)
  
  rsrcPath := Rsrc.BuildPath ("$DIALOGPATH", DialogBundle.Get());
  ObliqRuntime.Setup();

  attributes := NEW(Attributes.T).init();

  (* Initialize all the modules *)
  NodeVBT.Initialize();

  (* Initialize Extension Modules *)

  Browser.Initialize();
  Clickable.Initialize();
  Setting.Initialize();
  Textual.Initialize();
  VideoWidget.Initialize(); 

  WITH z = NewDialog() DO
    INC(screens);
    z.screenindex := screens;
    Trestle.Install(
      z, applName := "VisualObliq",
      inst := "VO Editor (" & Fmt.Int(screens) & ")",
      windowTitle :=
        "Visual Obliq Editor(" & Fmt.Int(screens) & ") - " & filename);
    screen[screens] := z;


    DialogMenu.Initialize();
    GenerateObliq.Initialize();
  END;
  WHILE screen[1] # NIL DO Trestle.AwaitDelete(screen[1]) END;
END Dialog.
