(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE Client EXPORTS Main;

IMPORT VBT, Trestle, FormsVBT, NetObj, Atom, AtomList, Err, Thread, TrestleComm, 
       HighlightVBT, Pixmap, TextureVBT, Color, Rect, PaintOp,
       Rd, Rsrc, Fmt, Text, Scan, 
       View, Win, WinUIBundle, Board, BoardServer, 
       ItemFont, Focus, PointR;

TYPE WinMouse = Win.T OBJECT
  OVERRIDES
    mouse := Mouse;
  END;

VAR wn: WinMouse := NIL;

TYPE State = {Disconnected, Focus, Text, Draw, Select};

VAR state := State.Disconnected;

(* The "state" is "Disconnected" when there is no "Win.T" installed. 
   All other states imply that a "Win.T" is installed. 
*)


PROCEDURE Mouse (wn: WinMouse; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      CASE state OF
      | State.Focus => 
        CASE cd.whatChanged OF
        | VBT.Modifier.MouseL => 
          Win.Magnifying (wn, cd);
        | VBT.Modifier.MouseM => 
          IF VBT.Modifier.Shift IN cd.modifiers THEN
            Win.Panning (wn, cd);
          ELSE
            Win.Dragging (wn, cd);
          END;
        | VBT.Modifier.MouseR => 
          Win.Reducing (wn, cd);
        ELSE (*SKIP*)
        END;
      | State.Text =>
        CASE cd.whatChanged OF
        | VBT.Modifier.MouseL => 
          Win.Typing (wn, cd);
        | VBT.Modifier.MouseM => 
          Win.PasteSource (wn, cd);
        | VBT.Modifier.MouseR => 
          (*SKIP*)
        ELSE (*SKIP*)
        END;
      | State.Draw =>
        CASE cd.whatChanged OF
        | VBT.Modifier.MouseL => 
          Win.Ruling (wn, cd);
        | VBT.Modifier.MouseM => 
          (*SKIP*)
        | VBT.Modifier.MouseR => 
          (*SKIP*)
        ELSE (*SKIP*)
        END;
      | State.Select =>
        CASE cd.whatChanged OF
        | VBT.Modifier.MouseL => 
          Win.Selecting (wn, cd);
        | VBT.Modifier.MouseM => 
          Win.SelectItem (wn, cd);
        | VBT.Modifier.MouseR => 
          Win.Moving (wn, cd);
        ELSE (*SKIP*)
        END;
      ELSE (*SKIP*)          
      END;
    ELSIF 
      cd.clickType = VBT.ClickType.LastUp THEN
      CASE Win.GetStatus (wn) OF
      | Win.Status.Ruling => Win.Nothing (wn);
      | Win.Status.Selecting => Win.Nothing (wn);
      | Win.Status.Panning => Win.Nothing (wn);
      | Win.Status.Dragging => Win.Nothing (wn);
      | Win.Status.Magnifying => Win.Nothing (wn);
      | Win.Status.Reducing => Win.Nothing (wn);
      | Win.Status.Moving => Win.Nothing (wn);
      ELSE (*SKIP: true of Status.Typing *)
      END;
    END;
  END Mouse;

CONST
  FormFile = "WinUI.fv";

<* FATAL FormsVBT.Error *>
<* FATAL FormsVBT.Unimplemented *>
<* FATAL Thread.Alerted *>
<* FATAL TrestleComm.Failure *>

PROCEDURE NewForm (): FormsVBT.T RAISES {FormsVBT.Error} =
  <* FATAL Thread.Alerted *>
  VAR fv: FormsVBT.T;
  BEGIN
    TRY
      fv := NEW(FormsVBT.T).initFromRsrc(
          FormFile, Rsrc.BuildPath("$WinUIPath", WinUIBundle.Get()));
    EXCEPT
    | Rd.Failure =>
        Err.Print ("Rd.Failure -- cannot read " & FormFile);
    | Rsrc.NotFound =>
        Err.Print ("Rsrc.NotFound -- cannot find resource " & FormFile);
    END;

    (* on the menu bar *)

    FormsVBT.AttachProc(fv, "create", Create);
    FormsVBT.AttachProc(fv, "open", Open);
    FormsVBT.AttachProc(fv, "save", Save);
    FormsVBT.AttachProc(fv, "close", Close);
    FormsVBT.AttachProc(fv, "remove", Remove);
    FormsVBT.AttachProc(fv, "quit", Quit);

    FormsVBT.AttachProc(fv, "contactserver", ContactServer);
    FormsVBT.AttachProc(fv, "board", BoardInput);
    FormsVBT.AttachProc(fv, "server", ServerInput);

    FormsVBT.AttachProc(fv, "f_1", ChangeFont);
    FormsVBT.AttachProc(fv, "f_2", ChangeFont);
    FormsVBT.AttachProc(fv, "f_3", ChangeFont);
    FormsVBT.AttachProc(fv, "f_4", ChangeFont);
    FormsVBT.AttachProc(fv, "f_5", ChangeFont);
    FormsVBT.AttachProc(fv, "f_6", ChangeFont);
    FormsVBT.AttachProc(fv, "f_7", ChangeFont);
    FormsVBT.AttachProc(fv, "f_8", ChangeFont);

    FormsVBT.AttachProc(fv, "fs_1", ChangeFontSize);
    FormsVBT.AttachProc(fv, "fs_2", ChangeFontSize);
    FormsVBT.AttachProc(fv, "fs_3", ChangeFontSize);
    FormsVBT.AttachProc(fv, "fs_4", ChangeFontSize);
    FormsVBT.AttachProc(fv, "fs_5", ChangeFontSize);
    FormsVBT.AttachProc(fv, "fs_6", ChangeFontSize);
    FormsVBT.AttachProc(fv, "fs_7", ChangeFontSize);

    FormsVBT.AttachProc(fv, "c_1", ChangeColor);
    FormsVBT.AttachProc(fv, "c_2", ChangeColor);
    FormsVBT.AttachProc(fv, "c_3", ChangeColor);
    FormsVBT.AttachProc(fv, "c_4", ChangeColor);
    FormsVBT.AttachProc(fv, "c_5", ChangeColor);
    FormsVBT.AttachProc(fv, "c_6", ChangeColor);
    FormsVBT.AttachProc(fv, "c_7", ChangeColor);
    FormsVBT.AttachProc(fv, "c_8", ChangeColor);

    (* on the side bar *)

    FormsVBT.AttachProc(fv, "state", ChangeState);

    FormsVBT.AttachProc(fv, "off_h", ChangeFocus);
    FormsVBT.AttachProc(fv, "off_v", ChangeFocus);
    FormsVBT.AttachProc(fv, "scale", ChangeFocus);
    FormsVBT.AttachProc(fv, "zoomrate", ChangeZoomRate);
    FormsVBT.AttachProc(fv, "reset", ResetFocus);

    FormsVBT.AttachProc(fv, "unselect", Unselect);
    FormsVBT.AttachProc(fv, "delete", Delete);
    FormsVBT.AttachProc(fv, "undo", Undo);
    FormsVBT.AttachProc(fv, "refresh", Refresh);

    RETURN fv;
  END NewForm;

VAR jobName: TEXT;
    serverName, boardName: TEXT;
    server: BoardServer.T;

PROCEDURE ContactServer (fv : FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    TRY
      VAR
        newServerName := FormsVBT.GetText (fv, "server");
        newBoardName := FormsVBT.GetText (fv, "board");
        newBoard: Board.T;
        newServer: BoardServer.T;
        serverDaemon: NetObj.Address;
      BEGIN
        IF Text.Equal (newServerName, "") THEN
          serverDaemon := NIL;
        ELSE
          TRY 
            serverDaemon := NetObj.Locate (newServerName);
          EXCEPT 
          | NetObj.Invalid => Error (fv, "Invalid server name");
          | NetObj.Error => 
            Error (fv, "Could not locate NetObj daemon at server"); 
          END;
        END;
        TRY 
          newServer := NetObj.Import ("BoardServer", serverDaemon);
        EXCEPT 
        | NetObj.Error => 
          Error (fv, "Could not import server object from NetObj daemon"); 
        END;
        IF newServer = NIL THEN
          Error (fv, Fmt.F ("BoardServer not running on %s", newServerName));
          RETURN;
        END;
        TRY 
          IF Text.Equal (jobName, "create") THEN
            newBoard := newServer.create (newBoardName);
          ELSIF Text.Equal (jobName, "open") THEN
            newBoard := newServer.open (newBoardName);
          ELSIF Text.Equal (jobName, "remove") THEN
            newServer.remove (newBoardName);
            FormsVBT.PopDown (fv, "contact");
            RETURN;
          END;
        EXCEPT
        | BoardServer.Failed (text) => 
          Error (fv, text);
          RETURN;
        END;

        IF state # State.Disconnected THEN
          Close (fv, "close", NIL, 0);
        END;

        boardName := newBoardName;
        serverName := newServerName;
        server := newServer;

        wn := NEW (WinMouse);
        FormsVBT.PutGeneric (fv, "win", wn.init (newBoard));
        wn.reportFocus := DisplayFocus;
        wn.reportError := DisplayError;
        Win.ChangeFont (wn, font);
        Win.ChangeColor (wn, color);
        Win.ChangeZoomRate (wn, zoomrate);
        View.ChangeFocus (wn, focus);
        HighlightVBT.SetTexture (wn, Pixmap.Gray, 
          op := PaintOp.Pair (PaintOp.Transparent, 
                              PaintOp.SwapPair (PaintOp.FromRGB (1.0, 0.0, 0.0),
                                                PaintOp.Bg)));
        HighlightVBT.SetRect (wn, Rect.Empty);
        FormsVBT.PutText (fv, "info",  serverName & ":" & boardName);
        FormsVBT.PopDown (fv, "contact");
        SetState (fv);
      END;
    EXCEPT
    | NetObj.Error (atom) => Error (fv, AtomList2Text (atom)); 
    | Thread.Alerted => Error (fv, "Thread.Alerted");
    END;
  END ContactServer; 

PROCEDURE Disconnect (fv : FormsVBT.T) =
  BEGIN
    wn.quit ();
    FormsVBT.PutGeneric (fv, "win", 
                         TextureVBT.New (txt := Pixmap.Gray));
    FormsVBT.PutText (fv, "mousel", "L:");
    FormsVBT.PutText (fv, "mousem", "M:");
    FormsVBT.PutText (fv, "mouser", "R:");
    FormsVBT.PutText (fv, "info", "Nothing open");
    ChangeFocus (fv, "", NIL, 0);
    state := State.Disconnected;
  END Disconnect;

PROCEDURE Create (fv : FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    jobName := "create";
    FormsVBT.PutText (fv, "jobname", "Create Board");
  END Create;

PROCEDURE Open (fv : FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    jobName := "open";
    FormsVBT.PutText (fv, "jobname", "Open Board");
  END Open;

PROCEDURE  Save (fv : FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    IF state = State.Disconnected THEN
      Error (fv, "Nothing open");
      RETURN;
    END;
    TRY
      server.save (boardName);
    EXCEPT
    | BoardServer.Failed (text) => 
      Error (fv, text);
    | NetObj.Error (atom) => Error (fv, AtomList2Text (atom)); 
    | Thread.Alerted => Error (fv, "Thread.Alerted");
    END;
  END Save;

PROCEDURE  Close (fv : FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    IF state = State.Disconnected THEN
      Error (fv, "Nothing open");
      RETURN;
    END;
    Disconnect (fv);
    TRY
      server.close (boardName);
    EXCEPT
    | BoardServer.Failed (text) => 
      Error (fv, text);
    | NetObj.Error (atom) => Error (fv, AtomList2Text (atom)); 
    | Thread.Alerted => Error (fv, "Thread.Alerted");
    END;
  END Close;

PROCEDURE Remove (fv : FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    jobName := "remove";
    FormsVBT.PutText (fv, "jobname", "Remove Board");
  END Remove;

PROCEDURE Quit (fv : FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    IF state # State.Disconnected THEN
      Close (fv, "close", NIL, 0);
    END;
    Trestle.Delete(fv);
  END Quit;

PROCEDURE BoardInput (fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  BEGIN
    FormsVBT.TakeFocus (fv, "server", ts);
  END BoardInput;

PROCEDURE ServerInput (fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; ts: VBT.TimeStamp) =
  BEGIN
    FormsVBT.TakeFocus (fv, "board", ts);
  END ServerInput; 

PROCEDURE ChangeState (fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    IF state # State.Disconnected THEN SetState (fv) END;
  END ChangeState;

PROCEDURE SetState (fv: FormsVBT.T) =
  BEGIN
      Win.Nothing (wn);
      VAR stateName := FormsVBT.GetChoice (fv, "state");
      BEGIN
        IF Text.Equal (stateName, "focus") THEN 
          state := State.Focus;
          FormsVBT.PutText (fv, "mousel", "L: magnify");
          FormsVBT.PutText (fv, "mousem", "M: pan");
          FormsVBT.PutText (fv, "mouser", "R: reduce");
        ELSIF Text.Equal (stateName, "text") THEN 
          state := State.Text;
          FormsVBT.PutText (fv, "mousel", "L: type");
          FormsVBT.PutText (fv, "mousem", "M: paste");
          FormsVBT.PutText (fv, "mouser", "R:");
        ELSIF Text.Equal (stateName, "draw") THEN 
          state := State.Draw;
          FormsVBT.PutText (fv, "mousel", "L: rule");
          FormsVBT.PutText (fv, "mousem", "M:");
          FormsVBT.PutText (fv, "mouser", "R:");
        ELSIF Text.Equal (stateName, "select") THEN 
          state := State.Select;
          FormsVBT.PutText (fv, "mousel", "L: many");
          FormsVBT.PutText (fv, "mousem", "M: one");
          FormsVBT.PutText (fv, "mouser", "R: move");
        ELSE (*SKIP*)
        END;
      END;
  END SetState;

VAR font: TEXT;

PROCEDURE ChangeFont (fv: FormsVBT.T; event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    font := FormsVBT.GetTextProperty (fv, event, "LabelFont");
    SetFontSize ();
    FormsVBT.PutTextProperty (fv, "info", "LabelFont", font);
    IF state # State.Disconnected THEN
      Win.ChangeFont (wn, font);
    END;
  END ChangeFont; 

VAR fontSize: TEXT;

PROCEDURE ChangeFontSize (fv: FormsVBT.T; event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    fontSize := FormsVBT.GetTextProperty (fv, event, "LabelFont");
    SetFontSize ();
    FormsVBT.PutTextProperty (fv, "info", "LabelFont", font);
    IF state # State.Disconnected THEN
      Win.ChangeFont (wn, font);
    END;
  END ChangeFontSize; 

PROCEDURE SetFontSize () =
  (* Sets the point size in "font" to that in "fontSize". 
     This uses knowledge of the X font naming scheme. The size component 
     follows the 8th hyphen in the name.
  *)
  VAR pre1, size1, post1: TEXT;
      pre2, size2, post2: TEXT;
  BEGIN
    ItemFont.SplitName (font, pre1, size1, post1);
    ItemFont.SplitName (fontSize, pre2, size2, post2);
    font := pre1 & size2 & post1;
  END SetFontSize;

VAR color := Color.Black;

PROCEDURE ChangeColor (fv: FormsVBT.T; event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
      color := FormsVBT.GetColorProperty (fv, event, "BgColor");
      FormsVBT.PutColorProperty (fv, "info", "Color", color);
      IF state # State.Disconnected THEN
        Win.ChangeColor (wn, color);
      END;
  END ChangeColor; 

VAR zoomrate := 0.5;

PROCEDURE ChangeZoomRate (fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    zoomrate := FLOAT (FormsVBT.GetInteger (fv, "zoomrate")) / 10.0;
    FormsVBT.PutText (fv, "zoomrate_feedback", Fmt.Real (zoomrate, 1));
    IF state # State.Disconnected THEN
      Win.ChangeZoomRate (wn, zoomrate);
    END;
  END ChangeZoomRate; 

VAR focus := NEW (Focus.T, offset := PointR.T {0.0, 0.0}, scale := 1.0);

PROCEDURE ChangeFocus (fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    TRY
      focus.offset.h := Scan.Real (FormsVBT.GetText (fv, "off_h"));
      focus.offset.v := Scan.Real (FormsVBT.GetText (fv, "off_v"));
      focus.scale := Scan.Real (FormsVBT.GetText (fv, "scale"));
      IF state # State.Disconnected THEN
        View.ChangeFocus (wn, focus);
      END;
    EXCEPT
    | Scan.BadFormat => Error (fv, "Bad format for a number");
      DisplayFocus (View.GetFocus (wn));
    END;
  END ChangeFocus;

PROCEDURE ResetFocus (<*UNUSED*> fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    focus.offset.h := 0.0;
    focus.offset.v := 0.0;
    focus.scale := 1.0;
    IF state # State.Disconnected THEN
      View.ChangeFocus (wn, focus);
    END;
    DisplayFocus (focus);
  END ResetFocus;

PROCEDURE DisplayFocus (READONLY focus: Focus.T) =
  (* makes use of the global variable "fv". *)
  BEGIN
    FormsVBT.PutText (fv, "off_h", Fmt.Real (focus.offset.h, 3, Fmt.Style.Sci));
    FormsVBT.PutText (fv, "off_v", Fmt.Real (focus.offset.v, 3, Fmt.Style.Sci));
    FormsVBT.PutText (fv, "scale", Fmt.Real (focus.scale, 3, Fmt.Style.Sci));
  END DisplayFocus; 

PROCEDURE Unselect (<*UNUSED*> fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    IF state # State.Disconnected THEN
      Win.DiscardSelection (wn);
    END;
  END Unselect; 

PROCEDURE Delete (<*UNUSED*> fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    IF state # State.Disconnected THEN
      Win.DeleteSelection (wn);
    END;
  END Delete;

PROCEDURE Undo (<*UNUSED*> fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    IF state # State.Disconnected THEN
      Win.Undo (wn);
    END;
  END Undo;

PROCEDURE Refresh (<*UNUSED*> fv: FormsVBT.T; <*UNUSED*> event: TEXT;
                <*UNUSED*> data: REFANY; <*UNUSED*> ts: VBT.TimeStamp) =
  BEGIN
    IF state # State.Disconnected THEN
      wn.refresh (Rect.Full);
      VBT.Sync (wn);
    END;
  END Refresh;

PROCEDURE Error (fv: FormsVBT.T; msg: TEXT) =
  BEGIN
    FormsVBT.PutText (fv, "errmsg", msg);
    FormsVBT.PopUp (fv, "error");
  END Error;

PROCEDURE DisplayError (msg: TEXT) =
  (* makes use of the global variable "fv". *)
  BEGIN
    Error (fv, msg);
  END DisplayError; 

PROCEDURE AtomList2Text (al: AtomList.T): TEXT =
  VAR text := "";
  BEGIN
    WHILE al # NIL DO
      text := text & " " & Atom.ToText (al.head);
      al := al.tail;
    END;
    RETURN text;
  END AtomList2Text;

VAR fv := NewForm ();
BEGIN
  Trestle.Install (fv);
  fontSize := FormsVBT.GetTextProperty (fv, "fs_4", "LabelFont");
  ChangeFont (fv, "f_2", NIL, 0);
  Trestle.AwaitDelete (fv);
END Client.

