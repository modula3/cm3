(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Postcard - a user interface for mail and news *)
(* Main program and main window *)

(* Last modified on Fri May 17 23:29:02 PDT 1996 by mhb       *)
(*      modified on Thu Oct 26 09:45:59 PDT 1995 by heydon    *)
(*      modified on Thu Jun  1 10:43:28 PDT 1995 by mcjones   *)
(*      modified on Wed Feb  1 12:20:17 PST 1995 by kalsow    *)
(*      modified on Wed Oct 12 13:55:37 PDT 1994 by birrell   *)
(*      modified on Fri Apr  9 20:50:39 PDT 1993 by meehan    *)
(*      modified on Fri Dec 20 11:24:26 PST 1991 by steveg *)

MODULE PostcardMain EXPORTS Main, Closure;

(* Imports. PLEASE KEEP IN ALPHABETICAL ORDER! *)
IMPORT
  AnyEvent, Axis,
  BuiltInCompose, BuiltInDisplay,
  Closure, Color, Config, Cursor,
  Env,
  FastMenu, Filter, FilterClass, Fmt,
    FormsVBT,
  HVSplit,
  Image,
  ListVBT,
  MailUtilities, MiscUtils, MultiSplit,
  NI,
  PaintOp, Params, Pixmap, PixmapVBT, Point, PostcardBundle, Process,
  Rd, Rect, Rescreen, Rsrc,
  ScrollerVBTClass, Split, StableVBT, Stdio,
  Text, TextEditVBT, TextList, TextPort, TextRd,
    TextWr, Thread, Trestle, TrestleComm,
  UnixMail,
  VBT, VBTClass,
  Wr,
  XTrestle,
  ZSplit;

<*FATAL Wr.Failure*>
<*FATAL Thread.Alerted*>
<*FATAL Rd.Failure*>
<*FATAL FormsVBT.Error*>
<*FATAL FormsVBT.Unimplemented*>

(********* Version ********)

(* 
** 5.9.8 (mhb): Moved calls to ChangeAllTextPorts into Config's SetFonts
** 5.9.7 (mhb): Exposed the X-URL field in "short headers"
** 5.9.6 (mhb): Changed calls to TextPort.ChangeAllTextPorts 
** previous: "Version 5.9.5 of 1st June 1995";
*)

CONST Version = "Version 5.9.8 of May 17, 1996";


(* *)
(* NOTE: all procedures in this program should have locking-level comments *)
(* *)


(* *)
(* A note on synchronization. *)
(* *)

(*
    There are three classes of thread executing within this module:
        a) notifier threads reporting events from Trestle via FormsVBT.
        b) up to one worker thread forked by ApplyOp.
        c) background threads.
    -  Class (a) holds VBT.mu; it cannot acquire "actions".
    -  Class (b) holds "actions" and can acquire VBT.mu.
    -  Class (c) can acquire "actions" and/or "VBT.mu", in that order.
    -  While (b) is executing the user interface components that form
       parameters for (b)'s operation are passive, and ApplyOp will not
       fork another worker thread, nor perform an ImmediateOp (though it
       will perform an AsyncOp).
    -  (b) and (c) can be made mutually exclusive by acquiring "actions".
    -  (a) and (c) can be made mutually exclusive by acquiring VBT.mu.
    
    No file system operations are performed with VBT.mu held.  Well,
    that's the intention; actually, FolderInsideClick calls UpdateButtons which
    calls UnixMail.IsBBoard, which which can block waiting for the file system
    in obscure cases.

    The locks are ordered: first "actions", then VBT.mu.

    All procedures are commented with "LL = ..."; please maintain this.
*)


(* *)
(* Globals *)
(* *)

CONST
  FolderModifiedChar = '*';       (* "news" marker in folder browser entries *)
  MailAliasFilename = "/.mail_aliases.sx";
  AppName = "Postcard";           (* for window decoration *)

CONST (* some FormsVBT symbol names used in multiple places *)
  Status = "Status";
  Splitter = "Splitter";
  NewFolderText = "NewFolderTxt";
  FindText = "FindTxt";
  PrintFilterText = "PrintTxt";
  SaveFile = "SaveFile";
  Compose = "Compose";
  AsyncErrorText = "AsyncErrorText";

TYPE Op = {
    Null,               (* Do nothing *)
  (* Initialization *)
    Init,
  (* Popup window(s) *)
    AsyncErrorClose,    (* Close AsyncErrorDlg *)
    ClosePopup,         (* Closes popup dialog *)
    Stop,               (* Alerts current worker thread *)
    Yes,                (* Generic positive response *)
    No,                 (* Generic negative response *)
    FindFirst,          (* "First" in "Find" dialog *)
    FindPrevious,       (* "Prev" in "Find" dialog *)
    FindNext,           (* "Next" in "Find" dialog *)
    FindStop,           (* "Stop" in "Find" dialog *)
    NIClose,            (* closing NI dialog *)
    NIReset,            (* "Reset" in NI dialog *)
    NIFlip,             (* "Flip" in NI dialog *)
    NICount,            (* "Count" in NI dialog *)
    NIBrowse,           (* "Browse" in NI dialog *)
    ConfigRevert,       (* Revert-to-saved in configuration dialog *)
  (* Main window's fixed buttons and menus *)
    About,              (* Pops up the "About" dialog *)
    Help,               (* Opens "Help" window *)
    SetConfiguration,   (* "Set Configuration" command *)
    Rescreen,           (* "Rescreen" command *)
    Quit,               (* "Quit" command *)
    NewFolder,          (* "Add Folder" command *)
    Rescan,             (* "Rescan" command *)
    SortPack,           (* "SortPack" command *)
    Purge,              (* "Purge" command *)
    RemoveFolder,       (* "Remove Folder" command *)
    Find,               (* "Search Private Folders" command *)
    NI,                 (* "Browse Bulletin Boards" command *)
    ShowConversation,   (* "Browse Discussion" command *)
    LoadPrevious,       (* "Load Previous Contents" command *)
    Print,              (* "Print" command in menus *)
    Save,               (* "Save" command *)
    Inc,                (* "Inc" command *)
    News,               (* "News" command *)
    FoldersFlip,        (* "Flip Folder List" command *)
    Move,               (* "Move" command *)
    Copy,               (* "Copy" command *)
    Delete,             (* "Delete" command *)
    DownArrow,          (* Down-arrow command *)
    UpArrow,            (* Up-arrow command *)
    Compose,            (* "Compose" command *)
    Forward,            (* "Forward" command *)
    ReplyToSender,      (* Reply to sender *)
    ReplyToAll,         (* Reply to all *)
  (* Message and folder browser *)
    DisplayMessage,     (* Display selected message *)
    OpenFolder,         (* Open selected folder *)
    HereOnlyToAvoidTrailingCommaProblems
  };

CONST
  NumericOps = SET OF Op {
    (* Op's that need filtered to remove NumericVBT's up/down arrows *)
  };

  ResponseOps = SET OF Op {
    (* Op's that are responses that complete a modal wait *)
    Op.Yes,
    Op.No,
    Op.ConfigRevert
  };

  AsyncOps = SET OF Op {
    (* Op's that can be performed asynchronously with worker threads *)
    Op.Stop,
    Op.AsyncErrorClose,
    Op.FindStop,
    Op.NIClose,
    Op.Quit,
    Op.FoldersFlip,
    Op.Compose
  };

  ImmediateOps = SET OF Op {
    (* Op's that can be handled synchronously, if no work in progress *)
    Op.Null,
    Op.ClosePopup,
    Op.Find,
    Op.NI,
    Op.NIReset,
    Op.About,
    Op.Rescreen
  };

TYPE MyForm = FormsVBT.T OBJECT
    cl: MyClosure;
    deletionOp := Op.Null; (* invoked when VBT gets deleted *)
  METHODS
    attach(name: TEXT; op: Op) RAISES { FormsVBT.Error } := MyFormAttach;
  OVERRIDES
    misc := MyFormMisc;
    realize := MyFormRealize;
  END;

TYPE LifterFV = MyForm OBJECT
  OVERRIDES
    mouse := LifterMouse;
  END;

TYPE MyClosure = Closure.T OBJECT
  (* The following fields are read-only after initialization *)
    window: VBT.T;                      (* main window *)
    fv: MyForm;                         (* the main form *)
    popupFV: MyForm;                    (* the pop-up form *)
    niBrowserFV: MyForm;                (* the NI browser form and window *)
    display: BuiltInDisplay.T;
    compose: BuiltInCompose.T;
    ni: NI.T;                           (* NI browser machinery *)
    msgBrowser, folderBrowser: ListVBT.T;
    contentsColors: PaintOp.ColorScheme;
    scrollColors:   PaintOp.ColorQuad;
  (* The following fields require actions+VBT.mu for updates, one for reads *)
    configFV: MyForm := NIL;            (* the config dlg *)
    helpWindow: VBT.T := NIL;           (* the help window *)
    config: Config.T := NIL;            (* current configuration *)
  (* The following fields are protected by VBT.mu *)
    worker: Thread.T := NIL;            (* thread forked by ApplyOp *)
    showingError := FALSE;              (* whether popup has error dlg in it *)
    firstPopup := TRUE;                 (* to center popup first time *)
    isPassive := FALSE;                 (* recorded by last call of .passive *)
    response := Op.Null;                (* response to modal wait *)
    responseTime: VBT.TimeStamp;        (* event time of response *)
    responseCV: Thread.Condition;       (* block here waiting for a response *)
    statusText := "";                   (* cached status line contents *)
    statusLineUp := FALSE;              (* status line not yet cleared *)
    statusLineThread: Thread.T := NIL;  (* auto-clearing thread *)
    titleText := "";                    (* main window's title bar *)
    mailCount := 0;                     (* unseen messages in inbox *)
    newsCount := 0;                     (* bboard's with new msgs *)
    firstUpdateButtons := TRUE;         (* LockedUpdateButtons *)
    messageSelected: BOOLEAN;           (* LockedUpdateButtons *)
    targetMutable: BOOLEAN;             (* LockedUpdateButtons *)
    sourceBBoard: BOOLEAN;              (* LockedUpdateButtons *)
    sourceNI: BOOLEAN;                  (* LockedUpdateButtons *)
    sourceInbox: BOOLEAN;               (* LockedUpdateButtons *)
    sourceDeleted: BOOLEAN;             (* LockedUpdateButtons *)
    hadPrevOpen: BOOLEAN;               (* LockedUpdateButtons *)
    stopFindRequest := FALSE;           (* "stop" button for "find" *)
    foldersFlipDelta: INTEGER := 0;     (* amount added to window width *)
  (* The following fields are protected by "actions" *)
    bbPtr: UnixMail.FolderPtr;          (* current pos for news *)
    downward: BOOLEAN;                  (* direction of last up/down arrow *)
    openFolder: TEXT;                   (* currently open folder *)
    prevOpenFolder: TEXT := NIL;        (* saved state for "load previous" *)
    prevOpenContents: REF ARRAY OF TEXT := NIL;
    prevSel: REF ARRAY OF ListVBT.Cell;
    findPtr: UnixMail.FolderPtr := NIL; (* state of "find" machinery *)
    oldFindText: TEXT := "";            (* previous "find" target *)
  METHODS
    error(msg: TEXT) := Error;
      (* LL = VBT.mu *)
      (* Pops up error dialog *)
    unlockedError(msg: TEXT) := UnlockedError;
      (* LL = actions *)
      (* Acquires VBT>mu and calls .error *)
    passive(bePassive: BOOLEAN) := Passive;
      (* LL = VBT.mu *)
      (* Make UI passive enough that no new commands will be invoked and
         no command parameters will change; or undoes that state *)
    doModalLocked(VAR time: VBT.TimeStamp): Op := DoModalLocked;
      (* LL = VBT.mu *)
      (* Blocks until .applyOp wakes it up, which happens when .applyOp
         gets called with an Op in ResponseOps.  Returns the op and time *)
    doModal(VAR time: VBT.TimeStamp): Op := DoModal;
      (* LL = actions *)
      (* Calls doModalLocked under VBT.mu *)
    doModalPopup(name: TEXT;
                 focus: TEXT := NIL;
                 replaceMode := FALSE;
                 time: VBT.TimeStamp := 0): Op := DoModalPopup;
      (* LL = actions *)
      (* Pops up given named popup, and uses .doModal to get a response,
         then pops down the dialog and returns the response Op. *)
    confirm(msg: TEXT; time: VBT.TimeStamp): BOOLEAN := Confirm;
      (* LL = actions *)
      (* Uses .doModalPopup with ConfirmDlg to block until we get a
         boolean response from the user. *)
    initInstance() := InitInstance;
      (* LL = actions *)
      (* Called for Op.Init, only; and only from within ForkedOp *)
      (* Performs most of the initialization. *)
    applyOp(op: Op; time: VBT.TimeStamp; event: AnyEvent.T := NIL) := ApplyOp;
      (* LL = VBT.mu *)
      (* Performs the operation: by forking a worker thread, or synchronously,
         or by waking up the worker thread. *)
  OVERRIDES
    AsyncErrorHandler := AsyncErrorHandler;
    GetPrintFilter := GetPrintFilter;
    GetEditorFilter := GetEditorFilter;
    GetPSViewFilter := GetPSViewFilter;
    GetPSPrintFilter := GetPSPrintFilter;
    SetTextPortColors := SetTextPortColors;
    SetFonts := SetFonts;
    InsertSelectedMessages := InsertSelectedMessages;
    invokeNIBrowse := InvokeNIBrowse;
    loadFromNI := LoadFromNI;
  END;

VAR
  user, home: TEXT;                                         (* read-only *)
  actions := NEW(Thread.Mutex);
  mailCheckMutex := NEW(Thread.Mutex);         (* protect spooled mail state *)


(* *)
(* Minor VBT subroutines *)
(* *)

TYPE CenterReshape = ZSplit.ReshapeControl OBJECT
  OVERRIDES
    apply := MaintainCenter;
  END;

PROCEDURE EnsureVisible(c, p: Rect.T): Rect.T =
  (* LL = any *)
  (* Ensure that "c" is reasonably visible in "p".  More precisely: return
     a rectangle "r" congruent to "c", and closest to "c", such that r.north
     is in [p.north..p.south), and such that the middle of "c" is as nearly
     as possible within "p" *)
  BEGIN
    WITH r1 = Rect.Center(c, Rect.Project(p, Rect.Middle(c))) DO
      RETURN Rect.Add(r1, Point.T{h := 0,
                                  v := MAX(0, p.north-r1.north)});
    END;
  END EnsureVisible;

PROCEDURE MaintainCenter(<*UNUSED*> self: CenterReshape;
                         <*UNUSED*> ch: VBT.T;
                         READONLY old, new, prev: Rect.T): Rect.T =
  (* LL.sup = VBT.mu.ch *)
  (* Maintain delta between centers, subject to keeping visible *)
  BEGIN
    RETURN EnsureVisible(Rect.Center(prev,
                                     Point.Add(Rect.Middle(prev),
                                               Point.Sub(Rect.Middle(new),
                                                         Rect.Middle(old)))),
                         new);
  END MaintainCenter;

PROCEDURE Beep(v: VBT.T) =
    (* LL = VBT.mu *)
  BEGIN
    VBT.PaintTint(v, Rect.Full, PaintOp.Swap);
    VBT.Sync(v);
    Thread.Pause(0.2D0);
    VBT.PaintTint(v, Rect.Full, PaintOp.Swap);
    VBT.Sync(v);
  END Beep;

PROCEDURE ShowWindow(w, near: VBT.T; title: TEXT): BOOLEAN =
    (* LL = VBT.mu *)
    (* Ensure "w" is visible; if installing, place it near "near". *)
  VAR soRec := Trestle.ScreenOf(w, Point.Origin);
  BEGIN
    TRY
      IF soRec.trsl = NIL THEN
        Trestle.Attach(w);
        Trestle.Decorate(v := w,
                         applName := AppName,
                         windowTitle := title,
                         iconTitle := title);
      END;
      IF soRec.id = Trestle.NoScreen THEN
        Trestle.MoveNear(w, near);
      ELSE
        Trestle.MoveNear(w, NIL);
      END;
    EXCEPT TrestleComm.Failure =>
      RETURN FALSE;
    END;
    RETURN TRUE
  END ShowWindow;

PROCEDURE Retitle(w: VBT.T; title: TEXT) =
    (* LL = VBT.mu *)
  BEGIN
    TRY
      Trestle.Decorate(v := w, windowTitle := title, iconTitle := title);
    EXCEPT
    | TrestleComm.Failure =>
    END;
  END Retitle;

PROCEDURE HideWindow(w: VBT.T) =
    (* LL = VBT.mu *)
  VAR soRec := Trestle.ScreenOf(w, Point.Origin);
  BEGIN
    IF soRec.trsl # NIL THEN Trestle.Delete(w) END;
  END HideWindow;

PROCEDURE ImageFromRsrc(name: TEXT; path: Rsrc.Path): Pixmap.T
                       RAISES { Thread.Alerted } =
    (* LL = any *)
  <* FATAL Rsrc.NotFound, Rd.Failure, Image.Error *>
  BEGIN
    RETURN Image.Unscaled(Image.FromRd(Rsrc.Open(name, path)))
  END ImageFromRsrc;


(* *)
(* Title bar *)
(* *)

PROCEDURE CountAsText (n: INTEGER; thing: TEXT): TEXT =
  (* LL = arbitrary *)
  VAR wr := TextWr.New ();
  BEGIN
    Wr.PutText (wr, Fmt.Int (n));
    Wr.PutChar (wr, ' ');
    Wr.PutText (wr, thing);
    IF n # 1 THEN Wr.PutChar (wr, 's') END;
    RETURN TextWr.ToText (wr)
  END CountAsText;

PROCEDURE LockedUpdateTitle (cl: MyClosure) =
  (* Gets called whenever there's a change in new mail state, or open folder,
     or selected folder.  Today, only new mail state is considered in this
     code. *)
  (* LL = VBT.mu *)
  VAR
    title, newMailText: TEXT;
    wr                       := TextWr.New ();
  BEGIN
    IF (cl.mailCount = 0) AND (cl.newsCount = 0) THEN
      Wr.PutText (wr, "No new mail");
    ELSE
      IF cl.mailCount # 0 THEN
        Wr.PutText (wr, CountAsText (cl.mailCount, "message"));
        IF cl.newsCount # 0 THEN Wr.PutText (wr, ",  "); END;
      END;
      IF cl.newsCount # 0 THEN
        Wr.PutText (wr, CountAsText (cl.newsCount, "bboard"));
      END;
    END;
    newMailText := TextWr.ToText (wr);
    title := AppName & ": " & newMailText;
    IF NOT Text.Equal (title, cl.titleText) THEN
      cl.titleText := title;
      Retitle(cl.window, title);
    END;
  END LockedUpdateTitle;

PROCEDURE UpdateTitle(cl: MyClosure) =
    (* LL = actions *)
  VAR count: CARDINAL;
  BEGIN
    count := UnixMail.GetNewsCount();
    LOCK VBT.mu DO cl.newsCount := count; LockedUpdateTitle(cl);  END;
  END UpdateTitle;


(* *)
(* Button enable/disable  *)
(* *)

PROCEDURE LockedUpdateButtons(cl: MyClosure) =
    (* Gets called whenever there's a state change that might affect buttons *)
    (* LL = VBT.mu *)
  VAR target: TEXT;
  VAR
    messageSelected, targetMutable, sourceBBoard, sourceNI, sourceInbox,
    sourceDeleted, prevOpen: BOOLEAN;
  PROCEDURE DoButton(name: TEXT; active: BOOLEAN) =
    BEGIN
      IF active THEN
        FormsVBT.MakeActive(cl.fv, name);
      ELSE
        FormsVBT.MakeDormant(cl.fv, name);
      END;
    END DoButton;
  BEGIN
    messageSelected := SelectedMessage(cl);
    target := GetSelectedFolder(cl, FALSE);
    targetMutable :=  NOT Text.Empty(target) AND
      NOT UnixMail.IsBBoard(target) AND
      NOT Text.Equal(target, cl.openFolder);
    sourceBBoard := UnixMail.IsBBoard(cl.openFolder);
    sourceNI := Text.Equal(cl.openFolder, UnixMail.NIFolderName);
    sourceInbox := Text.Equal(cl.openFolder, UnixMail.InboxName);
    sourceDeleted
      := Text.Equal(cl.openFolder, cl.config.deleteMessagesToFolder);
    prevOpen := (cl.prevOpenFolder # NIL);
    IF (messageSelected # cl.messageSelected)
      OR (targetMutable # cl.targetMutable)
      OR (sourceBBoard # cl.sourceBBoard) OR (sourceNI # cl.sourceNI)
      OR (sourceInbox # cl.sourceInbox) OR (sourceDeleted # cl.sourceDeleted)
      OR (prevOpen # cl.hadPrevOpen)
      OR cl.firstUpdateButtons THEN
          (* Something changed, so fix up all the buttons *)
      cl.messageSelected := messageSelected;
      cl.targetMutable := targetMutable;
      cl.sourceBBoard := sourceBBoard;
      cl.sourceNI := sourceNI;
      cl.sourceInbox := sourceInbox;
      cl.sourceDeleted := sourceDeleted;
      cl.hadPrevOpen := prevOpen;
      cl.firstUpdateButtons := FALSE;
      DoButton("RescanFilter",  NOT sourceNI);
      DoButton("SortPackFilter", ( NOT sourceNI) AND ( NOT sourceBBoard));
      DoButton("PurgeFilter",  NOT sourceNI);
      DoButton("RemoveFilter", ( NOT sourceInbox) AND ( NOT sourceDeleted)
                AND ( NOT sourceNI));
      DoButton("ShowConvFilter", messageSelected
                AND (sourceNI OR sourceBBoard));
      DoButton("LoadPrevFilter", prevOpen);
      DoButton("PrintFilter", messageSelected);
      DoButton("SaveFilter", messageSelected);
      DoButton("MoveFilter", messageSelected AND ( NOT sourceNI)
                AND ( NOT sourceBBoard) AND targetMutable);
      DoButton("CopyFilter", messageSelected AND targetMutable);
      DoButton("DeleteFilter", messageSelected AND (NOT sourceNI)
                AND (NOT sourceBBoard) AND (NOT sourceDeleted));
      DoButton("DownArrowFilter", messageSelected OR sourceBBoard);
      DoButton("UpArrowFilter", messageSelected);
      DoButton("ForwFilter", messageSelected);
      DoButton("ReplyToSenderFilter", messageSelected);
      DoButton("ReplyToAllFilter", messageSelected);
      VAR menu: FastMenu.T; BEGIN
        menu := FormsVBT.GetVBT(cl.fv, "BrowserMenu");
        menu.middle := FormsVBT.GetVBT(cl.fv, "NI");
        IF prevOpen THEN
          menu.right := FormsVBT.GetVBT(cl.fv, "LoadPrevious");
        ELSE
          menu.right := NIL;
        END;
        menu := FormsVBT.GetVBT(cl.fv, "ReplyMenu");
        IF messageSelected THEN
          menu.middle := FormsVBT.GetVBT(cl.fv, "ReplyToSender");
        ELSE
          menu.middle := NIL;
        END;
        IF messageSelected THEN
          menu.right := FormsVBT.GetVBT(cl.fv, "ReplyToAll");
        ELSE
          menu.right := NIL;
        END;
      END;
    END;
  END LockedUpdateButtons;

PROCEDURE UpdateButtons(cl: MyClosure) =
    (* LL = actions *)
  BEGIN
    LOCK VBT.mu DO LockedUpdateButtons(cl) END;
  END UpdateButtons;

PROCEDURE MakeActive(<*UNUSED*>cl: MyClosure; fv: FormsVBT.T; name: TEXT) =
    (* LL = VBT.mu *)
  BEGIN
    FormsVBT.MakeActive(fv, name);
    VBT.SetCursor(FormsVBT.GetVBT(fv, name), Cursor.TextPointer);
  END MakeActive;

PROCEDURE MakePassive(cl: MyClosure; fv: FormsVBT.T; name: TEXT) =
    (* LL = VBT.mu *)
  BEGIN
    VBT.SetCursor(FormsVBT.GetVBT(fv, name), cl.waitCursor);
    FormsVBT.MakePassive(fv, name);
  END MakePassive;

PROCEDURE MakeDormant(<*UNUSED*> cl: MyClosure; fv: FormsVBT.T; name: TEXT) =
    (* LL = VBT.mu *)
  BEGIN
    VBT.SetCursor(FormsVBT.GetVBT(fv, name), Cursor.DontCare);
    FormsVBT.MakeDormant(fv, name);
  END MakeDormant;


(* *)
(* Status line  *)
(* *)

(* The status line gets set during command execution, by calls of
   SetStatusLine.  At command termination, ClearStatusLine is called if
   showingError or not statusLineUp.  At start of command execution,
   ClearStatusLine is called if statusLineUp.  Asynchronously, if
   SetStatusLine said AutoFlush, the StatusLineThread will call
   ClearStatusLine after a time delay and provided statusLineUp is still
   true. *)

PROCEDURE OpenFolderStatus(cl: MyClosure): TEXT =
    (* LL = VBT.mu *)
  BEGIN
    IF Text.Equal(cl.openFolder, UnixMail.NIFolderName) THEN
      RETURN "NI browser";
    ELSE
      RETURN cl.openFolder;
    END;
  END OpenFolderStatus;

PROCEDURE ReallySetStatusLine(cl: MyClosure; msg: TEXT) =
    (* Places "msg" in the status line, regardless of other considerations. *)
    (* Private to the StatusLine machinery *)
    (* LL = VBT.mu *)
  BEGIN
    IF NOT Text.Equal(cl.statusText, msg) THEN
      FormsVBT.PutText(cl.fv, Status, msg);
      cl.statusText := msg;
    END;
  END ReallySetStatusLine;

PROCEDURE ClearStatusLine(cl: MyClosure) =
    (* LL = VBT.mu *)
  BEGIN
    ReallySetStatusLine(cl, OpenFolderStatus(cl));
    cl.statusLineUp := FALSE;
  END ClearStatusLine;

TYPE StatusLineForkee = Thread.Closure OBJECT
      cl: MyClosure;
    OVERRIDES
      apply := StatusLineThread
    END;

PROCEDURE StatusLineThread(self: StatusLineForkee): REFANY =
    (* Provides AutoFlush feature of SetStatusLine *)
    (* LL = 0 *)
  BEGIN
    TRY
      Thread.AlertPause(5.0d0);
      LOCK VBT.mu DO
        IF self.cl.statusLineUp THEN ClearStatusLine(self.cl) END;
      END;
    EXCEPT
      | Thread.Alerted =>
    END;
    RETURN NIL
  END StatusLineThread;

TYPE StatusLineStyle = {
    Progress,       (* msg is flushed on normal command termination *)
    Outcome,        (* msg persists after normal command termination *)
    AutoFlush       (* msg persists, but is flushed 2 minutes after posting *)
    (* But msg is flushed on command termination if error dlg is posted *)
  };

PROCEDURE SetStatusLine(cl: MyClosure; msg: TEXT; style: StatusLineStyle) =
    (* LL = actions *)
  VAR oldThread: Thread.T := NIL;
  BEGIN
    LOCK VBT.mu DO
      IF cl.statusLineThread # NIL THEN
        Thread.Alert(cl.statusLineThread);
        oldThread := cl.statusLineThread;
        cl.statusLineThread := NIL;
      END;
      CASE style OF
        | StatusLineStyle.Progress => cl.statusLineUp := FALSE;
        | StatusLineStyle.Outcome => cl.statusLineUp := TRUE;
        | StatusLineStyle.AutoFlush =>
            cl.statusLineUp := TRUE;
            cl.statusLineThread := Thread.Fork(NEW(StatusLineForkee,cl := cl));
      END;
      ReallySetStatusLine(cl, msg);
    END;
    (* clean up, so we don't have too many threads lying around *)
    IF oldThread # NIL THEN EVAL Thread.Join(oldThread) END;
  END SetStatusLine;


(* *)
(* Pop up dialog and modal actions *)
(* *)

PROCEDURE ShowZ(<*UNUSED*> cl: MyClosure; child: VBT.T) =
    (* LL = VBT.mu *)
  BEGIN
    ZSplit.Lift(child, ZSplit.Altitude.Top);
    ZSplit.Map(child);
(* We could do the following, but it makes things jump around too much ...
    IF ShowWindow(cl.window, cl.window, cl.titleText) THEN
    ELSE
      TRY
        Wr.PutText(Stdio.stderr, AppName & ": can't contact display server\n");
        Wr.Flush(Stdio.stderr);
        Process.Exit(1);
      EXCEPT
        Thread.Alerted =>
      END;
    END;
*)
  END ShowZ;

PROCEDURE HideZ(<*UNUSED*>cl: MyClosure; child: VBT.T) =
    (* LL = VBT.mu *)
  BEGIN
    IF child # NIL AND ZSplit.IsMapped(child) THEN ZSplit.Unmap(child) END;
  END HideZ;

PROCEDURE DoPopup(cl: MyClosure; name: TEXT;
                  focus: TEXT := NIL;
                  replaceMode := FALSE;
                  time: VBT.TimeStamp := 0) =
  (* LL = VBT.mu *)
  (* Make one flavor of popup visible; handles all errors. If "focus" is
     non-NIL, try to give it the keyboard focus. *)
  VAR
    index: INTEGER;
    prev := ZSplit.GetDomain(cl.popupFV);
  BEGIN
    <* ASSERT cl.popupFV # NIL *>
    cl.showingError := FALSE;
    IF Text.Equal(name, "AboutDlg") THEN index := 0;
    ELSIF Text.Equal(name, "NewFolderDlg") THEN index := 1;
    ELSIF Text.Equal(name, "FindDlg") THEN index := 2;
    ELSIF Text.Equal(name, "PrintDlg") THEN index := 3;
    ELSIF Text.Equal(name, "SaveDlg") THEN index := 4;
    ELSIF Text.Equal(name, "ErrorDlg") THEN index := 5;
    ELSIF Text.Equal(name, "ConfirmDlg") THEN index := 6;
    ELSE <*ASSERT FALSE*>
    END;
    FormsVBT.PutInteger(cl.popupFV, "DlgTSplit", index);
    WITH z = ZSplit.GetParentDomain(FormsVBT.GetVBT(cl.fv, "MainZSplit")) DO
      IF NOT Rect.IsEmpty(z) THEN
        IF cl.firstPopup THEN prev := z; cl.firstPopup := FALSE; END;
        ZSplit.Move(cl.popupFV,
                    EnsureVisible(Rect.Center(ZSplit.GetDomain(cl.popupFV),
                                              Rect.Middle(prev)),
                                  z));
      END;
    END;
    ShowZ(cl, cl.popupFV);
    IF focus # NIL THEN
      WITH tp = FormsVBT.GetVBT(cl.popupFV, focus) DO
        IF TextPort.TryFocus(tp, time) AND replaceMode THEN
          TextPort.Select(v := tp, time := time, replaceMode := TRUE);
        END;
      END;
    END;
  END DoPopup;

PROCEDURE DoPopDown(cl: MyClosure; <*UNUSED*> time: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  BEGIN
    IF cl.popupFV # NIL THEN HideZ(cl, cl.popupFV) END;
    cl.showingError := FALSE;
  END DoPopDown;

PROCEDURE DoModalLocked(cl: MyClosure; VAR time: VBT.TimeStamp): Op =
  (* Wait for a response, in a modal dialog style *)
  (* LL = VBT.mu *)
  VAR response: Op;
  BEGIN
    cl.passive(FALSE);
    WHILE cl.response = Op.Null DO Thread.Wait(VBT.mu, cl.responseCV) END;
    response := cl.response; cl.response := Op.Null;
    time := cl.responseTime;
    RETURN response
  END DoModalLocked;

PROCEDURE DoModal(cl: MyClosure; VAR time: VBT.TimeStamp): Op =
  (* LL = actions *)
  BEGIN
    LOCK VBT.mu DO RETURN cl.doModalLocked(time) END;
  END DoModal;

PROCEDURE DoModalPopup(cl: MyClosure; name: TEXT;
                       focus: TEXT := NIL;
                       replaceMode := FALSE;
                       time: VBT.TimeStamp := 0): Op =
  (* Pop-up, wait for modal response, then pop down *)
  (* LL = actions *)
  VAR
    responseTime: VBT.TimeStamp;
  BEGIN
    LOCK VBT.mu DO
      DoPopup(cl, name, focus, replaceMode, time);
      WITH op = cl.doModalLocked(responseTime) DO
        DoPopDown(cl, responseTime);
        RETURN op;
      END;
    END;
  END DoModalPopup;

PROCEDURE Confirm(cl: MyClosure; message: TEXT; time: VBT.TimeStamp): BOOLEAN =
  (* LL = actions *)
  BEGIN
    LOCK VBT.mu DO
      FormsVBT.PutText(cl.popupFV, "ConfirmText", message);
    END;
    RETURN cl.doModalPopup("ConfirmDlg", "ConfirmText", FALSE, time) = Op.Yes
  END Confirm;


(* *)
(* Error message reporting dialogs  *)
(* *)

PROCEDURE AsyncErrorHandler(cl: MyClosure; msg: TEXT) =
    (* LL < VBT.mu *)
  BEGIN
    LOCK VBT.mu DO
      FormsVBT.PutText(cl.fv, AsyncErrorText, msg);
      FormsVBT.PopUp(cl.fv, AsyncErrorText);
    END;
  END AsyncErrorHandler;

PROCEDURE Error(cl: MyClosure; msg: TEXT) =
    (* LL = VBT.mu *)
  BEGIN
    FormsVBT.PutText(cl.popupFV, "ErrorText", msg);
    DoPopup(cl, "ErrorDlg");
    cl.showingError := TRUE;
  END Error;

PROCEDURE UnlockedError(cl: MyClosure; msg: TEXT) =
    (* LL = actions *)
  BEGIN
    LOCK VBT.mu DO
      cl.error(msg);
    END;
  END UnlockedError;


(* *)
(*  Configuration Procedures  *)
(* *)

PROCEDURE AdoptConfig (cl: MyClosure; new: Config.T) =
  (* LL = actions *)
  BEGIN
    LOCK VBT.mu DO
      cl.config := new;
      TextPort.DefaultModel := cl.config.model;
      IF cl.compose # NIL THEN
        IF cl.config.composeWindow THEN
          cl.compose.SetSplitter(NIL, DoCompose, cl.window);
          cl.config.setFonts(cl.compose.ownWindow);
        ELSE
          cl.compose.SetSplitter(
            FormsVBT.GetVBT(cl.fv, Splitter), DoCompose, cl.window);
        END;
        cl.compose.SetExternal(cl.config.externalCompose);
      END;
      cl.config.setFonts(cl.fv);
      cl.config.setFonts(cl.popupFV);
      cl.config.setFonts(cl.niBrowserFV);
      IF cl.configFV # NIL THEN
        cl.config.setFonts(cl.configFV);
      END;
      IF cl.helpWindow # NIL THEN
        cl.config.setFonts(cl.helpWindow);
      END;
    END;
  END AdoptConfig;

PROCEDURE FccFolder(cl: MyClosure): TEXT =
    (* LL = actions or LL = VBT.mu *)
  BEGIN
    IF cl.config.autoFcc THEN
      RETURN cl.config.autoFccFolder
    ELSE
      RETURN ""
    END;
  END FccFolder;

PROCEDURE GetPrintFilter(cl: MyClosure): TEXT =
    (* LL < VBT.mu *)
  BEGIN
    LOCK VBT.mu DO RETURN cl.config.printFilter END;
  END GetPrintFilter;

PROCEDURE GetEditorFilter(cl: MyClosure): TEXT RAISES { BadFilter } =
    (* LL < VBT.mu *)
  VAR filter: TEXT;
  BEGIN
    LOCK VBT.mu DO filter := cl.config.editorFilter END;
    IF Text.Empty(filter) THEN
      RAISE BadFilter("Use \"Set Configuration\" to define " &
                      "your external editor command, " &
                      "e.g. \"xterm -e vi\" or \"epochclient\".");
    END;
    RETURN filter
  END GetEditorFilter;

PROCEDURE GetPSViewFilter(cl: MyClosure): TEXT =
    (* LL < VBT.mu *)
  BEGIN
    LOCK VBT.mu DO RETURN cl.config.psViewFilter END;
  END GetPSViewFilter;

PROCEDURE GetPSPrintFilter(cl: MyClosure): TEXT =
    (* LL < VBT.mu *)
  BEGIN
    LOCK VBT.mu DO RETURN cl.config.psPrintFilter END;
  END GetPSPrintFilter;

VAR
  ConfigTemplate: TEXT;

CONST
  Font6x13 = "6x13";

PROCEDURE CreateConfigWindow(cl: MyClosure) =
    (* LL < VBT.mu *)
    (* Creation of config dialog window. *)
  VAR fv: MyForm;
  BEGIN
    fv := NEW(MyForm, cl := cl,
              deletionOp := Op.No).init(ConfigTemplate, FALSE, cl.path);
    LOCK VBT.mu DO
      fv.attach("ConfigConfirm", Op.Yes);
      fv.attach("ConfigCancel", Op.No);
      fv.attach("ConfigRevert", Op.ConfigRevert);
      FormsVBT.AttachProc(fv, "FontFixed", DoFont, "Fixed");
      FormsVBT.AttachProc(fv, "FontCourier", DoFont, "Courier");
      FormsVBT.AttachProc(fv, "FontBuiltIn", DoFont, Config.BuiltInFontFamily);
      FormsVBT.AttachProc(fv, "Font6x13", DoFont, Font6x13);
      FormsVBT.AttachProc(fv, "Size9", DoFontSize, "9");
      FormsVBT.AttachProc(fv, "Size10", DoFontSize, "10");
      FormsVBT.AttachProc(fv, "Size11", DoFontSize, "11");
      FormsVBT.AttachProc(fv, "Size12", DoFontSize, "12");
      FormsVBT.AttachProc(fv, "Size14", DoFontSize, "14");
      FormsVBT.AttachProc(fv, "SizeNone", DoFontSize, Config.NoFontSize);
      cl.config.setFonts(fv);
      cl.configFV := fv;
    END;
  END CreateConfigWindow;

PROCEDURE InitConfiguration(cl: MyClosure) =
    (* LL = actions *)
  VAR tempConfig := NEW(Config.T).init();
  BEGIN
    TRY
      tempConfig.fromFile();
      tempConfig.check(cl.fv);
    EXCEPT Config.Error(errmsg) =>
      cl.unlockedError(errmsg);
    END;
    AdoptConfig(cl, tempConfig);
    TRY
      ConfigTemplate := Rsrc.Get("config.fv", cl.path);
    EXCEPT Rd.Failure, Rsrc.NotFound =>
      Wr.PutText(Stdio.stderr, "Can\'t read configuration window form\n");
      Process.Exit(2);
    END;
  END InitConfiguration;


(* *)
(* Miscellaneous methods of the MyClosure object *)
(* *)

PROCEDURE SetTextPortColors (cl: MyClosure; fv: FormsVBT.T; name: TEXT) =
  (* LL = VBT.mu *)
  VAR te: TextEditVBT.T := FormsVBT.GetVBT (fv, name);
  BEGIN
    te.tp.setColorScheme (cl.contentsColors); (* re-color text *)
    (* Since that also re-colors the scrollbar, do that explicitly: *)
    ScrollerVBTClass.Colorize (te.sb, cl.scrollColors);
  END SetTextPortColors;

PROCEDURE SetFonts(cl: MyClosure; v: VBT.T) =
  (* LL = VBT.mu *)
  BEGIN
    cl.config.setFonts(v);
  END SetFonts;

PROCEDURE InvokeNIBrowse(cl: MyClosure; time: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  BEGIN
    cl.applyOp(Op.NIBrowse, time);
  END InvokeNIBrowse;


(* *)
(* Show queue monitor *)
(* *)

(* The show queue represents the sequence of messages to be displayed in the
   "display port". The show queue never
   gets more than one deep, because additional entries overwrite the last one.
   *)

VAR
  showQueueLock := NEW(Thread.Mutex);
  showQueueIn, showQueueOut: CARDINAL;
  showQueueFolderName, showQueueMsgID: TEXT;
  showQueueThread: Thread.T; (* for debugging *)

PROCEDURE InitShowQueue() =
    (* LL = arbitrary *)
  BEGIN
    showQueueIn := 0;
    showQueueOut := 0;
    showQueueThread := NIL;
  END InitShowQueue;

PROCEDURE UpdateDisplayPort(cl: MyClosure; msgID: TEXT) =
    (* LL >= actions and < showQueueLock *)
  VAR wasEmpty: BOOLEAN;
  BEGIN
    LOCK showQueueLock DO
      wasEmpty := showQueueOut = showQueueIn;
      INC(showQueueIn);
      showQueueFolderName := cl.openFolder;
      showQueueMsgID := msgID;
      IF wasEmpty THEN
        showQueueThread := Thread.Fork(NEW(ShowQueueForkee, cl := cl));
      END;
    END; (* LOCK *)
    IF ( NOT UnixMail.Incoming(cl.openFolder))
      AND ( NOT Text.Empty(msgID)) THEN
      UnixMail.SetCurrent(cl.openFolder, msgID);
    END;
  END UpdateDisplayPort;

PROCEDURE GetLastDisplayed(): TEXT =
    (* Determine last in queue. *)
    (* LL = arbitrary *)
  BEGIN
    LOCK showQueueLock DO RETURN showQueueMsgID END; (* LOCK *)
  END GetLastDisplayed;

TYPE ShowQueueForkee = Thread.Closure OBJECT
      cl: MyClosure;
    OVERRIDES
      apply := ShowQueueThread
    END;

PROCEDURE ShowQueueThread(self: ShowQueueForkee): REFANY =
    (* Queue server. *)
    (* LL = 0 *)
  VAR cl: MyClosure; seq: CARDINAL; folder, msgID: TEXT;
  BEGIN
    cl := self.cl;
    LOOP (* until queue empties *)
      Thread.Pause(0.120d0); (* wait for screen updates to happen *)
          (* Find next entry. *)
      LOCK showQueueLock DO
        <* ASSERT(showQueueIn # showQueueOut) *>
        seq := showQueueIn;
        folder := showQueueFolderName;
        msgID := showQueueMsgID;
      END;
      (* Show the message. *)
      cl.display.Display(folder, msgID);
      (* Advance queue. *)
      LOCK showQueueLock DO
        showQueueOut := seq;
        IF showQueueOut = showQueueIn THEN
          showQueueThread := NIL;
          EXIT;
        END;
      END;
    END; (* LOOP *)
    RETURN NIL;
  END ShowQueueThread;


(* *)
(* Message Browser Access Procedures  *)
(* *)

PROCEDURE MsgHdrToId(msgHdr: TEXT): TEXT =
    (* LL = arbitrary *)
    (* Return first blank-terminated field, including any preceding blanks *)
  VAR rd: Rd.T;
  VAR wr: Wr.T;
  VAR c: CHAR;
  <*FATAL Rd.EndOfFile*>
  BEGIN
    IF Text.Empty(msgHdr) THEN RETURN "" END;
    rd := TextRd.New(msgHdr);
    wr := TextWr.New();
    LOOP
      c := Rd.GetChar(rd);
      IF c # ' ' THEN EXIT END;
      IF Rd.EOF(rd) THEN EXIT END;
      Wr.PutChar(wr, c);
    END;
    LOOP
      Wr.PutChar(wr, c);
      IF Rd.EOF(rd) THEN EXIT END;
      c := Rd.GetChar(rd);
      IF (c = ' ') OR (c = '\n') THEN EXIT END;
    END;
    RETURN TextWr.ToText(wr)
  END MsgHdrToId;

PROCEDURE GetMessageID(cl: MyClosure; index: CARDINAL): TEXT =
    (* returns message id of given message browser index *)
    (* LL = arbitrary *)
  VAR msgHdr: TEXT;
  BEGIN
    msgHdr := cl.msgBrowser.getValue(index);
    RETURN MsgHdrToId(msgHdr);
  END GetMessageID;

PROCEDURE GetFirstMsgId(msgList: TextList.T): TEXT =
    (* LL = arbitrary *)
  BEGIN
    RETURN msgList.head;
  END GetFirstMsgId;

PROCEDURE SelectMessage(cl: MyClosure; msgNum: INTEGER;
                         forceDisplay: BOOLEAN := FALSE) =
    (* Make a singleton selection.  If autoDisplay or "forceDisplay", also
       update the display port *)
    (* LL = arbitrary *)
  BEGIN
    cl.msgBrowser.selectOnly(msgNum);
    cl.msgBrowser.scrollToShow(msgNum);
    IF cl.config.autoDisplayMessages OR forceDisplay THEN
      UpdateDisplayPort(cl, GetMessageID(cl, msgNum));
    END;
  END SelectMessage;

PROCEDURE SelectedMessage(cl: MyClosure): BOOLEAN =
    (* Returns whether there is a selected message *)
    (* LL = arbitrary *)
  VAR this: ListVBT.Cell;
  BEGIN
    RETURN cl.msgBrowser.getFirstSelected(this)
  END SelectedMessage;

PROCEDURE GetSelectedMessages(cl: MyClosure): TextList.T =
    (* returns message id's of selected messages *)
    (* LL = arbitrary *)
  VAR list: TextList.T := NIL; selections: REF ARRAY OF ListVBT.Cell;
  BEGIN
    selections := cl.msgBrowser.getAllSelected();
    FOR i := NUMBER(selections^)-1 TO 0 BY -1 DO
      list := TextList.Cons(
          MsgHdrToId(cl.msgBrowser.getValue(selections^[i])),
          list);
    END;
    RETURN list
  END GetSelectedMessages;

PROCEDURE RequireSelectedMessages(cl: MyClosure): TextList.T =
    (* LL = actions *)
  VAR msgList: TextList.T;
  BEGIN
    msgList := GetSelectedMessages(cl);
    IF msgList = NIL THEN
      cl.unlockedError("No message is selected.");
    END;
    RETURN msgList
  END RequireSelectedMessages;

PROCEDURE GetLastSelectedMessage(cl: MyClosure): INTEGER =
    (* returns browser index for last selected, except returns -1 if:
            a) there is no selection (also posts error)
            b) last selected wasn't displayed (displays it)
       This is specialized for use by DoUpArrow/DoDownArrow *)
    (* LL = actions *)
  VAR
    selections: REF ARRAY OF ListVBT.Cell;
    lastSelIndex: ListVBT.Cell;
    msgID: TEXT;
  BEGIN
    selections := cl.msgBrowser.getAllSelected();
    IF NUMBER(selections^) = 0 THEN
      cl.unlockedError("No message is selected.");
      RETURN -1
    ELSE
      lastSelIndex := selections^[NUMBER(selections^)-1];
        (* When auto-display is off and the selected message is not the one
           being displayed, we just display the selected message. *)
      IF NOT cl.config.autoDisplayMessages THEN
        msgID := GetMessageID(cl, lastSelIndex);
        IF NOT Text.Equal(msgID, GetLastDisplayed()) THEN
          UpdateDisplayPort(cl, msgID);
          RETURN  -1
        END;
      END;
      RETURN lastSelIndex
    END;
  END GetLastSelectedMessage;

PROCEDURE InsertSelectedMessages(cl: MyClosure; tp: TextPort.T)
        RAISES { CantInsert } =
  (* For composition windows: insert selected message into a TextPort *)
  (* LL = 0 *)
  VAR
    msgs: TextList.T;
    body: TEXT;
  BEGIN
    LOCK actions DO
      WITH folder = cl.openFolder,
           prefix = cl.config.includeReplyString DO
        msgs := GetSelectedMessages(cl);
        IF msgs = NIL THEN
          RAISE CantInsert("No messages selected")
        END;
        WHILE msgs # NIL DO
          TRY
            body := UnixMail.IncludeMsg(folder, msgs.head, prefix);
          EXCEPT UnixMail.Error(errmsg) => RAISE Closure.CantInsert(errmsg);
          END;
          LOCK VBT.mu DO TextPort.Insert(tp, body) END;
          msgs := msgs.tail;
        END
      END
    END;
  END InsertSelectedMessages;

PROCEDURE ResetMsgBrowser(cl: MyClosure;
                          selectFirstNewMsg := FALSE;
                          forceDisplay := FALSE;
                          forceScan := FALSE;
                          withInc := FALSE;
                          niRd: Rd.T := NIL;
                          niCount := 0)
  RAISES {UnixMail.Error} =
  (* Reload the message browser with the currently open folder.  If
     "selectFirstNewMsg" is TRUE, then set the highlighted message to be
     the first newly incorporated one.  This is usually done for "inbox"
     and bulletin board folders only.  If the message browser ends up with
     a selected message, and autoDisplay or "forceDisplay", then update the
     display port (done inside "SelectMessage"). *)
  (* LL = actions *)
  VAR
    nMsgs: INTEGER;             (* number of messages in the browser *)
    rd   : Rd.T;                (* source of browser material *)
    newValues, tempValues: REF ARRAY OF TEXT; (* accumulating browser
                                                 values *)
    curMsgId : TEXT;            (* contents of "cur" file *)
    curMsgRd : Rd.T;            (* reader on curMsgID *)
    curMsgLen: INTEGER;         (* length of curMsgID *)
    curIndex: INTEGER;          (* browser index for message indicated by
                                   "cur" file *)
    subBuf  : ARRAY [0 .. 80] OF CHAR; (* current line *)
    subCount: INTEGER;                 (* length of current line *)
    subPos  : INTEGER;                 (* for scanning curent line *)
  PROCEDURE GetLine (rd: Rd.T) =
    (* Get however much of the current line will fit in subBuf, discarding
       the remainder.  Stops at '\n' or EOF. *)
    BEGIN
      subCount := Rd.GetSubLine (rd, subBuf);
      IF subCount = 0 THEN
        (* unexpected EOF; recover by using null line *)
        subBuf [0] := '\n';
        subCount := 1;
      ELSE
        IF subBuf [subCount - 1] # '\n' THEN
          TRY
            WHILE Rd.GetChar (rd) # '\n' DO END;
          EXCEPT
            Rd.EndOfFile =>
          END;
        END;
      END;
    END GetLine;
  BEGIN
    IF Text.Equal(cl.openFolder, UnixMail.NIFolderName) THEN
      nMsgs := niCount;
      newValues := NEW(REF ARRAY OF TEXT, nMsgs);
      FOR i := 0 TO nMsgs - 1 DO
        GetLine(niRd);
        newValues^[i] := Text.FromChars(SUBARRAY (subBuf, 0, subCount - 1));
      END;
      LOCK VBT.mu (* to prevent repaints *) DO
        (* cl.msgBrowser.removeCells(0, cl.msgBrowser.count()); *)
        VAR xx := cl.msgBrowser; yy := xx.count ();
        BEGIN
          xx.removeCells (0, yy);
          xx.insertCells (0, nMsgs);
          FOR i := 0 TO nMsgs - 1 DO xx.setValue (i, newValues^ [i]); END;
        END;
        IF NUMBER(newValues^) > 0 THEN SelectMessage(cl, 0, forceDisplay) END;
      END;
      SetStatusLine(cl, "Loaded " & CountAsText (nMsgs, "message"),
                    StatusLineStyle.AutoFlush);
    ELSE
      SetStatusLine (cl, "Loading \"" & cl.openFolder & "\" ...",
                     StatusLineStyle.Progress);
      Thread.Pause (0.120d0);      (* wait for screen updates *)
      curIndex := -1;
      rd := UnixMail.GetMsgList(cl.openFolder, forceScan, withInc, curMsgId);
      TRY
        TRY
          newValues := NEW (REF ARRAY OF TEXT, 100);
          curMsgRd := TextRd.New (curMsgId);
          curMsgLen := Text.Length (curMsgId);
          nMsgs := 0;
          WHILE NOT Rd.EOF (rd) DO
            IF MiscUtils.RdFindChar (rd, ':') < 0 THEN
              RAISE UnixMail.Error ("malformed .inodecache file")
            END;
            GetLine (rd);
            (* Check for msgID = curMsgId *)
            IF curMsgLen > 0 THEN
              subPos := 0;
              Rd.Seek (curMsgRd, 0);
              LOOP
                IF subPos >= subCount THEN
                  EXIT
                END;            (* must have n+1 chars *)
                IF subPos = curMsgLen THEN (* curMsgID is prefix of
                                              subBuf *)
                  IF (subBuf [subPos] # ' ') AND (subBuf [subPos] # '\n') THEN
                    EXIT
                  END;
                  (* curMsgID is a whitespace-terminated prefix of
                     subBuf; *)
                  (* curMsgID came from GetMsgID originally; *)
                  (* => GetMsgID would deliver a text equal to curMsgID *)
                  curIndex := nMsgs;
                  EXIT
                END;
                <*FATAL Rd.EndOfFile*>
                BEGIN
                  IF subBuf [subPos] # Rd.GetChar (curMsgRd) THEN EXIT END;
                END;
                INC (subPos);
              END;
            END;
            (* copy this buffer and rest of line to newValues *)
            IF nMsgs >= NUMBER (newValues^) THEN
              tempValues := newValues;
              newValues :=
                NEW (REF ARRAY OF TEXT, NUMBER (newValues^) * 2);
              SUBARRAY (newValues^, 0, NUMBER (tempValues^)) :=
                tempValues^;
            END;
            newValues^ [nMsgs] :=
              Text.FromChars (SUBARRAY (subBuf, 0, subCount - 1));
            INC (nMsgs);
          END;
        FINALLY
          Rd.Close (rd);
        END;
      EXCEPT
        Rd.Failure =>
          RAISE UnixMail.Error ("Error while reading message headers");
      END;
      IF selectFirstNewMsg THEN
        IF (curIndex >= 0) AND (curIndex + 1 < nMsgs) THEN
          INC (curIndex)
        END;
      END;
      IF curIndex < 0 THEN (* bad "cur" file *) curIndex := nMsgs - 1 END;
      LOCK VBT.mu (* to prevent repaints *) DO
        cl.msgBrowser.removeCells (0, cl.msgBrowser.count ());
        cl.msgBrowser.insertCells (0, nMsgs);
        (* scroll early to avoid unnecessary repaints *)
        IF curIndex >= 0 THEN cl.msgBrowser.scrollToShow (curIndex) END;
        FOR i := 0 TO nMsgs - 1 DO
          cl.msgBrowser.setValue (i, newValues^ [i]);
        END;
        IF curIndex >= 0 THEN
          SelectMessage (cl, curIndex, forceDisplay)
        END;
      END;
      IF UnixMail.Incoming (cl.openFolder) AND (nMsgs > 0) THEN
        UnixMail.SetCurrent (cl.openFolder, GetMessageID (cl, nMsgs - 1));
      END;
    END;
  END ResetMsgBrowser;

PROCEDURE DeleteSelected(cl: MyClosure) =
    (* Delete selected items from the msgBrowser *)
    (* LL = actions *)
  VAR
    msgCount: INTEGER;
    newSel: ListVBT.Cell;
    selections: REF ARRAY OF ListVBT.Cell;
  BEGIN
    selections := cl.msgBrowser.getAllSelected();
    <*ASSERT (NUMBER(selections^) > 0) *>
    FOR j := 0 TO LAST(selections^) DO
      cl.msgBrowser.removeCells(selections^[j] - j, 1);
    END;
    msgCount := cl.msgBrowser.count();
    newSel := selections^[0];
    IF (NOT cl.downward) AND (newSel > 0) THEN DEC(newSel) END;
    IF newSel >= msgCount THEN DEC(newSel) END;
    IF newSel >= 0 THEN
      SelectMessage(cl, newSel);
    ELSE
      (* no selection, but there was one before *)
      UpdateButtons(cl);
    END;
      (* update "cur" file *)
    IF UnixMail.Incoming(cl.openFolder) AND (msgCount > 0) THEN
      UnixMail.SetCurrent(cl.openFolder, GetMessageID(cl, msgCount - 1));
    END;
  END DeleteSelected;


(**)
(* Folder operations *)
(**)

PROCEDURE InitFolderBrowser(cl: MyClosure) =
    (* LL = actions *)
  VAR l: TextList.T;
  BEGIN
    l := UnixMail.FolderList();
    cl.folderBrowser.removeCells(0, LAST(CARDINAL));
    cl.folderBrowser.insertCells(0, TextList.Length(l));
    FOR i := 0 TO cl.folderBrowser.count()-1 DO
      <*ASSERT(l#NIL)*>
      cl.folderBrowser.setValue(i, l.head);
      l := l.tail;
    END;
  END InitFolderBrowser;

PROCEDURE DoOpenFolder(cl: MyClosure; folder: TEXT;
                       selectFirstNewMsg, withInc: BOOLEAN;
                       niRd: Rd.T := NIL;
                       niCount := 0) =
    (* LL = actions *)
  VAR
    oldFolder: TEXT;
    oldContents: REF ARRAY OF TEXT;
    oldSel: REF ARRAY OF ListVBT.Cell;
  BEGIN
    oldFolder := cl.openFolder;
    oldContents := NEW(REF ARRAY OF TEXT, cl.msgBrowser.count());
    FOR i := 0 TO LAST(oldContents^) DO
      oldContents^[i] := cl.msgBrowser.getValue(i);
    END;
    oldSel := cl.msgBrowser.getAllSelected();
    cl.openFolder := folder;
    (* cl.openFolder is used by UpdateDisplayPort inside ResetMsgBrowser *)
    TRY
      ResetMsgBrowser(cl, selectFirstNewMsg,
                      (*forceDisplay:*) Text.Empty(oldFolder),
                      (*forceScan:*)    FALSE,
                      (*withInc:*)      withInc,
                      (*niRd:*)         niRd,
                      (*niCount:*)      niCount
                      );
    EXCEPT UnixMail.Error(errmsg) =>
      cl.openFolder := oldFolder; (* back out *)
      UpdateTitle(cl);
      UpdateButtons(cl);
      cl.unlockedError(errmsg);
      RETURN
    END;
      (* Succeeded - fix up the display, etc. *)
    cl.downward := TRUE;
    IF ((NOT Text.Empty(oldFolder)) AND  NOT Text.Equal(folder, oldFolder))
      OR Text.Equal(oldFolder, UnixMail.NIFolderName) THEN
      cl.prevOpenFolder := oldFolder;
      cl.prevOpenContents := oldContents;
      cl.prevSel := oldSel;
    ELSE
      oldContents := NIL; (* encourage the garbage collector *)
    END;
    IF UnixMail.Incoming(folder) THEN
          (* ... if asterisk prefixes folder browser item, remove it *)
      cl.folderBrowser.setValue(FindValue(cl.folderBrowser, folder), folder);
    END;
    UpdateTitle(cl);
    UpdateButtons(cl);
  END DoOpenFolder;

PROCEDURE LoadFromNI(cl: MyClosure; rd: Rd.T; count: CARDINAL) =
  BEGIN
    SetStatusLine(cl, "Loading " & CountAsText(count, "message") & " ...",
                  StatusLineStyle.Progress);
    DoOpenFolder(cl, UnixMail.NIFolderName, FALSE, FALSE, rd, count);
  END LoadFromNI;

PROCEDURE InitOpenFolder(cl: MyClosure) =
    (* LL = actions *)
  BEGIN
    cl.openFolder := "";
    DoOpenFolder(cl, UnixMail.InboxName, TRUE, FALSE);
  END InitOpenFolder;

PROCEDURE LoadPrevFolder(cl: MyClosure) =
    (* LL = actions *)
  VAR
    oldFolder: TEXT;
    oldContents: REF ARRAY OF TEXT;
    oldSel: REF ARRAY OF ListVBT.Cell;
  BEGIN
    IF cl.prevOpenFolder # NIL THEN
      oldFolder := cl.openFolder;
      oldContents := NEW(REF ARRAY OF TEXT, cl.msgBrowser.count());
      FOR i := 0 TO LAST(oldContents^) DO
        oldContents^[i] := cl.msgBrowser.getValue(i);
      END;
      oldSel := cl.msgBrowser.getAllSelected();
      cl.openFolder := cl.prevOpenFolder;
      LOCK VBT.mu(* to avoid repaints *)  DO
        cl.msgBrowser.removeCells(0, LAST(CARDINAL));
        IF cl.prevOpenContents # NIL THEN
          cl.msgBrowser.insertCells(0, NUMBER(cl.prevOpenContents^));
          IF NUMBER(cl.prevSel^) > 0 THEN
            cl.msgBrowser.scrollToShow(cl.prevSel[LAST(cl.prevSel^)]);
          END;
          FOR i := 0 TO LAST(cl.prevOpenContents^) DO
            cl.msgBrowser.setValue(i, cl.prevOpenContents^[i]);
          END;
          FOR i := 0 TO LAST(cl.prevSel^) DO
            cl.msgBrowser.select(cl.prevSel^[i], TRUE);
          END;
          IF NUMBER(cl.prevSel^) > 0 THEN
            IF cl.config.autoDisplayMessages THEN
              UpdateDisplayPort(cl,
                              GetMessageID(cl, cl.prevSel[LAST(cl.prevSel^)]));
            END;
          END;
        END;
      END;
      cl.prevOpenFolder := oldFolder;
      cl.prevOpenContents := oldContents;
      cl.prevSel := oldSel;
      UpdateTitle(cl);
      UpdateButtons(cl);
    END;
  END LoadPrevFolder;

PROCEDURE GetSelectedFolder(cl: MyClosure; show: BOOLEAN): TEXT =
    (* Return name of selected folder, omitting marker; optionally
       ScrollToShow *)
    (* LL = arbitrary *)
  VAR folder: TEXT;
  VAR this: ListVBT.Cell;
  BEGIN
    IF NOT cl.folderBrowser.getFirstSelected(this) THEN
      RETURN ""
    ELSE
      folder := cl.folderBrowser.getValue(this);
      IF (folder # NIL) AND (NOT Text.Empty(folder)) AND
         (Text.GetChar(folder, 0) = FolderModifiedChar) THEN
          folder := Text.Sub(folder, 1, LAST(CARDINAL));
      END;
      IF show THEN cl.folderBrowser.scrollToShow(this) END;
      RETURN folder
    END;
  END GetSelectedFolder;


(* *)
(*  Background threads  *)
(* *)

TYPE MailCheckerForkee = Thread.Closure OBJECT
      cl: MyClosure;
    OVERRIDES
      apply := MailCheckerThread
    END;

PROCEDURE MailCheckerThread(self: MailCheckerForkee): REFANY =
    (* LL = 0 *)
  VAR
    cl: MyClosure;
    newMsgs: INTEGER;
    waitTime: LONGREAL;
  BEGIN
    cl := self.cl;
(* TEMP (grump)
    IF ThreadFriends.SetPriority(ThreadFriends.BackgroundPriority) = 0 THEN
    END;
*)
    LOOP
      TRY
        LOCK mailCheckMutex DO
          newMsgs := UnixMail.NewMailPoll();
          LOCK VBT.mu DO
            cl.mailCount := newMsgs;
            LockedUpdateTitle(cl);
          END;
        END;
      EXCEPT UnixMail.Error => (* ignore *)
      END;
      LOCK VBT.mu DO
        waitTime := FLOAT(cl.config.mailCheckInterval * 60, LONGREAL)
      END;
      Thread.Pause(waitTime);
    END; (* LOOP *)
  END MailCheckerThread;

TYPE NewsCheckerForkee = Thread.Closure OBJECT
      cl: MyClosure;
    OVERRIDES
      apply := NewsCheckerThread
    END;

PROCEDURE NewsCheckerThread(self: NewsCheckerForkee): REFANY =
    (* LL = 0 *)
  VAR
    cl: MyClosure;
    this: UnixMail.FolderPtr;
    folder: TEXT;
    hasNews: BOOLEAN;
    waitTime: LONGREAL;
  BEGIN
    cl := self.cl;
    LOOP
      this := NIL;
      LOOP
        this := UnixMail.GetNextBBoard(b := this,
                                       checkNewsDir := TRUE,
                                       folder := folder,
                                       hasNews := hasNews);
        IF this = NIL THEN EXIT END;
        LOCK actions DO
          IF hasNews THEN
            cl.folderBrowser.setValue(FindValue(cl.folderBrowser, folder),
                                   Text.FromChar(FolderModifiedChar) & folder);
          ELSE
            cl.folderBrowser.setValue(FindValue(cl.folderBrowser, folder),
                                      folder);
          END;
          UpdateTitle(cl);
        END;
      END;
      LOCK VBT.mu DO
        waitTime := FLOAT(MAX(cl.config.newsCheckInterval * 60, 60), LONGREAL);
      END;
      Thread.Pause(waitTime);
    END; (*LOOP*)
  END NewsCheckerThread;

TYPE ComposeForkee = Thread.Closure OBJECT
      fv: FormsVBT.T;
      eventName: TEXT;
      cl: MyClosure;
    OVERRIDES
      apply := ComposeThread
    END;

PROCEDURE ComposeThread(self: ComposeForkee): REFANY =
    (* LL = 0 *)
    (* Don't acquire "actions" in here: allow concurrent mail composition *)
  VAR cl: MyClosure; fcc, cc: TEXT;
  BEGIN
    cl := self.cl;
    LOCK VBT.mu DO
      fcc := FccFolder(cl);
      IF cl.config.autoCcToYourself THEN cc := user;  ELSE cc := "";  END;
    END;
    cl.compose.Compose("", cc, fcc, "", "", "");
    LOCK VBT.mu DO
      MakeActive(cl, self.fv, self.eventName);
    END;
    RETURN NIL
  END ComposeThread;


(* *)
(* Common sub-routines of the command execution call-backs *)
(* *)

PROCEDURE OpenFolderSearch(cl: MyClosure; searchFromEnd: BOOLEAN;
                            forward: BOOLEAN;
                            pat: TEXT): BOOLEAN =
    (* LL = actions *)
  VAR
    current: INTEGER;
    items: INTEGER;
    target: TEXT;
  BEGIN
    SetStatusLine(cl, "Searching open folder ...", StatusLineStyle.Progress);
    items := cl.msgBrowser.count();
    IF items = 0 THEN RETURN FALSE END;
    IF searchFromEnd OR NOT cl.msgBrowser.getFirstSelected(current) THEN
      IF forward THEN current := -1 ELSE current := items END;
    END;
    LOOP (* "current" is previous candidate cell *)
      IF forward THEN
        INC(current);
        IF current = items THEN RETURN FALSE END;
      ELSE
        IF current = 0 THEN RETURN FALSE END;
        DEC(current);
      END;
      target := cl.msgBrowser.getValue(current);
      IF MiscUtils.Find(target, 0, pat, TRUE) >= 0 THEN EXIT END;
    END;
    SetStatusLine(cl, "Found in \"" & cl.openFolder & "\"",
                  StatusLineStyle.AutoFlush);
    SelectMessage(cl, current);
    RETURN TRUE
  END OpenFolderSearch;


(* *)
(* Worker procs: called from ForkedOp *)
(* *)

(* Layed out in screen order, approximately *)

PROCEDURE DoConfigWindow(cl: MyClosure) =
    (* LL = actions *)
  VAR
    responseTime: VBT.TimeStamp;
    tempConfig := NEW(Config.T).init();
  BEGIN
    IF cl.configFV = NIL THEN
      SetStatusLine(cl, "Creating dialog ...", StatusLineStyle.Progress);
      CreateConfigWindow(cl);
      cl.config.toDlg(cl.configFV);
    END;
    LOCK VBT.mu DO
      IF NOT ShowWindow(cl.configFV, cl.window,
                        AppName & ": configuration") THEN
        cl.error("Failed to install configuration dialog window");
        RETURN;
      END;
    END;
    SetStatusLine(cl, "Set Configuration ...", StatusLineStyle.Progress);
    WHILE TRUE DO
      WITH op = cl.doModal(responseTime) DO
        CASE op OF
        | Op.Yes =>
            SetStatusLine(cl, "Checking ...", StatusLineStyle.Progress);
            tempConfig.fromDlg(cl.configFV);
            TRY
              tempConfig.check(cl.configFV);
              SetStatusLine(cl, "Saving ...", StatusLineStyle.Progress);
              tempConfig.toFile(Version);
              AdoptConfig(cl, tempConfig);
            EXCEPT Config.Error(errmsg) =>
              cl.unlockedError(errmsg);
            END;
            EXIT;
        | Op.No =>
            cl.config.toDlg(cl.configFV);
            EXIT;
        | Op.ConfigRevert =>
            cl.config.toDlg(cl.configFV);
        ELSE
            <* ASSERT FALSE *>
        END;
      END;
    END;
    LOCK VBT.mu DO Trestle.Delete(cl.configFV) END;
  END DoConfigWindow;

PROCEDURE DoHelpWindow(cl: MyClosure) =
    (* opening the help window *)
    (* LL = actions *)
  <*FATAL Rsrc.NotFound*>
  BEGIN
    IF cl.helpWindow = NIL THEN
      SetStatusLine(cl, "Creating dialog ...", StatusLineStyle.Progress);
      WITH myWindow = StableVBT.New(
                     NEW(MyForm, cl := cl).initFromRsrc("help.fv", cl.path)) DO
        LOCK VBT.mu DO
          WITH sh = VBTClass.GetShapes(cl.window) DO
            StableVBT.SetShape(myWindow, sh[Axis.T.Hor].pref, 0);
          END;
          cl.helpWindow := myWindow;
          cl.config.setFonts(cl.helpWindow);
        END;
      END;
    END;
    LOCK VBT.mu DO
      IF NOT ShowWindow(cl.helpWindow, cl.window, AppName & ": help") THEN
        cl.error("Failed to install help window.");
      END;
    END;
  END DoHelpWindow;

PROCEDURE DoNewFolder(cl: MyClosure; time: VBT.TimeStamp) =
    (* LL = actions *)
  VAR folder, comp: TEXT;
  VAR folderNum, totalFolders: INTEGER;
  BEGIN
    IF cl.doModalPopup("NewFolderDlg", NewFolderText, TRUE, time) # Op.Yes THEN
      RETURN;
    END;
    SetStatusLine(cl, "Please wait ...", StatusLineStyle.Progress);
    UnixMail.ChangeFolders();
    folder := FormsVBT.GetText(cl.popupFV, NewFolderText);
    IF Text.Empty(folder) THEN
      cl.unlockedError("No folder name specified.");
    ELSIF Text.FindChar(folder, ' ', 0) >= 0 THEN
      cl.unlockedError("A folder name must not a contain space.");
    ELSIF UnixMail.FolderExists(folder) THEN
      cl.unlockedError("A folder named \"" & folder &"\" already exists.");
    ELSE
      SetStatusLine(cl, "Creating ...", StatusLineStyle.Progress);
      TRY
        UnixMail.CreateFolder(folder);
      EXCEPT
        | UnixMail.Error(errmsg) =>
            cl.unlockedError(errmsg);
            RETURN
      END;
      folderNum := 0;
      totalFolders := cl.folderBrowser.count();
      LOOP
        IF folderNum >= totalFolders THEN EXIT END;
        comp := cl.folderBrowser.getValue(folderNum);
        IF  NOT Text.Empty(comp)
          AND (Text.GetChar(comp, 0) = FolderModifiedChar) THEN
          comp := Text.Sub(comp, 1, LAST(CARDINAL));
        END;
        IF Text.Compare(folder, comp) <= 0 THEN EXIT END;
        INC(folderNum)
      END;
      cl.folderBrowser.insertCells(folderNum, 1);
      cl.folderBrowser.setValue(folderNum, folder);
      cl.folderBrowser.scrollToShow(folderNum);
    END;
  END DoNewFolder;

PROCEDURE DoRescan(cl: MyClosure; time: VBT.TimeStamp) =
    (* LL = actions *)
  BEGIN
    IF Text.Equal(cl.openFolder, UnixMail.NIFolderName) THEN
      cl.unlockedError("You can\'t rescan this browser - it\'s not a folder.");
      RETURN;
    END;
    IF NOT cl.confirm("Rescan the folder \"" & cl.openFolder & "\"?",
                      time) THEN
      RETURN
    END;
    SetStatusLine(cl, "Rescanning ...", StatusLineStyle.Progress);
    TRY
      UnixMail.RescanFolder(cl.openFolder);
      ResetMsgBrowser(cl, (*selectFirstNewMsg:*) FALSE,
                      (*forceDisplay:*) FALSE,
                      (*forceScan:*) TRUE,
                      (*withInc:*) FALSE);
    EXCEPT
    | UnixMail.Error(errmsg) => cl.unlockedError(errmsg);
    END;
  END DoRescan;

PROCEDURE DoSortPack(cl: MyClosure; time: VBT.TimeStamp) =
    (* LL = actions *)
  BEGIN
    IF Text.Equal(cl.openFolder, UnixMail.NIFolderName) THEN
      cl.unlockedError("You can\'t sort and pack this browser - " &
                       "it\'s not a folder.");
      RETURN;
    END;
    IF UnixMail.IsBBoard(cl.openFolder) THEN
      cl.unlockedError("You can\'t sort and pack a bulletin board.");
      RETURN;
    END;
    IF cl.msgBrowser.count() > 500 AND
       NOT cl.confirm("Sort and pack the folder \"" &
                      cl.openFolder & "\"?",
                      time) THEN
      RETURN
    END;
    SetStatusLine(cl, "Sorting and packing ...", StatusLineStyle.Progress);
    TRY
      UnixMail.SortFolder(cl.openFolder);
      ResetMsgBrowser(cl);
    EXCEPT
    | UnixMail.Error(errmsg) => cl.unlockedError(errmsg);
    END;
  END DoSortPack;

PROCEDURE DoPurge(cl: MyClosure; time: VBT.TimeStamp) =
    (* LL = actions *)
  VAR total, toSave: INTEGER;
  BEGIN
    IF Text.Equal(cl.openFolder, UnixMail.NIFolderName) THEN
      cl.unlockedError("You can\'t purge this browser - it\'s not a folder.");
      RETURN;
    END;
    IF NOT cl.confirm("Purge the folder \"" &
                      cl.openFolder &
                      "\", retaining only " &
                      Fmt.Int(cl.config.purgeSaveMessages) &
                      " messages?",
                      time) THEN
      RETURN
    END;
    total := cl.msgBrowser.count();
    toSave := cl.config.purgeSaveMessages;
    IF total > toSave THEN
      SetStatusLine(cl, "Purging ...", StatusLineStyle.Progress);
      TRY
        UnixMail.PurgeFolder(cl.openFolder, GetMessageID(cl, total-toSave-1));
        ResetMsgBrowser(cl);
      EXCEPT UnixMail.Error(errmsg) => cl.unlockedError(errmsg);
      END;
    END;
  END DoPurge;

PROCEDURE DoRemove(cl: MyClosure; time: VBT.TimeStamp) =
    (* LL = actions *)
  VAR folderNum: INTEGER;
  VAR confirmText: TEXT;
  BEGIN
    IF Text.Equal(cl.openFolder, UnixMail.NIFolderName) THEN
      cl.unlockedError("You can\'t remove this browser - it\'s not a folder.");
      RETURN;
    END;
    IF Text.Equal(cl.openFolder, UnixMail.InboxName) THEN
      cl.unlockedError("You can\'t remove your inbox.");
      RETURN;
    END;
    IF Text.Equal(cl.openFolder, cl.config.deleteMessagesToFolder) THEN
      cl.unlockedError("You can\'t remove your \"deleted messages\" folder.");
      RETURN;
    END;
    IF UnixMail.IsBBoard(cl.openFolder) THEN
      confirmText := "Remove the bulletin board folder \"" &
                     cl.openFolder & "\"?";
    ELSE
      confirmText := "Remove and destroy the message folder \"" &
                     cl.openFolder & "\"?";
    END;
    IF NOT cl.confirm(confirmText, time) THEN RETURN END;
    SetStatusLine(cl, "Removing ...", StatusLineStyle.Progress);
    TRY
      IF cl.bbPtr = UnixMail.RemoveFolder(cl.openFolder) THEN
        cl.bbPtr := NIL;
      END;
    EXCEPT
    | UnixMail.Error(errmsg) =>
        cl.unlockedError(errmsg);
        RETURN
      END;
    folderNum := FindValue(cl.folderBrowser, cl.openFolder);
    cl.folderBrowser.removeCells(folderNum, 1);
    InitOpenFolder(cl);
  END DoRemove;

PROCEDURE DoFind(cl: MyClosure; op: Op) =
    (* LL = actions *)
  VAR
    findText: TEXT;
    findInOpen, findInPrivate: BOOLEAN;
    forward: BOOLEAN;
    candidateFolder: TEXT;
    startPtr, candidatePtr: UnixMail.FolderPtr;
    candidateBBoard: BOOLEAN;
    rd: Rd.T;
    found: INTEGER;
    subBuf: ARRAY [0..100] OF CHAR;
    subCount: INTEGER;
  BEGIN
    LOCK VBT.mu DO
      findInPrivate := FormsVBT.GetBoolean(cl.popupFV, "FindInPrivate");
      IF findInPrivate THEN
        FormsVBT.PutBoolean(cl.popupFV, "FindInOpen", TRUE);
      END;
      findInOpen := FormsVBT.GetBoolean(cl.popupFV, "FindInOpen");
      findText := FormsVBT.GetText(cl.popupFV, FindText);
      cl.stopFindRequest := FALSE;
      MakeActive(cl, cl.popupFV, "FindCancel");
    END;
    IF NOT Text.Equal(findText, cl.oldFindText) THEN
      cl.oldFindText := findText;
      cl.findPtr := NIL;
    END;
    IF Text.Empty(findText) THEN
      cl.unlockedError("Empty search string.");
      RETURN;
    END;
    forward := op # Op.FindPrevious;
    IF Text.Equal(cl.openFolder, UnixMail.NIFolderName) THEN
      startPtr := NIL
    ELSE
      TRY
          startPtr := UnixMail.FindFolder(cl.openFolder);
      EXCEPT UnixMail.Error =>
        startPtr := NIL
    END
    END;
    (* If previous search was interrupted, and search string is unchanged,
       last searched folder is cl^.findPtr; otherwise cl^.findPtr=NIL *)
    IF op = Op.FindFirst THEN
      cl.findPtr := NIL;
      IF  NOT findInOpen THEN startPtr := NIL END;
    END;
    IF cl.findPtr = NIL THEN
      IF findInOpen THEN
        IF OpenFolderSearch(cl, op = Op.FindFirst, forward, findText) THEN
          RETURN
          END;
      END;
      candidatePtr := startPtr;
    ELSE (* resume search *)
      candidatePtr := cl.findPtr;
      cl.findPtr := NIL
    END;
    IF  NOT findInPrivate THEN
      SetStatusLine(cl, "Not found", StatusLineStyle.AutoFlush);
      RETURN
      END;
    (* Now search folders after candidatePtr *)
    LOOP
      candidatePtr := UnixMail.GetNextFolder(candidatePtr, forward,
                                             candidateFolder,
                                             candidateBBoard);
      IF candidatePtr = startPtr THEN
        SetStatusLine(cl, "Not found", StatusLineStyle.AutoFlush);
        RETURN
        END;
      IF (candidatePtr # NIL) AND  NOT candidateBBoard THEN
        SetStatusLine(cl, "Searching \"" & candidateFolder & "\" ...",
                      StatusLineStyle.Progress);
        TRY
          rd := UnixMail.GetInodecache(candidateFolder);
        EXCEPT UnixMail.Error(errmsg) =>
          cl.unlockedError(errmsg);
          RETURN
        END;
        TRY
          found := -1;
          LOOP
            subCount := Rd.GetSubLine(rd, subBuf);
            IF subCount = 0 THEN EXIT END;
            found := MiscUtils.FindInSub(
                         SUBARRAY(subBuf, 0, subCount), findText, TRUE);
            IF found >= 0 THEN EXIT END;
          END;
        EXCEPT Rd.Failure =>
          cl.unlockedError("Error while reading folder");
          RETURN;
        END;
        TRY Rd.Close(rd) EXCEPT Rd.Failure => END;
        IF found >= 0 THEN
          UnixMail.ChangeFolders();
          DoOpenFolder(cl, candidateFolder, FALSE, FALSE);
          IF OpenFolderSearch(cl, TRUE, forward, findText) THEN
            RETURN;
          END;
        END;
        IF cl.stopFindRequest THEN
          SetStatusLine(cl, "Stopped after \"" & candidateFolder & "\"",
                        StatusLineStyle.AutoFlush);
          cl.findPtr := candidatePtr;
          RETURN;
        END;
      END;
    END;
  END DoFind;

PROCEDURE DoNI(cl: MyClosure; op: Op; time: VBT.TimeStamp) =
  BEGIN
    TRY
      IF op = Op.NIFlip THEN
        cl.ni.flip(time)
      ELSE
        SetStatusLine(cl, "Searching ...", StatusLineStyle.Progress);
        CASE op OF
        | Op.NICount =>
            WITH count = cl.ni.count() DO
              SetStatusLine(cl, "Matched " & CountAsText(count, "message"),
                            StatusLineStyle.AutoFlush);
            END;
        | Op.NIBrowse =>
            cl.ni.browse();
        | Op.ShowConversation =>
            WITH msgList = RequireSelectedMessages(cl) DO
              IF msgList # NIL THEN
                cl.ni.showConversation(msgList, cl.openFolder);
              END;
            END;
        ELSE
            <* ASSERT FALSE *>
        END;
      END;
    EXCEPT NI.Error(errmsg) => cl.unlockedError(errmsg)
    END;
  END DoNI;

PROCEDURE DoPrint(cl: MyClosure; time: VBT.TimeStamp) =
    (* LL = actions *)
  VAR msgList: TextList.T; printFilter: TEXT;
  BEGIN
    LOCK VBT.mu DO
      FormsVBT.PutText(cl.popupFV, PrintFilterText, cl.config.printFilter);
    END;
    IF cl.doModalPopup("PrintDlg", PrintFilterText, TRUE, time) # Op.Yes THEN
      RETURN;
    END;
    msgList := RequireSelectedMessages(cl);
    IF msgList = NIL THEN RETURN END;
    SetStatusLine(cl, "Spooling ...", StatusLineStyle.Progress);
    LOCK VBT.mu DO
      printFilter := FormsVBT.GetText(cl.popupFV, PrintFilterText);
    END;
    IF Text.Empty(printFilter) THEN printFilter := cl.GetPrintFilter() END;
    TRY
      UnixMail.PrintMsg(cl.openFolder, msgList, printFilter);
    EXCEPT
    | UnixMail.Error(errmsg) =>
        cl.unlockedError(errmsg);
        RETURN
    END;
    SetStatusLine(cl, "Spooled via " & printFilter,
                  StatusLineStyle.AutoFlush);
  END DoPrint;

PROCEDURE DoLoadPrev(cl: MyClosure) =
    (* LL = actions *)
  BEGIN
    LoadPrevFolder(cl);
  END DoLoadPrev;

PROCEDURE DoSave(cl: MyClosure; time: VBT.TimeStamp) =
    (* LL = actions *)
  VAR msgList: TextList.T; fileName: TEXT;
  BEGIN
    IF Text.Equal(cl.openFolder, UnixMail.NIFolderName) THEN
      cl.unlockedError("You can't save from this browser - " &
                       "it's not implemented.");
      RETURN;
    END;
    IF cl.doModalPopup("SaveDlg", "SaveHelper", TRUE, time) # Op.Yes THEN
      RETURN;
    END;
    LOCK VBT.mu DO fileName := FormsVBT.GetText(cl.popupFV, SaveFile);  END;
(*
    TRY
      IF OSUtils.GetInfo(fileName, mtime) = OSUtils.FileType.Dir THEN
        LOCK VBT.mu DO FormsVBT.PutText(cl.popupFV, SaveFile, fileName) END;
        RETURN;
      END;
    EXCEPT OSUtils.FileNotFound, OSUtils.FileError =>
      (* Treat as file - we'll get the error message when saving it. *)
    END;
*)
    msgList := RequireSelectedMessages(cl);
    IF msgList = NIL THEN RETURN END;
    SetStatusLine(cl, "Saving ...", StatusLineStyle.Progress);
    TRY
      UnixMail.SaveMsg(cl.openFolder, msgList, fileName);
    EXCEPT UnixMail.Error(errmsg) =>
      cl.unlockedError(errmsg);
      RETURN
    END;
  END DoSave;

PROCEDURE DoInc(cl: MyClosure) =
    (* LL = actions *)
  BEGIN
    SetStatusLine(cl, "Please wait ...", StatusLineStyle.Progress);
    UnixMail.ChangeFolders();
    LOCK mailCheckMutex DO
      LOCK VBT.mu DO cl.mailCount := 0 END;
      DoOpenFolder(cl, UnixMail.InboxName, TRUE, TRUE);
    END;
  END DoInc;

PROCEDURE DoNews(cl: MyClosure) =
    (* LL = actions *)
  VAR
    folder: TEXT; hasNews: BOOLEAN;
    this := cl.bbPtr;
  BEGIN
    SetStatusLine(cl, "Please wait ...", StatusLineStyle.Progress);
    LOOP
      this := UnixMail.GetNextBBoard(b := this,
                                     checkNewsDir := FALSE,
                                     folder := folder,
                                     hasNews := hasNews);
      IF hasNews THEN EXIT END;
      IF this = cl.bbPtr THEN
        SetStatusLine(cl, "No more news", StatusLineStyle.AutoFlush);
        RETURN
      END;
    END;
    cl.bbPtr := this;
    UnixMail.ChangeFolders();
    DoOpenFolder(cl, folder, TRUE, FALSE);
  END DoNews;

PROCEDURE DoFoldersFlip(cl: MyClosure) =
    (* LL = VBT.mu *)
  <* FATAL MultiSplit.NotAChild, Split.NotAChild *>
  VAR
    zChild := FormsVBT.GetVBT(cl.fv, "FoldersZChild");
    list := FormsVBT.GetVBT(cl.fv, "FoldersWrap");
    small := FormsVBT.GetVBT(cl.fv, "BrowserTile");
    big := FormsVBT.GetVBT(cl.fv, "MainHTile");
    from, to: VBT.T;
    width := Rect.HorSize(VBT.Domain(list));
    windowDelta: INTEGER;
  BEGIN
    IF MultiSplit.Pred(big, NIL) = list THEN
      from := big; to := small;
    ELSE
      from := small; to := big;
    END;
    IF longFolders THEN
      windowDelta := 0;
    ELSIF cl.foldersFlipDelta # 0 THEN
      windowDelta := -cl.foldersFlipDelta; cl.foldersFlipDelta := 0;
    ELSE
      windowDelta := VBT.Domain(list).east -
                     VBT.Domain(MultiSplit.Pred(from, list)).east;
      IF to = small THEN
        windowDelta := -windowDelta;
      END;
      cl.foldersFlipDelta := windowDelta;
    END;
    IF NOT Rect.IsEmpty(VBT.Domain(to)) THEN
      ZSplit.SetReshapeControl(zChild, ZSplit.ENChains);
    END;
    MultiSplit.Delete(from, list);
    MultiSplit.Insert(to, MultiSplit.Pred(to, NIL), list);
    HVSplit.Adjust(to, Split.Pred(to, list),
                   Rect.HorSize(VBT.Domain(to)) - width);
    StableVBT.SetShape(VBT.Parent(cl.window),
                       Rect.HorSize(VBT.Domain(cl.window))+windowDelta, 0);
  END DoFoldersFlip;

PROCEDURE DoMoveCopyDelete(cl: MyClosure; op: Op) =
    (* LL = actions *)
  VAR msgList: TextList.T; destFolder, errmsg, statusMsg: TEXT;
  BEGIN
    (* Make sure the command is legal. *)
    IF op # Op.Copy THEN
      IF Text.Equal(cl.openFolder, UnixMail.NIFolderName) THEN
        cl.unlockedError(
               "You can\'t move or delete from the NI browser; use \"copy\".");
        RETURN
      ELSIF UnixMail.IsBBoard(cl.openFolder) THEN
        cl.unlockedError(
             "You can\'t move or delete from a bulletin board; use \"copy\".");
        RETURN
      END;
    END;
    IF op = Op.Delete THEN
      IF (cl.config.reallyDeleteMessages
         OR Text.Equal(cl.openFolder, cl.config.deleteMessagesToFolder)) THEN
        destFolder := "";
      ELSE
        op := Op.Move;
        destFolder := cl.config.deleteMessagesToFolder;
      END;
    ELSE
      destFolder := GetSelectedFolder(cl, TRUE);
    END;
    IF op # Op.Delete THEN
      IF Text.Empty(destFolder) THEN
        cl.unlockedError("No folder selected.");
        RETURN
      END;
      IF UnixMail.IsBBoard(destFolder) THEN
        cl.unlockedError(
                   "You can\'t move or copy a message into a bulletin board.");
        RETURN
      END;
      IF Text.Equal(cl.openFolder, destFolder) THEN
        cl.unlockedError(
      "You can\'t move or copy a message into the folder from which it came.");
        RETURN
      END;
    END;
    msgList := RequireSelectedMessages(cl);
    IF msgList = NIL THEN RETURN  END;
      (* Do the real work. *)
    SetStatusLine(cl, "Please wait ...", StatusLineStyle.Progress);
    TRY
      CASE op OF
        | Op.Delete =>
            UnixMail.DeleteMsg(cl.openFolder, msgList);
            statusMsg := "Deleting";
        | Op.Move =>
            UnixMail.MoveMsg(cl.openFolder, destFolder, msgList);
            statusMsg := "Moving to \"" & destFolder & "\"";
        | Op.Copy =>
            UnixMail.CopyMsg(cl.openFolder, destFolder, msgList);
            statusMsg := "Copying to \"" & destFolder & "\"";
      ELSE
          <*ASSERT FALSE*>
      END;
      SetStatusLine(cl, statusMsg, StatusLineStyle.AutoFlush);
    EXCEPT
      | UnixMail.Error(z_19) =>
          errmsg := z_19;
          cl.unlockedError(errmsg);
          RETURN
    END;
    IF op # Op.Copy THEN DeleteSelected(cl) END;
  END DoMoveCopyDelete;

PROCEDURE DoDownArrow(cl: MyClosure) =
    (* LL = actions *)
  VAR msgCount, mailCount, lastSelIndex: INTEGER;
  BEGIN
    cl.downward := TRUE;
    lastSelIndex := GetLastSelectedMessage(cl);
    IF lastSelIndex >= 0 THEN
      msgCount := cl.msgBrowser.count();
      INC(lastSelIndex);
      IF lastSelIndex < msgCount THEN
        SelectMessage(cl, lastSelIndex, (*forceDisplay:*) TRUE);
      ELSE
        LOCK VBT.mu DO mailCount := cl.mailCount END;
        IF Text.Equal(cl.openFolder, UnixMail.InboxName) THEN
          IF mailCount > 0 THEN
            DoInc(cl); RETURN
          ELSIF UnixMail.GetNewsCount() > 0 THEN
            DoNews(cl); RETURN
          ELSE
            SetStatusLine(cl, "No new messages",
                          StatusLineStyle.AutoFlush);
          END;
        ELSIF UnixMail.IsBBoard(cl.openFolder) THEN
          IF UnixMail.GetNewsCount() > 0 THEN
            DoNews(cl); RETURN
          ELSIF mailCount > 0 THEN
            UnixMail.ChangeFolders();
            DoInc(cl); RETURN
          ELSE
            SetStatusLine(cl, "No new messages",
                          StatusLineStyle.AutoFlush);
          END;
        ELSE
          SetStatusLine(cl, "Bottom of folder",
                        StatusLineStyle.AutoFlush);
        END;
      END;
    END;
  END DoDownArrow;

PROCEDURE DoUpArrow(cl: MyClosure) =
    (* LL = actions *)
  VAR lastSelIndex: INTEGER;
  BEGIN
    cl.downward := FALSE;
    lastSelIndex := GetLastSelectedMessage(cl);
    IF lastSelIndex > 0 THEN
      DEC(lastSelIndex);
      SelectMessage(cl, lastSelIndex, (*forceDisplay:*) TRUE);
    ELSE
      SetStatusLine(cl, "Top of folder", StatusLineStyle.AutoFlush);
    END;
  END DoUpArrow;

PROCEDURE DoCompose(fv: FormsVBT.T;
                    name: TEXT;
                    arg: REFANY;
                    <*UNUSED*> ticks: VBT.TimeStamp) =
    (* Comp button *)
    (* LL = VBT.mu *)
  BEGIN
    MakePassive(arg, fv, Compose);
    EVAL Thread.Fork(NEW(ComposeForkee, cl := arg, fv := fv,
                         eventName := name));
  END DoCompose;

PROCEDURE DoForw(cl: MyClosure) =
    (* LL = actions *)
  VAR msgList: TextList.T; cc, body, fcc: TEXT;
  BEGIN
    msgList := RequireSelectedMessages(cl);
    IF msgList = NIL THEN RETURN END;
    fcc := FccFolder(cl);
    SetStatusLine(cl, "Constructing draft ...", StatusLineStyle.Progress);
    TRY
      UnixMail.ForwardMsg(cl.openFolder, msgList,
                          cl.config.autoCcToYourself, cc, body);
    EXCEPT
    | UnixMail.Error(errmsg) =>
        cl.unlockedError(errmsg);
        RETURN
    END;
    cl.compose.Compose("", cc, fcc, "", "", body);
  END DoForw;

PROCEDURE DoReply(cl: MyClosure; op: Op; event: AnyEvent.T) =
    (* LL = actions *)
  VAR
    msgList: TextList.T;
    incMsg: BOOLEAN;
    msgID, to, cc, subject, inReplyTo, body, fcc: TEXT;
  BEGIN
    msgList := RequireSelectedMessages(cl);
    IF msgList = NIL THEN RETURN END;
    IF TextList.Length(msgList) > 1 THEN
      cl.unlockedError("Can\'t reply to multiple messages.");
      RETURN
      END;
    msgID := GetFirstMsgId(msgList);
    fcc := FccFolder(cl);
    SetStatusLine(cl, "Constructing draft ...", StatusLineStyle.Progress);
    incMsg := cl.config.includeMessageInDraft;
    IF NOT incMsg THEN 
      IF event # NIL AND ISTYPE(event, AnyEvent.Mouse) THEN
        incMsg := VBT.Modifier.Control IN
                               NARROW(event, AnyEvent.Mouse).mouse.modifiers;
      END;
    END;
    TRY
      UnixMail.ReplyMsg(cl.openFolder, msgID,
                        op = Op.ReplyToSender,
                        cl.config.autoCcToYourself, incMsg,
                        cl.config.includeReplyString, to, cc, subject,
                        inReplyTo, body);
    EXCEPT UnixMail.Error(errmsg) =>
      cl.unlockedError(errmsg);
      RETURN
    END;
    cl.compose.Compose(to, cc, fcc, subject, inReplyTo, body);
  END DoReply;

PROCEDURE DoMsgDisplay(cl: MyClosure) =
    (* Double-click on message, or single-click if autoDisplay *)
    (* LL = actions *)
  VAR msgList: TextList.T; msgID: TEXT;
  BEGIN
    msgList := GetSelectedMessages(cl);
    IF TextList.Length(msgList) = 1 THEN
      msgID := GetFirstMsgId(msgList);
      UpdateDisplayPort(cl, msgID);
    END;
    UpdateButtons(cl);
  END DoMsgDisplay;

PROCEDURE DoFolderOpen(cl: MyClosure) =
    (* double-click on folder *)
    (* LL = actions *)
  VAR folder: TEXT;
  BEGIN
    folder := GetSelectedFolder(cl, FALSE);
    IF Text.Empty(folder) THEN
      cl.unlockedError(
                       "Folder browser completion event without a selection.");
      RETURN
    END;
    SetStatusLine(cl, "Please wait ...", StatusLineStyle.Progress);
    UnixMail.ChangeFolders();
    DoOpenFolder(cl, folder, UnixMail.IsBBoard(folder), FALSE);
  END DoFolderOpen;


(* *)
(* Command invocation: Passive, ApplyOp and ForkedOp *)
(* *)

PROCEDURE Passive(cl: MyClosure; bePassive: BOOLEAN) =
    VAR
      cursor: Cursor.T;
  PROCEDURE AdjustReactivity(fv: FormsVBT.T; name: TEXT) =
    BEGIN
      IF bePassive THEN
        FormsVBT.MakePassive(fv, name);
      ELSE
        FormsVBT.MakeActive(fv, name);
      END;
    END AdjustReactivity;
  BEGIN
    IF bePassive THEN
      cursor := cl.waitCursor;
    ELSE
      cursor := Cursor.TextPointer;
    END;
    cl.isPassive := bePassive;
    IF cl.fv # NIL THEN
      VBT.SetCursor(FormsVBT.GetVBT(cl.fv, "FixedBtnsAndBrowsers"),
                    cursor);
      AdjustReactivity(cl.fv, "GeneralMenu");
      AdjustReactivity(cl.fv, "FoldersMenu");
      AdjustReactivity(cl.fv, "BrowserMenu");
      AdjustReactivity(cl.fv, "Inc");
      AdjustReactivity(cl.fv, "News");
      AdjustReactivity(cl.fv, "Move");
      AdjustReactivity(cl.fv, "Copy");
      AdjustReactivity(cl.fv, "Delete");
      AdjustReactivity(cl.fv, "DownArrow");
      AdjustReactivity(cl.fv, "UpArrow");
      AdjustReactivity(cl.fv, "Forward");
      AdjustReactivity(cl.fv, "ReplyMenu");
    END;
    IF cl.popupFV # NIL THEN
      VBT.SetCursor(cl.popupFV,  cursor);
      AdjustReactivity(cl.popupFV, NewFolderText);
      AdjustReactivity(cl.popupFV, FindText);
      AdjustReactivity(cl.popupFV, "FindFirst");
      AdjustReactivity(cl.popupFV, "FindNext");
      AdjustReactivity(cl.popupFV, "FindPrevious");
      AdjustReactivity(cl.popupFV, PrintFilterText);
      AdjustReactivity(cl.popupFV, SaveFile);
    END;
    IF cl.niBrowserFV # NIL THEN
      VBT.SetCursor(cl.niBrowserFV,  cursor);
      AdjustReactivity(cl.niBrowserFV, "maxNIMessages");
      AdjustReactivity(cl.niBrowserFV, "PredicateTSplit");
      AdjustReactivity(cl.niBrowserFV, "NIBrowse");
      AdjustReactivity(cl.niBrowserFV, "NICount");
      AdjustReactivity(cl.niBrowserFV, "NIReset");
      AdjustReactivity(cl.niBrowserFV, "NIFlip");
    END;
    IF cl.configFV # NIL THEN
      VBT.SetCursor(cl.configFV,  cursor);
      AdjustReactivity(cl.configFV, "ConfigFilter");
    END;
  END Passive;

TYPE ForkedOpClosure = Thread.Closure OBJECT
      cl: MyClosure;
      op: Op;
      time: VBT.TimeStamp;
      event: AnyEvent.T;
    OVERRIDES
      apply := ForkedOp;
    END;

PROCEDURE ForkedOp(self: ForkedOpClosure): REFANY =
    (* LL = 0, interface is passive *)
    (* Forked by ApplyOp; not used anywhere else *)
  VAR
    op := self.op;
    cl := self.cl;
  BEGIN
    LOCK actions DO
      CASE op OF
      | Op.Init => cl.initInstance();
      | Op.FindFirst, Op.FindPrevious, Op.FindNext =>
          DoFind(cl, op);
          LOCK VBT.mu DO MakeDormant(cl, cl.popupFV, "FindCancel") END;
      | Op.NICount, Op.NIBrowse, Op.NIFlip, Op.ShowConversation =>
          DoNI(cl, op, self.time);
      | Op.Help => DoHelpWindow(cl);
      | Op.SetConfiguration => DoConfigWindow(cl);
      | Op.NewFolder => DoNewFolder(cl, self.time);
      | Op.Rescan => DoRescan(cl, self.time);
      | Op.SortPack => DoSortPack(cl, self.time);
      | Op.Purge => DoPurge(cl, self.time);
      | Op.RemoveFolder => DoRemove(cl, self.time);
      | Op.LoadPrevious => DoLoadPrev(cl);
      | Op.Print => DoPrint(cl, self.time);
      | Op.Save => DoSave(cl, self.time);
      | Op.Inc => DoInc(cl);
      | Op.News => DoNews(cl);
      | Op.Move, Op.Copy, Op.Delete => DoMoveCopyDelete(cl, op);
      | Op.DownArrow => DoDownArrow(cl);
      | Op.UpArrow => DoUpArrow(cl);
      | Op.Forward => DoForw(cl);
      | Op.ReplyToSender, Op.ReplyToAll => DoReply(cl, op, self.event);
      | Op.DisplayMessage => DoMsgDisplay(cl);
      | Op.OpenFolder => DoFolderOpen(cl);
      ELSE
      END;
      LOCK VBT.mu DO
        IF cl.showingError OR NOT cl.statusLineUp THEN
          ClearStatusLine(cl);
        END;
        cl.passive(FALSE);
        cl.worker := NIL;
      END;
      (* Note: a "stop" operation won't find us now *)
      EVAL Thread.TestAlert();
    END;
    RETURN NIL
  END ForkedOp;

PROCEDURE ApplyOp(cl: MyClosure; op: Op; time: VBT.TimeStamp;
                  event: AnyEvent.T := NIL) =
    (* LL = VBT.mu *)
    (* All operation invocation gesturess call this procedure, which either
       performs the entire operation, or makes the UI passive and forks a
       thread to perform the operation. *)
  VAR forkCl: ForkedOpClosure;
  BEGIN
    IF op IN ResponseOps THEN
      cl.passive(TRUE);
      cl.response := op;
      cl.responseTime := time;
      Thread.Signal(cl.responseCV);
    ELSIF op IN AsyncOps THEN
      CASE op OF
      | Op.Stop => IF cl.worker # NIL THEN Thread.Alert(cl.worker) END;
      | Op.AsyncErrorClose => FormsVBT.PopDown(cl.fv, AsyncErrorText);
      | Op.FindStop => cl.stopFindRequest := TRUE;
      | Op.NIClose => HideWindow(cl.niBrowserFV);
      | Op.Quit => UnixMail.CleanUp(); Process.Exit(0);
      | Op.FoldersFlip => DoFoldersFlip(cl);
      | Op.Compose => DoCompose(cl.fv, Compose, cl, time);
      ELSE
      END;
    ELSIF cl.worker # NIL THEN
      IF NOT cl.isPassive THEN Beep(cl.window) END;
    ELSE
      IF cl.showingError THEN DoPopDown(cl, time) END;
      IF cl.statusLineUp THEN ClearStatusLine(cl) END;
      IF op IN ImmediateOps THEN
        CASE op OF
        | Op.Null =>
        | Op.ClosePopup =>
            DoPopDown(cl, time);
        | Op.Find =>
            DoPopup(cl, "FindDlg", FindText, TRUE, time);
        | Op.NI =>
            FormsVBT.PutInteger(cl.niBrowserFV, "maxNIMessages", 9999);
            IF NOT ShowWindow(cl.niBrowserFV, cl.window,
                              AppName & ": NI Query") THEN
              cl.error("Can't install NI query window");
            END;
        | Op.NIReset => cl.ni.reset(time);
        | Op.About =>
            DoPopup(cl, "AboutDlg");
            cl.showingError := TRUE; (* so that it will go away promptly *)
        | Op.Rescreen =>
            Rescreen.DoIt("rescreen.fv", "Confirm", "Cancel",
                          cl.path, cl.window);
        ELSE
        END;
      ELSE
        cl.passive(TRUE);
        cl.response := Op.Null;
        forkCl := NEW(ForkedOpClosure, cl := cl, op := op,
                      time := time, event := event);
        cl.worker := Thread.Fork(forkCl);
      END;
    END;
  END ApplyOp;


(* *)
(* Other event procedures *)
(* *)

PROCEDURE DoFont(fv: FormsVBT.T;
                 <*UNUSED*> name: TEXT;
                 arg: REFANY;
                 <*UNUSED*> ticks: VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "displayFont", NARROW(arg, TEXT));
    IF MiscUtils.Equal(arg, Font6x13, TRUE) THEN
      FormsVBT.PutText(fv, "displayFontSize", Config.NoFontSize);
    END;
  END DoFont;

PROCEDURE DoFontSize(fv: FormsVBT.T;
                     <*UNUSED*> name: TEXT;
                     arg: REFANY;
                     <*UNUSED*> ticks: VBT.TimeStamp) =
  BEGIN
    FormsVBT.PutText(fv, "displayFontSize", NARROW(arg, TEXT));
  END DoFontSize;


(* *)
(* Methods for sub-class of FormsVBT.T for creation and event handling *)
(* *)

TYPE FVClosure = FormsVBT.Closure OBJECT
  cl: MyClosure;
  op: Op;
  OVERRIDES
    apply := FVApply;
  END;

PROCEDURE MyFormMisc(self: MyForm; READONLY cd: VBT.MiscRec) =
  (* LL = VBT.mu *)
  BEGIN
    IF cd.type = VBT.Deleted OR cd.type = VBT.Disconnected THEN
      self.cl.applyOp(self.deletionOp, cd.time);
    END;
    FormsVBT.T.misc(self, cd);
  END MyFormMisc;

PROCEDURE MyFormRealize(self: MyForm; type, name: TEXT): VBT.T
                   RAISES {FormsVBT.Error} =
  BEGIN
    IF Text.Equal(type, "Menu") THEN
      RETURN NEW(FastMenu.T)
    ELSE
      RETURN FormsVBT.T.realize(self, type, name)
    END;
  END MyFormRealize;

PROCEDURE MyFormAttach(self: MyForm; name: TEXT;
                       op: Op) RAISES { FormsVBT.Error } =
  BEGIN
    FormsVBT.Attach(self, name, NEW(FVClosure, cl := self.cl, op := op));
  END MyFormAttach;

PROCEDURE LifterMouse(v: VBT.T; READONLY cd: VBT.MouseRec) =
    (* LL.sup = VBT.mu *)
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      ZSplit.Lift(v, ZSplit.Altitude.Top);
    END;
    FormsVBT.T.mouse(v, cd);
  END LifterMouse;

PROCEDURE FVApply(self: FVClosure;
                  fv: FormsVBT.T;
                  <*UNUSED*> name: TEXT;
                  time: VBT.TimeStamp) =
  (* Minor preparation and tidying, then forks an action *)
  (* LL = VBT.mu *)
  BEGIN
    WITH event = FormsVBT.GetTheEvent(fv) DO
      IF self.op IN NumericOps AND ISTYPE(event,
                                          AnyEvent.Mouse) THEN
        RETURN; (* up/down buttons *)
      ELSE
        self.cl.applyOp(self.op, time, event);
      END;
    END;
  END FVApply;


(* *)
(* Browser classes *)
(* *)

TYPE MyMsgBrowserSelector = ListVBT.MultiSelector OBJECT
    cl: MyClosure;
  OVERRIDES
    insideClick := MsgBrowserInsideClick;
      (* Calls inherited then invokes action if clickType=LastUp *)
  END;

TYPE MyFolderSelector = ListVBT.UniSelector OBJECT
    cl: MyClosure;
  OVERRIDES
    insideClick := FolderInsideClick;
      (* Inherited, then various actions if clickType = lastUp *)
  END;

PROCEDURE InitBrowsers (cl: MyClosure) =
  (* LL = VBT.mu *)
  PROCEDURE FromRGB(READONLY rgb : Color.T;
                    mode := PaintOp.Mode.Normal;
                    gray := -1.0;
                    bw := PaintOp.BW.UseIntensity): PaintOp.T =
    BEGIN
      RETURN PaintOp.FromRGB (rgb.r, rgb.g, rgb.b, mode, gray, bw)
    END FromRGB;
  VAR
    contentsBgRGB := FormsVBT.GetColorProperty(
                       cl.fv, "ContentsColor", "BgColor");
    scrollBgRGB := FormsVBT.GetColorProperty(
                     cl.fv, "BrowserScrollColor", "BgColor");
    hiliteBgRGB := FormsVBT.GetColorProperty(
                     cl.fv, "BrowserHiliteColor", "BgColor");
    hiliteFgRGB := FormsVBT.GetColorProperty(
                     cl.fv, "BrowserHiliteColor", "Color");
    contentsBg := FromRGB(
                    rgb := contentsBgRGB, mode := PaintOp.Mode.Accurate,
                    bw := PaintOp.BW.UseBg);
    scrollBg := FromRGB(rgb := scrollBgRGB, mode := PaintOp.Mode.Accurate,
                        bw := PaintOp.BW.UseBg);
    hiliteBg := FromRGB(rgb := hiliteBgRGB, mode := PaintOp.Mode.Accurate,
                        bw := PaintOp.BW.UseFg);
    hiliteFg := FromRGB(rgb := hiliteFgRGB, mode := PaintOp.Mode.Accurate,
                        bw := PaintOp.BW.UseBg);
    contentsColors := PaintOp.MakeColorScheme(contentsBg, PaintOp.Fg);
    scrollColors := PaintOp.MakeColorQuad(scrollBg, PaintOp.Fg);
    msgs := NEW(ListVBT.T);
    msgsPainter := NEW (ListVBT.TextPainter).init(
                     bg := contentsBg, fg := PaintOp.Fg, hiliteBg := hiliteBg,
                     hiliteFg := hiliteFg);
    msgsSelector := NEW(MyMsgBrowserSelector, cl := cl).init(msgs);
    folders := NEW(ListVBT.T);
    folderPainter := NEW(ListVBT.TextPainter).init(
                       bg := contentsBg, fg := PaintOp.Fg,
                       hiliteBg := hiliteBg, hiliteFg := hiliteFg);
    folderSelector := NEW(MyFolderSelector, cl := cl).init(folders);
  BEGIN
    cl.contentsColors := contentsColors;
    cl.scrollColors := scrollColors;
    msgs.painter := msgsPainter;
    msgs.selector := msgsSelector;
    cl.msgBrowser := msgs.init(colors := scrollColors);
    FormsVBT.PutGeneric(cl.fv, "Headers", msgs);
    folders.painter := folderPainter;
    folders.selector := folderSelector;
    cl.folderBrowser := folders.init(colors := scrollColors);
    FormsVBT.PutGeneric(cl.fv, "Folders", folders);
  END InitBrowsers;

PROCEDURE MsgBrowserInsideClick(selector: MyMsgBrowserSelector;
                                READONLY cd: VBT.MouseRec;
                                this: ListVBT.Cell) =
  (* LL = VBT.mu *)
  VAR cl := selector.cl;
  BEGIN
    IF NOT cl.isPassive THEN
      ListVBT.MultiSelector.insideClick(selector, cd, this);
      IF cd.clickType = VBT.ClickType.LastUp THEN
        IF (cd.clickCount = 3) # cl.config.autoDisplayMessages THEN
          cl.applyOp(Op.DisplayMessage, cd.time);
          LockedUpdateButtons(cl);
        END;
      END;
    END;
  END MsgBrowserInsideClick;

PROCEDURE FolderInsideClick(selector: MyFolderSelector;
                            READONLY cd: VBT.MouseRec;
                            this: ListVBT.Cell) =
  (* LL = VBT.mu *)
  VAR cl := selector.cl;
  BEGIN
    IF NOT cl.isPassive THEN
      ListVBT.UniSelector.insideClick(selector, cd, this);
      IF cd.clickType = VBT.ClickType.LastUp THEN
        IF VBT.Modifier.Control IN cd.modifiers THEN
          IF VBT.Modifier.Shift IN cd.modifiers THEN
            cl.applyOp(Op.Move, cd.time)
          ELSE
            cl.applyOp(Op.Copy, cd.time);
          END
        ELSIF cd.clickCount = 3 THEN
          cl.applyOp(Op.OpenFolder, cd.time);
        END;
        LockedUpdateTitle(cl);
        LockedUpdateButtons(cl);
      END;
    END;
  END FolderInsideClick;

PROCEDURE FindValue(list: ListVBT.T; value: TEXT): ListVBT.Cell =
  (* LL = actions *)
  (* Could use binary search; but for today, just linear *)
  (* Ignores an initial FolderModifiedChar *)
  VAR found, modifiedValue: TEXT;
  BEGIN
    modifiedValue := Text.FromChar(FolderModifiedChar) & value;
    FOR i := 0 TO list.count()-1 DO
      found := list.getValue(i);
      IF Text.Equal(found, value) THEN RETURN i END;
      IF Text.Equal(found, modifiedValue) THEN RETURN i END;
    END;
    RETURN list.count()
  END FindValue;


(* *)
(* Initialization *)
(* *)

PROCEDURE InitGlobals() =
  (* LL = init? *)
  BEGIN
    InitShowQueue();
    user := Env.Get("USER");
    IF user = NIL THEN
      Wr.PutText (Stdio.stderr, "Who are you?  Please define $USER\n");
      Process.Exit(4);
    END;
    home := Env.Get("HOME");
    IF home = NIL THEN
      Wr.PutText(Stdio.stderr, "Where do you live?  Please define $HOME.\n");
      Process.Exit (5);
    END;
  END InitGlobals;

TYPE InitImage = PixmapVBT.T OBJECT
  OVERRIDES
    shape := DefaultShape;
    misc := InitImageMisc;
  END;

PROCEDURE InitImageMisc(self: PixmapVBT.T; READONLY cd: VBT.MiscRec) =
  (* LL = VBT.mu *)
  BEGIN
    IF cd.type = VBT.Deleted OR cd.type = VBT.Disconnected THEN
      Process.Exit(0);
    END;
    PixmapVBT.T.misc(self, cd);
  END InitImageMisc;

PROCEDURE DefaultShape(v: VBT.T; ax: Axis.T;
                       <*UNUSED*> n: CARDINAL): VBT.SizeRange =
    (* LL = VBT.mu.v *)
  VAR sr: VBT.SizeRange;
  BEGIN
    IF ax = Axis.T.Hor THEN
      sr.pref := ROUND(VBT.MMToPixels(v, 580.0 * 25.4 / 72.0, ax));
    ELSE
      sr.pref := ROUND(VBT.MMToPixels(v, 350.0 * 25.4 / 72.0, ax));
    END;
    sr.lo := 0;
    sr.hi := VBT.DefaultShape.hi;
    RETURN sr
  END DefaultShape;

PROCEDURE InitInstance(cl: MyClosure) =
    (* LL = actions *)
  VAR
    bg := PaintOp.FromRGB(0.325, 0.585, 0.650,
                          PaintOp.Mode.Accurate,
                          1.0,
                          PaintOp.BW.UseBg);
    decFg := PaintOp.FromRGB(0.256, 0.0, 0.029,
                             PaintOp.Mode.Accurate,
                             0.0,
                             PaintOp.BW.UseFg);
    path := Rsrc.BuildPath("$PostcardPATH", PostcardBundle.Get());
    logo := ImageFromRsrc("logo.pbm", path);
    initImage := NEW(InitImage).init(logo,
      op := PaintOp.Pair(bg, decFg), bg := bg);
  BEGIN
    cl.path := path;
    cl.responseCV := NEW(Thread.Condition);
    cl.waitCursor := Cursor.NotReady;
    LOCK VBT.mu DO VBT.SetCursor(initImage, cl.waitCursor) END;
    cl.window := NEW(StableVBT.T).init(initImage);
    TRY
      LOCK VBT.mu DO
        (* Install window at earliest opportunity, to entertain user *)
        cl.passive(TRUE);
        XTrestle.Install(v := cl.window,
                         applName := AppName,
                         windowTitle := AppName & ": initializing",
                         iconTitle := AppName & ": initializing",
                         iconWindow := NIL);
      END;
    EXCEPT
    | TrestleComm.Failure =>
       Wr.PutText(Stdio.stderr, AppName & ": can't contact display server\n");
       Wr.Flush(Stdio.stderr);
       Process.Exit(6);
    | XTrestle.Error =>
        Wr.PutText(Stdio.stderr,
                   "Error in \"-geometry\" or \"-display\" parameter");
        Wr.Flush(Stdio.stderr);
        Process.Exit(7);
    END;
    Thread.Pause(0.25d0);
    cl.config := NEW(Config.T).init(); (* temporary configuration *)
    MailUtilities.SetAliasFile(home & MailAliasFilename);
    UnixMail.BeginInit(user, home, cl);
    TRY
      LOCK VBT.mu DO
        cl.fv := NEW(MyForm, cl := cl,
            deletionOp := Op.Quit).initFromRsrc("postcard.fv", cl.path);
        cl.popupFV := NEW(LifterFV, cl := cl).initFromRsrc("popup.fv",
                                                           cl.path, TRUE);
        WITH z = FormsVBT.GetVBT(cl.fv, "MainZSplit") DO
          ZSplit.InsertAt(z,  cl.popupFV,
                          Point.Origin,
                          ZSplit.Altitude.Top,
                          FALSE);
          cl.firstPopup := TRUE; (* causes next DoPopup to center it *)
        END;
        ZSplit.SetReshapeControl(cl.popupFV, NEW(CenterReshape));
        cl.niBrowserFV := NEW(MyForm, cl := cl,
            deletionOp := Op.NIClose).initFromRsrc("niBrowser.fv", cl.path);
        InitBrowsers(cl);
        cl.fv.attach("About", Op.About);
        cl.fv.attach("Help", Op.Help);
        cl.fv.attach("SetConfiguration", Op.SetConfiguration);
        cl.fv.attach("Rescreen", Op.Rescreen);
        cl.fv.attach("Quit", Op.Quit);
        cl.fv.attach("NewFolder", Op.NewFolder);
        cl.fv.attach("Rescan", Op.Rescan);
        cl.fv.attach("SortPack", Op.SortPack);
        cl.fv.attach("Purge", Op.Purge);
        cl.fv.attach("RemoveFolder", Op.RemoveFolder);
        cl.fv.attach("Find", Op.Find);
        cl.fv.attach("NI", Op.NI);
        cl.fv.attach("ShowConversation", Op.ShowConversation);
        cl.fv.attach("LoadPrevious", Op.LoadPrevious);
        cl.fv.attach("Print", Op.Print);
        cl.fv.attach("Save", Op.Save);
        cl.fv.attach("Inc", Op.Inc);
        cl.fv.attach("News", Op.News);
        cl.fv.attach("FoldersFlip", Op.FoldersFlip);
        cl.fv.attach("Move", Op.Move);
        cl.fv.attach("Copy", Op.Copy);
        cl.fv.attach("Delete", Op.Delete);
        cl.fv.attach("DownArrow", Op.DownArrow);
        cl.fv.attach("UpArrow", Op.UpArrow);
        cl.fv.attach("Compose", Op.Compose);
        cl.fv.attach("Forward", Op.Forward);
        cl.fv.attach("ReplyToSender", Op.ReplyToSender);
        cl.fv.attach("ReplyToAll", Op.ReplyToAll);
        cl.fv.attach("AsyncErrorClose", Op.AsyncErrorClose);
        IF Rescreen.Screens(cl.window) <= 1 THEN
          FormsVBT.MakeDormant(cl.fv, "Rescreen");
        END;
        IF longFolders THEN DoFoldersFlip(cl) END;
        IF NOT foldersFlip THEN
          HideZ(cl, FormsVBT.GetVBT(cl.fv, "FoldersZChild"));
        END;
        cl.popupFV.attach("AboutClose", Op.ClosePopup);
        cl.popupFV.attach("NewFolderClose", Op.No);
        cl.popupFV.attach(NewFolderText, Op.Yes);
        cl.popupFV.attach("NewFolderConfirm", Op.Yes);
        cl.popupFV.attach("NewFolderCancel", Op.No);
        cl.popupFV.attach(FindText, Op.FindFirst);
        cl.popupFV.attach("FindClose", Op.ClosePopup);
        cl.popupFV.attach("FindFirst", Op.FindFirst);
        cl.popupFV.attach("FindNext", Op.FindNext);
        cl.popupFV.attach("FindPrevious", Op.FindPrevious);
        cl.popupFV.attach("FindCancel", Op.FindStop);
        cl.popupFV.attach("PrintClose", Op.No);
        cl.popupFV.attach(PrintFilterText, Op.Yes);
        cl.popupFV.attach("PrintConfirm", Op.Yes);
        cl.popupFV.attach("PrintCancel", Op.No);
        cl.popupFV.attach("SaveClose", Op.No);
        cl.popupFV.attach("SaveFile", Op.Yes);
        cl.popupFV.attach("SaveConfirm", Op.Yes);
        cl.popupFV.attach("SaveCancel", Op.No);
        cl.popupFV.attach("ErrorClose", Op.ClosePopup);
        cl.popupFV.attach("ConfirmText", Op.Yes);
        cl.popupFV.attach("Yes", Op.Yes);
        cl.popupFV.attach("No", Op.No);
        FormsVBT.PutText(cl.popupFV, "Version", Version);
        cl.niBrowserFV.attach("PredicateTxt", Op.NIBrowse);
        cl.niBrowserFV.attach("NIBrowse", Op.NIBrowse);
        cl.niBrowserFV.attach("NICount", Op.NICount);
        cl.niBrowserFV.attach("NIReset", Op.NIReset);
        cl.niBrowserFV.attach("NIFlip", Op.NIFlip);
      END;
      cl.display :=
            NEW(BuiltInDisplay.T).Init(cl, FormsVBT.GetVBT(cl.fv, Splitter));
      cl.compose := NEW(BuiltInCompose.T).Init(cl);
      cl.ni := NEW(NI.T).init(cl, cl.niBrowserFV);
      UnixMail.CompleteInit();
      InitConfiguration(cl);
      LOCK VBT.mu DO
        MakeActive(cl, cl.fv, Compose);
        cl.passive(TRUE);
        EVAL Filter.Replace(cl.window, cl.fv);
      END;
      InitFolderBrowser(cl);
      InitOpenFolder(cl);
      EVAL Thread.Fork(NEW(MailCheckerForkee, cl := cl));
      EVAL Thread.Fork(NEW(NewsCheckerForkee, cl := cl));
      cl.bbPtr := NIL;
    EXCEPT
    | FormsVBT.Error(why) =>
        Wr.PutText(Stdio.stderr, AppName & ": initialization failed - "
                   & why & "\n");
        Wr.Flush(Stdio.stderr);
        Process.Exit(8);
    | Rsrc.NotFound =>
        Wr.PutText(Stdio.stderr, AppName & ": initialization failed - "
                   & "resource not found\n");
        Wr.Flush(Stdio.stderr);
        Process.Exit(9);
    END;
  END InitInstance;

VAR (* commmand line options *)
  longFolders := FALSE;
  foldersFlip := FALSE;

PROCEDURE Init() =
  VAR param: TEXT; paramNum := 1;
  BEGIN
    WHILE paramNum < Params.Count DO
      param := Params.Get(paramNum);
      IF Text.Empty(param) THEN
      ELSIF Text.Equal(param, "-geometry") OR
        Text.Equal(param, "-display") THEN
        paramNum := paramNum+1;(*next param is the argument of the option*)
      ELSIF Text.Equal(param, "-longfolders") OR
            Text.Equal(param, "-longFolders") THEN
        longFolders := TRUE;
      ELSIF Text.Equal(param, "-foldersflip") OR
            Text.Equal(param, "-foldersFlip") THEN
        foldersFlip := TRUE;
      ELSE
        Wr.PutText(Stdio.stderr, "Unknown option \"" & param & "\"\n");
        Wr.PutText(Stdio.stderr,
                "Usage:\n    Postcard [-geometry WxH+X+Y] [-display name]\n");
        Process.Exit(10)
      END;
      paramNum := paramNum+1;
    END;
    Thread.IncDefaultStackSize(4000);
    InitGlobals();
    LOCK VBT.mu DO
      NEW(MyClosure).applyOp(Op.Init, 0);
    END;
    WHILE TRUE DO Thread.Pause(1000.0D0) END;
  END Init;


BEGIN
  Init();
END PostcardMain.
