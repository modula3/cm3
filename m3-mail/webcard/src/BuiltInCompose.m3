(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Postcard - UI for built-in display *)

(* Last modified on Wed Nov  1 16:23:01 PST 1995 by mhb      *)
(*      modified on Wed Feb  1 12:19:55 PST 1995 by kalsow   *)
(*      modified on Fri Apr 22 17:01:35 PDT 1994 by birrell  *)
(*      modified on Mon Mar 29 12:34:06 PST 1993 by meehan   *)

MODULE BuiltInCompose EXPORTS BuiltInCompose;

IMPORT Axis, Closure, Cursor, Fmt, FormsVBT, FVTypes, KeyboardKey, KeyTrans,
       MiscUtils,
       MTextRd, MTextUnit, MailUtilities, MultiSplit, OSUtils, Point, Process,
       Rd, RefList, Rescreen, Rsrc, StableVBT, Stdio, Text, TextEditVBT,
       TextPort, TextPortClass, TextWr, Thread, Time, Trestle, TrestleComm,
       UnixMail, VBT, VBTClass, VText, Wr;

TYPE
  Window = FormsVBT.T OBJECT           (* Composition sub-window *)
    (* Read-only fields ... *)
             number: INTEGER;
             port  : TextPort.T;
    (* fields protected by sendLock ... *)
             usingExternal := FALSE;   (* using external editor *)
    (* Fields protected bu VBT.mu ... *)
             inUse := FALSE;           (* set by compose/modify; cleared by
                                          send/discard *)
             modified := FALSE;        (* backing file out of date *)
             backupTime := 0.0d0;      (* mtime last wrote backing file *)
           OVERRIDES
             realize := RealizeWindow
           END;

REVEAL
  T = Public BRANDED OBJECT
   (* Read-only fields ... *)
      cl: Closure.T;                   (* for callbacks to main form *)
      template: TEXT;                  (* form source *)
      windowTemplate: TEXT;            (* form for ownWindow *)
   (* Fields protected by VBT.mu ... *)
      ownFV: FormsVBT.T;               (* the form for ownWindow *)
      splitter: MultiSplit.T;          (* where to display the windows *)
      external: BOOLEAN;
      windows: RefList.T (* OF Window *); (* the extant composition windows *)
      count: CARDINAL;
      creator: Thread.T;               (* forkee parsing form *)
      near: VBT.T;                     (* for placing drafts window *)
    OVERRIDES
      Init := Init;
      Compose := Compose;
      SetSplitter := SetSplitter;
      SetExternal := SetExternal;
  END;

<* FATAL FormsVBT.Error *>
<* FATAL FormsVBT.Unimplemented *>
<* FATAL Thread.Alerted *>
<* FATAL Wr.Failure *>
<* FATAL Rd.Failure *>

(* *)
(* Keyboard actions *)
(* *)

CONST
  ComposeWidth = 80;
  Bell = ComposeWidth - 15;

PROCEDURE BuiltInJustify(tp: TextPort.T; vText: VText.T; caret: CARDINAL) =
    (* LL = VBT.mu *)
  VAR
    rd: Rd.T;
    wr: Wr.T;
    c: CHAR;
    i,
    blanks,            (* left margin padding *)
    bufPos,            (* index for next char into buffer *)
    lastBreak,         (* index in buffer of last line break point *)
    pos,               (* result of RdFindChar *)
    oldIndex: INTEGER; (* index in source of current pos *)
    newCaret: INTEGER; (* intended new index for setting caret *)
    buffer: ARRAY [0..ComposeWidth] OF CHAR;
    white, squashing: BOOLEAN;
    extent := MTextUnit.ParagraphExtent(vText.mtext, caret); (* JRM *)
    indexL := extent.left;
    indexR := extent.right;
  PROCEDURE CopyChar() RAISES { Rd.EndOfFile }=
    BEGIN
      IF oldIndex = caret THEN
        newCaret := indexL + Wr.Index(wr) + bufPos;
      END;
      c := Rd.GetChar(rd);
      INC(oldIndex);
      IF c = '\n' THEN
        squashing := TRUE;
        IF white THEN RETURN  END;
        c := ' ';
      ELSIF c = ' ' THEN
        IF squashing THEN RETURN  END;
      ELSE
        squashing := FALSE;
        IF white THEN lastBreak := bufPos END;
      END;
      IF (bufPos >= ComposeWidth) OR (lastBreak >= Bell) THEN
        IF lastBreak <= blanks THEN lastBreak := bufPos END;
        Wr.PutString(wr, SUBARRAY(buffer, 0, lastBreak));
        Wr.PutChar(wr, '\n');
        FOR z_7 := lastBreak TO bufPos - 1 DO
          i := z_7;
          buffer[i - lastBreak + blanks] := buffer[i];
        END;
        DEC(bufPos, lastBreak);
        FOR z_8 := 0 TO blanks - 1 DO i := z_8; buffer[i] := ' ' END;
        INC(bufPos, blanks);
        lastBreak := 0;
      END;
      buffer[bufPos] := c;
      INC(bufPos);
      white := (c = ' ');
    END CopyChar;
  BEGIN
    IF extent.inside THEN
      rd := MTextRd.New(vText.mtext, indexL, indexL, indexR);
      wr := TextWr.New();
      TRY
        pos := MiscUtils.RdFindChar(rd, '\n');
        IF (pos < 0) OR (pos + 1 >= indexR - indexL) THEN
          Rd.Seek(rd, 0)
        END;
        blanks := 0;
        LOOP
          c := Rd.GetChar(rd);
          IF c # ' ' THEN EXIT END;
          INC(blanks);
        END;
      EXCEPT Rd.EndOfFile =>
      END;
      Rd.Seek(rd, 0);
      TRY
        bufPos := 0;
        lastBreak := 0;
        oldIndex := indexL;
        newCaret := indexR;
        white := TRUE;
        squashing := FALSE;
        LOOP CopyChar();  END;
      EXCEPT Rd.EndOfFile =>
      END;
      Wr.PutString(wr, SUBARRAY(buffer, 0, bufPos));
      Wr.PutChar(wr, '\n');
      TextPort.Replace(tp, indexL, indexR, TextWr.ToText(wr));
      TextPort.Seek (tp, newCaret);
    END;
  END BuiltInJustify;

PROCEDURE BuiltInLineBreak(tp: TextPort.T; vText: VText.T; caret: INTEGER) =
    (* LL = VBT.mu *)
  VAR
    lastBreak, newCaret, bufPos: INTEGER;
    nonWhite: BOOLEAN;
    rd: Rd.T;
  <*FATAL Rd.EndOfFile*>
  BEGIN
    IF caret < Bell THEN RETURN (* too few characters in text *) END;
    rd := MTextRd.New(vText.mtext, caret, 0, caret, (*reverse*)TRUE);
    FOR i := 0 TO Bell - 1 DO
      IF Rd.GetChar(rd) = '\n' THEN RETURN  END
    END;
      (* Now we know there at least Bell chars after last newline *)
    bufPos := Bell;
    LOOP
      IF bufPos >= MIN(caret, ComposeWidth) THEN EXIT END;
      IF Rd.GetChar(rd) = '\n' THEN EXIT END;
      INC(bufPos);
    END;
    (* Now "bufPos" is MIN(number of chars after last newline, ComposeWidth) *)
    Rd.Seek(rd, 0);
    nonWhite := FALSE;
    lastBreak := 0;
    LOOP
      IF lastBreak >= bufPos THEN lastBreak := 0; EXIT END;
      IF Rd.GetChar(rd) = ' ' THEN
        IF nonWhite THEN EXIT END;
      ELSE
        nonWhite := TRUE;
      END;
      INC(lastBreak);
    END;
    IF (bufPos >= ComposeWidth) OR (bufPos - lastBreak >= Bell) THEN
          (* break it *)
      TextPort.Seek (tp, caret - lastBreak);
      TextPort.NewlineAndIndent(tp);
      newCaret := TextPort.Index(tp) + lastBreak;
      TextPort.Seek(tp, newCaret);
    END;
  END BuiltInLineBreak;

PROCEDURE BuiltInFilter (tp: FVTypes.Port; cd: VBT.KeyRec) =
  (* LL = VBT.mu *)
  VAR
    caret := TextPort.Index (tp);
    vText := TextPort.GetVText (tp);
    c     := KeyTrans.Latin1 (cd.whatChanged);
    model := tp.getModel ();
  BEGIN
    IF VBT.Modifier.Control IN cd.modifiers THEN
      IF c = 't' AND model # TextPort.Model.Emacs THEN
        BuiltInJustify (tp, vText, caret);
        RETURN
      END
    ELSIF VBT.Modifier.Option IN cd.modifiers THEN
      IF c = 'q' AND model = TextPort.Model.Emacs THEN
        BuiltInJustify (tp, vText, caret);
        RETURN
      END
    ELSIF cd.whatChanged = KeyboardKey.BackSpace THEN
    ELSIF cd.whatChanged = KeyboardKey.Delete THEN
    ELSIF cd.whatChanged = KeyboardKey.Return THEN
    ELSIF c = KeyTrans.NullKey THEN
    ELSIF TextPort.IsReplaceMode (tp) THEN
    ELSE
      BuiltInLineBreak (tp, vText, caret);
    END;
    TextPort.T.filter (tp, cd)
  END BuiltInFilter;


(* *)
(* Errors *)
(* *)

PROCEDURE ReportError(self: T; msg: TEXT) =
  (* LL = 0 *)
  BEGIN
    IF self.ownWindow = NIL THEN
      self.cl.AsyncErrorHandler(msg);
    ELSE
      LOCK VBT.mu DO
        FormsVBT.PutText(self.ownFV, "ErrorText", msg);
        FormsVBT.PopUp(self.ownFV, "ErrorText");
        IF self.ownWindow # NIL THEN ShowWindow(self) END;
      END;
    END;
  END ReportError;


(* *)
(* Auto-save daemon *)
(* *)

PROCEDURE SetModified(this: Window; modified: BOOLEAN) =
    (* LL = VBT.mu *)
  BEGIN
    IF modified THEN
      FormsVBT.PutText(this, "ComposeModified", "*");
    ELSE
      FormsVBT.PutText(this, "ComposeModified", "");
    END;
    TextPort.SetModified(this.port, modified);
    this.modified := modified;
  END SetModified;

PROCEDURE SetNotInUse (this: Window; action: TEXT) =
  (* LL = VBT.mu *)
  VAR draft: TextEditVBT.T;
  BEGIN
    this.inUse := FALSE;
    FormsVBT.PutText (this, "ComposeStatus", action);
    FormsVBT.PutInteger (this, "ComposeTSplit", 1);
    FormsVBT.PutText (this, "ComposeModified", "");
    FormsVBT.PutText (this, "Title", Fmt.Int(this.number));
    draft := FormsVBT.GetVBT(this, "Draft");
    draft.tp.setReadOnly(TRUE);
    TextPort.SetModified(this.port, FALSE);
  END SetNotInUse;

TYPE BuiltInComposeWatcherForkee = Thread.Closure OBJECT
      t: T;
      this: Window;
    OVERRIDES
      apply := BuiltInComposeWatcher;
    END;

PROCEDURE Save(w: Window) RAISES { UnixMail.Error } =
    (* Save the draft in its file if needed. *)
    (* On failures, switches window to notInUse. This prevents user making
       edits that will get lost, and ensures that we don't later
       pollute a more recent backing file written by another instance of
       the application. *)
    (* LL = sendLock *)
  VAR ok: BOOLEAN := FALSE; body: TEXT; mtime: Time.T;
  BEGIN
    LOCK VBT.mu DO
      body := FormsVBT.GetText(w, "Draft");
      SetModified(w, FALSE);
      (* set FALSE now, so we won't miss modifications while we write file *)
    END;
    TRY
      UnixMail.WriteDraftFile(body, w.number);
      mtime := UnixMail.DraftFileTime(w.number);
      ok := TRUE;
    FINALLY
      LOCK VBT.mu DO
        IF ok THEN
          w.backupTime := mtime;
        ELSE
          SetNotInUse(w, "Backup failed");
        END;
      END;
    END;
  END Save;

PROCEDURE BuiltInComposeWatcher(self: BuiltInComposeWatcherForkee): REFANY =
    (* LL = 0 *)
    VAR lastSave := Time.Now(); needsBackup: BOOLEAN;
  BEGIN
    LOOP
      Thread.Pause(1.0d0);
      LOCK sendLock DO
        LOCK VBT.mu DO
          IF NOT self.this.modified THEN
            IF TextPort.IsModified(self.this.port) THEN
              SetModified(self.this, TRUE);
            END;
          END;
          needsBackup := self.this.modified AND self.this.inUse AND
                             NOT self.this.usingExternal;
        END;
        IF needsBackup AND Time.Now() > lastSave + 30.0d0 THEN
          lastSave := Time.Now();
          TRY
            Save(self.this)
          EXCEPT UnixMail.Error(msg) =>
            ReportError(self.t, msg);
          END;
        END;
      END;
    END;
  END BuiltInComposeWatcher;


(* *)
(* Creating the form *)
(* *)

TYPE CreateForkee = Thread.Closure OBJECT
      t: T;
    OVERRIDES
      apply := Create
    END;

PROCEDURE Create (self: CreateForkee): REFANY =
  (* LL = 0 *)
  VAR
    this: Window := NEW (Window).init (self.t.template, TRUE, self.t.cl.path);
  BEGIN
    LOCK VBT.mu DO
      this.port := NARROW (FormsVBT.GetVBT (this, "Draft"), TextEditVBT.T).tp;
      self.t.cl.SetTextEditColors (FormsVBT.GetVBT(this, "Draft"));
      FormsVBT.AttachProc (this, "SendOpen", OnComposeAction, self.t);
      FormsVBT.AttachProc (this, "DiscardOpen", OnComposeAction, self.t);
      FormsVBT.AttachProc (this, "UseExternalOpen", OnComposeAction, self.t);
      FormsVBT.AttachProc (this, "ExpandOpen", OnComposeAction, self.t);
      FormsVBT.AttachProc (this, "IncludeOpen", OnComposeAction, self.t);
      FormsVBT.AttachProc (this, "FileToInclude", OnComposeAction, self.t);
      FormsVBT.AttachProc (this, "IncludeFileButton", OnComposeAction, self.t);
      FormsVBT.AttachProc (this, "CloseOpen", OnComposeClose, self.t);
      FormsVBT.AttachProc (this, "ReuseOpen", OnComposeReuse, self.t);
      FormsVBT.AttachEditOps (this, "Draft", "Cut", "Copy", "Paste", "Clear",
                              "SelectAll", "Undo", "Redo");
    END;
    EVAL Thread.Fork(NEW(BuiltInComposeWatcherForkee,
                         t := self.t, this := this));
    RETURN this
  END Create;

PROCEDURE RealizeWindow (fv: Window; type, name: TEXT): VBT.T =
  BEGIN
    IF Text.Equal (name, "Draft") THEN
      RETURN NEW (FVTypes.FVTextEdit,
                  tp := NEW (FVTypes.Port, filter := BuiltInFilter))
    ELSE
      RETURN FormsVBT.T.realize (fv, type, name)
    END
  END RealizeWindow;

PROCEDURE ShowWindow(self: T) =
    (* LL = VBT.mu *)
  VAR soRec := Trestle.ScreenOf(self.ownWindow, Point.Origin);
  BEGIN
    TRY
      IF soRec.trsl=NIL THEN
        Trestle.Attach(self.ownWindow);
        Trestle.Decorate(v := self.ownWindow,
                         applName := "Postcard",
                         windowTitle := "Postcard: drafts",
                         iconTitle := "Postcard: drafts");
        IF Rescreen.Screens(self.ownWindow) <= 1 THEN
          FormsVBT.MakeDormant(self.ownFV, "Rescreen");
        END;
      END;
      IF soRec.id = Trestle.NoScreen THEN
        Trestle.MoveNear(self.ownWindow, self.near)
      ELSE
        Trestle.MoveNear(self.ownWindow, NIL);
      END;
    EXCEPT TrestleComm.Failure =>
      self.cl.AsyncErrorHandler("Failed to install composition window.");
    END;
  END ShowWindow;


(* *)
(* Implementing the buttons part 1: forked buttons. During execution of these,
   the interface is made passive by filters. *)
(* *)

PROCEDURE ExpandAliases(this: Window) RAISES { UnixMail.Error } =
    (* LL < VBT.mu *)
  VAR body, newBody: TEXT;
  BEGIN
    LOCK VBT.mu DO body := FormsVBT.GetText(this, "Draft") END;
    TRY
      newBody := MailUtilities.ExpandAliases(body);
    EXCEPT MailUtilities.Error(errmsg) =>
      RAISE UnixMail.Error(errmsg);
    END;
    IF newBody # body THEN
      LOCK VBT.mu DO
        FormsVBT.PutText(this, "Draft", newBody);
        SetModified(this, TRUE);
      END;
    END;
  END ExpandAliases;

TYPE ComposeActionForkee = Thread.Closure OBJECT
      t: T;
      w: Window;
      eventName: TEXT;
    OVERRIDES
      apply := DoComposeAction;
    END;

VAR sendLock := NEW(Thread.Mutex);
  (* Protects relationship between compositions sub-windows and the backing
     files *)

PROCEDURE DoComposeSend(self: ComposeActionForkee) RAISES { UnixMail.Error } =
    (* LL = 0 *)
  BEGIN
    ExpandAliases(self.w);
    LOCK sendLock DO
      Save(self.w);
      UnixMail.SendMsg(self.w.number);
      LOCK VBT.mu DO SetNotInUse(self.w, "Sent") END;
    END;
  END DoComposeSend;

PROCEDURE DoComposeDiscard(self: ComposeActionForkee) RAISES { UnixMail.Error } =
    (* Discard built-in composition *)
    (* LL = 0 *)
  VAR modified: BOOLEAN;
  BEGIN
    LOCK sendLock DO
      LOCK VBT.mu DO modified := TextPort.IsModified(self.w.port) END;
      IF modified THEN Save(self.w) END;
      UnixMail.DiscardDraftFile(self.w.number);
      LOCK VBT.mu DO SetNotInUse(self.w, "Discarded") END;
    END;
  END DoComposeDiscard;

PROCEDURE DoComposeUseExternal(self: ComposeActionForkee)
        RAISES { UnixMail.Error } =
    (* Transfer built-in composition to an external editor window *)
    (* LL = 0 *)
  BEGIN
    LOCK sendLock DO Save(self.w); self.w.usingExternal := TRUE END;
    TRY
      LOCK VBT.mu DO
        FormsVBT.PutInteger(self.w, "ComposeTSplit", 2);
        FormsVBT.PutInteger(self.w, "DraftTSplit", 1);
      END;
      TRY
        UnixMail.RunFilterFile(UnixMail.DraftFileName(self.w.number),
                               self.t.cl.GetEditorFilter());
      EXCEPT Closure.BadFilter(errmsg) => RAISE UnixMail.Error(errmsg);
      END;
    FINALLY
      TRY
        LOCK sendLock DO
          self.w.usingExternal := FALSE;
          WITH mtime = UnixMail.DraftFileTime(self.w.number) DO
            WITH draft = UnixMail.ReadDraftFile(self.w.number) DO
              LOCK VBT.mu DO
                FormsVBT.PutText(self.w, "Draft", draft);
                self.w.backupTime := mtime;
              END;
            END;
          END;
        END;
      FINALLY
        LOCK VBT.mu DO
          FormsVBT.PutInteger(self.w, "ComposeTSplit", 0);
          FormsVBT.PutInteger(self.w, "DraftTSplit", 0);
          IF self.t.ownWindow # NIL THEN ShowWindow(self.t) END;
        END;
      END;
    END;
  END DoComposeUseExternal;

PROCEDURE DoComposeExpand(self: ComposeActionForkee)
        RAISES { UnixMail.Error } =
    (* LL = 0 *)
  BEGIN
    ExpandAliases(self.w);
  END DoComposeExpand;

PROCEDURE DoComposeInclude (self: ComposeActionForkee)
  RAISES {UnixMail.Error} =
  (* LL = 0 *)
  VAR te: TextEditVBT.T;
  BEGIN
    LOCK VBT.mu DO te := FormsVBT.GetVBT (self.w, "Draft") END;
    TRY
      self.t.cl.InsertSelectedMessages (te.tp);
    EXCEPT Closure.CantInsert(errmsg) => RAISE UnixMail.Error(errmsg);
    END;
    LOCK VBT.mu DO SetModified (self.w, TRUE) END;
  END DoComposeInclude;

PROCEDURE DoComposeIncludeFile(self: ComposeActionForkee): BOOLEAN
      RAISES {UnixMail.Error} =
  (* LL = 0 *)
  VAR
    te: TextEditVBT.T;
    body, fileName: TEXT;
    mtime: Time.T;
  BEGIN
    LOCK VBT.mu DO fileName := FormsVBT.GetText(self.w, "FileToInclude") END;
    TRY
      IF OSUtils.GetInfo(fileName, mtime) = OSUtils.FileType.Dir THEN
        LOCK VBT.mu DO FormsVBT.PutText(self.w, "FileToInclude", fileName) END;
        RETURN FALSE;
      END;
    EXCEPT OSUtils.FileNotFound, OSUtils.FileError =>
      (* Treat as file - we'll get the error message when reading it. *)
    END;
    body := UnixMail.ReadFileText(fileName);
    LOCK VBT.mu DO
      te := FormsVBT.GetVBT(self.w, "Draft");
      TextPort.Insert(te.tp, body);
      SetModified(self.w, TRUE);
    END;
    RETURN TRUE
  END DoComposeIncludeFile;

PROCEDURE DoComposeAction(self: ComposeActionForkee): REFANY =
  (* LL = 0 *)
  BEGIN
    TRY
      IF Text.Equal(self.eventName, "SendOpen") THEN
        DoComposeSend(self)
      ELSIF Text.Equal(self.eventName, "DiscardOpen") THEN
        DoComposeDiscard(self)
      ELSIF Text.Equal(self.eventName, "UseExternalOpen") THEN
        DoComposeUseExternal(self)
      ELSIF Text.Equal(self.eventName, "ExpandOpen") THEN
        DoComposeExpand(self)
      ELSIF Text.Equal(self.eventName, "IncludeOpen") THEN
        DoComposeInclude(self)
      ELSIF Text.Equal(self.eventName, "FileToInclude") OR
            Text.Equal(self.eventName, "IncludeFileButton") THEN
        IF NOT DoComposeIncludeFile(self) THEN RETURN NIL END
      ELSE
        <*ASSERT(FALSE)*>
      END;
    EXCEPT UnixMail.Error(msg) =>
      ReportError(self.t, msg);
    END;
    LOCK VBT.mu DO
      FormsVBT.MakeActive(self.w, "ComposeFilter");
      VBT.SetCursor(FormsVBT.GetVBT(self.w, "ComposeFilter"), Cursor.DontCare);
      FormsVBT.PopDown(self.w, "IncludeFileDlg")
    END;
    RETURN NIL
  END DoComposeAction;


(* *)
(* Implementing the buttons part 2: FormsVBT action procedures for sub-windows *)
(* *)

PROCEDURE OnComposeAction(fv: FormsVBT.T;
                          name: TEXT;
                          arg: REFANY;
                          <*UNUSED*> ticks: VBT.TimeStamp) =
  (* Forked actions *)
  (* LL = VBT.mu *)
  VAR self := NARROW(arg, T);
  BEGIN
    VBT.SetCursor(FormsVBT.GetVBT(fv, "ComposeFilter"), self.cl.waitCursor);
    FormsVBT.MakePassive (fv, "ComposeFilter");
    EVAL Thread.Fork(NEW(ComposeActionForkee, t := self, w := fv,
                             eventName := name))
  END OnComposeAction;

PROCEDURE OnComposeClose(fv: FormsVBT.T;
                         <*UNUSED*> name: TEXT;
                         arg: REFANY;
                         <*UNUSED*> ticks: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  <*FATAL MultiSplit.NotAChild*>
  VAR self := NARROW(arg, T);
  BEGIN
    MultiSplit.Delete(self.splitter, fv);
  END OnComposeClose;

PROCEDURE OnComposeReuse( fv: FormsVBT.T;
                         <*UNUSED*> name: TEXT;
                         arg: REFANY;
                         ticks: VBT.TimeStamp) =
  (* LL = VBT.mu *)
  VAR
    this : Window := fv;
    draft: TextEditVBT.T := FormsVBT.GetVBT(this, "Draft");
  VAR self := NARROW(arg, T);
  BEGIN
    FormsVBT.PutText(this, "ComposeStatus", "");
    FormsVBT.PutText(this, "Title", UnixMail.DraftFileName(this.number));
    draft.tp.setReadOnly(FALSE);
    this.inUse := TRUE;
    SetModified(this, TRUE);
    IF self.external THEN
      OnComposeAction(fv, "UseExternalOpen", arg, ticks);
    ELSE
      FormsVBT.PutInteger(this, "ComposeTSplit", 0);
    END;
  END OnComposeReuse;


(* *)
(* Implementing the buttons part 3: FormsVBT action procedures for main window *)
(* *)

PROCEDURE OnRescreen(<*UNUSED*> fv: FormsVBT.T;
                     <*UNUSED*> name: TEXT;
                     arg: REFANY;
                     <*UNUSED*> ticks: VBT.TimeStamp) =
  (* LL = VBT.mu *)
  VAR self := NARROW(arg, T);
  BEGIN
    Rescreen.DoIt("rescreen.fv", "Confirm", "Cancel", self.cl.path,
                  self.ownWindow);
  END OnRescreen;


(* *)
(* Subroutines for the "Compose" method *)
(* *)

PROCEDURE Insert(splitter: MultiSplit.T; this: Window) =
  (* LL = VBT.mu *)
  (* Insert sub-window into the splitter *)
  <*FATAL MultiSplit.NotAChild*>
  VAR ch: VBT.T;
  BEGIN
    (* Search for "this" in the split. *)
    ch := MultiSplit.Succ(splitter, NIL);
    WHILE (ch # NIL) AND (ch # this) DO
      ch := MultiSplit.Succ(splitter, ch);
    END;
    IF ch # this THEN
      (* It wasn't there, so insert it at the end (after the last child). *)
      MultiSplit.Insert(splitter, MultiSplit.Pred(splitter, NIL), this);
    END;
  END Insert;

PROCEDURE UseBuiltInCompose(self: T;
                            msgText: TEXT;
                            this: Window;) =
    (* LL = VBT.mu *)
    (* Show draft in draft window "this" *)
  BEGIN
    FormsVBT.PutText(this, "Draft", msgText);
    OnComposeReuse(this, NIL, self, 0);
    IF self.splitter # NIL THEN
      Insert(self.splitter, this);
      self.cl.SetFonts(this);
    END;
  END UseBuiltInCompose;

PROCEDURE JoinBuiltInComposeCreator(self: T; msgText: TEXT) =
    (* LL < VBT.mu *)
    (* Finish off creation of a composition window, including adding to list.
       If msgText is non-NIL, install new composition window as inUse with
       given text; otherwise, leave new composition window in unused state. *)
  VAR creator: Thread.T; this: Window;
  BEGIN
    LOCK VBT.mu DO
      creator := self.creator;
      self.creator := Thread.Fork(NEW(CreateForkee, t := self));
    END;
    this := Thread.Join(creator);
    LOCK VBT.mu DO
      INC(self.count);
      this.number := self.count;
      self.windows := RefList.Cons(this, self.windows);
      IF msgText # NIL THEN UseBuiltInCompose(self, msgText, this) END;
    END;
  END JoinBuiltInComposeCreator;


(* *)
(* Top-level methods *)
(* *)

PROCEDURE CheckDraftFiles(self: T) RAISES { UnixMail.Error } =
    (* LL = sendLock *)
  VAR
    oldDrafts: REF ARRAY OF CARDINAL;
    maxNew: CARDINAL;
    list: RefList.T;
    this: Window;
    msgText: TEXT;
    mtime: Time.T;
    found, added: BOOLEAN;
  BEGIN
    oldDrafts := UnixMail.GetOldDrafts();
    added := FALSE;
    (* For each existing draft file, ensure composition window is OK *)
    FOR i := 0 TO NUMBER(oldDrafts^)-1 DO
      mtime := UnixMail.DraftFileTime(oldDrafts^[i]);
      msgText := UnixMail.ReadDraftFile(oldDrafts^[i]);
      LOOP (* ensure sub-window for oldDrafts[i] has been created *)
        LOCK VBT.mu DO maxNew := self.count END;
        (* Locking: it's ok if maxNew increases here *)
        IF maxNew >= oldDrafts^[i] THEN EXIT END;
        JoinBuiltInComposeCreator(self, NIL);
      END;
      (* Locking: composition windows are never destroyed, so we know that
         the windows for oldDrafts[i] exists now. *)
      LOCK VBT.mu DO
        list := self.windows;
        LOOP
          <* ASSERT(list#NIL) *>
          this := list.head;
          IF this.number = oldDrafts^[i] THEN EXIT END;
          list := list.tail;
        END;
        IF mtime <= this.backupTime THEN
          (* current contents might be more recent than backup file *)
          msgText := FormsVBT.GetText(this, "Draft");
          mtime := this.backupTime;
        END;
        IF (NOT this.inUse) OR
               ((mtime > this.backupTime) AND
                 NOT TextPort.IsModified(this.port) AND
                 NOT this.usingExternal) THEN
          added := NOT this.inUse; (* so we know to open the separate window *)
          UseBuiltInCompose(self, msgText, this);
          SetModified(this, FALSE); (* so we don't just re-write the file *)
          this.backupTime := mtime; (* so we don't re-read the file later *)
        END;
      END;
    END;
    (* For each remaining composition window, consider setting not-in-use *)
    LOCK VBT.mu DO
      list := self.windows;
      WHILE list # NIL DO
        this := list.head;
        IF this.inUse THEN
          found := FALSE;
          FOR i := 0 TO NUMBER(oldDrafts^)-1 DO
            IF oldDrafts[i] = this.number THEN found := TRUE; EXIT END;
          END;
          IF NOT found AND
               NOT TextPort.IsModified(this.port) AND
               NOT this.usingExternal THEN
            SetNotInUse(this, "Gone");
          END;
        END;
        list := list.tail;
      END;
    END;
    (* If appropriate, ensure separate composition window is visible *)
    IF added THEN
      LOCK VBT.mu DO
        IF self.ownWindow # NIL THEN ShowWindow(self) END;
      END;
    END;
  END CheckDraftFiles;

TYPE CheckerForkee = Thread.Closure OBJECT
      t: T;
    OVERRIDES
      apply := DoDraftChecker;
    END;

PROCEDURE DoDraftChecker(self: CheckerForkee): REFANY =
  BEGIN
    LOOP
      Thread.Pause(30.0d0);
      LOCK sendLock DO
        TRY CheckDraftFiles(self.t) EXCEPT UnixMail.Error => (*ignore*) END;
      END;
    END;
 END DoDraftChecker;

PROCEDURE Init(self: T; cl: Closure.T): T =
  BEGIN
    self.cl := cl;
    self.splitter := NIL;
    self.ownWindow := NIL;
    self.external := FALSE;
    TRY
      self.template := Rsrc.Get("builtInCompose.fv", cl.path);
      self.windowTemplate := Rsrc.Get("composeWindow.fv", cl.path);
    EXCEPT Rd.Failure, Rsrc.NotFound =>
      Wr.PutText(Stdio.stderr, "Can\'t read composition sub-window form\n");
      Process.Exit(30);
    END;
    LOCK VBT.mu DO
      self.creator := Thread.Fork(NEW(CreateForkee, t := self));
      self.windows := NIL;
      self.count := 0;
    END;
    LOCK sendLock DO
      TRY CheckDraftFiles(self)
      EXCEPT UnixMail.Error(msg) =>
        ReportError(self, msg);
      END;
    END;
    EVAL(Thread.Fork(NEW(CheckerForkee, t := self)));
    RETURN self
  END Init;

PROCEDURE Compose(self: T; to, cc, fcc, subject, inReplyTo, body: TEXT) =
    (* LL < VBT.mu *)
  VAR list: RefList.T;
  VAR this, best: Window;
  VAR msgText: TEXT;
  BEGIN
    msgText := MailUtilities.NewMessage(to, cc, fcc, subject, inReplyTo, body);
    LOCK sendLock DO
      TRY
        CheckDraftFiles(self);
        LOCK VBT.mu DO
          list := self.windows;
          best := NIL;
          WHILE list # NIL DO
            this := NARROW(list.head, Window);
            IF NOT this.inUse THEN
              IF (best = NIL) OR (this.number < best.number) THEN
                best := this;
              END;
            END;
            list := list.tail;
          END;
          IF best # NIL THEN
            UseBuiltInCompose(self, msgText, best);
          END;
        END;
        IF best = NIL THEN JoinBuiltInComposeCreator(self, msgText) END;
        LOCK VBT.mu DO
          IF (NOT self.external) AND (self.ownWindow # NIL) THEN
            ShowWindow(self);
          END;
        END;
      EXCEPT UnixMail.Error(msg) =>
        (* If we can't enumerate the drafts, better not assign new number *)
        ReportError(self, msg);
      END;
    END;
  END Compose;

PROCEDURE SetSplitter(self: T; splitter: MultiSplit.T; doCompose: FormsVBT.Proc;
                      near: VBT.T) =
    (* LL = VBT.mu *)
  <*FATAL MultiSplit.NotAChild*>
  VAR list: RefList.T;
  VAR this: Window;
  VAR newSplitter: MultiSplit.T;
  VAR showIt := FALSE;
  VAR prev, v: VBT.T;
  BEGIN
    self.near := near;
    IF ((splitter # NIL) AND (splitter # self.splitter)) OR
       ((splitter = NIL) AND (self.ownWindow = NIL)) THEN
      (* need to place sub-windows in new splitter, perhaps creating it first *)
      IF splitter # NIL THEN
        newSplitter := splitter;
      ELSE
        (* splitter = NIL and self.ownWindow = NIL  *)
        TRY
          self.ownFV := NEW(FormsVBT.T).init(self.windowTemplate, FALSE,
                                             self.cl.path);
          FormsVBT.AttachProc(self.ownFV, "Compose", doCompose, self.cl);
          FormsVBT.AttachProc(self.ownFV, "Rescreen", OnRescreen, self);
          newSplitter := FormsVBT.GetVBT(self.ownFV, "Splitter");
          self.ownWindow := StableVBT.New(self.ownFV);
          WITH sh = VBTClass.GetShapes(near) DO
            StableVBT.SetShape(self.ownWindow, sh[Axis.T.Hor].pref, 0);
          END;
          VBT.SetCursor(self.ownWindow, Cursor.TextPointer);
        EXCEPT
          | FormsVBT.Error(t) =>
              Wr.PutText(Stdio.stderr,
                         "Can\'t read composition window form: " & t & "\n");
              Process.Exit(40);
        END;
      END;
      IF self.splitter = NIL THEN
        (* No previous display - insert "inUse" sub-windows into newSplitter *)
        list := self.windows;
        WHILE list # NIL DO
          this := list.head;
          IF this.inUse THEN
            MultiSplit.Insert(newSplitter, MultiSplit.Pred(newSplitter, NIL), this);
            showIt := TRUE;
          END;
          list := list.tail;
        END;
      ELSE
        (* transfer our sub-windows from previous splitter to newSplitter *)
        prev := NIL;
        LOOP
          v := MultiSplit.Succ(self.splitter, prev);
          IF v = NIL THEN EXIT END;
          IF ISTYPE(v, Window) THEN
            MultiSplit.Delete(self.splitter, v);
            MultiSplit.Insert(newSplitter, MultiSplit.Pred(newSplitter, NIL), v);
            showIt := TRUE;
          ELSE
            prev := v;
          END;
        END;
      END;
      IF splitter # NIL THEN
        IF self.ownWindow # NIL THEN
          Trestle.Delete(self.ownWindow);
          self.ownWindow := NIL;
        END
      ELSE
        IF showIt THEN ShowWindow(self) END;
      END;
      self.splitter := newSplitter;
    END;
  END SetSplitter;

PROCEDURE SetExternal(self: T; external: BOOLEAN) =
  BEGIN
    self.external := external;
  END SetExternal;

BEGIN
END BuiltInCompose.
