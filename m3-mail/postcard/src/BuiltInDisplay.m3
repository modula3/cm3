(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Postcard - UI for built-in display *)

(* Last modified on Thu Apr 21 13:14:18 PDT 1994 by birrell *)
(*      modified on Thu Feb 18 17:28:14 PST 1993 by meehan  *)

MODULE BuiltInDisplay EXPORTS BuiltInDisplay;

IMPORT Closure, Cursor, FormsVBT, MiscUtils, MultiSplit, Rd, Rsrc, Process, Stdio, Text,
       TextEditVBT, TextPort, Thread, UnixMail, VBT, Wr;

TYPE
  Window = FormsVBT.T OBJECT
      mu: MUTEX;
   (* Fields protected by VBT.mu ... *)
      header, brief, body, title: TEXT;
   (* Fields protected by mu ... *)
      statusLineThread: Thread.T := NIL; (* for resetting status line *)
    END;

REVEAL
  T = Public BRANDED OBJECT
    mu: MUTEX;                          (* Used in "Display" method, q.v. *)
   (* Read-only fields ... *)
      cl: Closure.T;                    (* for callbacks to main form *)
      splitter: MultiSplit.T;                (* where to display the windows *)
      template: TEXT;                   (* form source *)
   (* Fields protected by VBT.mu ... *)
      active: Window := NIL;            (* form affected by "Display", or NIL *)
      creator: Thread.T;                (* forkee parsing form *)
    OVERRIDES
      Init := Init;
      Display := Display;
  END;

<* FATAL FormsVBT.Error *>
<* FATAL FormsVBT.Unimplemented *>
<* FATAL Thread.Alerted *>
<* FATAL Wr.Failure *>


(* *)
(* Creating the form *)
(* *)

TYPE CreateForkee = Thread.Closure OBJECT
      t: T;
    OVERRIDES
      apply := Create
    END;

PROCEDURE Create(self: CreateForkee): REFANY =
    (* LL = 0 *)
  VAR
    this := NEW(Window, mu := NEW(MUTEX)).init(
                                         self.t.template, TRUE, self.t.cl.path);
  BEGIN
    LOCK VBT.mu DO self.t.cl.SetTextPortColors(this, "Display") END;
    RETURN this
  END Create;


(* *)
(* Status line *)
(* *)

TYPE StatusLineForkee = Thread.Closure OBJECT
      w: Window;
    OVERRIDES
      apply := StatusLineThread
    END;

PROCEDURE StatusLineThread(self: StatusLineForkee): REFANY =
    (* Resets status line after a delay *)
    (* LL = 0 *)
  BEGIN
    TRY
      Thread.AlertPause(5.0d0);
      LOCK VBT.mu DO
        ResetStatusLine(self.w);
      END;
    EXCEPT Thread.Alerted =>
    END;
    RETURN NIL
  END StatusLineThread;

PROCEDURE ResetStatusLine(self: Window) =
    (* LL = VBT.mu *)
  BEGIN
    FormsVBT.PutText(self, "Title", self.title);
  END ResetStatusLine;

PROCEDURE SetStatusLine(self: Window; status: TEXT) =
    (* LL = 0 *)
  BEGIN
    LOCK self.mu DO
      IF self.statusLineThread # NIL THEN
        Thread.Alert(self.statusLineThread);
        EVAL Thread.Join(self.statusLineThread);
        self.statusLineThread := NIL;
      END;
      self.statusLineThread := Thread.Fork(NEW(StatusLineForkee, w := self));
      LOCK VBT.mu DO
        FormsVBT.PutText(self, "Title", status);
      END;
    END;
  END SetStatusLine;


(* *)
(* Implementing the buttons part 1: forked buttons. *)
(* *)

TYPE DisplayForkee = Thread.Closure OBJECT
      (* Closure used for forked actions from built-in display buttons *)
      (* All data is in this closure; the forked actions don't use the window
         except to update the status line *)
      t: T;
      w: Window;
      eventName: TEXT;
      pathName: TEXT;
      body: TEXT;
    OVERRIDES
      apply := DoDisplayAction;
    END;

PROCEDURE DoExternalDisplay(self: DisplayForkee)
        RAISES { UnixMail.Error } =
    (* LL = 0 *)
  VAR filter: TEXT;
  BEGIN
    TRY
      filter := self.t.cl.GetEditorFilter();
    EXCEPT Closure.BadFilter(errmsg) => RAISE UnixMail.Error(errmsg);
    END;
    SetStatusLine(self.w, "Running editor: " & filter);
    UnixMail.RunFilterFile(self.pathName, filter);
  END DoExternalDisplay;

PROCEDURE DoPostScript(self: DisplayForkee; filter, status: TEXT)
                       RAISES { UnixMail.Error } =
    (* LL = 0 *)
  VAR start := MiscUtils.Find(self.body, 0, "\n%!");
  BEGIN
    IF start < 0 THEN
      RAISE UnixMail.Error("I can't find any imbedded PostScript - there's " &
                           "no line beginning with \"%!\".");
    END;
    SetStatusLine(self.w, status & filter);
    UnixMail.RunFilterText(self.body, start+1, filter);
  END DoPostScript;

PROCEDURE DoPSView(self: DisplayForkee) RAISES { UnixMail.Error } =
    (* LL = 0 *)
  BEGIN
    DoPostScript(self, self.t.cl.GetPSViewFilter(), "Previewing via: ");
  END DoPSView;

PROCEDURE DoPrintDisplay(self: DisplayForkee) RAISES { UnixMail.Error } =
    (* LL = 0 *)
  VAR filter := self.t.cl.GetPrintFilter();
  BEGIN
    SetStatusLine(self.w, "Printing via: " & filter);
    UnixMail.RunFilterFile(self.pathName, filter);
  END DoPrintDisplay;

PROCEDURE DoPSPrint(self: DisplayForkee) RAISES { UnixMail.Error } =
    (* LL = 0 *)
  BEGIN
    DoPostScript(self, self.t.cl.GetPSPrintFilter(), "Printing PS via: ");
  END DoPSPrint;

PROCEDURE DoDisplayAction(self: DisplayForkee): REFANY =
    (* Dispatcher for forked buttons. We aren't synchronized to access window *)
    (* LL = 0 *)
  BEGIN
    TRY
      IF Text.Equal(self.eventName, "UseExternalOpen") THEN
        DoExternalDisplay(self)
      ELSIF Text.Equal(self.eventName, "UsePostScriptOpen") THEN
        DoPSView(self)
      ELSIF Text.Equal(self.eventName, "PrintOpen") THEN
        DoPrintDisplay(self)
      ELSIF Text.Equal(self.eventName, "PrintPostScriptOpen") THEN
        DoPSPrint(self)
      ELSE
        <*ASSERT(FALSE)*>
      END;
    EXCEPT UnixMail.Error(errmsg) =>
      self.t.cl.AsyncErrorHandler(errmsg);
    END;
    RETURN NIL
  END DoDisplayAction;


(* *)
(* Implementing the buttons part 2: FormsVBT action procedures *)
(* *)

PROCEDURE OnDisplayClose (           fv   : FormsVBT.T;
                          <*UNUSED*> name : TEXT;
                                     arg  : REFANY;
                          <*UNUSED*> ticks: VBT.TimeStamp) =
  (* LL = VBT.mu *)
  <* FATAL MultiSplit.NotAChild *>
  VAR self: T := arg;
  BEGIN
    MultiSplit.Delete (self.splitter, fv);
  END OnDisplayClose;

PROCEDURE OnDisplayDetach(fv: FormsVBT.T;
                         <*UNUSED*> name: TEXT;
                         arg: REFANY;
                         <*UNUSED*> ticks: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  VAR self := NARROW(arg, T);
  BEGIN
    self.active := NIL;
    self.creator := Thread.Fork(NEW(CreateForkee, t := self));
    FormsVBT.PutInteger(fv, "DetachTSplit", 1);
  END OnDisplayDetach;

PROCEDURE DoShowEither (w: Window; n: INTEGER) =
  VAR t: TEXT;
  BEGIN
    IF n = 0 THEN t := w.brief; ELSE t := w.header; END;
    FormsVBT.PutText (w, "Display", t);
    WITH tp = NARROW (FormsVBT.GetVBT (w, "Display"), TextEditVBT.T).tp DO
      TextPort.Insert (tp, w.body);
    END;
    FormsVBT.PutInteger (w, "HeaderTSplit", n);
  END DoShowEither;

PROCEDURE OnShowEntire(fv: FormsVBT.T;
                      <*UNUSED*> name: TEXT;
                      <*UNUSED*> arg: REFANY;
                      <*UNUSED*> ticks: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  BEGIN
    DoShowEither(fv, 1);
  END OnShowEntire;

PROCEDURE OnShowBrief(fv: FormsVBT.T;
                      <*UNUSED*> name: TEXT;
                      <*UNUSED*> arg: REFANY;
                      <*UNUSED*> ticks: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  BEGIN
    DoShowEither(fv, 0);
  END OnShowBrief;

PROCEDURE OnDisplayAction(fv: FormsVBT.T;
                          name: TEXT;
                          arg: REFANY;
                          <*UNUSED*> ticks: VBT.TimeStamp) =
    (* Event procedure for built-in display buttons that fork actions *)
    (* LL = VBT.mu *)
  VAR self := NARROW(arg, T); w: Window := fv;
  BEGIN
    EVAL Thread.Fork(NEW(DisplayForkee, t := self, w := w, eventName := name,
                         pathName := w.title,
                         body := w.body));
  END OnDisplayAction;



(* *)
(* Top-level methods *)
(* *)

PROCEDURE Init(self: T; cl: Closure.T; splitter: MultiSplit.T): T =
  BEGIN
    self.mu := NEW(MUTEX);
    self.cl := cl;
    self.splitter := splitter;
    TRY
      self.template := Rsrc.Get("builtInDisplay.fv", cl.path);
    EXCEPT Rd.Failure, Rsrc.NotFound =>
      Wr.PutText(Stdio.stderr, "Can\'t read display window form\n");
      Process.Exit(20);
    END;
    LOCK VBT.mu DO
      self.creator := Thread.Fork(NEW(CreateForkee, t := self));
      self.active := NIL;
    END;
    (* Add one window to the split early, to reduce flicker *)
    self.Display("", "");
    RETURN self
  END Init;

PROCEDURE Display (self: T; folder, messageID: TEXT) =
  (* LL = 0 *)
  VAR myCreator: Thread.T;
  VAR myFV: Window;
  VAR header, brief, body, msgPath := "";
  BEGIN
    IF NOT Text.Empty (folder) THEN
      TRY
        UnixMail.GetMsg (folder, messageID, header, brief, body);
        msgPath := UnixMail.MsgPath (folder, messageID);
      EXCEPT
        UnixMail.Error (t) => self.cl.AsyncErrorHandler (t);
      END;
    END;
    LOCK self.mu DO
      (* Holding self.mu here ensures atomicity of installing a new .active *)
      LOCK VBT.mu DO myCreator := self.creator; self.creator := NIL; END;
      IF myCreator # NIL THEN
        (* Don't hold VBT.mu during the Join, which might access files *)
        myFV := Thread.Join(myCreator);
        LOCK VBT.mu DO
          self.active := myFV;
          FormsVBT.AttachProc (myFV, "CloseOpen", OnDisplayClose, self);
          FormsVBT.AttachProc (myFV, "DetachOpen", OnDisplayDetach, self);
          FormsVBT.AttachProc (myFV, "ShowEntire", OnShowEntire, self);
          FormsVBT.AttachProc (myFV, "ShowBrief", OnShowBrief, self);
          FormsVBT.AttachProc (myFV, "UseExternalOpen", OnDisplayAction, self);
          FormsVBT.AttachProc (
            myFV, "UsePostScriptOpen", OnDisplayAction, self);
          FormsVBT.AttachProc (myFV, "PrintOpen", OnDisplayAction, self);
          FormsVBT.AttachProc (
            myFV, "PrintPostScriptOpen", OnDisplayAction, self);
          FormsVBT.AttachEditOps (myFV, "Display", copy := "Copy", selectAll := "SelectAll");
          VBT.SetCursor(myFV, Cursor.TextPointer);
        END;
      END;
      LOCK VBT.mu DO
        self.active.header := header;
        self.active.brief := brief;
        self.active.body := body;
        self.active.title := msgPath;
        ResetStatusLine (self.active);
        DoShowEither (self.active, 0);
        <*FATAL MultiSplit.NotAChild*>
        BEGIN
          IF MultiSplit.Succ(self.splitter, NIL) # self.active THEN
            self.cl.SetFonts(self.active);
            MultiSplit.Insert(self.splitter, NIL, self.active);
          END;
        END;
      END;
    END;
  END Display;

BEGIN
END BuiltInDisplay.
