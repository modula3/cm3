(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Postcard - UI for built-in display *)

(* Last modified on Tue Nov 14 10:41:34 PST 1995 by mhb     *)
(*      modified on Thu Apr 21 13:14:18 PDT 1994 by birrell *)
(*      modified on Thu Feb 18 17:28:14 PST 1993 by meehan  *)

MODULE BuiltInDisplay EXPORTS BuiltInDisplay;

IMPORT Closure, Cursor, FormsVBT, MailUtilities, MiscUtils,
       MultiSplit, Rd, Rsrc, Process, Stdio, Text, TextRd, TextEditVBT,
       TextPort, Thread, UnixMail, VBT, Wr;
IMPORT Env, Fmt, FmtTime, FileWr, HTML, TextList, Time, Web, URLCache, WebBrowserVBT;
IMPORT Lex, TextPortButton, TextPortWithButtons;


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
   (* most recent call to Display; protected by mu *) 
      folder, messageID: TEXT;
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

PROCEDURE Create (self: CreateForkee): REFANY =
  (* LL <= VBT.mu *)
  VAR
    this := NEW(Window, mu := NEW(MUTEX)).init(
              self.t.template, TRUE, self.t.cl.path);
  VAR
    tp := NEW(TextPortWithButtons.T).init(readOnly := TRUE);
    te := NEW(TextEditVBT.T);
  BEGIN
    te.tp := tp;
    EVAL TextEditVBT.T.init(te);
    FormsVBT.PutGeneric(this, "Display", te);
    self.t.cl.SetTextEditColors(te);

    WITH webvbt = NEW(MyWebVBT).init() DO
      FormsVBT.PutGeneric(this, "DisplayGen", webvbt);
    END;


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

PROCEDURE OnWBDisplayClose (           fv   : FormsVBT.T;
                            <*UNUSED*> name : TEXT;
                                       arg  : REFANY;
                            <*UNUSED*> ticks: VBT.TimeStamp) =
  (* LL = VBT.mu *)
  <* FATAL MultiSplit.NotAChild *>
  VAR
    webvbt: MyWebVBT := fv;
    self             := webvbt.self;
  BEGIN
    MultiSplit.Delete(self.splitter, arg);
  END OnWBDisplayClose;

PROCEDURE OnWBDisplayDetach (           fv   : FormsVBT.T;
                             <*UNUSED*> name : TEXT;
                                        arg  : REFANY;
                             <*UNUSED*> ticks: VBT.TimeStamp) =
  (* LL = VBT.mu *)
  VAR
    webvbt: MyWebVBT := fv;
    self             := webvbt.self;
  BEGIN
    self.active := NIL;
    self.creator := Thread.Fork(NEW(CreateForkee, t := self));
    FormsVBT.PutInteger(webvbt, "DetachTSplit", 1);
    DisplayL(self)
  END OnWBDisplayDetach;

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
    DisplayL(self)
  END OnDisplayDetach;

PROCEDURE OnWWW (           fv   : FormsVBT.T;
                 <*UNUSED*> name : TEXT;
                            arg  : REFANY;
                 <*UNUSED*> ticks: VBT.TimeStamp) =
  (* LL = VBT.mu *)
  BEGIN
    ShowURL(Env.Get("WWW_HOME"), fv, arg)
  END OnWWW;


PROCEDURE DoShowEither (w: Window; n: INTEGER; self: T) =
  VAR url: TEXT;
  BEGIN
    IF IsMsgURL(w, url) THEN
      ShowURL(url, w, self)
    ELSE
      ShowMsg(w, n, self)
    END
  END DoShowEither;

PROCEDURE ShowURL (url: TEXT; w: Window; self: T) =
  VAR webvbt: MyWebVBT := FormsVBT.GetGeneric(w, "DisplayGen");
  BEGIN
    webvbt.w := w;
    webvbt.self := self;
    webvbt.visit(url);
    FormsVBT.PutInteger(w, "DisplayTSplit", 1);
  END ShowURL;

PROCEDURE ShowMsg (w: Window; n: INTEGER; self: T) =
  VAR
    tp: TextPortWithButtons.T := NARROW(FormsVBT.GetGeneric(w, "Display"),
                                        TextEditVBT.T).tp;
  BEGIN
    PROCEDURE Insert (text: TEXT) =
      BEGIN
        IF MiscUtils.Find(text, 0, "\n%!", FALSE) >= 0
             OR MiscUtils.Find(text, 0, "http://", TRUE) < 0 THEN
          (* either it's PostScript or contains no URLs *)
          TextPort.Insert(tp, text)
        ELSE
          InsertTextWithURLs(self, tp, text)
        END
      END Insert;
    BEGIN
      FormsVBT.PutInteger(w, "DisplayTSplit", 0);
      TextPort.SetText(tp, "");
      tp.clearButtons();
      IF n = 0 THEN Insert(w.brief) ELSE Insert(w.header) END;
      Insert(w.body);
    END;
    FormsVBT.PutInteger(w, "HeaderTSplit", n);
  END ShowMsg;

PROCEDURE InsertTextWithURLs (self: T; tp: TextPortWithButtons.T; text: TEXT) =
  CONST URLChars = Lex.NonBlanks - SET OF CHAR{'>', '"'};
  VAR rd := TextRd.New(text); url: TEXT; curr, next: INTEGER;
  BEGIN
    curr := 0;
    LOOP
      next := MiscUtils.Find(text, curr, "http://", TRUE);
      IF next < 0 THEN 
        TextPort.Insert(tp, Text.Sub(text, curr, LAST(CARDINAL)));
        EXIT;
      ELSE
        TextPort.Insert(tp, Text.Sub(text, curr, next-curr));
        Rd.Seek(rd, next+7);   
        url := Lex.Scan(rd, URLChars);
        curr := Rd.Index(rd);
        WITH button = NEW(URLButton, self:=self, w:=self.active, label:="http://"&url) DO
          tp.insertButton(button) 
        END
      END
    END;
    Rd.Close(rd);
  END InsertTextWithURLs;

<* UNUSED *> PROCEDURE Suffix (t, suffix: TEXT): BOOLEAN =
  VAR
    len       := Text.Length(t);
    suffixLen := Text.Length(suffix);
  BEGIN
    RETURN
      len >= suffixLen
        AND Text.Equal(Text.Sub(t, len - suffixLen, suffixLen), suffix)
  END Suffix;

TYPE
 URLButton = TextPortButton.T OBJECT
    self: T; 
    w: Window;
  OVERRIDES
    callback := URLButtonCallback;
  END;

PROCEDURE URLButtonCallback (button: URLButton; READONLY cd: VBT.MouseRec) =
  VAR w := button.self.active;
  BEGIN
    IF w = NIL THEN w := button.w END;
    ShowURL(button.label, w, button.self);
  END URLButtonCallback;

PROCEDURE IsMsgURL (w: Window; VAR url: TEXT): BOOLEAN =
  BEGIN
    IF NOT Text.Empty(w.header) THEN
      url := MailUtilities.LTrim(
               MailUtilities.GetFieldValue(w.header, "X-Url"));
      IF Text.Length(url) > 0 THEN
        IF Text.Empty(w.body) THEN RETURN TRUE END;
        WITH rd = TextRd.New(w.body) DO
          Lex.Skip(rd, Lex.Blanks + SET OF CHAR{'-'});
          RETURN Rd.EOF(rd)
        END
      END
    END;
    RETURN FALSE
  END IsMsgURL;

PROCEDURE OnShowEntire(fv: FormsVBT.T;
                      <*UNUSED*> name: TEXT;
                      <*UNUSED*> arg: REFANY;
                      <*UNUSED*> ticks: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  BEGIN
    DoShowEither(fv, 1, arg);
  END OnShowEntire;

PROCEDURE OnShowBrief(fv: FormsVBT.T;
                      <*UNUSED*> name: TEXT;
                      <*UNUSED*> arg: REFANY;
                      <*UNUSED*> ticks: VBT.TimeStamp) =
    (* LL = VBT.mu *)
  BEGIN
    DoShowEither(fv, 0, arg);
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
    LOCK self.mu DO
       self.folder := folder; self.messageID := messageID
    END;
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
          
          (*
          FormsVBT.AttachProc (myFV, "DisplayURL", OnDisplayURL, self);
          FormsVBT.AttachProc (myFV, "ExpandURL", OnExpandURL, self);
          *)
          FormsVBT.AttachProc (myFV, "WWW", OnWWW, self);

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
           (*
          FormsVBT.AttachEditOps (myFV, "Display", copy := "Copy", selectAll := "SelectAll");
          *)
          VBT.SetCursor(myFV, Cursor.TextPointer);

          WITH webvbt = FormsVBT.GetGeneric(self.active, "DisplayGen") DO
            (* sleazy: using same names in two forms *)
            FormsVBT.AttachProc(webvbt, "CloseOpen", OnWBDisplayClose, self.active);
            FormsVBT.AttachProc(webvbt, "DetachOpen", OnWBDisplayDetach, NIL);
          END;


        END;
      END;
      LOCK VBT.mu DO

        self.active.header := header;
        self.active.brief := brief;
        self.active.body := body;
        self.active.title := msgPath;
        ResetStatusLine (self.active);
        DoShowEither (self.active, 0, self);
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

PROCEDURE DisplayL (self: T) =
  (* Just like Display, but LL=VBT.mu rather than LL=0 and uses self.folder, self.messageID *)
  VAR myCreator: Thread.T;
  VAR myFV: Window;
  VAR header, brief, body, msgPath := ""; 
  VAR folder, messageID: TEXT;
  BEGIN
    LOCK self.mu DO
      folder := self.folder; messageID := self.messageID;
    END;
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
      (* LOCK VBT.mu DO *) myCreator := self.creator; self.creator := NIL; (* END; *)
      IF myCreator # NIL THEN
        (* Don't hold VBT.mu during the Join, which might access files *)
        myFV := Thread.Join(myCreator);
        (* LOCK VBT.mu DO *)
          self.active := myFV;

          FormsVBT.AttachProc (myFV, "WWW", OnWWW, self);

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
           (*
          FormsVBT.AttachEditOps (myFV, "Display", copy := "Copy", selectAll := "SelectAll");
          *)
          VBT.SetCursor(myFV, Cursor.TextPointer);

          WITH webvbt = FormsVBT.GetGeneric(self.active, "DisplayGen") DO
            (* sleazy: using same names in two forms *)
            FormsVBT.AttachProc(webvbt, "CloseOpen", OnWBDisplayClose, self.active);
            FormsVBT.AttachProc(webvbt, "DetachOpen", OnWBDisplayDetach, NIL);
          END;

        (* END; *)
      END;
      (* LOCK VBT.mu DO *)
        self.active.header := header;
        self.active.brief := brief;
        self.active.body := body;
        self.active.title := msgPath;
        ResetStatusLine (self.active);
        DoShowEither (self.active, 0, self);
        <*FATAL MultiSplit.NotAChild*>
        BEGIN
          IF MultiSplit.Succ(self.splitter, NIL) # self.active THEN
            self.cl.SetFonts(self.active);
            MultiSplit.Insert(self.splitter, NIL, self.active);
          END;
        END;
      (* END; *)
    END;
  END DisplayL;

(*
PROCEDURE OnDisplayURL (           fv   : FormsVBT.T;
                        <*UNUSED*> name : TEXT;
                                   arg  : REFANY;
                                   ticks: VBT.TimeStamp) =
  (* LL = VBT.mu *)
  <* FATAL MultiSplit.NotAChild *>
  VAR
    w   : Window := fv;
    self: T      := arg;
    url : TEXT;
  BEGIN
    TYPECASE VBT.Read(fv, VBT.Source, ticks).toRef() OF
    | NULL => RAISE VBT.Error(VBT.ErrorCode.WrongType)
    | TEXT (t) => url := t
    ELSE
      url := "oops";
    END;
    ShowURL(url, w, self);
  END OnDisplayURL;
*)

TYPE MyWebVBT = WebBrowserVBT.T OBJECT
   self: T;
   w: Window;
  OVERRIDES
    hotlink := Hotlink;
    surf := Surf;
  END;

PROCEDURE Hotlink (         web           : MyWebVBT;
                            destinationURL: TEXT;
                   READONLY cd            : VBT.MouseRec) =
  VAR w := web.self.active;
  BEGIN
    IF w = NIL THEN
      web.visit(destinationURL)
    ELSE
      ShowURL(destinationURL, w, web.self)
    END
  END Hotlink;

CONST Folder = "--Surf--";

PROCEDURE Surf (web: MyWebVBT; base: TEXT; links: TextList.T) =
  VAR ct: INTEGER;
  BEGIN
    IF FormsVBT.GetInteger(web.w, "DisplayTSplit") = 1 THEN
      TRY
        UnixMail.PurgeFolder(Folder, Fmt.Int(10000));
      EXCEPT
        UnixMail.Error =>
      END;
      ct := 1;
      WHILE links # NIL DO
        WITH abs = Web.AbsoluteURL(links.head, base) DO
          IF Text.Equal(Text.Sub(abs, Text.Length(abs)-5), ".html") THEN
            Fetch(ct, Web.AbsoluteURL(links.head, base));
            INC(ct);
          END
        END;
        links := links.tail;
      END;
      web.self.cl.invokeOpenWebFolder(0);
    END;
  END Surf;


VAR 
  date := "Date: " & FmtTime.Long(Time.Now()) & "\n";

PROCEDURE Fetch (id: INTEGER; url: TEXT) =
  VAR
    path := UnixMail.MsgPath(Folder, Fmt.Int(id));
    wr   := FileWr.Open(path);
  BEGIN
    Wr.PutText(wr, "From: " & "..." & GetTail(url) & "\n");
    Wr.PutText(wr, "Subject: " & GetSubject(url) & "\n");
    Wr.PutText(wr, "X-URL: " & url & "\n");
    Wr.PutText(wr, date);
    Wr.PutText(wr, "------\n\n");
    Wr.Close(wr);
  END Fetch;

PROCEDURE GetProtocol (url: TEXT): TEXT =
  VAR prefix := Text.FindChar(url, '/');
  BEGIN
    IF prefix = -1 THEN RETURN "" ELSE RETURN Text.Sub(url, 0, prefix) END
  END GetProtocol;

PROCEDURE GetTail (url: TEXT): TEXT =
  VAR suffix := Text.FindCharR(url, '/');
  BEGIN
    IF suffix = -1 THEN RETURN url ELSE RETURN Text.Sub(url, suffix) END
  END GetTail;

PROCEDURE GetSubject (url: TEXT): TEXT =
  VAR html: HTML.T;
  BEGIN
    IF URLCache.GetHTML(url, html) THEN
      RETURN html.title
    ELSE
      RETURN url
    END
  END GetSubject;

BEGIN
END BuiltInDisplay.

