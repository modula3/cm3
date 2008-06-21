MODULE Builder;

IMPORT AtomList, FmtTime, IntRefTbl, Pathname;
IMPORT Quake, QMachine, QValue, Text, Thread, Time, Wr;
IMPORT ConfigItem, BrowserDB, BuildCache, Default, ErrLog, HTML, ID;
IMPORT LineWr, Node, Pkg, Text2, Wx;

TYPE
  CI = ConfigItem.T;

TYPE
  State = REF RECORD
    proc       : CI;
    cmd        : TEXT;
    arg1, arg2 : TEXT;
    root       : Pkg.T;
    active     : Port;
    attaching  : Port;
    buffer     : Port;
    buf_wx     : Wx.T;
    cache      : Node.T;
    mu         : MUTEX;
    changed    : Thread.Condition;
    n_live     : INTEGER := 0;  (* # of active ports with non-NIL wxs *)
    done       : BOOLEAN := FALSE;
    abort      : BOOLEAN := FALSE;
    key        : ID.T := ID.NoID;
  END;

TYPE
  Port = REF RECORD
    next      : Port       := NIL;
    wx        : Wx.T       := NIL;
    wr_error  : AtomList.T := NIL;
  END;

PROCEDURE NewState (ci: CI;  arg1, arg2: TEXT;  root: Pkg.T;  wx: Wx.T): State =
  VAR s := NEW (State);
  BEGIN
    s.proc      := ci;
    s.cmd       := ConfigItem.Desc[ci].name;
    s.arg1      := arg1;
    s.arg2      := arg2;
    s.root      := root;
    s.active    := NEW (Port, wx := wx);
    s.attaching := NIL;
    s.buffer    := NIL;
    s.buf_wx    := NIL;
    s.cache     := NIL;
    s.mu        := NIL;
    s.changed   := NIL;
    s.n_live    := 1;
    s.done      := FALSE;
    s.abort     := FALSE;
    s.key       := ID.NoID;
    RETURN s;
  END NewState;

VAR
  ongoing_mu := NEW (MUTEX);
  ongoing    := NEW (IntRefTbl.Default).init ();

PROCEDURE Build (root: Pkg.T;  pkg_dir: Pathname.T;  args: TEXT;  wx: Wx.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    key := ID.Add (Node.FullPath (root));
    ref: REFANY;
    cl: BuildClosure;
    self: Port;
    buf_wx: Wx.T;
  BEGIN
    LOCK ongoing_mu DO
      IF ongoing.get (key, ref) THEN
        (* join an existing build by listening in *)
        cl := ref;
        self := NEW (Port, wx := wx, next := cl.s.attaching);
        cl.s.attaching := self;
      ELSE
        (* start a new build *)
        cl           := NEW (BuildClosure);
        cl.s         := NewState (CI.Build_package, pkg_dir, args, root, wx);
        cl.s.cache   := BuildCache.New (root, pkg_dir);
        cl.s.mu      := NEW (MUTEX);
        cl.s.changed := NEW (Thread.Condition);
        cl.s.key     := key;

        (* create the writer that'll buffer stuff for the build cache *)
        self        := cl.s.active;
        buf_wx      := NEW (Wx.T).init (NIL);
        cl.s.buffer := NEW (Port, wx := buf_wx, next := self);
        cl.s.active := cl.s.buffer;
        cl.s.buf_wx := buf_wx;
        INC (cl.s.n_live);

        (* initialize the cache buffer *)
        HTML.Begin (cl.s.root, buf_wx);
        buf_wx.put ("<P><STRONG>Directory:</STRONG> <TT>", cl.s.arg1, "</TT>\n");
        Put (cl.s, "<BR><STRONG>Build time:</STRONG> <TT>");
        Put (cl.s, FmtTime.Short (Time.Now()));
        Put (cl.s, "</TT>\n<P>\n");
        Put (cl.s, "<FORM method=get action=\"./[interrupt]\">");
        Put (cl.s, "<INPUT TYPE=submit VALUE=\"Interrupt build\"></FORM>\n<P>\n");

        (* finally, let'er rip... *)
        cl.handler := Thread.Fork (cl);
        EVAL ongoing.put (key, cl);
      END;
    END;

    (* wait until we're done, or the thread's TCP connection breaks *)
    LOCK cl.s.mu DO
      WHILE (NOT cl.s.done) AND (self.wr_error = NIL) DO
        Thread.AlertWait (cl.s.mu, cl.s.changed);
      END;
    END;

    IF (self.wr_error # NIL) THEN
      RAISE Wr.Failure (self.wr_error);
    END;
  END Build;

PROCEDURE InterruptBuild (root: Pkg.T) =
  VAR
    key := ID.Add (Node.FullPath (root));
    ref: REFANY;
    cl: BuildClosure;
  BEGIN
    LOCK ongoing_mu DO
      IF NOT ongoing.get (key, ref) THEN RETURN; END;
      cl := NARROW (ref, BuildClosure);
      cl.s.abort := TRUE;
    END;

    (* give the builder a chance to quit on its own... *)
    Thread.Pause (10.0d0);

    IF NOT cl.s.done THEN
      (* too late, we're blowing him away! *)
      Thread.Alert (cl.handler);
    END;
  END InterruptBuild;

TYPE
  BuildClosure = Thread.Closure OBJECT
    s       : State;
    handler : Thread.T;
  OVERRIDES
    apply := DoBuild;
  END;

PROCEDURE DoBuild (cl: BuildClosure): REFANY =
  VAR buf := cl.s.buffer;  wx := cl.s.buf_wx;  ref: REFANY;
  BEGIN
    TRY
      RunCmd (cl.s);
      Flush (cl.s);
      IF (cl.s.root # NIL) THEN
        wx.put ("<P>\n");
        BrowserDB.ScanOne (ID.ToText (cl.s.root.arcname ()), cl.s.root.parent, wx);
      END;
      HTML.End (wx);
      Flush (cl.s);
    EXCEPT
    | Wr.Failure (ec) => buf.wr_error := ec;
    | Thread.Alerted  => (* ignore, since we're almost done anyway... *)
    END;

    BuildCache.AttachBody (cl.s.cache, wx.toText ());

    LOCK ongoing_mu DO
      IF ongoing.get (cl.s.key, ref) AND (ref = cl) THEN
        EVAL ongoing.delete (cl.s.key, ref);
      END;
    END;

    SignalDone (cl.s);
    RETURN NIL;
  END DoBuild;

PROCEDURE Clean (root: Pkg.T;  pkg_dir: Pathname.T;  wx: Wx.T) 
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    RunCmd (NewState (CI.Clean_package, pkg_dir, NIL, root, wx));
  END Clean;

PROCEDURE Ship (root: Pkg.T;  pkg_dir: Pathname.T;  wx: Wx.T) 
  RAISES {Thread.Alerted, Wr.Failure} = 
  BEGIN  
    RunCmd (NewState (CI.Ship_package, pkg_dir, NIL, root, wx));
  END Ship;

PROCEDURE Run (root: Pkg.T;  prog, wd: Pathname.T;  wx: Wx.T) 
  RAISES {Thread.Alerted, Wr.Failure} = 
  BEGIN  
    RunCmd (NewState (CI.Run_program, wd, prog, root, wx));
  END Run;

PROCEDURE RunCmd (s: State)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    mach       : Quake.Machine;
    proc       : QValue.T;
    mach_wr    : LineWr.T := LineWr.New (ProcessLine, s);
    saved_wr   : Wr.T;
    saved_echo : BOOLEAN;
    n_args     : INTEGER := 0;
    arg_txt    : TEXT;
  BEGIN
    Put(s, "<P>");
    IF (s.arg1 # NIL) AND (s.arg2 # NIL) THEN
      arg_txt := "(\"" & s.arg1 & "\", \"" & s.arg2 & "\")";
    ELSIF (s.arg1 # NIL) THEN
      arg_txt := "(\"" & s.arg1 & "\")";
    ELSIF (s.arg2 # NIL) THEN
      arg_txt := "(\"" & s.arg2 & "\")";
    ELSE
      arg_txt := "()";
    END;
    ErrLog.Msg ("calling ", s.cmd, arg_txt);
    Flush (s);

    Default.GetConfigProc (s.proc, mach, proc);
    IF (mach # NIL) THEN
      TRY
        saved_echo := mach.exec_echo (ConfigItem.X[ConfigItem.T.Verbose_log].bool);
        saved_wr   := mach.cur_wr ();
        mach.set_wr (mach_wr);
        mach.start_call (proc);
        IF (s.arg1 # NIL) THEN
          QMachine.PushText (mach, s.arg1);  INC (n_args);
        END;
        IF (s.arg2 # NIL) THEN
          QMachine.PushText (mach, s.arg2);  INC (n_args);
        END;
        mach.call_proc (n_args, isFunc := FALSE);
        mach.set_wr (saved_wr);
        EVAL mach.exec_echo (saved_echo);
      EXCEPT Quake.Error (msg) =>
        Wr.PutText (mach_wr, msg);
        LineWr.Clear (mach_wr);
        ErrLog.Msg ("** error while running " & s.cmd, arg_txt, " **");
        ErrLog.Msg (msg);
      END;
    END;

    (* process any remaining output *)
    LineWr.Clear (mach_wr);

    Put (s, "\n<P><STRONG>Done.</STRONG><BR>\n");
    Flush (s);
  END RunCmd;

TYPE
  LineInfo = RECORD
    header : TEXT;
    output : TEXT;
    log    : TEXT;
  END;

PROCEDURE ProcessLine (ref: REFANY;  line: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR s: State := ref;  info: LineInfo;
  BEGIN
    IF s.abort THEN
      Put (s, "<P><STRONG>Build aborted.</STRONG><BR>\n");
      RAISE Thread.Alerted;
    END;

    ParseLine (s, line, info);
    IF info.log # NIL THEN
      ErrLog.Msg (info.log);
    END;
    IF info.header # NIL THEN
      Put (s, info.header);
      Put (s, "<BR>\n");
    END;
    IF info.output # NIL THEN
      Put (s, "<TT>");
      Put (s, info.output);
      Put (s, "</TT><BR>\n");
    END;

    Flush (s);
  END ProcessLine;

PROCEDURE ParseLine (s: State;  line: TEXT;  VAR(*OUT*) info: LineInfo)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    info.output := line;
    info.log    := NIL;
    info.header := NIL;

    IF (line # NIL) AND (Text.Length (line) > 1) THEN
      CASE Text.GetChar (line, 0) OF
      | '/'  => ParseCmd (line, info);       (* "/usr/local/bin/stubgen ..." *)
      | ' '  => ParseMisc (line, info);      (* " -> linking" ? *)
      | '\"' => ParseError (s, line, info);  (* "../src/foobar.i3"... *)
      ELSE ParseCompileStep (line, info);    (* "stale -> compiling foo.i3" *)
      END;
    END;
  END ParseLine;

PROCEDURE ParseCmd (line: TEXT;  VAR info: LineInfo) =
  (* expecting:  /foo/baz/command arg0 arg1 ...  *)
  VAR cmd, stmt: TEXT;  space := Text.FindChar (line, ' ');
  BEGIN
    IF space = -1 THEN RETURN END;
    cmd := Pathname.Last (Text.Sub (line, 0, space));

    IF Text.Equal (cmd, "m3bundle") THEN
      stmt := "Bundling resources";
    ELSIF Text.Equal (cmd, "stubgen") THEN
      stmt := "Preprocessing for Network Objects";
    ELSIF Text.Equal (cmd, "stablegen") THEN
      stmt := "Preprocessing for Stable Objects";
    ELSE
      stmt := "Running " & cmd;
    END;

    info.log    := stmt;
    info.header := stmt;
  END ParseCmd;

PROCEDURE ParseMisc (line: TEXT;  VAR info: LineInfo) =
  VAR len := Text.Length (line);
  BEGIN
    IF (len > 2) AND (Text.GetChar (line, 1) = '-') THEN
      (* something like:  " -> linking ..."  *)
      WITH space = Text.FindChar (line, ' ', 1) + 1,
           what = Text.Sub (line, space) DO
        info.log    := what;
        info.header := what;
        info.output := NIL;
      END;
    END;
  END ParseMisc;

PROCEDURE ParseError (s: State;  line: TEXT;  VAR info: LineInfo)
  RAISES {Wr.Failure, Thread.Alerted} =
  CONST Tag = ARRAY BOOLEAN OF TEXT { "error", "warn" };
  VAR x0, x1, x2, x3: INTEGER;  file, err_line, msg: TEXT;  n: Node.T;
  BEGIN
    (* extract the file name *)
    x0 := Text.FindChar (line, '\"', 1);
    IF (x0 < 2) THEN RETURN END;
    file := Text.Sub (line, 1, x0-1);

    (* find the line number *)
    x1 := Text2.FindSubstring (line, "line ");
    x2 := Text.FindChar (line, ':', x1+5);
    IF (x1 > 0) AND (x2 > x1)
      THEN err_line := Text.Sub (line, x1+5, x2 - x1 - 5);
      ELSE err_line := "1";
    END;

    (* is it an error or warning? *)
    x3 := Text2.FindSubstring (line, "warning:");
    IF    (x3 >= 0) THEN  msg := Text.Sub (line, x3);
    ELSIF (x2 >= 0) THEN  msg := Text.Sub (line, x2 + 1);
    ELSE                  msg := Text.Sub (line, x0 + 3);
    END;

    IF (s.cache # NIL) THEN
      n := BuildCache.AddError (s.cache, file, err_line, msg, x3 >= 0);
      PutImg (s, Tag [x3 >= 0]);
      Put (s, " <TT>");
      PutRef (s, n, "ERROR-LINE-" & err_line);
      Put (s, Text.Sub (line, x0+3));
      Put (s, "</A>  ");
      PutActionRef (s, n, "edit." & err_line, "ERROR-LINE-" & err_line);
      Put (s, "[edit]"); (** PutImg (s, "edit"); **)
      Put (s, "</A>");
      Put (s, "</TT><BR>\n");
      info.output := NIL;
    ELSE
      info.output := line;
    END;
  END ParseError;

PROCEDURE ParseCompileStep (line: TEXT;  VAR info: LineInfo) =
  VAR arrow, space: INTEGER;  file, reason, unit: TEXT;  ch: CHAR;
  BEGIN
    arrow  := Text2.FindSubstring (line, " -> ");
    IF (arrow < 0) THEN RETURN; END;
    space  := Text.FindCharR (line, ' ');
    file   := Text.Sub (line, space+1);
    reason := Text.Sub (line, 0, arrow);
    unit   := Pathname.Last (file);

    ch := Text.GetChar (unit, Text.Length (unit) - 1);
    IF (ch = '3') OR (ch = 'g') THEN
      (* mumble.{i3,m3,ig,mg} *)
      info.header := "Compiling <B>" & file & "</B> (" & reason & ")";
    ELSE
      info.header := Text.Sub (line, arrow+4);
    END;
    info.log    := "Compiling " & file & " (" & reason & ")";
    info.output := NIL;
  END ParseCompileStep;

PROCEDURE NoteAttachments (s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR txt: TEXT;  p: Port;
  BEGIN
    LOCK ongoing_mu DO
      IF (s.attaching # NIL) THEN
        (* grab a copy of what's been produced so far *)
        txt := s.buf_wx.toText ();
        TRY s.buf_wx.put (txt);
        EXCEPT Wr.Failure (ec) => KillPort (s, s.buffer, ec);
        END;
        (* and push it out to each of the new ports as they become active *)
        WHILE (s.attaching # NIL) DO
          p := s.attaching;    s.attaching := p.next;
          p.next := s.active;  s.active := p;
          INC (s.n_live);
          TRY p.wx.put (txt);
          EXCEPT Wr.Failure (ec) => KillPort (s, p, ec);
          END;
        END;
      END;
    END;
  END NoteAttachments;

PROCEDURE Put (s: State;  txt: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR p: Port;
  BEGIN
    IF (s.attaching # NIL) THEN NoteAttachments (s); END;
    p := s.active;
    WHILE (p # NIL) DO
      IF (p.wx # NIL) THEN
        TRY p.wx.put (txt);
        EXCEPT Wr.Failure (ec) => KillPort (s, p, ec);
        END;
      END;
      p := p.next;
    END;
  END Put;

PROCEDURE PutImg (s: State;  icon: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR p: Port;
  BEGIN
    IF (s.attaching # NIL) THEN NoteAttachments (s); END;
    p := s.active;
    WHILE (p # NIL) DO
      IF (p.wx # NIL) THEN
        TRY HTML.PutImg (icon, p.wx);
        EXCEPT Wr.Failure (ec) => KillPort (s, p, ec);
        END;
      END;
      p := p.next;
    END;
  END PutImg;

PROCEDURE PutRef (s: State;  node: Node.T;  tag: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR p: Port;
  BEGIN
    IF (s.attaching # NIL) THEN NoteAttachments (s); END;
    p := s.active;
    WHILE (p # NIL) DO
      IF (p.wx # NIL) THEN
        TRY HTML.GenRef (node, p.wx, tag);
        EXCEPT Wr.Failure (ec) => KillPort (s, p, ec);
        END;
      END;
      p := p.next;
    END;
  END PutRef;

PROCEDURE PutActionRef (s: State;  node: Node.T;  action, tag: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR p: Port;
  BEGIN
    IF (s.attaching # NIL) THEN NoteAttachments (s); END;
    p := s.active;
    WHILE (p # NIL) DO
      IF (p.wx # NIL) THEN
        TRY HTML.GenActionRef (node, p.wx, action, tag);
        EXCEPT Wr.Failure (ec) => KillPort (s, p, ec);
        END;
      END;
      p := p.next;
    END;
  END PutActionRef;

PROCEDURE Flush (s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR p: Port;
  BEGIN
    IF (s.attaching # NIL) THEN NoteAttachments (s); END;
    p := s.active;
    WHILE (p # NIL) DO
      IF (p.wx # NIL) THEN
        TRY p.wx.flush ();
        EXCEPT Wr.Failure (ec) => KillPort (s, p, ec);
        END;
      END;
      p := p.next;
    END;
  END Flush;

PROCEDURE KillPort (s: State;  p: Port;  ec: AtomList.T)
  RAISES {Wr.Failure} =
  BEGIN
    IF (p.wx # NIL) THEN
      p.wx := NIL;
      p.wr_error := ec;
      DEC (s.n_live);
    END;
    IF (s.n_live <= 0) THEN RAISE Wr.Failure (ec); END;
  END KillPort;

PROCEDURE SignalDone (s: State) =
  BEGIN
    LOCK s.mu DO  s.done := TRUE;  END;
    Thread.Broadcast (s.changed);
  END SignalDone;

BEGIN 
END Builder.
