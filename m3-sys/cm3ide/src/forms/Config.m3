(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Config;

IMPORT Fmt, Text, Thread, Wr;
IMPORT BrowserDB, ConfigItem, Default, Display, Form, HTML, ID;
IMPORT LexMisc, Node, PkgRoot, Text2, UserState, WebServer, Wx;
IMPORT ErrLog;

TYPE
  CI = ConfigItem.T;

PROCEDURE Init () =
  BEGIN
    Form.Register ("configure", DoConfig);
  END Init;

PROCEDURE DoConfig (self: Node.T;  data: Node.FormData;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    item_handled: BOOLEAN;
    changed: ARRAY CI OF BOOLEAN;
    pre, post: TEXT;
    root_info := NewRootTable ();
    restart := NEW (RestartClosure);
  BEGIN
    FOR i := FIRST (changed) TO LAST (changed) DO
      changed [i] := FALSE;
    END;

    HTML.BeginXX (self, wx, "CM3-IDE Configuration");

    (* process any new data *)
    WHILE (data # NIL) DO
      item_handled := FALSE;
      IF (data.field # NIL) THEN

        (* try the predefined configuration items first *)
        FOR ci := FIRST (CI) TO LAST (CI) DO
          IF Text.Equal (data.field, ConfigItem.Desc[ci].name) THEN
            pre := ConfigItem.ToText (ci);
            ConfigItem.Set (ci, data.value);
            post := ConfigItem.ToText (ci);
            IF (pre = NIL) # (post = NIL) THEN
              changed[ci] := TRUE;
            ELSIF (pre # NIL) AND NOT Text.Equal (pre, post) THEN
              changed[ci] := TRUE;
            END;
            item_handled := TRUE;
            EXIT;
          END;
        END;

        IF NOT item_handled THEN
          item_handled := AddRootInfo (root_info, data.field, data.value);
        END;
        IF NOT item_handled THEN
          wx.put ("<STRONG>Unrecognized field: ", data.field, "</STRONG><BR>\n");
        END;

      END;
      data := data.next;
    END;

    (* recompute any derived configuration items *)
    IF changed[CI.Server_machine]
    OR changed[CI.IP_address]
    OR changed[CI.Server_port] THEN
      Default.server_href := "http://" & ConfigItem.ToText (CI.Server_machine)
                             & ":" & ConfigItem.ToText (CI.Server_port)
                             & "/";
      restart.server := TRUE;
    END;
    IF changed[CI.Start_browser] THEN
      restart.browser := TRUE;
    END;
    IF NOT CompareRoots (root_info) THEN
      ResetRoots (root_info);
      root_info := NewRootTable ();
      restart.scan := TRUE;
    END;

    wx.put ("<FORM action=\"/form/configure\" method=\"get\">\n");
    wx.put ("<PRE>\n");
    wx.put ("<INPUT TYPE=submit VALUE=\"Save and apply changes\">\n");

    (*---*)
    GenHeader (wx, "Display", "display");
    GenForm (wx, "Home page",                   CI.Homepage);
    GenForm (wx, "Max display items",           CI.Max_display_items);
    GenForm (wx, "Max display width (chars)",   CI.Max_display_width);
    GenForm (wx, "Max display width (columns)", CI.Max_display_columns);
    GenForm (wx, "Multiple windows",            CI.Use_multiple_windows);

    (*---*)
    GenHeader (wx, "Package roots", "package-roots");

    FOR i := 0 TO LAST (root_info^) DO
      WITH z = root_info[i].old DO
        GenRoot (wx, i, z.name, z.path, z.build);
      END;
    END;

    (*---*)
    GenHeader (wx, "Communication", "communication");
    GenForm (wx, "Host name",     CI.Server_machine);
    GenForm (wx, "IP address",    CI.IP_address);
    GenForm (wx, "Server port",   CI.Server_port);

    (*---*)
    GenHeader (wx, "Misc", "misc");
    GenForm (wx, "Verbose log", CI.Verbose_log);
    GenForm (wx, "Automatic package scans", CI.Auto_pkg_scan);
    GenForm (wx, "Server threads", CI.Num_server_threads);
    GenForm (wx, "Refresh interval (minutes)", CI.Refresh_interval);
    wx.put ("  <B>CM3-IDE URL: </B>", Default.server_href, "\n");
    wx.put ("  <B>System package root: </B>", Default.system_root, "\n");
    wx.put ("  <B>Build directory: </B>", Default.build_dir, "\n");

    (*---*)
    GenHeader (wx, "Helper procedures", "helper-procs");
    GenForm (wx, "Browser",  CI.Start_browser);
    GenForm (wx, "Build",    CI.Build_package);
    GenForm (wx, "Ship",     CI.Ship_package);
    GenForm (wx, "Clean",    CI.Clean_package);
    GenForm (wx, "Run",      CI.Run_program);
    GenForm (wx, "Edit",     CI.Edit_file);

    wx.put ("</PRE>\n");
    wx.put ("<INPUT TYPE=submit VALUE=\"Save and apply changes\">\n");
    wx.put ("</FORM>\n");

    HTML.NoData (data, wx);
    HTML.End (wx);
    wx.flush ();

    IF (restart.server) OR (restart.browser) OR (restart.scan) THEN
      EVAL Thread.Fork (restart);
    END;
  END DoConfig;

TYPE
  RestartClosure = Thread.Closure OBJECT
    server  : BOOLEAN := FALSE;
    browser : BOOLEAN := FALSE;
    scan    : BOOLEAN := FALSE;
  OVERRIDES
    apply := Restart;
  END;

PROCEDURE Restart (cl: RestartClosure): REFANY =
  BEGIN
    IF (cl.server) THEN
      ErrLog.Msg ("restarting server and browser because of configuration change");
      WebServer.Restart ();
      Display.Start ();
    ELSIF (cl.browser) THEN
      ErrLog.Msg ("restarting browser because of configuration change");
      Display.Start ();
    END;
    IF (cl.scan) THEN
      ErrLog.Msg ("rescanning packages because of configuration change");
      TRY
        BrowserDB.Refresh ();
      EXCEPT Thread.Alerted =>
        (* ignore *)
      END;
    END;
    RETURN NIL;
  END Restart;

PROCEDURE GenForm (wx: Wx.T;  title: TEXT;  ci: CI)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    nm := ConfigItem.Desc[ci].name;
    val := ConfigItem.ToText (ci);
  BEGIN
    wx.put ("  <B>", title, ": </B>");
    CASE ConfigItem.Desc[ci].kind OF

    | ConfigItem.Kind.Bool =>
        wx.put ("<INPUT TYPE=RADIO NAME=\"", nm, "\" VALUE=\"FALSE\"");
        IF NOT ConfigItem.X[ci].bool THEN wx.put (" CHECKED=TRUE"); END;
        wx.put (">off</INPUT> ");
        wx.put ("<INPUT TYPE=RADIO NAME=\"", nm, "\" VALUE=\"TRUE\"");
        IF ConfigItem.X[ci].bool THEN wx.put (" CHECKED=TRUE"); END;
        wx.put (">on</INPUT>\n");

    | ConfigItem.Kind.Int =>
        wx.put ("<INPUT TYPE=TEXT NAME=\"", nm, "\" SIZE=10");
        wx.put (" VALUE=\"", val, "\">\n");

    | ConfigItem.Kind.Text =>
        wx.put ("<INPUT TYPE=TEXT NAME=\"", nm, "\" SIZE=50");
        wx.put (" VALUE=\"", val, "\">\n");

    | ConfigItem.Kind.Proc =>
        wx.put ("\n    <TEXTAREA ROWS=5 COLS=70 NAME=\"", nm, "\">");
        wx.put (val, "</TEXTAREA>\n");

    | ConfigItem.Kind.IPAddr =>
        wx.put ("<INPUT TYPE=TEXT NAME=\"", nm, "\" SIZE=20");
        wx.put (" VALUE=\"", val, "\">\n");
    END;
  END GenForm;

PROCEDURE GenRoot (wx: Wx.T;  n: INTEGER;  name: ID.T;  path: TEXT;  build: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR key := Fmt.Int (n);
  BEGIN
    wx.put ("  <INPUT TYPE=TEXT NAME=\"root-", key, "-name\" SIZE=12");
    IF (name # ID.NoID) THEN  wx.put (" VALUE=\"", ID.ToText(name), "\"");  END;
    wx.put (">");
    wx.put ("  <INPUT TYPE=TEXT NAME=\"root-", key, "-path\" SIZE=50");
    wx.put (" VALUE=\"", path, "\">");
    wx.put ("  <INPUT TYPE=RADIO NAME=\"root-", key, "-build\" VALUE=FALSE");
    IF NOT build THEN wx.put (" CHECKED=TRUE"); END;
    wx.put (">browse</INPUT>");
    wx.put (" <INPUT TYPE=RADIO NAME=\"root-", key, "-build\" VALUE=TRUE");
    IF build THEN wx.put (" CHECKED=TRUE"); END;
    wx.put (">build</INPUT>\n");
  END GenRoot;

PROCEDURE GenHeader (wx: Wx.T;  title, tag: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wx.put ("\n<B>", title, ":</B>  ");
(***
    wx.put ("<A HREF=\"/rsrc/confighelp.html#", tag, "\">");
    wx.put ("<IMG SRC=\"/rsrc/help.gif\" height=24 width=24 align=\"bottom\"");
    wx.put (" border=0></A> ");
****)
    wx.put (" <A HREF=\"/rsrc/confighelp.html#", tag, "\">[Help]</A>\n\n");
  END GenHeader;

(*------------------------------------------------ package root table ---*)

TYPE
  RootInfo = REF ARRAY OF RootPair;
  RootPair = RECORD new, old: RootDesc;  root: PkgRoot.T := NIL; END;
  RootDesc = RECORD
    name  : ID.T    := ID.NoID;
    path  : TEXT    := NIL;
    build : BOOLEAN := FALSE;
  END;

PROCEDURE NewRootTable (): RootInfo =
  (* initialize a table with the current package roots *)
  CONST MaxRoots = ORD (Node.LastPkgRoot) - ORD (Node.FirstPkgRoot) + 1;
  VAR info: RootInfo;  cnt := 0;  r := PkgRoot.First ();  n_pre, n_post: INTEGER;
  BEGIN
    (*count'em first *)
    WHILE (r # NIL) DO INC (cnt);  r := r.sibling; END;

    IF    cnt + 4 <= MaxRoots THEN   n_pre := 2;  n_post := 2;
    ELSIF cnt + 3 <= MaxRoots THEN   n_pre := 2;  n_post := 1;
    ELSIF cnt + 2 <= MaxRoots THEN   n_pre := 1;  n_post := 1;
    ELSIF cnt + 1 <= MaxRoots THEN   n_pre := 1;  n_post := 0;
    ELSE                             n_pre := 0;  n_post := 0;
    END;

    info := NEW (RootInfo, cnt + n_pre + n_post);

    (* map the existing roots, leaving 2 holes at the top and bottom *)
    r := PkgRoot.First ();
    FOR i := n_pre TO n_pre + cnt-1 DO
      WITH z = info[i] DO
        z.old.name  := r.name;        z.new.name  := r.name;
        z.old.path  := r.path;        z.new.path  := r.path;
        z.old.build := r.buildable;   z.new.build := r.buildable;
        z.root      := r;
      END;
      r := r.sibling;
    END;

    RETURN info;
  END NewRootTable;

PROCEDURE AddRootInfo (info: RootInfo;  nm, value: TEXT): BOOLEAN =
  VAR buf: ARRAY [0..19] OF CHAR;  cursor, val: INTEGER;  tail: TEXT;
  BEGIN
    IF (nm = NIL) OR (value = NIL) THEN RETURN FALSE; END;
    IF NOT Text2.PrefixMatch ("root-", nm, 5) THEN RETURN FALSE; END;

    Text.SetChars (buf, nm);
    cursor := 5;
    val := LexMisc.ReadInt (buf, cursor);
    IF (val < 0) OR (val > LAST (info^)) THEN RETURN FALSE; END;

    tail := Text.Sub (nm, cursor);
    IF (tail = NIL) THEN
      RETURN FALSE;
    ELSIF Text.Equal (tail, "-name") THEN
      IF (value = NIL) OR  Text.Length (value) <= 0
        THEN info[val].new.name := ID.NoID;
        ELSE info[val].new.name := ID.Add (value);
      END;
      RETURN TRUE;
    ELSIF Text.Equal (tail, "-path") THEN
      info[val].new.path := value;
      RETURN TRUE;
    ELSIF Text.Equal (tail, "-build") THEN
      info[val].new.build := (value # NIL) AND Text.Equal (value, "TRUE");
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END AddRootInfo;

PROCEDURE CompareRoots (info: RootInfo): BOOLEAN =
  VAR x_old := 2;  root: PkgRoot.T;
  BEGIN
    FOR x_new := 0 TO LAST (info^) DO
      WITH z = info[x_new].new DO
        IF (z.name # ID.NoID) AND (z.path # NIL) AND Text.Length (z.path) > 0 THEN
          (* we've got a live one, see if it matches the next old one *)
          WITH zz = info[x_old].old DO
            IF (z.name = zz.name) AND (zz.path # NIL)
              AND Text.Equal (z.path, zz.path) THEN
              (* it's a match *)
              root := info[x_old].root;
              IF (root # NIL) THEN
                IF (z.build # root.buildable) THEN
                  zz.build := z.build;
                  root.buildable := z.build;
                  UserState.Put ("root-" & Fmt.Int (x_old-2) & "-build",
                                 Fmt.Bool (z.build));
                END;
              END;
              INC (x_old);
            ELSE
              RETURN FALSE;
            END;
          END;
        END;
      END;
    END;
    RETURN x_old = (NUMBER (info^) - 2);
  END CompareRoots;

PROCEDURE ResetRoots (info: RootInfo) =
  VAR cnt := 0;  key: TEXT;
  BEGIN
    PkgRoot.Reset ();
    FOR x_new := 0 TO LAST (info^) DO
      WITH z = info[x_new].new DO
        IF (z.name # ID.NoID) AND (z.path # NIL) AND Text.Length (z.path) > 0 THEN
          PkgRoot.Add (ID.ToText (z.name), z.path, z.build);
          key := "root-" & Fmt.Int (cnt);
          UserState.Put (key & "-name", ID.ToText (z.name));
          UserState.Put (key & "-path", z.path);
          UserState.Put (key & "-build", Fmt.Bool (z.build));
          INC (cnt);
        END;
      END;
    END;
    PkgRoot.Init ();
  END ResetRoots;

BEGIN
END Config.
