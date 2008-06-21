(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Pkg;

IMPORT Env, Text, Thread, Wr;
IMPORT BrowserDB, BuildCache, Builder, ConfigItem, Derived, Dir, Editor, HTML;
IMPORT ID, Node, OS, PkgRoot, Source, UserState, Wx;

REVEAL
  T = Tx BRANDED "Pkg.T" OBJECT
  OVERRIDES
    class    := Class;
    iterate  := Iterate;
    next     := Next;
    gen_page := GenPage;
  END;

VAR
  viewID      := ID.Add ("view");
  buildID     := ID.Add ("build");
  shipID      := ID.Add ("ship");
  cleanID     := ID.Add ("clean");
  editID      := ID.Add ("editmake");
  interruptID := ID.Add ("interrupt");
  rescanID    := ID.Add ("rescan");

PROCEDURE Class (t: T): Node.Class =
  BEGIN
    IF (t # NIL) AND (t.parent # NIL)
      THEN RETURN NARROW (t.parent, PkgRoot.T).kind;
      ELSE RETURN Node.Class.Unknown;
    END;
  END Class;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  BEGIN
    Dir.T.iterate (t, s);
  END Iterate;

PROCEDURE Next (t: T;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN Dir.T.next (t, s);
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    path     := Node.FullPath (t);
    build_ok := IsBuildable (t);
    makefile : TEXT;
  BEGIN
    IF (action = editID) THEN
      path := Node.FullPath (t);
      makefile := FindMakefile (path);
      IF (makefile = NIL) THEN
        (* let the user create one *)
        makefile := OS.MakePath (path, "src", "m3makefile");
      END;
      Editor.Run (makefile, "1");
      action := viewID;
    ELSIF (action = interruptID) THEN
      Builder.InterruptBuild (t);
      action := viewID;
    END;

    IF (action = rescanID)
      OR (action = viewID AND ConfigItem.X[ConfigItem.T.Auto_pkg_scan].bool) THEN
      t := Rescan (t);
      action := viewID;
    END;

    HTML.Begin (t, wx);
    GenFileNote (path, wx, is_dir := TRUE);
    GenBuildNote (t, wx);
    wx.put ("\n<P>");

    (* generate the page body *)
    IF (action = viewID) THEN
      GenView (t, path, wx);
      HTML.NoData (data, wx);
    ELSIF (build_ok) AND (action = buildID) THEN
      DoBuild (t, path, wx, data);
    ELSIF (build_ok) AND (action = shipID)  THEN
      Builder.Ship (t, path, wx);
      HTML.NoData (data, wx);
    ELSIF (build_ok) AND (action = cleanID) THEN
      Builder.Clean (t, path, wx);
      HTML.NoData (data, wx);
    ELSE
      HTML.NoAction (action, wx);
      HTML.NoData (data, wx);
    END;

    HTML.End (wx);
  END GenPage;

PROCEDURE DoBuild (t: T;  path: TEXT;  wx: Wx.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    key  := "BUILD|" & path;
    args := UserState.Get (key);
  BEGIN
    (* process the "build-args" form data *)
    WHILE (data # NIL) DO
      IF Text.Equal (data.field, "build-args") THEN
        args := data.value;
      ELSE
        wx.put ("<STRONG>Unrecognized field: ", data.field, "</STRONG><BR>\n");
      END;
      data := data.next;
    END;
    IF (args = NIL) THEN args := ""; END;
    UserState.Put (key, args);
    Builder.Build (t, path, args, wx);
  END DoBuild;

PROCEDURE Rescan (t: T): T   RAISES {Thread.Alerted} =
  VAR ref: REFANY;  nd: Node.List;  pkg: T;
  BEGIN
    BrowserDB.ScanOne (ID.ToText (t.name), t.parent, NIL);
    IF NOT BrowserDB.db.packages.get (t.name, ref) THEN RETURN t; END;
    nd := NARROW (ref, Node.List);
    WHILE (nd # NIL) DO
      pkg := nd.head;
      IF (pkg.parent = t.parent) THEN
        RETURN pkg; END;
      nd := nd.tail;
    END;
    RETURN t;
  END Rescan;

PROCEDURE GenView (t: T;  path: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Dir.GenReadmeInfo (path, wx);
    GenButtons (t, t, wx);
    Dir.GenContents (t, wx);
  END GenView;

PROCEDURE GenFileNote (path: TEXT;  wx: Wx.T;  is_dir: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  CONST Tag = ARRAY BOOLEAN OF TEXT { "Path", "Directory" };
  BEGIN
    wx.put ("<STRONG>", Tag[is_dir], ":</STRONG>&nbsp;<TT>");
    HTML.GenFileRef (path, wx);
    wx.put (path, "</A></TT>");
    IF NOT is_dir THEN GenModifiedNote (path, wx); END;
  END GenFileNote;

PROCEDURE GenModifiedNote (path: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR modtime := OS.LastModified (path);
  BEGIN
    IF (modtime # OS.NO_TIME) THEN
      wx.put ("&nbsp;&nbsp;&nbsp;&nbsp; ");
      wx.put ("<STRONG>Last&nbsp;modified:</STRONG>&nbsp;", OS.FmtFileTime (modtime));
    END;
  END GenModifiedNote;

PROCEDURE GenBuildNote (src: Node.Named_T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR nd: Node.T := src;  pkg: T := NIL;
  BEGIN
    LOOP
      TYPECASE nd OF
      | NULL            =>  EXIT;
      | T(t)            =>  pkg := t;  EXIT;
      | Node.Named_T(n) =>  nd := n.parent;
      ELSE                  pkg := NIL; EXIT;
      END;
    END;
    IF (pkg # NIL) AND IsBuildable (pkg) THEN
      nd := BuildCache.LookUp (pkg);
      IF (nd # NIL) THEN
        wx.put ("&nbsp;&nbsp;&nbsp;&nbsp; ");
        wx.put ("<STRONG>Last built:</STRONG>&nbsp;");
        HTML.GenRef (nd, wx);
        wx.put (OS.FmtFileTime(BuildCache.Timestamp (nd)), "</A>");
      END;
    END;
  END GenBuildNote;

PROCEDURE Home (nd: Node.T): T =
  BEGIN
    LOOP
      TYPECASE nd OF
      | NULL            =>  RETURN NIL;
      | T(t)            =>  RETURN t;
      | Node.Named_T(n) =>  nd := n.parent;
      ELSE                  RETURN NIL;
      END;
    END;
  END Home;

PROCEDURE GenActionButtons (src: Node.Named_T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    GenButtons (Home (src), src, wx);
  END GenActionButtons;

PROCEDURE GenButtons (pkg: T;  src: Node.Named_T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* HACK ALERT: If </FORM> appears at the end of a <TD></TD>,
     Netscape puts a blank line in the table entry.  If <FORM>
     appears outside a <TD></TD>, M$Explorer creates an empty
     table entry.  So... We start forms inside <TD></TD> pairs
     and we end the form inside the next table entry.  That's
     what "in_form" is all about.   Hack, hack, hack.... *)
  VAR buildable: BOOLEAN;  pgm: Derived.T := NIL;  in_form := FALSE;
  BEGIN
    IF (pkg = NIL) THEN RETURN; END;
    wx.put ("<HR><TABLE><TR>\n");

    buildable := IsBuildable (pkg);
    IF buildable THEN
      GenButton (pkg, wx, "ship", "Ship", in_form);
    END;
    IF NOT ConfigItem.X[ConfigItem.T.Auto_pkg_scan].bool THEN
      GenButton (pkg, wx, "rescan", "Rescan", in_form);
    END;
    IF buildable THEN
      GenButton (pkg, wx, "clean", "Clean", in_form);
    END;

    TYPECASE src OF
    | NULL =>
        (* ignore *)
    | Source.T(ss) =>
        IF buildable THEN
          IF NOT OS.FileNameEq (ID.ToText (ss.name), "m3makefile") THEN
            GenMakeButton (ss.parent, wx, in_form);
          END;
          GenButton (src, wx, "edit", "Edit source", in_form);
        END;
    | T =>
        IF buildable THEN GenMakeButton (pkg, wx, in_form); END;
    | Dir.T(dir) =>
        IF buildable THEN GenMakeButton (dir, wx, in_form); END;
    | Derived.T(der) =>
        IF buildable THEN GenMakeButton (pkg, wx, in_form); END;
        IF der.is_pgm THEN  pgm := der; END;
    ELSE
        IF buildable THEN GenMakeButton (pkg, wx, in_form); END;
    END;

    (* ... to fix alignment with following rows *)
    wx.put ("<TD>");
    IF (in_form) THEN wx.put ("</FORM>"); END;
    wx.put ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
    wx.put ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
    wx.put ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
    wx.put ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
    wx.put ("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
    wx.put ("</TD>");

    wx.put ("</TR>\n");

    IF buildable OR (pgm # NIL) THEN
      IF buildable THEN GenBuildForm (pkg, wx); END;
      IF pgm # NIL THEN GenRunForm (pgm, wx); END;
    END;
    wx.put ("</TABLE>\n");
  END GenButtons;

PROCEDURE GenBuildForm (pkg: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    key := "BUILD|" & Node.FullPath (pkg);
    options := UserState.Get (key);
  BEGIN
    wx.put ("<FORM method=get action=\"");
    HTML.GenURL (pkg, wx);
    wx.put ("[build]\"><TR>");
    wx.put ("<TD><INPUT TYPE=submit VALUE=\"Build\">  </TD><TD>Options: </TD>");
    wx.put ("<TD COLSPAN=5>");
    wx.put ("<INPUT TYPE=text NAME=build-args SIZE=50 VALUE=\"", options, "\">");
    wx.put ("</TD></TR></FORM>\n");
  END GenBuildForm;

PROCEDURE GenRunForm (pgm: Derived.T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    path   := Node.FullPath (pgm);
    cmdkey := "RUNCMD|" & path;
    dirkey := "RUNDIR|" & path;
    cmd    := UserState.Get (cmdkey);
    dir    := UserState.Get (dirkey);
  BEGIN
    IF (cmd = NIL) THEN cmd := Derived.PgmPath (pgm); END;
    IF (dir = NIL) THEN dir := Env.Get ("HOME"); END;
    IF (dir = NIL) THEN dir := Node.FullPath (pgm.parent); END;
    wx.put ("<FORM method=get action=\"");
    HTML.GenURL (pgm, wx);
    wx.put ("[run]\"><TR>");
    wx.put ("<TD><INPUT TYPE=submit VALUE=\" Run \"> </TD>");
    wx.put ("<TD>Command: </TD>");
    wx.put ("<TD COLSPAN=5>");
    wx.put ("<INPUT TYPE=text NAME=run-cmd SIZE=50 VALUE=\"", cmd, "\"></TD></TR>\n");
    wx.put ("<TR><TD>&nbsp;</TD><TD>Directory: </TD>");
    wx.put ("<TD COLSPAN=5>");
    wx.put ("<INPUT TYPE=text NAME=run-dir SIZE=50 VALUE=\"", dir, "\"></TD>");
    wx.put ("</TR></FORM>\n");
  END GenRunForm;

PROCEDURE GenMakeButton (n: Node.T;  wx: Wx.T;  VAR in_form: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR makefile := FindMakefile (Node.FullPath (n));
  BEGIN
    IF (makefile # NIL)
      THEN GenButton (n, wx, "editmake", "Edit m3makefile", in_form);
      ELSE GenButton (n, wx, "editmake", "Create m3makefile", in_form);
    END;
  END GenMakeButton;

PROCEDURE GenButton (n: Node.T;  wx: Wx.T;  action, label: TEXT;
                     VAR in_form: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wx.put ("<TD ALIGN=\"left\">");
    IF in_form THEN wx.put ("</FORM>"); in_form := FALSE; END;
    wx.put ("<FORM method=get action=\"");
    HTML.GenURL (n, wx);
    IF action # NIL THEN wx.put ("[", action, "]"); END;
    wx.put ("\">");
    wx.put ("<INPUT TYPE=submit VALUE=\"", label, "\"></TD>");
    in_form := TRUE;
  END GenButton;

PROCEDURE FindMakefile (path: TEXT): TEXT =
  VAR make := OS.MakePath (path, "m3makefile");
  BEGIN
    IF OS.LastModified (make) # OS.NO_TIME THEN RETURN make; END;
    make := OS.MakePath (path, "src", "m3makefile");
    IF OS.LastModified (make) # OS.NO_TIME THEN RETURN make; END;
    RETURN NIL;    
  END FindMakefile;

PROCEDURE IsBuildable (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.parent # NIL)
       AND NARROW (t.parent, PkgRoot.T).buildable;
  END IsBuildable;

PROCEDURE Init () =
  BEGIN
  END Init;

BEGIN
END Pkg.
