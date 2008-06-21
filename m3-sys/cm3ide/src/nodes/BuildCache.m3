(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE BuildCache;

IMPORT Fmt, FmtTime, IntRefTbl, Pathname, Text, Thread, Wr;
IMPORT BrowserDB, Buf, CMarkUp, Decl, Default, Editor, HTML, ID;
IMPORT LexMisc, Marker, MarkUp, Node, OS, Pkg, Roots, Source, Wx;

REVEAL
  T = Node.Named_T BRANDED "BuildCache.T" OBJECT
    root      : Pkg.T;
    wd        : TEXT;
    body      : TEXT;
    file_head : ErrFile;
    file_tail : ErrFile;
    built     : INTEGER;
  OVERRIDES
    class    := Class;
    iterate  := Iterate;
    next     := Next;
    gen_page := GenPage;
  END;

VAR
  mu := NEW (MUTEX);

PROCEDURE LookUp (root: Pkg.T): T =
  VAR ref: REFANY;
  BEGIN
    LOCK mu DO
      IF (cache # NIL) AND cache.get (GenName (root), ref) THEN
        RETURN NARROW (ref, Node.List).head;
      END;
    END;
    RETURN NIL;
  END LookUp;

PROCEDURE New (root: Pkg.T;  wd: TEXT): T =
  VAR t := NEW (T);
  BEGIN
    IF (cache = NIL) THEN
      LOCK mu DO
        IF (cache = NIL) THEN cache := NEW (IntRefTbl.Default).init (); END;
      END;
    END;

    t.name      := GenName (root);
    t.parent    := Roots.BuildCacheRoot;
    t.sibling   := NIL;
    t.root      := root;
    t.wd        := wd;
    t.body      := NIL;
    t.file_head := NIL;
    t.file_tail := NIL;
    t.built     := OS.Now ();
    RETURN t;
  END New;

PROCEDURE Timestamp (t: T): INTEGER =
  BEGIN
    RETURN t.built;
  END Timestamp;

PROCEDURE GenName (n: Node.T): ID.T =
  VAR
    arcs : ARRAY [0..19] OF Node.T;
    len  := Node.FindArcs (n, arcs);
    nm   : TEXT := ID.ToText (arcs[0].arcname ());
  BEGIN
    FOR i := 1 TO len-1 DO
      nm := nm & "_" & ID.ToText (arcs[i].arcname ());
    END;
    RETURN ID.Add (nm);
  END GenName;

PROCEDURE AttachBody (t: T;  body: TEXT) =
  VAR ref: REFANY;  nd: Node.List;
  BEGIN
    t.body := body;
    LOCK mu DO
      IF cache.get (t.name, ref) THEN
        nd := ref;  nd.head := t;
      ELSE
        EVAL cache.put (t.name, NEW (Node.List, head := t, tail := NIL));
      END;
    END;
  END AttachBody;

PROCEDURE AddError (t: T;  fname, lineno, msg: TEXT;  warn: BOOLEAN): Node.T =
  VAR
    file   := FindFile (t, fname);
    line   := LexMisc.ScanInt (lineno);
    err    := NEW (ErrMsg, line := line, msg := msg, warn := warn);
    e0, e1 : ErrMsg;
  BEGIN
    e0 := file.msg_tail;  e1 := NIL;
    WHILE (e0 # NIL) AND (e0.line > line) DO
      e1 := e0;
      e0 := e0.prev;
    END;
    IF (e1 = NIL) THEN
      (* add to the end of the list *)
      err.next := NIL;
      err.prev := file.msg_tail;
      IF (file.msg_tail = NIL)
        THEN file.msg_head := err;
        ELSE file.msg_tail.next := err;
      END;
      file.msg_tail := err;
    ELSIF (e0 = NIL) THEN
      (* add to the beginning of the list *)
      err.next := file.msg_head;
      err.prev := NIL;
      IF (file.msg_head # NIL) THEN file.msg_head.prev := err; END;
      file.msg_head := err;
    ELSE
      (* insert in the middle of the list *)
      err.next := e1;
      err.prev := e0;
      e0.next := err;
      e1.prev := err;
    END;
    RETURN file;
  END AddError;

PROCEDURE Class (<*UNUSED*> t: T): Node.Class =
  BEGIN
    RETURN Node.Class.CacheEntry;
  END Class;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  BEGIN
    s.d := t.file_head;
  END Iterate;

PROCEDURE Next (<*UNUSED*> t: T;  VAR s: Node.IteratorState): BOOLEAN =
  VAR f: ErrFile;
  BEGIN
    WHILE (s.d # NIL) DO
      f := s.d;   s.d := f.sibling;
      IF f.match (s.pattern) THEN  s.match := f;  RETURN TRUE; END;
    END;
    RETURN FALSE;
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wx.put (t.body);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END GenPage;

(*------------------------------------------------------ error files ---*)

VAR
  viewID     := ID.Add ("view");
  editMakeID := ID.Add ("editmake");

TYPE
  ErrFile = Node.Named_T OBJECT
    fname     : TEXT;
    path      : TEXT;
    time      : OS.FileTime;
    prev_file : ErrFile;
    msg_head  : ErrMsg;
    msg_tail  : ErrMsg;
  OVERRIDES
    class      := ErrClass;
    iterate    := ErrIterate;
    next       := ErrNext;
    gen_page   := ErrGenPage;
  END;

TYPE
  ErrMsg = REF RECORD
    line : INTEGER  := 0;
    msg  : TEXT     := NIL;
    warn : BOOLEAN  := FALSE;
    next : ErrMsg   := NIL;
    prev : ErrMsg   := NIL;
  END;

PROCEDURE FindFile (t: T;  nm: TEXT): ErrFile =
  VAR f: ErrFile;
  BEGIN
    f := t.file_tail;
    WHILE (f # NIL) DO
      IF Text.Equal (nm, f.fname) THEN RETURN f; END;
      f := f.prev_file;
    END;

    (* no match, create a new file node *)
    f           := NEW (ErrFile);
    f.name      := ID.Add (Pathname.Last (nm));
    f.fname     := nm;
    f.path      := FindPath (t, nm);
    f.time      := OS.LastModified (f.path);
    f.msg_head  := NIL;
    f.msg_tail  := NIL;
    f.parent    := t;
    f.sibling   := NIL;
    f.prev_file := t.file_tail;

    IF (t.file_tail # NIL)
      THEN t.file_tail.sibling := f;
      ELSE t.file_head := f;
    END;
    t.file_tail := f;

    RETURN f;
  END FindFile;

PROCEDURE FindPath (t: T;  nm: TEXT): TEXT =
  BEGIN
    IF Pathname.Absolute (nm) THEN
      RETURN nm;
    ELSIF Text.Equal (Text.Sub (nm, 0, 2), "..") THEN
      RETURN OS.MakePath (Node.FullPath (t.root), Text.Sub (nm, 3));
    ELSE
      RETURN OS.MakePath (Node.FullPath (t), Default.build_dir, nm);
    END;
  END FindPath;

PROCEDURE ErrClass (<*UNUSED*> t: ErrFile): Node.Class =
  BEGIN
    RETURN Node.Class.BuildError;
  END ErrClass;

PROCEDURE FindSource (t: ErrFile): Source.T =
  VAR
    key    := ID.Add (Pathname.Last (t.path));
    x_path : TEXT;
    ref    : REFANY;
    src    : Source.T;
    nd     : Node.List;
  BEGIN
    IF BrowserDB.db.units.get (key, ref) THEN
      nd := ref;
      WHILE (nd # NIL) DO
        src := nd.head;
        x_path := Node.FullPath (src);
        IF OS.FileNameEq (t.path, x_path) THEN
          RETURN src;
        END;
        nd := nd.tail;
      END;
    END;
    RETURN NIL;
  END FindSource;

CONST
  IsM3 = ARRAY Source.Kind OF BOOLEAN { TRUE, TRUE, TRUE, TRUE, FALSE, .. };

PROCEDURE ErrIterate (t: ErrFile;
                      VAR s: Node.IteratorState) =
  VAR nodes: Node.Set;  src := FindSource (t);
  BEGIN
    s.a := 1;  s.b := 0;  s.d := NIL;
    IF (src # NIL) AND IsM3 [src.kind] THEN
      nodes := Decl.FindNodes (src, s.pattern);
      IF (nodes.cnt > 0) THEN
        s.a := 0;
        s.b := nodes.cnt;
        s.d := nodes.elts;
      END;
    END;
  END ErrIterate;

PROCEDURE ErrNext (<*UNUSED*> t: ErrFile;  VAR s: Node.IteratorState): BOOLEAN =
  VAR elts: Node.Array := s.d;
  BEGIN
    IF (s.a < s.b) THEN
      s.match := elts[s.a];  INC (s.a);
      RETURN TRUE;
    END;
    RETURN FALSE;
  END ErrNext;

PROCEDURE ErrGenPage (t: ErrFile;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR txt: TEXT;  modtime := OS.LastModified (t.path);
  BEGIN
    IF (action = ID.NoID) OR (action = viewID) THEN
      action := viewID;
    ELSIF (action = editMakeID) THEN
      Editor.Run (OS.MakePath (Pathname.Prefix (t.path), "m3makefile"), "1");
      action := viewID;
    ELSE
      txt := ID.ToText (action);
      IF Text.Equal ("edit.", Text.Sub (txt, 0, 5)) THEN
        Editor.Run (t.path, Text.Sub (txt, 5));
        action := viewID;
      END;
    END;

    HTML.Begin (t, wx);
    wx.put ("<P><STRONG>Path:</STRONG>&nbsp;<TT>");
    HTML.GenFileRef (t.path, wx);
    wx.put (t.path, "</A></TT>");
    IF (modtime # OS.NO_TIME) THEN
      wx.put ("&nbsp;&nbsp;&nbsp;<STRONG>Last modified:</STRONG>&nbsp;",
              FmtTime.Short (OS.FileToM3Time (modtime)));
    END;
    Pkg.GenBuildNote (t, wx);
    wx.put ("\n");
    IF (modtime # t.time) THEN
      wx.put ("<BR><STRONG><FONT COLOR=#FF0000>This file has been modified",
              " since the errors were generated.</FONT></STRONG>\n");
    END;

    wx.put ("<P>\n");
    GenView (t, t.path, wx);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END ErrGenPage;

PROCEDURE GenView (t: ErrFile;  path: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    buf := Buf.FromFile (path, pad := 1);  ext: TEXT;
    makefile := OS.MakePath (Pathname.Prefix (path), "m3makefile"); 
  BEGIN
    IF (buf = NIL) THEN 
      wx.put ("<P><STRONG>Unable to open \"", path, "\"</STRONG>\n");
      RETURN;
    END;

    wx.put ("<P>\n");
    IF (OS.LastModified (makefile) # OS.NO_TIME) THEN
      wx.put ("<A HREF=\"./[editmake]\">[edit m3makefile]</A>");
    END;

    IF t.msg_head # NIL THEN
      wx.put ("  <A HREF=\"#ERROR-LINE-");
      wx.putInt (t.msg_head.line);
      wx.put ("\">[first error]</A>");
    END;
    IF t.msg_tail # NIL AND t.msg_tail # t.msg_head THEN
      wx.put ("  <A HREF=\"#ERROR-LINE-");
      wx.putInt (t.msg_tail.line);
      wx.put ("\">[last error]</A>");
    END;

    wx.put ("<HR>\n");

    ext := Pathname.LastExt (path);
    IF OS.FileNameEq (ext, "m3") OR OS.FileNameEq (ext, "i3")
      OR OS.FileNameEq (ext, "mg") OR OS.FileNameEq (ext, "ig") THEN
      MarkUp.Annotate (buf, wx, ErrMarkups (t), NIL);
    ELSE
      CMarkUp.Annotate (buf, wx, ErrMarkups (t));
    END;
  END GenView;

(*---------------------------------------------------- error annotations ---*)

TYPE
  MarkupList = RECORD head, tail : Marker.LineInsertion := NIL; END;

PROCEDURE ErrMarkups (f: ErrFile): Marker.LineInsertion =
  <*FATAL Wr.Failure, Thread.Alerted*>
  CONST Icon = ARRAY BOOLEAN OF TEXT { "error", "warn" };
  VAR
    err      : ErrMsg := f.msg_head;
    wx       := NEW (Wx.T).init (NIL);
    next_err : ErrMsg := NIL;
    prev_err : ErrMsg := NIL;
    marks    : MarkupList;
  BEGIN
    WHILE (err # NIL) DO
      IF (err.prev = NIL) OR (err.prev.line # err.line) THEN
        (* start a new error line *)
        wx.put ("<A NAME=\"ERROR-LINE-");
        wx.putInt (err.line);
        wx.put ("\"></A>");
        AddMarkup (marks, err.line-1, wx.toText ());
        wx.put ("\n\n");
      END;

      (* generate the current error message *)
      HTML.PutImg (Icon[err.warn], wx);
      wx.put (" <FONT COLOR=\"#FF0000\"><STRONG>*** ", err.msg,
              " ***</STRONG></FONT>\n");

      IF (err.next = NIL) OR (err.next.line # err.line) THEN
        (* finish the current error line *)
        wx.put ("  <A HREF=\"./[edit.");
        wx.putInt (err.line);
        wx.put ("]#ERROR-LINE-");
        wx.putInt (err.line);
        wx.put ("\">");
        wx.put ("[edit]"); (** HTML.PutImg ("edit", wx); **)
        wx.put ("</A>");

        IF (err.next # NIL) THEN
          wx.put ("  <A HREF=\"#ERROR-LINE-");
          wx.putInt (err.next.line);
          wx.put ("\">");
          wx.put ("[next error]"); (** HTML.PutSmallImg ("arrow-down", wx); **)
          wx.put ("</A>");
        ELSIF (f.sibling # NIL) THEN
          next_err := NARROW (f.sibling, ErrFile).msg_head;
          IF (next_err # NIL) THEN
            wx.put ("  ");
            HTML.GenRef (f.sibling, wx, "ERROR-LINE-" & Fmt.Int (next_err.line));
            wx.put ("[next error]");  (** HTML.PutSmallImg ("arrow-down", wx); **)
            wx.put ("</A>");
          END;
        END;

        IF (prev_err # NIL) THEN
          wx.put ("  <A HREF=\"#ERROR-LINE-");
          wx.putInt (prev_err.line);
          wx.put ("\">");
          wx.put ("[previous error]"); (** HTML.PutSmallImg ("arrow-up", wx); **)
          wx.put ("</A>");
        ELSIF (f.prev_file # NIL) THEN
          prev_err := f.prev_file.msg_tail;
          IF (prev_err # NIL) THEN
            wx.put ("  ");
            HTML.GenRef (f.prev_file, wx, "ERROR-LINE-" & Fmt.Int (prev_err.line));
            wx.put ("[previous error]");  (** HTML.PutSmallImg ("arrow-up", wx); **)
            wx.put ("</A>");
          END;
        END;

        wx.put ("\n");
        AddMarkup (marks, err.line, wx.toText ());
        prev_err := err;
      END;

      err := err.next;
    END;

    RETURN marks.head;
  END ErrMarkups;

PROCEDURE AddMarkup (VAR x: MarkupList;  line: INTEGER;  txt: TEXT) =
  VAR m := NEW (Marker.LineInsertion, line := line, insert := txt, next := NIL);
  BEGIN
    IF (x.head = NIL)
      THEN x.head := m;
      ELSE x.tail.next := m;
    END;
    x.tail := m;
  END AddMarkup;

BEGIN
END BuildCache.
