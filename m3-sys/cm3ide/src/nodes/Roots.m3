(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Roots;

IMPORT FmtTime, FS, IntList, IntRefTbl, OSError, Pathname, Text, Thread, Wr;
IMPORT BrowserDB, BuildCache, (**ClassDir,**) ConfigItem, Default, Dir, ErrLog, FileDir;
IMPORT FileNode, Fixed, HTML, ID, LexMisc, Node, OS, Pkg, PkgRoot, RegExpr, Type;
IMPORT WebServer, Wx;

TYPE NC = Node.Class;

VAR
  viewID   := ID.Add ("view");
  rescanID := ID.Add ("rescan");

PROCEDURE Init () =
  BEGIN
    PkgRootRoot := NEW (RootRoot, name := ID.Add ("root"));
    WebServer.RegisterRoot ("root", PkgRootRoot);

    AnyPkgRoot := NEW (PkgRoots, name := ID.Add ("package"));
    (** AddClassEntries (AnyPkgRoot); **)
    WebServer.RegisterRoot ("package", AnyPkgRoot);
    WebServer.RegisterRoot ("pkg", AnyPkgRoot);

    ResourceRoot := NEW (FixedRoot, name := ID.Add ("rsrc"));
    WebServer.RegisterRoot ("rsrc", ResourceRoot);

    TypeRoot := NEW (TNameRoot, name := ID.Add ("type"));
    WebServer.RegisterRoot ("type", TypeRoot);

    TypeUIDRoot := NEW (TUIDRoot, name := ID.Add ("type-uid"));
    WebServer.RegisterRoot ("type-uid", TypeUIDRoot);
    WebServer.RegisterRoot ("type uid", TypeUIDRoot);
    WebServer.RegisterRoot ("uid", TypeUIDRoot);

    InterfaceRoot := NEW (SourceRoot, name := ID.Add ("interface"),
                        kind := NC.Interface);
    WebServer.RegisterRoot ("interface", InterfaceRoot);
    WebServer.RegisterRoot ("intf", InterfaceRoot);

    ModuleRoot :=  NEW (SourceRoot, name := ID.Add ("module"),
                        kind := NC.Module);
    WebServer.RegisterRoot ("module", ModuleRoot);
    WebServer.RegisterRoot ("implementation", ModuleRoot);
    WebServer.RegisterRoot ("impl", ModuleRoot);

    GenIntfRoot :=  NEW (SourceRoot, name := ID.Add ("generic-interface"),
                        kind := NC.GenericInterface);
    WebServer.RegisterRoot ("generic-interface", GenIntfRoot);
    WebServer.RegisterRoot ("generic interface", GenIntfRoot);
    WebServer.RegisterRoot ("generic-intf", GenIntfRoot);
    WebServer.RegisterRoot ("generic intf", GenIntfRoot);
    WebServer.RegisterRoot ("gen-interface", GenIntfRoot);
    WebServer.RegisterRoot ("gen interface", GenIntfRoot);
    WebServer.RegisterRoot ("gen-intf", GenIntfRoot);
    WebServer.RegisterRoot ("gen intf", GenIntfRoot);

    GenImplRoot := NEW (SourceRoot, name := ID.Add ("generic-module"),
                        kind := NC.GenericModule);
    WebServer.RegisterRoot ("generic-module", GenImplRoot);
    WebServer.RegisterRoot ("generic module", GenImplRoot);
    WebServer.RegisterRoot ("generic-implementation", GenImplRoot);
    WebServer.RegisterRoot ("generic implementation", GenImplRoot);
    WebServer.RegisterRoot ("generic-impl", GenImplRoot);
    WebServer.RegisterRoot ("generic impl", GenImplRoot);
    WebServer.RegisterRoot ("gen-module", GenImplRoot);
    WebServer.RegisterRoot ("gen module", GenImplRoot);
    WebServer.RegisterRoot ("gen-implementation", GenImplRoot);
    WebServer.RegisterRoot ("gen implementation", GenImplRoot);
    WebServer.RegisterRoot ("gen-impl", GenImplRoot);
    WebServer.RegisterRoot ("gen impl", GenImplRoot);

    CsourceRoot := NEW (SourceRoot, name := ID.Add ("c-source"),
                        kind := NC.CSource);
    WebServer.RegisterRoot ("c-source", CsourceRoot);
    WebServer.RegisterRoot ("c source", CsourceRoot);

    HsourceRoot := NEW (SourceRoot, name := ID.Add ("h-source"),
                        kind := NC.HSource);
    WebServer.RegisterRoot ("h-source", HsourceRoot);
    WebServer.RegisterRoot ("h source", HsourceRoot);

    AnyUnitRoot := NEW (UnitRoot, name := ID.Add ("unit"));
    WebServer.RegisterRoot ("unit", AnyUnitRoot);
    WebServer.RegisterRoot ("source", AnyUnitRoot);

    ImporterRoot := NEW (ImportRoot, name := ID.Add ("importer"));
    WebServer.RegisterRoot ("importer", ImporterRoot);

    ExporterRoot := NEW (ExportRoot, name := ID.Add ("exporter"));
    WebServer.RegisterRoot ("exporter", ExporterRoot);

    LibraryRoot := NEW (DerivedRoot, name := ID.Add ("library"), pgm := FALSE);
    WebServer.RegisterRoot ("library", LibraryRoot);
    WebServer.RegisterRoot ("lib", LibraryRoot);

    ProgramRoot := NEW (DerivedRoot, name := ID.Add ("program"), pgm := TRUE);
    WebServer.RegisterRoot ("program", ProgramRoot);
    WebServer.RegisterRoot ("pgm", ProgramRoot);

    BuildCacheRoot := NEW (CacheRoot, name := ID.Add ("build-cache"));
    WebServer.RegisterRoot ("build-cache", BuildCacheRoot);

    TutorialRoot := NEW (DocumentRoot, name := ID.Add ("tutorial"),
                         base := "tutorial", title := "Modula-3 Tutorial");
    WebServer.RegisterRoot ("tutorial", TutorialRoot);

    HelpRoot := NEW (DocumentRoot,  name := ID.Add ("help"),
                     base := "help",  title := "CM3-IDE Help");
    WebServer.RegisterRoot ("help", HelpRoot);

    RefManualRoot := NEW (DocumentRoot, name := ID.Add ("reference"),
                          base := "reference",
                          title := "Critical Mass Modula-3 Reference Manual");
    WebServer.RegisterRoot ("ref", RefManualRoot);
    WebServer.RegisterRoot ("reference", RefManualRoot);

    SRCReportRoot := NEW (DocumentRoot, name := ID.Add ("SRC_report"),
                          base := "src_reports", title := "SRC Research Reports");
    WebServer.RegisterRoot ("SRC_report", SRCReportRoot);
    WebServer.RegisterRoot ("src_report", SRCReportRoot);

    ExampleRoot := NEW (ExamplesRoot, name := ID.Add ("example"));
    WebServer.RegisterRoot ("example", ExampleRoot);

    ConsoleLogRoot := NEW (LogRoot, name := ID.Add ("log"));
    WebServer.RegisterRoot ("log", ConsoleLogRoot);

    UserHomeDir := NEW (UserRoot, name := ID.Add ("user"));
    WebServer.RegisterRoot ("user", UserHomeDir);
  END Init;

PROCEDURE RootClass (<*UNUSED*> self: Node.T): Node.Class =
  BEGIN
    RETURN Node.Class.Root;
  END RootClass;

PROCEDURE GenScanWarning (wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF BrowserDB.n_updates < 1 THEN
      wx.put ("<P>\n<STRONG>Initial package scan is still underway...</STRONG>\n");
      wx.put ("<META HTTP-EQUIV=\"Refresh\" CONTENT=2>\n");
    ELSE
      wx.put ("<P>\nLast scanned: ",
              FmtTime.Short (OS.FileToM3Time (BrowserDB.last_update)),
              "\n");
    END;
  END GenScanWarning;

PROCEDURE TableIterate (tbl: IntRefTbl.T;  VAR s: Node.IteratorState) =
  VAR nm := RegExpr.SimpleString (s.pattern);  ref: REFANY;
  BEGIN
    s.d := NIL;
    s.e := NIL;
    s.f := NIL;

    (* try a direct hit instead of a full scan! *)
    IF (nm # NIL) AND (tbl # NIL) AND tbl.get (ID.Add (nm), ref)
      THEN s.d := ref;
      ELSE s.e := tbl.iterate ();
    END;
  END TableIterate;

TYPE FilterProc = PROCEDURE (root, node: Node.T): BOOLEAN;

PROCEDURE TableNext (root: Node.T;  VAR s: Node.IteratorState;
                     filter: FilterProc): BOOLEAN =
  VAR
    nd   : Node.List;
    n    : Node.T;
    iter : IntRefTbl.Iterator;
    nm   : INTEGER;
    ref  : REFANY;
  BEGIN
    IF (s.d # NIL) THEN
      (* try the direct hits first *)
      nd := s.d;  s.d := nd.tail;
      WHILE (nd # NIL) DO
        n := nd.head;  nd := nd.tail;  s.d := nd;
        IF (filter = NIL) OR filter (root, n) THEN
          s.match := n;
          RETURN TRUE;
        END;
      END;
    END;

    WHILE (s.e # NIL) DO
      nd := s.f;
      WHILE (nd # NIL) DO
        n := nd.head;  nd := nd.tail;  s.f := nd;
        IF ((filter = NIL) OR filter (root, n)) AND n.match (s.pattern) THEN
          s.match := n;
          RETURN TRUE;
        END;
      END;

      iter := s.e;
      IF NOT iter.next (nm, ref) THEN EXIT; END;
      s.f := ref;
    END;

    (* failed... *)
    s.d := NIL;
    s.e := NIL;
    s.f := NIL;
    RETURN FALSE;
  END TableNext;

PROCEDURE NameTableNext (VAR s: Node.IteratorState): BOOLEAN =
  VAR
    nd   : Node.List;
    iter : IntRefTbl.Iterator;
    nms  : IntList.T;
    nm   : INTEGER;
    ref  : REFANY;
  BEGIN
    WHILE (s.f # NIL) OR (s.e # NIL) OR (s.d # NIL) DO
      IF (s.f # NIL) THEN
        (* return the next unit *)
        nd := s.f;  s.f := nd.tail;
        s.match := nd.head;
        RETURN TRUE;
      END;

      IF (s.d # NIL) THEN
        (* try the current name list *)
        nms := s.d;
        WHILE (nms # NIL) AND (s.f = NIL) DO
          nm := nms.head;  nms := nms.tail;  s.d := nms;
          IF BrowserDB.db.units.get (nm, ref) THEN
            s.f := ref;
          END;
        END;
      END;

      WHILE (s.e # NIL) AND (s.d = NIL) AND (s.f = NIL) DO
        iter := s.e;
        IF NOT iter.next (nm, ref) THEN s.e := NIL; EXIT; END;
        IF RegExpr.Match (s.pattern, ID.ToText (nm)) THEN
          s.d := ref;
        END;
      END;

    END; (*WHILE*)
    RETURN FALSE;
  END NameTableNext;

PROCEDURE TableEnumerate (root: Node.T;  tbl: IntRefTbl.T;
                          filter: FilterProc): Node.Set =
  VAR
    results : Node.Set;
    iter    := tbl.iterate ();
    nm      : INTEGER;
    ref     : REFANY;
    nd      : Node.List;
  BEGIN
    WHILE iter.next (nm, ref) DO
      nd := ref;
      WHILE (nd # NIL) DO
        IF (filter = NIL) OR filter (root, nd.head) THEN
          Node.Append (results, nd.head);
        END;
        nd := nd.tail;
      END;
    END;
    RETURN results;
  END TableEnumerate;

PROCEDURE GenTable (root: Node.T;  tbl: IntRefTbl.T;
                    filter: FilterProc;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR results := TableEnumerate (root, tbl, filter);
  BEGIN
    HTML.GenChoices (results, wx);
  END GenTable;

(*------------------------------------------------ package root nodes ---*)

TYPE
  RootRoot = Node.Named_T OBJECT
  OVERRIDES
    class      := RootClass;
    iterate    := RootRootIterate;
    next       := RootRootNext;
    gen_page   := RootRootPage;
  END;

PROCEDURE RootRootIterate (<*UNUSED*> self: RootRoot;  VAR s: Node.IteratorState) =
  BEGIN
    s.d := PkgRoot.First ();
  END RootRootIterate;

PROCEDURE RootRootNext (<*UNUSED*> self: RootRoot;
                        VAR s: Node.IteratorState): BOOLEAN =
  VAR nd: PkgRoot.T;
  BEGIN
    nd := s.d;
    WHILE (nd # NIL) DO
      s.d := nd.sibling;
      IF nd.match (s.pattern) THEN
        s.match := nd;
        RETURN TRUE;
      END;
      nd := nd.sibling;
    END;
    RETURN FALSE;
  END RootRootNext;

PROCEDURE RootRootPage (self: RootRoot;  wx: Wx.T;
                       action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR nd: PkgRoot.T;  results: Node.Set;
  BEGIN
    HTML.BeginXX (self, wx, "Package roots");
    GenScanWarning (wx);

    wx.put ("<P>\n<TABLE><TR>\n");
    GenButton ("./[rescan]", "Rescan", wx);
    GenButton ("/form/new-pkg/", "Create package", wx);
    wx.put ("</TR></TABLE>\n");

    IF (action = rescanID) THEN
      BrowserDB.Refresh (wx);
      action := viewID;
    ELSE
      nd := PkgRoot.First ();
      WHILE (nd # NIL) DO
        Node.Append (results, nd);
        nd := nd.sibling;
      END;
      HTML.GenChoices (results, wx);
    END;

    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END RootRootPage;

(*------------------------------------------------ package root nodes ---*)

TYPE
  PkgRoots = Node.Named_T OBJECT
(***
    build_class  : ClassDir.T;
    browse_class : ClassDir.T;
**)
  OVERRIDES
    class      := RootClass;
    iterate    := PkgRootIterate;
    next       := PkgRootNext;
    gen_page   := PkgRootPage;
  END;

(***
PROCEDURE AddClassEntries (t: PkgRoots) =
  BEGIN
    t.build_class := NEW (ClassDir.T,
                          name := Node.ClassID[Node.Class.BuildPackage],
                          kind := Node.Class.BuildPackage,
                          parent := t);
    t.browse_class := NEW (ClassDir.T,
                           name := Node.ClassID[Node.Class.BrowsePackage],
                           kind := Node.Class.BrowsePackage,
                           parent := t);
  END AddClassEntries;
***)

PROCEDURE PkgRootIterate (<*UNUSED*> self: PkgRoots;  VAR s: Node.IteratorState) =
  BEGIN
    TableIterate (BrowserDB.db.packages, s);
    s.a := 0; (* phase *)
    s.f := NIL;
  END PkgRootIterate;

PROCEDURE PkgRootNext (self: PkgRoots;  VAR s: Node.IteratorState): BOOLEAN =
  VAR root: Node.Named_T;
  BEGIN
    IF (s.a = 0) THEN
      IF TableNext (self, s, NIL) THEN RETURN TRUE; END;
      s.f := PkgRoot.First ();
      INC (s.a);
    END;
    IF (s.a = 1) THEN
      WHILE (s.f # NIL) DO
        root := s.f;  s.f := root.sibling;
        IF root.match (s.pattern) THEN
          s.match := root;
          RETURN TRUE;
        END;
      END;
      INC (s.a);
    END;
    RETURN FALSE;
  END PkgRootNext;

PROCEDURE PkgRootPage (self: PkgRoots;  wx: Wx.T;
                       action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, "Packages");
    GenScanWarning (wx);

    wx.put ("<P>\n<TABLE><TR>\n");
    GenButton ("./[rescan]", "Rescan", wx);
    GenButton ("/form/new-pkg/", "Create package", wx);
    wx.put ("</TR></TABLE>\n");

    IF (action = rescanID) THEN
      BrowserDB.Refresh (wx);
      action := viewID;
    ELSE
      GenTable (self, BrowserDB.db.packages, NIL, wx);
    END;

    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END PkgRootPage;

PROCEDURE GenButton (url, label: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wx.put ("<TD><FORM method=get action=\"", url, "\">");
    wx.put ("<INPUT TYPE=submit VALUE=\"", label, "\"></FORM></TD>\n");
  END GenButton;

(*-------------------------------------------------- fixed root nodes ---*)

TYPE
  FixedRoot = Node.Named_T OBJECT
  OVERRIDES
    class      := RootClass;
    iterate    := FixedRootIterate;
    next       := FixedRootNext;
    gen_page   := FixedRootPage;
  END;

PROCEDURE FixedRootIterate (<*UNUSED*> self: FixedRoot;
                            VAR s: Node.IteratorState) =
  BEGIN
    s.d := RegExpr.SimpleString (s.pattern);
  END FixedRootIterate;

PROCEDURE FixedRootNext (<*UNUSED*> self: FixedRoot;
                         VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    IF (s.d # NIL) THEN
      s.match := Fixed.Find (s.d);
      s.d := NIL;
      IF (s.match # NIL) THEN RETURN TRUE; END;
    END;
    RETURN FALSE;
  END FixedRootNext;

PROCEDURE FixedRootPage (self: FixedRoot;  wx: Wx.T;
                         action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, "Fixed resources");
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END FixedRootPage;

(*-------------------------------------------------- user root nodes ---*)

TYPE
  UserRoot = Node.Named_T OBJECT
    home  : TEXT;
    root  : FileDir.T := NIL;
  OVERRIDES
    class      := RootClass;
    iterate    := UserRootIterate;
    next       := UserRootNext;
    gen_page   := UserRootPage;
  END;

PROCEDURE UserRootIterate (self: UserRoot;
                           VAR s: Node.IteratorState) =
  VAR user_home := ConfigItem.X [ConfigItem.T.Homepage].text;
  BEGIN
    IF (user_home # NIL) AND Text.Length (user_home) > 0 THEN
      IF (self.home = NIL) OR NOT OS.FileNameEq (user_home, self.home) THEN
        (* we have a new root *)
        self.home := user_home;
        self.root := NEW (FileDir.T, name := ID.Add ("user"),
                           path := Pathname.Prefix (user_home));
      END;
    ELSE
      self.root := NIL;
    END;
    IF (self.root # NIL) THEN self.root.iterate (s); END;
  END UserRootIterate;

PROCEDURE UserRootNext (self: UserRoot;
                        VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    IF (self.root # NIL)
      THEN RETURN self.root.next (s);
      ELSE RETURN FALSE;
    END;
  END UserRootNext;

PROCEDURE UserRootPage (self: UserRoot;  wx: Wx.T;
                         action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (self.root # NIL) THEN
      self.root.gen_page (wx, action, data);
    ELSE
      HTML.BeginXX (self, wx, "User pages");
      HTML.ViewOnly (action, data, wx);
      HTML.End (wx);
    END;
  END UserRootPage;

(*---------------------------------------------- type name root nodes ---*)

TYPE
  TNameRoot = Node.Named_T OBJECT
  OVERRIDES
    class      := RootClass;
    iterate    := TNameRootIterate;
    next       := TNameRootNext;
    gen_page   := TNameRootPage;
  END;

PROCEDURE TNameRootIterate (<*UNUSED*> self: TNameRoot;
                            VAR s: Node.IteratorState) =
  BEGIN
    TableIterate (BrowserDB.db.type_names, s);
  END TNameRootIterate;

PROCEDURE TNameRootNext (self: TNameRoot;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN TableNext (self, s, NIL);
  END TNameRootNext;

PROCEDURE TNameRootPage (self: TNameRoot;  wx: Wx.T;
                         action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, "Types");
    GenScanWarning (wx);
    GenTable (self, BrowserDB.db.type_names, NIL, wx);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END TNameRootPage;

(*----------------------------------------------- type UID root nodes ---*)

TYPE
  TUIDRoot = Node.Named_T OBJECT
  OVERRIDES
    class      := RootClass;
    iterate    := TUIDRootIterate;
    next       := TUIDRootNext;
    gen_page   := TUIDRootPage;
  END;

PROCEDURE TUIDRootIterate (<*UNUSED*> self: TUIDRoot;  VAR s: Node.IteratorState) =
  VAR txt := RegExpr.SimpleString (s.pattern);
  BEGIN
    IF (txt # NIL) THEN
      IF NOT BrowserDB.db.types.get (LexMisc.ScanUID (txt), s.d) THEN
        s.d := NIL;
      END;
      s.e := NIL;
    ELSE
      s.d := NIL;
      s.e := BrowserDB.db.types.iterate ();
    END;
  END TUIDRootIterate;

PROCEDURE TUIDRootNext (<*UNUSED*> self: TUIDRoot;
                        VAR s: Node.IteratorState): BOOLEAN =
  VAR
    iter : IntRefTbl.Iterator;
    uid  : INTEGER;
    ref  : REFANY;
    info : Type.Info;
  BEGIN
    IF (s.d # NIL) THEN
      info := s.d;  s.d := NIL;
      IF (info.names # NIL)
        THEN s.match := info.names;
        ELSE s.match := NEW (Type.T, uid := info.uid);
      END;
      RETURN TRUE;
    END;

    IF (s.e # NIL) THEN
      iter := s.e;
      WHILE iter.next (uid, ref) DO
        IF RegExpr.Match (s.pattern, LexMisc.FmtUID (uid)) THEN
          info := ref;
          IF (info.names # NIL) THEN
            s.match := info.names;
            RETURN TRUE;
          END;
        END;
      END;
      s.e := NIL;
    END;

    RETURN FALSE;
  END TUIDRootNext;

PROCEDURE TUIDRootPage (self: TUIDRoot;  wx: Wx.T;
                        action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, "Types");
    GenScanWarning (wx);
    GenTable (self, BrowserDB.db.type_names, NIL, wx);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END TUIDRootPage;

(*------------------------------------------------- source root nodes ---*)

TYPE
  SourceRoot = Node.Named_T OBJECT
    kind : Node.Class;
  OVERRIDES
    class      := RootClass;
    iterate    := SourceRootIterate;
    next       := SourceRootNext;
    gen_page   := SourceRootPage;
  END;

PROCEDURE SourceRootIterate (<*UNUSED*> self: SourceRoot;
                             VAR s: Node.IteratorState) =
  BEGIN
    TableIterate (BrowserDB.db.units, s);
  END SourceRootIterate;

PROCEDURE SourceRootNext (self: SourceRoot;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN TableNext (self, s, SourceFilter);
  END SourceRootNext;

PROCEDURE SourceFilter (root, node: Node.T): BOOLEAN =
  VAR self: SourceRoot := root;
  BEGIN
    RETURN (self.kind = node.class ());
  END SourceFilter;

PROCEDURE SourceRootPage (self: SourceRoot;  wx: Wx.T;
                          action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, Node.ClassPlural [self.kind]);
    GenScanWarning (wx);
    GenTable (self, BrowserDB.db.units, SourceFilter, wx);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END SourceRootPage;

(*--------------------------------------------------- unit root nodes ---*)

TYPE
  UnitRoot = Node.Named_T OBJECT
    pgm : BOOLEAN;
  OVERRIDES
    class      := RootClass;
    iterate    := UnitRootIterate;
    next       := UnitRootNext;
    gen_page   := UnitRootPage;
  END;

PROCEDURE UnitRootIterate (<*UNUSED*> self: UnitRoot;
                           VAR s: Node.IteratorState) =
  BEGIN
    TableIterate (BrowserDB.db.units, s);
  END UnitRootIterate;

PROCEDURE UnitRootNext (self: UnitRoot;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN TableNext (self, s, NIL);
  END UnitRootNext;

PROCEDURE UnitRootPage (self: UnitRoot;  wx: Wx.T;
                        action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, "Source units");
    GenScanWarning (wx);
    GenTable (self, BrowserDB.db.units, NIL, wx);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END UnitRootPage;

(*------------------------------------------------- import root nodes ---*)

TYPE
  ImportRoot = Node.Named_T OBJECT
  OVERRIDES
    class      := RootClass;
    iterate    := ImportRootIterate;
    next       := ImportRootNext;
    gen_page   := ImportRootPage;
  END;

PROCEDURE ImportRootIterate (<*UNUSED*> self: ImportRoot;
                             VAR s: Node.IteratorState) =
  BEGIN
    TableIterate (BrowserDB.db.importers, s);
  END ImportRootIterate;

PROCEDURE ImportRootNext (<*UNUSED*> self: ImportRoot;
                          VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN NameTableNext (s);
  END ImportRootNext;

PROCEDURE ImportRootPage (self: ImportRoot;  wx: Wx.T;
                          action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, "Interface importers");
    GenScanWarning (wx);
    GenTable (self, BrowserDB.db.units, ImportFilter, wx);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END ImportRootPage;

PROCEDURE ImportFilter (<*UNUSED*> root: Node.T;  node: Node.T): BOOLEAN =
  VAR c := node.class ();
  BEGIN
    (* assume any M3 source is an importer *)
    RETURN (Node.Class.Interface <= c)
       AND (c <= Node.Class.GenericModule);
  END ImportFilter;

(*------------------------------------------------- export root nodes ---*)

TYPE
  ExportRoot = Node.Named_T OBJECT
  OVERRIDES
    class      := RootClass;
    iterate    := ExportRootIterate;
    next       := ExportRootNext;
    gen_page   := ExportRootPage;
  END;

PROCEDURE ExportRootIterate (<*UNUSED*> self: ExportRoot;
                             VAR s: Node.IteratorState) =
  BEGIN
    TableIterate (BrowserDB.db.exporters, s);
  END ExportRootIterate;

PROCEDURE ExportRootNext (<*UNUSED*> self: ExportRoot;
                          VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN NameTableNext (s);
  END ExportRootNext;

PROCEDURE ExportRootPage (self: ExportRoot;  wx: Wx.T;
                          action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, "Interface exporters");
    GenScanWarning (wx);
    GenTable (self, BrowserDB.db.units, ExportFilter, wx);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END ExportRootPage;

PROCEDURE ExportFilter (<*UNUSED*> root: Node.T;  node: Node.T): BOOLEAN =
  BEGIN
    RETURN node.class () = Node.Class.Module;
  END ExportFilter;

(*------------------------------------------------ derived root nodes ---*)

TYPE
  DerivedRoot = Node.Named_T OBJECT
    pgm : BOOLEAN;
  OVERRIDES
    class      := RootClass;
    iterate    := DerivedRootIterate;
    next       := DerivedRootNext;
    gen_page   := DerivedRootPage;
  END;

PROCEDURE DerivedRootIterate (self: DerivedRoot;  VAR s: Node.IteratorState) =
  BEGIN
    IF self.pgm
      THEN TableIterate (BrowserDB.db.pgms, s);
      ELSE TableIterate (BrowserDB.db.libs, s);
    END;
  END DerivedRootIterate;

PROCEDURE DerivedRootNext (self: DerivedRoot;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN TableNext (self, s, NIL);
  END DerivedRootNext;

PROCEDURE DerivedRootPage (self: DerivedRoot;  wx: Wx.T;
                           action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  CONST Map = ARRAY BOOLEAN OF NC { NC.Library, NC.Program };
  BEGIN
    HTML.BeginXX (self, wx, Node.ClassPlural [Map [self.pgm]]);
    GenScanWarning (wx);
    IF self.pgm
      THEN GenTable (self, BrowserDB.db.pgms, NIL, wx);
      ELSE GenTable (self, BrowserDB.db.libs, NIL, wx);
    END;
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END DerivedRootPage;

(*-------------------------------------------- build cache root nodes ---*)

TYPE
  CacheRoot = Node.Named_T OBJECT
  OVERRIDES
    class      := RootClass;
    iterate    := CacheRootIterate;
    next       := CacheRootNext;
    gen_page   := CacheRootPage;
  END;

PROCEDURE CacheRootIterate (<*UNUSED*> self: CacheRoot;
                            VAR s: Node.IteratorState) =
  VAR c := BuildCache.cache;
  BEGIN
    IF (c # NIL)
      THEN s.a := 1; TableIterate (c, s);
      ELSE s.a := 0;
    END;
  END CacheRootIterate;

PROCEDURE CacheRootNext (self: CacheRoot;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    IF (s.a # 0) THEN RETURN TableNext (self, s, NIL); END;
    RETURN FALSE;
  END CacheRootNext;

PROCEDURE CacheRootPage (self: CacheRoot;  wx: Wx.T;
                         action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.BeginXX (self, wx, "Build cache");
    GenScanWarning (wx);
    IF (BuildCache.cache # NIL) THEN
      GenTable (self, BuildCache.cache, NIL, wx);
    END;
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END CacheRootPage;

(*------------------------------------------ documentation root nodes ---*)

TYPE
  DocumentRoot = Dir.T OBJECT
    base  : TEXT;
    title : TEXT;
    tbl   : IntRefTbl.T := NIL;
  OVERRIDES
    class      := RootClass;
    iterate    := DocumentRootIterate;
    next       := DocumentRootNext;
    gen_page   := DocumentRootPage;
  END;

PROCEDURE DocumentRootIterate (self: DocumentRoot;
                            VAR s: Node.IteratorState) =
  BEGIN
    IF self.tbl = NIL THEN ScanDocDir (self); END;
    TableIterate (self.tbl, s);
  END DocumentRootIterate;

PROCEDURE DocumentRootNext (self: DocumentRoot;
                            VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN TableNext (self, s, NIL);
  END DocumentRootNext;

PROCEDURE DocumentRootPage (self: DocumentRoot;  wx: Wx.T;
                         action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ref: REFANY;
  BEGIN
    IF self.tbl = NIL THEN ScanDocDir (self); END;
    IF self.tbl.get (ID.Add ("index.html"), ref) THEN
      NARROW (ref, Node.List).head.gen_page (wx, action, data);
    ELSE
      HTML.BeginXX (self, wx, self.title);
      GenTable (self, self.tbl, NIL, wx);
      HTML.ViewOnly (action, data, wx);
      HTML.End (wx);
    END;
  END DocumentRootPage;

PROCEDURE ScanDocDir (self: DocumentRoot) =
  VAR
    root := OS.MakePath (Default.doc_root, self.base);
    d: Dir.T;
    n: Node.Named_T;
  BEGIN
    self.scanned  := OS.Now ();
    self.contents := NIL;
    self.tbl      := NEW (IntRefTbl.Default).init ();

    IF OS.IsDirectory (root) THEN
      d := ScanDir (self.base, root);
      self.contents := d.contents;
    END;

    (* steal the directory's contents away & build the table *)
    n := self.contents;
    WHILE (n # NIL) DO
      n.parent := self;
      EVAL self.tbl.put (n.name, NEW (Node.List, head := n, tail := NIL));
      n := n.sibling;
    END;
  END ScanDocDir;

PROCEDURE ScanDir (dir_name, dir_path: TEXT): Dir.T =
  VAR iter: FS.Iterator;  nm, path: TEXT;   self: Dir.T;  n: Node.Named_T;
  BEGIN
    self := NEW (Dir.T, name := ID.Add (dir_name), contents := NIL);
    TRY
      iter := FS.Iterate (dir_path);
      TRY
        WHILE iter.next (nm) DO
          path := OS.MakePath (dir_path, nm);
          IF OS.IsDirectory (path)
            THEN n := ScanDir (nm, path);
            ELSE n := NEW (FileNode.T, name := ID.Add (nm), path := path);
          END;
          n.parent := self;
          n.sibling := self.contents;
          self.contents := n;
        END;
      FINALLY
        iter.close ();
      END;
    EXCEPT OSError.E (ec) =>
      ErrLog.Msg ("trouble scanning document directory: ",
                   dir_path, OS.Err (ec));
    END;
    RETURN self;
  END ScanDir;

(*----------------------------------------------- example root nodes ---*)

TYPE
  ExamplesRoot = Node.Named_T OBJECT
    root     : TEXT    := NIL;
    contents : Example := NIL;
  OVERRIDES
    class      := RootClass;
    iterate    := ExamplesRootIterate;
    next       := ExamplesRootNext;
    gen_page   := ExamplesRootPage;
  END;

  Example = REF RECORD
    name : TEXT    := NIL;
    node : Node.T  := NIL;
    next : Example := NIL;
  END;

PROCEDURE ExamplesRootIterate (self: ExamplesRoot;
                            VAR s: Node.IteratorState) =
  BEGIN
    IF self.root = NIL THEN ScanExamples (self); END;
    s.d := self.contents;
  END ExamplesRootIterate;

PROCEDURE ExamplesRootNext (self: ExamplesRoot;
                            VAR s: Node.IteratorState): BOOLEAN =
  VAR ex: Example;
  BEGIN
    WHILE s.d # NIL DO
      ex := s.d;  s.d := ex.next;
      IF RegExpr.Match (s.pattern, ex.name) THEN
        IF (ex.node # NIL)
          THEN s.match := ex.node;
          ELSE s.match := FindExamplePkg (self.root, ex);
        END;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END ExamplesRootNext;

PROCEDURE ExamplesRootPage (self: ExamplesRoot;  wx: Wx.T;
                         action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ex: Example;
  BEGIN
    IF self.root = NIL THEN ScanExamples (self); END;
    ex := self.contents;
    WHILE (ex # NIL)
      AND (NOT OS.FileNameEq (ex.name, "index.html"))
      AND (NOT OS.FileNameEq (ex.name, "index.htm")) DO
      ex := ex.next;
    END;
    IF (ex # NIL) THEN
      ex.node.gen_page (wx, action, data);
    ELSE
      HTML.BeginXX (self, wx, "CM3-IDE examples");
      wx.put ("<STRONG>The CM3-IDE examples are missing.</STRONG>\n");
      HTML.ViewOnly (action, data, wx);
      HTML.End (wx);
    END;
  END ExamplesRootPage;

PROCEDURE ScanExamples (self: ExamplesRoot) =
  VAR ex: Example;  iter: FS.Iterator;  nm, path: TEXT;
  BEGIN
    self.root := Default.example_root;
    TRY
      iter := FS.Iterate (self.root);
      TRY
        WHILE iter.next (nm) DO
          path := OS.MakePath (self.root, nm);
          ex := NEW (Example, next := self.contents, name := nm);
          self.contents := ex;
          IF NOT OS.IsDirectory (path) THEN
            (* convert non-directories into simple file nodes *)
            ex.node := NEW (FileNode.T, name := ID.Add (nm), path := path,
                            parent := self);
          END;
        END;
      FINALLY
        iter.close ();
      END;
    EXCEPT OSError.E (ec) =>
      ErrLog.Msg ("trouble scanning example directory: ",
                   self.root, OS.Err (ec));
    END;
  END ScanExamples;

PROCEDURE FindExamplePkg (root: TEXT;  ex: Example): Node.T =
  VAR pkg_root: PkgRoot.T;  n: Node.Named_T;
  BEGIN
    IF (Default.user_home = NIL) THEN
      (* There's no HOME root.  We can't auto-clone examples. *)
      ex.node := ScanDir (ex.name, OS.MakePath (root, ex.name));
      RETURN ex.node;
    END;

    (* find the package root that corresponds to the user's "home" *)
    pkg_root := PkgRoot.First ();
    LOOP
      IF (pkg_root = NIL) THEN
        (* didn't find it!  We can't auto-clone examples. *)
        ex.node := ScanDir (ex.name, OS.MakePath (root, ex.name));
        RETURN ex.node;
      END;
      IF OS.FileNameEq (pkg_root.path, Default.user_home) THEN
        EXIT;
      END;
      pkg_root := pkg_root.sibling;
    END;

    (* find the named package in that root *)
    n := pkg_root.contents;
    WHILE (n # NIL) DO
      TYPECASE n OF
      | Pkg.T (pkg) => 
          IF OS.FileNameEq (ID.ToText (pkg.name), ex.name) THEN
            RETURN pkg;
          END;
      ELSE (* skip *)
      END;
      n := n.sibling;
    END;

    (* no matching package found => clone the example directory
       and return it as a newly created package.  *)
    OS.CopyDirectory (OS.MakePath (root, ex.name),
                      OS.MakePath (pkg_root.path, ex.name));
    TRY
      RETURN Pkg.Rescan (NEW (Pkg.T, name := ID.Add (ex.name), parent := pkg_root));
    EXCEPT Thread.Alerted =>
      RETURN ExampleRoot;
    END;
  END FindExamplePkg;

(*------------------------------------------------- console log node ---*)

TYPE
  LogRoot = Node.Named_T OBJECT
  OVERRIDES
    class      := RootClass;
    iterate    := LogRootIterate;
    next       := LogRootNext;
    gen_page   := LogRootPage;
  END;

PROCEDURE LogRootIterate (<*UNUSED*> self: LogRoot;
                          <*UNUSED*> VAR s: Node.IteratorState) =
  BEGIN
  END LogRootIterate;

PROCEDURE LogRootNext (<*UNUSED*> self: LogRoot;
                       <*UNUSED*> VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END LogRootNext;

PROCEDURE LogRootPage (self: LogRoot;  wx: Wx.T;
                         action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  CONST Title = "CM3-IDE console log: ";
  VAR now := "<TT>" & FmtTime.Short (OS.FileToM3Time (OS.Now ())) & "</TT>";
  BEGIN
    HTML.BeginXX (self, wx, Title, now);
    wx.put ("<A NAME=\"HEAD\"><HR></A>\n<PRE>\n");
    LOCK ErrLog.log_mu DO
      FOR i := ErrLog.log_head - ErrLog.log_len TO ErrLog.log_head - 1 DO
        IF (i < 0)
          THEN wx.put (ErrLog.log[i + NUMBER (ErrLog.log)], "\n");
          ELSE wx.put (ErrLog.log[i], "\n");
        END;
      END;
    END;
    wx.put ("</PRE>\n<A NAME=\"TAIL\"><HR></A>\n");
    wx.put ("<H2>", Title, now, "</H2>\n");
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END LogRootPage;

BEGIN
END Roots.
