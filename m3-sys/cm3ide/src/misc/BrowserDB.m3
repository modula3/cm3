(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)
(*                                                               *)
(* Also (C) 1992, Digital Equipment Corporation                  *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Tue Mar  7 14:38:18 PST 1995 by kalsow       *)
(*                                                               *)
(* Enhanced by Peter Klein (pk@i3.informatik.rwth-aachen.de) to  *)
(* parse procedure signatures and connect procedure declarations *)
(* in interfaces with their implmentations. -  Mar 7, 1995       *)

MODULE BrowserDB;

IMPORT FmtTime, FS, IntRefTbl, IntList, OSError;
IMPORT Pathname, Rd, RTCollector, Text, TextRd, Thread, Wr;

(* defined in this package... *)
IMPORT Default, ClassDir, ConfigItem, Derived, Dir, ErrLog, HTML, ID;
IMPORT Loc, Node, OS, Pkg, PkgRoot, Source, Type, Wx;

FROM LexMisc IMPORT ReadUID, SkipBlanks, FmtUID;

TYPE SK = Source.Kind;

(*------------------------------------------------------ initialization ---*)

PROCEDURE Init () =
  (* create an initial, empty database *)
  BEGIN
    InitDB (db);
  END Init;

(*------------------------------------------------------ periodic refresh ---*)

VAR
  mu := NEW (MUTEX);
  refresh_busy := FALSE;

PROCEDURE Refresh (wx: Wx.T := NIL)
  RAISES {Thread.Alerted} =
  VAR
    now  := OS.Now ();
    date := FmtTime.Short (OS.FileToM3Time (now));
  BEGIN
    IF (n_updates < 1) THEN
      (* give the browser a chance to start... *)
      Thread.Pause (5.0D0);
    END;
    LOCK mu DO
      IF NOT refresh_busy THEN
        ErrLog.Msg ("Scanning Packages: ", date, "...");
        refresh_busy := TRUE;
        TRY
          ScanPackages (wx, now);
        FINALLY
          INC (n_updates);
          last_update  := now;
          refresh_busy := FALSE;
          RTCollector.Collect ();
          ErrLog.Msg ("scan done: ", FmtTime.Short (OS.FileToM3Time (OS.Now())));
        END;
      ELSE
        ErrLog.Msg ("still scanning: ", date, "...");
      END;
    END;
  END Refresh;

(*--------------------------------------------------- single root scan ---*)

VAR
  export_mu := NEW (MUTEX);

PROCEDURE ScanRoot (pkg_root: PkgRoot.T;  wx: Wx.T)
  RAISES {Thread.Alerted} =
  VAR s: Scan;
  BEGIN
    s.now         := OS.Now ();
    s.wx          := wx;
    s.n_pkgs      := 0;
    s.n_unit_refs := 0;
    s.unit_refs   := NEW (Derived.NodeRefSet, 100);
    s.pkg_root    := pkg_root;

    LOCK export_mu DO
      s.new := db;  (* start with the current database! *)
      ScanRepository (s);
      db := s.new;
    END;

    (* make sure the collector has a chance *)
    ResetDB (s.new);
  END ScanRoot;

(*--------------------------------------------------- single package scan ---*)

PROCEDURE ScanOne (nm: TEXT;  pkg_root: PkgRoot.T;  wx: Wx.T)
  RAISES {Thread.Alerted} =
  VAR s: Scan;  path: TEXT;
  BEGIN
    s.now         := OS.Now ();
    s.wx          := wx;
    s.n_pkgs      := 0;
    s.n_unit_refs := 0;
    s.unit_refs   := NEW (Derived.NodeRefSet, 100);
    s.pkg_root    := pkg_root;
    s.cur_pkg     := ID.Add (nm);

    path := OS.MakePath (s.pkg_root.path, nm);

    LOCK export_mu DO
      s.new := db;  (* start with the current database! *)
      IF OS.IsDirectory (path) THEN ScanPkg (s, path); END;
      db := s.new;
    END;

    (* make sure the collector has a chance *)
    ResetDB (s.new);
  END ScanOne;

(*------------------------------------------------------- package browser ---*)

TYPE
  Scan = RECORD
    now         : OS.FileTime;
    wx          : Wx.T;
    new         : DataBase;
    n_pkgs      : CARDINAL;
    pkg_root    : PkgRoot.T;
    cur_pkg     : ID.T;
    n_unit_refs : INTEGER;
    unit_refs   : Derived.NodeRefSet;
  END;

PROCEDURE ScanPackages (wx: Wx.T := NIL;  now: OS.FileTime)
  RAISES {Thread.Alerted} =
  VAR s: Scan;
  BEGIN
    s.now     := now;
    s.wx      := wx;
    s.n_pkgs  := 0;
    s.n_unit_refs := 0;
    s.unit_refs := NEW (Derived.NodeRefSet, 100);
    InitDB (s.new);
    AddBuiltinTypes (s);

    IF (n_updates < 1) THEN
      (* this is the first database load, might as well let the
         user see stuff as soon as we have it....  *)
      db := s.new;
    END;

    s.pkg_root := PkgRoot.First ();
    WHILE (s.pkg_root # NIL) DO
      ScanRepository (s);
      s.pkg_root := s.pkg_root.sibling;
    END;

    LOCK export_mu DO
      db := s.new;  (* export the new database *)
    END;
    Thread.Pause (0.5D0);
    OutWx (s, "<P>Package scan completed.\n");

    (* make sure the collector has a chance *)
    ResetDB (s.new);
  END ScanPackages;

PROCEDURE ScanRepository (VAR s: Scan)
  RAISES {Thread.Alerted} = 
  VAR iter: FS.Iterator;  nm, path: TEXT;
  BEGIN
    OutWx (s, "<H2>");
    IF (s.wx # NIL) THEN
      TRY HTML.PutImg (Node.ClassIcon[s.pkg_root.class()], s.wx);
      EXCEPT Wr.Failure =>  s.wx := NIL;
      END;
    END;
    OutWx (s, " \"", s.pkg_root.printname(), "\" root");
    OutWx (s, "  (", s.pkg_root.path, ")</H2>\n<PRE>\n");

    s.n_pkgs := 0;
    TRY
      iter := FS.Iterate (s.pkg_root.path);
      TRY
        WHILE iter.next (nm) DO
          path := OS.MakePath (s.pkg_root.path, nm);
          IF NOT Text.Equal(nm, ".svn") AND OS.IsDirectory (path) THEN
            s.cur_pkg := ID.Add (nm);
            ScanPkg (s, path);
          END;
        END;
      FINALLY
        iter.close ();
      END;
    EXCEPT OSError.E (ec) =>
      ErrLog.Msg ("trouble scanning packages in ",
                  s.pkg_root.path, OS.Err (ec));
    END;
    IF (s.n_pkgs > 0) THEN OutWx (s, "\n"); END;

    OutWx (s, "</PRE>\n<HR>\n");
  END ScanRepository;

(*----------------------------------------------------------- packages ---*)

PROCEDURE ScanPkg (VAR s: Scan;  path: TEXT)
  RAISES {Thread.Alerted} =
  VAR old_pkg, new_pkg: Pkg.T;
  BEGIN
    new_pkg := FindPkg (s.new.packages, s.cur_pkg, s.pkg_root);
    IF (new_pkg = NIL) THEN
      old_pkg := FindPkg (db.packages, s.cur_pkg, s.pkg_root);
      IF (old_pkg = NIL) OR PkgChanged (old_pkg, path)
        THEN new_pkg := ScanNewPkg (s, path);
        ELSE new_pkg := old_pkg;
      END;
      AddNewPkg (s, new_pkg);
      AddPkgNames (s, new_pkg);
      NotePkg (s, new_pkg);
      IF (n_updates > 0) THEN
        Thread.Pause (0.2d0); (* try not to swamp the file system *)
      END;
    ELSIF (s.new.packages = db.packages) THEN
      (* we're scanning directly into the existing database! *)
      IF PkgChanged (new_pkg, path) THEN
        new_pkg := ScanNewPkg (s, path);
        AddNewPkg (s, new_pkg);
      END;
      AddPkgNames (s, new_pkg);
      NotePkg (s, new_pkg);
      IF (n_updates > 0) THEN
        Thread.Pause (0.2d0); (* try not to swamp the file system *)
      END;
    END;
  END ScanPkg;

PROCEDURE PkgChanged (old: Pkg.T;  path: TEXT): BOOLEAN =
  BEGIN
    RETURN DirChanged (old, path, in_src_dir := FALSE);
  END PkgChanged;

PROCEDURE ScanNewPkg (VAR s: Scan;  path: TEXT): Pkg.T
  RAISES {Thread.Alerted} =
  VAR pkg := NEW (Pkg.T, name := s.cur_pkg, parent := s.pkg_root);
  BEGIN
    ScanNewDir (s, pkg, path, in_src_dir := FALSE);
    RETURN pkg;
  END ScanNewPkg;

PROCEDURE FindPkg (pkgs: IntRefTbl.T;  nm: ID.T;  root: PkgRoot.T): Pkg.T =
  VAR ref: REFANY;  nd: Node.List;  pkg: Pkg.T;
  BEGIN
    IF pkgs.get (nm, ref) THEN
      nd := ref;
      WHILE (nd # NIL) DO
        pkg := nd.head;
        IF (pkg.parent = root) THEN RETURN pkg; END;
        nd := nd.tail;
      END;
    END;
    RETURN NIL;
  END FindPkg;

PROCEDURE AddNewPkg (VAR s: Scan;  pkg: Pkg.T) =
  VAR ref: REFANY;  nd: Node.List;  p: Pkg.T;
  BEGIN
    IF s.new.packages.get (pkg.name, ref) THEN
      nd := ref;
      WHILE (nd # NIL) DO
        p := nd.head;
        IF (p = pkg) THEN RETURN; END;
        IF (p.parent = pkg.parent) THEN
          nd.head := pkg;
          ReplacePkg (s.pkg_root, p, pkg);
          RETURN;
        END;
        nd := nd.tail;
      END;
      (* no match => splice the new guy into the existing list *)
      nd := ref;
      nd.tail := NEW (Node.List, head := pkg, tail := nd.tail);
    ELSE
      EVAL s.new.packages.put (pkg.name, NEW (Node.List, head := pkg, tail := NIL));
    END;
    ReplacePkg (s.pkg_root, NIL, pkg);
  END AddNewPkg;

PROCEDURE ReplacePkg (root: PkgRoot.T;  old, new: Pkg.T) =
  VAR n := root.contents;  last_n: Node.Named_T := NIL;
  BEGIN
    WHILE (n # NIL) DO
      IF (n = new) THEN
        (* this root already has the new package. *)
        RETURN;
      END;
      IF (n = old) THEN
        new.sibling := old.sibling;
        IF last_n = NIL
          THEN root.contents := new;
          ELSE last_n.sibling := new;
        END;
        RETURN;
      END;
      last_n := n;
      n := n.sibling;
    END;
    new.sibling := root.contents;
    root.contents := new;
  END ReplacePkg;

PROCEDURE NotePkg (VAR s: Scan;  pkg: Pkg.T)
  RAISES {Thread.Alerted} =
  VAR
    nm  := ID.ToText (pkg.name);
    len := Text.Length (nm);
  BEGIN
    IF ConfigItem.X [ConfigItem.T.Verbose_log].bool THEN
      ErrLog.Msg ("scanned: ", nm);
    END;
    OutWx (s, "<a href=\"/", ID.ToText (pkg.parent.arcname()));
    OutWx (s, "/", nm, "\">");
    IF len > MaxPad THEN
      nm  := Text.Sub (nm, 0, MaxPad-3) & "...";
      len := MaxPad;
    END;
    OutWx (s, nm, "</A> ", pad := MaxPad - len);
    INC (s.n_pkgs);
    IF (s.n_pkgs = 4) THEN 
      OutWx  (s, "\n");
      s.n_pkgs := 0;
    END;
  END NotePkg;

(*------------------------------------------------- directories ---*)

PROCEDURE DirChanged (old: Dir.T;  path: TEXT;  in_src_dir: BOOLEAN): BOOLEAN =
  VAR n: Node.Named_T;  file: TEXT;
  BEGIN
    IF (old = NIL) OR (OS.LastModified (path) > old.scanned) THEN
      RETURN TRUE;
    END;

    in_src_dir := in_src_dir OR OS.FileNameEq ("src", ID.ToText (old.name));

    IF NOT Default.on_unix THEN
      (* Windows doesn't update the directory's timestamp when
         files are added or deleted, so we need to explicitly
         rescan the directory.  Damn Windows. *)
      IF DirContentsChanged (old, path, in_src_dir) THEN RETURN TRUE; END;
    END;

    (* if we got this far, the set of names within this
       directory have not changed since the last scan.
       We just need to make sure they still point to
       the right stuff. *)

    n := old.contents;
    WHILE (n # NIL) DO
      TYPECASE n OF
      | Dir.T (x) =>
          file := OS.MakePath (path, ID.ToText (x.name));
          IF DirChanged (x, file, in_src_dir) THEN  RETURN TRUE;  END;
      | Derived.T (x) =>
          IF DerivedChanged (x, path) THEN  RETURN TRUE;  END;
      ELSE (* skip *)
      END;
      n := n.sibling;
    END;

    (* nothing changed! *)
    RETURN FALSE;
  END DirChanged;

TYPE
  DirNames = REF ARRAY OF TEXT;

PROCEDURE DirContentsChanged (dir: Dir.T;  path: TEXT;
                              in_src_dir: BOOLEAN): BOOLEAN =
  VAR
    iter    : FS.Iterator;
    file    : TEXT;
    names   : DirNames;
    n_names : INTEGER    := MapNames (dir, names);
    i       : CARDINAL;
  BEGIN
    TRY
      iter := FS.Iterate (path);
      TRY
        WHILE iter.next (file) DO
          i := 0;
          LOOP
            IF (i >= n_names) THEN
              (* we found a new name... *)
              IF IsSourceName (path, file, in_src_dir) THEN
                RETURN TRUE;
              END;
              EXIT;
            ELSIF OS.FileNameEq (names[i], file) THEN
              (* we found a match => delete this name and try the next file *)
              DEC (n_names);
              names [i] := names [n_names];
              names [n_names] := NIL;
              EXIT;
            END;
            INC (i);
          END; (*LOOP*)
        END; (* WHILE iter.next *)
      FINALLY
        iter.close ();
      END;
    EXCEPT OSError.E (ec) =>
      ErrLog.Msg ("trouble scanning directory ", path, OS.Err (ec));
    END;

    RETURN (n_names > 0);
  END DirContentsChanged;

PROCEDURE MapNames (dir: Dir.T;  VAR names: DirNames): CARDINAL =
  VAR cnt := 0;  n: Node.Named_T;
  BEGIN
    (* count the entries that correspond to real directory entries *)
    n := dir.contents;
    LOOP
      TYPECASE n OF
      | NULL => EXIT;
      | ClassDir.T => (* skip this pseudo entry *)
      ELSE INC (cnt);
      END;
      n := n.sibling;
    END;

    names := NEW (DirNames, cnt);

    (* finally, map the entries that correspond to real directory entries *)
    n := dir.contents;  cnt := 0;
    LOOP
      TYPECASE n OF
      | NULL => EXIT;
      | ClassDir.T => (* skip this pseudo entry *)
      ELSE names [cnt] := ID.ToText (n.name);  INC (cnt);
      END;
      n := n.sibling;
    END;

    <*ASSERT cnt = NUMBER (names^) *>
    RETURN cnt;
  END MapNames;

CONST
  KnownExts = ARRAY [0..15] OF TEXT {
    "i3", "m3", "ig", "mg", "c", "h", "tmpl",
    "NO.MISC.EXTENSION", "io", "mo", "o", "obj",
    "lib", "a", "exe", "bak" };

  IsDirFile = ARRAY [0..15] OF BOOLEAN {
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE,
    TRUE, TRUE, TRUE, FALSE };

PROCEDURE IsSourceName (path, file: TEXT;  in_src_dir: BOOLEAN): BOOLEAN =
  VAR ext: TEXT;
  BEGIN
    (* derived file? *)
    IF OS.FileNameEq (file, ".M3WEB") THEN RETURN FALSE; END;

    (* source file? *)
    ext := Pathname.LastExt (file);
    IF (ext # NIL) THEN
      FOR i := FIRST (KnownExts) TO LAST (KnownExts) DO
        IF OS.FileNameEq (KnownExts[i], ext) THEN
          RETURN IsDirFile [i];
        END;
      END;
    END;

    (* special source file? *)
    IF OS.FileNameEq (file, "m3makefile")
      OR OS.FileNameEq (file, "m3overrides") THEN
      RETURN TRUE;
    END;
    IF OS.FileNameEq (file, "COPYRIGHT") THEN
      RETURN FALSE;
    END;

    IF Text.Equal(file, ".svn") THEN
      RETURN FALSE;
    END;
    
    (* subdirectory? *)
    path := OS.MakePath (path, file);
    IF OS.IsDirectory (path) THEN RETURN TRUE; END;

    IF (in_src_dir) AND NOT IsEditorTempFile (file) THEN
      RETURN TRUE;
    END;

    RETURN FALSE;
  END IsSourceName;

PROCEDURE ScanNewDir (VAR s: Scan;  dir: Dir.T;  path: TEXT;  in_src_dir: BOOLEAN)
  RAISES {Thread.Alerted} =
  VAR iter: FS.Iterator;  file: TEXT;  n: Node.Named_T;
  BEGIN
    dir.scanned  := s.now;
    dir.contents := NIL;
    in_src_dir := in_src_dir OR OS.FileNameEq ("src", ID.ToText (dir.name));

    TRY
      iter := FS.Iterate (path);
      TRY
        WHILE iter.next (file) DO
          (* This is where we can ignore subdirectories like .svn, CVS..
          *)
          IF NOT Text.Equal(file, ".svn") THEN
            n := ScanFile (s, path, file, in_src_dir);
            IF (n # NIL) THEN
              n.parent := dir;
              n.sibling := dir.contents;
              dir.contents := n;
            END;
          END;
        END;
      FINALLY
        iter.close ();
      END;
    EXCEPT OSError.E (ec) =>
      ErrLog.Msg ("trouble scanning directory ", path, OS.Err (ec));
    END;

    AddClassEntries (dir);
  END ScanNewDir;

PROCEDURE ScanFile (VAR s: Scan;  path, file: TEXT;
                    in_src_dir: BOOLEAN): Node.Named_T
  RAISES {Thread.Alerted} =
  VAR ext: TEXT;  dir: Dir.T;
  BEGIN
    (* derived file? *)
    IF OS.FileNameEq (file, ".M3WEB") THEN
      RETURN ScanNewDerived (s, path);
    END;

    (* source file? *)
    ext := Pathname.LastExt (file);
    IF (ext # NIL) THEN
      FOR i := FIRST (KnownExts) TO LAST (KnownExts) DO
        IF OS.FileNameEq (KnownExts[i], ext) THEN
          IF (i < NUMBER (SK)) THEN
            RETURN NEW (Source.T, name := ID.Add (file), kind := VAL (i, SK));
          END;
          RETURN NIL;
        END;
      END;
    END;

    (* special source file? *)
    IF OS.FileNameEq (file, "m3makefile")
      OR OS.FileNameEq (file, "m3overrides") THEN
      RETURN NEW (Source.T, name := ID.Add (file), kind := SK.Quake);
    END;
    IF OS.FileNameEq (file, "COPYRIGHT") THEN
      RETURN NIL;
    END;

    (* subdirectory? *)
    path := OS.MakePath (path, file);
    IF OS.IsDirectory (path) THEN
      dir := NEW (Dir.T, name := ID.Add (file));
      ScanNewDir (s, dir, path, in_src_dir);
      RETURN dir;
    END;

    IF (in_src_dir) AND NOT IsEditorTempFile (file) THEN
      RETURN NEW (Source.T, name := ID.Add (file), kind := SK.Other);
    END;

    RETURN NIL;
  END ScanFile;

PROCEDURE IsEditorTempFile (nm: TEXT): BOOLEAN =
  VAR last_ch := Text.GetChar (nm, Text.Length (nm) - 1);
  BEGIN
    RETURN (last_ch = '#') OR (last_ch = '~');
  END IsEditorTempFile;

TYPE ClassMap = ARRAY Node.Class OF BOOLEAN;

PROCEDURE AddClassEntries (dir: Dir.T) =
  VAR seen: ClassMap;
  BEGIN
    (* build the class pseudo-directories *)
    FOR c := FIRST (seen) TO LAST (seen) DO seen[c] := FALSE; END;
    ScanDirClasses (dir, seen);
    FOR c := FIRST (seen) TO LAST (seen) DO
      IF seen[c] AND (Node.ClassID[c] # ID.NoID) THEN
        dir.contents := NEW (ClassDir.T, name := Node.ClassID[c], kind := c,
                             parent := dir, sibling := dir.contents);
      END;
    END;
  END AddClassEntries;

PROCEDURE ScanDirClasses (dir: Dir.T;  VAR seen: ClassMap) =
  VAR n := dir.contents;  c: Node.Class;
  BEGIN
    WHILE (n # NIL) DO
      c := n.class ();
      seen[c] := TRUE;
      TYPECASE n OF
      | Dir.T(x) =>
          ScanDirClasses (x, seen);
      | Derived.T(x) =>
          FOR i := FIRST (x.seen) TO LAST (x.seen) DO
            IF x.seen[i] THEN seen [Source.NodeClass [i]] := TRUE; END;
          END;
      ELSE
          (* skip *)
      END;
      n := n.sibling;
    END;
  END ScanDirClasses;

(*-------------------------------------------------------- derived files ---*)

TYPE
  FileInfo = RECORD name: TEXT;  time: OS.FileTime; END;
  DerivedInfo = RECORD  m3web, m3exports: FileInfo;  END;

PROCEDURE DerivedChanged (old: Derived.T;  dir_path: TEXT): BOOLEAN =
  VAR info: DerivedInfo;
  BEGIN
    IF (old = NIL) THEN RETURN TRUE; END;

    info := GetDerivedInfo (dir_path);
    IF (info.m3exports.time = OS.NO_TIME)
      OR (info.m3web.time = OS.NO_TIME) THEN
      (* it looks like it's been deleted... *)
      RETURN TRUE;
    END;

    RETURN (info.m3web.time > old.scanned)
        OR (info.m3exports.time > old.scanned);
  END DerivedChanged;

PROCEDURE ScanNewDerived (VAR s: Scan;  dir_path: TEXT): Derived.T
  RAISES {Thread.Alerted} =
  VAR
    x := NEW (Derived.T);
    info := GetDerivedInfo (dir_path);
  BEGIN
    x.is_pgm  := FALSE;
    x.scanned := MAX (info.m3web.time, info.m3exports.time);

    ScanExports (s, x, info.m3exports.name);
    IF (x.name = ID.NoID) THEN
      ErrLog.Msg ("unable to determine name of derived object in ", dir_path);
    END;

    ScanWebInfo (s, info.m3web.name, rd := NIL);

    RETURN x;
  END ScanNewDerived;

PROCEDURE GetDerivedInfo (path: TEXT): DerivedInfo =
  VAR info: DerivedInfo;
  BEGIN
    info.m3web.name := OS.MakePath (path, ".M3WEB");
    info.m3web.time := OS.LastModified (info.m3web.name);
    info.m3exports.name := OS.MakePath (path, ".M3EXPORTS");
    info.m3exports.time := OS.LastModified (info.m3exports.name);
    RETURN info;
  END GetDerivedInfo;

(*------------------------------------------------------------ .M3EXPORTS ---*)

TYPE
  ParseWord = RECORD start, len: INTEGER; END;
  ScanLine  = ARRAY [0..511] OF CHAR;

VAR
  add_intf_id    := ID.Add ("_map_add_interface");
  add_mod_id     := ID.Add ("_map_add_module");
  add_gintf_id   := ID.Add ("_map_add_generic_interface");
  add_gmod_id    := ID.Add ("_map_add_generic_module");
  add_c_id       := ID.Add ("_map_add_c_source");
  add_h_id       := ID.Add ("_map_add_h_source");
  define_lib_id  := ID.Add ("_define_lib");
  define_pgm_id  := ID.Add ("_define_pgm");

PROCEDURE ScanExports (VAR s: Scan;  pgm: Derived.T;  file: TEXT)
  RAISES {Thread.Alerted} =
  VAR rd := OS.OpenRd (file);
  BEGIN
    IF (rd = NIL) THEN RETURN; END;
    TRY
      TRY
        ParseExports (s, pgm, rd);
      EXCEPT Rd.Failure(ec) =>
        ErrLog.Msg ("Trouble reading \"", file, "\"", OS.Err (ec));
      END;
    FINALLY
      OS.CloseRd (rd);
    END;
  END ScanExports;

PROCEDURE ParseExports (VAR s: Scan;  pgm: Derived.T;  rd: Rd.T)

  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    key  : ID.T;
    len  : INTEGER;
    n    : INTEGER;
    x    : ARRAY [0..9] OF ParseWord;
    line : ScanLine;
  BEGIN
    FOR s := FIRST (pgm.seen) TO LAST (pgm.seen) DO pgm.seen[s] := FALSE; END;
    WHILE NOT Rd.EOF (rd) DO
      len := Rd.GetSubLine (rd, line);
      n := ParseLine (line, len, x);
      IF (n > 0) THEN
        key := ParseID (line, x[0]);
        IF (key = add_intf_id) THEN
          AddUnit (s, line, x[1], x[2], x[3]);
          pgm.seen [SK.I3] := TRUE;
        ELSIF (key = add_mod_id) THEN
          AddUnit (s, line, x[1], x[2], x[3]);
          pgm.seen [SK.M3] := TRUE;
        ELSIF (key = add_gintf_id) THEN
          AddUnit (s, line, x[1], x[2], x[3]);
          pgm.seen [SK.IG] := TRUE;
        ELSIF (key = add_gmod_id) THEN
          AddUnit (s, line, x[1], x[2], x[3]);
          pgm.seen [SK.MG] := TRUE;
        ELSIF (key = add_c_id) THEN
          AddUnit (s, line, x[1], x[2], x[3]);
          pgm.seen [SK.C] := TRUE;
        ELSIF (key = add_h_id) THEN
          AddUnit (s, line, x[1], x[2], x[3]);
          pgm.seen [SK.H] := TRUE;
        ELSIF (key = define_lib_id) THEN
          pgm.is_pgm := FALSE;
          pgm.name   := ParseID (line, x[1]);
          Derived.FixName (pgm);
        ELSIF (key = define_pgm_id) THEN
          pgm.is_pgm := TRUE;
          pgm.name   := ParseID (line, x[1]);
          Derived.FixName (pgm);
        END;
      END;
    END;
    (* grab the derived object's contents *)
    pgm.n_elts    := s.n_unit_refs;
    pgm.contents  := NEW (Derived.NodeRefSet, pgm.n_elts);
    pgm.contents^ := SUBARRAY (s.unit_refs^, 0, pgm.n_elts);
    s.n_unit_refs := 0;
  END ParseExports;

PROCEDURE ParseLine (READONLY line: ScanLine;  eol: INTEGER;
                      VAR x: ARRAY [0..9] OF ParseWord): INTEGER =
  VAR
    cur := 0;
    len := 0;
    cnt := 0;
    ch  : CHAR;
  BEGIN
    FOR i := FIRST (x) TO LAST (x) DO  x[i].start := 0;  x[i].len := 0;  END;

    ch := line[cur]; INC (cur);
    WHILE (cur <= eol) AND (ch # '\n') DO
      IF (ch = '%') THEN
        (* comment to end of line *)
        WHILE (cur < eol) AND (ch # '\n') DO ch := line[cur];  INC (cur);  END;
        EXIT;
      ELSIF (ch = '(') OR (ch = ',') OR (ch = ')')
        OR (ch = ' ') OR (ch = '\t') OR (ch = '"') OR (ch = '\r') THEN
        (* misc. punctuation *)
        IF (len > 0) THEN
          x [cnt].len := len;
          INC (cnt);
          len := 0;
        END;
      ELSIF (len <= 0) THEN
        (* start a new word *)
        x [cnt].start := cur-1;
        len := 1;
      ELSE
        INC (len);
      END;
      ch := line[cur]; INC (cur);
    END;

    IF (len > 0) THEN
      x [cnt].len := len;
      INC (cnt);
      len := 0;
    END;
    RETURN cnt;
  END ParseLine;

PROCEDURE AddUnit (VAR s: Scan;  READONLY line: ScanLine;
                     READONLY file, pkg, pkg_dir: ParseWord) =
  VAR
    pkg_id  := ParseID (line, pkg);
    dir_id  := ParseID (line, pkg_dir);
    file_id := ParseID (line, file);
  BEGIN
    IF (s.n_unit_refs >= NUMBER (s.unit_refs^)) THEN ExpandUnitRefs (s); END;
    WITH z = s.unit_refs [s.n_unit_refs] DO
      z.loc  := Loc.Add (pkg_id, dir_id);
      z.file := file_id;
    END;
    INC (s.n_unit_refs);
  END AddUnit;

PROCEDURE ExpandUnitRefs (VAR s: Scan) =
  VAR n := NUMBER (s.unit_refs^);  new := NEW (Derived.NodeRefSet, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := s.unit_refs^;
    s.unit_refs := new;
  END ExpandUnitRefs;

PROCEDURE ParseID (READONLY line: ScanLine;  READONLY x: ParseWord): ID.T =
  BEGIN
    RETURN ID.FromStr (SUBARRAY (line, x.start, x.len));
  END ParseID;

(*---------------------------------------------------------------- .M3WEB ---*)

PROCEDURE ScanWebInfo (VAR s: Scan;  file: TEXT;  rd: Rd.T)
  RAISES {Thread.Alerted} =
  BEGIN
    IF (rd = NIL) THEN rd := OS.OpenRd (file); END;
    IF (rd = NIL) THEN RETURN; END;

    TRY
      TRY
        ParseWebInfo (s, rd, file);
      EXCEPT Rd.Failure(ec) =>
        ErrLog.Msg ("Trouble reading \"", file, "\"", OS.Err (ec));
      END;
    FINALLY
      OS.CloseRd (rd);
    END;
  END ScanWebInfo;

PROCEDURE ParseWebInfo (VAR s: Scan;  rd: Rd.T;  file: TEXT)
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    is_intf   : BOOLEAN;
    cur       : INTEGER := 0;
    xx        : INTEGER;
    len       : INTEGER;
    eol       : INTEGER;
    cur_file  : ID.T;
    cur_unit  : ID.T := 0;
    unit      : ID.T;
    uid       : INTEGER;
    type_name : ID.T;
    lhs, rhs  : INTEGER;
    super     : INTEGER;
    line      : ScanLine;
  BEGIN
    IF (rd = NIL) THEN RETURN END;

    (* skip the table of contents *)
    REPEAT
      line[0] := ' ';
      len := Rd.GetSubLine (rd, line);
      INC (cur, len);
    UNTIL (Rd.EOF (rd) OR line[0] = '$');

    WHILE NOT Rd.EOF (rd) DO
      len := Rd.GetSubLine (rd, line);
      eol := len;
      WHILE (eol > 0) AND ((line[eol-1] = '\n') OR (line[eol-1] = '\r')) DO
        DEC (eol);
      END;
      xx := 1; (* offset in the current line *)
      CASE line[0] OF
      | '@' => (* file name  *)
          cur_file := ID.FromStr (SUBARRAY (line, xx, eol-xx));
      | 'A' => (* module name *)
          cur_unit := UnitName (line, xx, eol-xx, FALSE);
          is_intf := FALSE;
      | 'B' => (* interface name *)
          cur_unit := UnitName (line, xx, eol-xx, TRUE);
          is_intf := TRUE;
      | 'C' => (* import *)
          unit := UnitName (line, xx, eol-xx, TRUE);
          IF NOT is_intf OR (unit # cur_unit) THEN
            NoteUse (s.new.importers, cur_unit, unit);
          END;
      | 'D' => (* export *)
          unit := UnitName (line, xx, eol-xx, TRUE);
          IF NOT is_intf OR (unit # cur_unit) THEN
            NoteUse (s.new.exporters, cur_unit, unit);
          END;
      | 'E' => (* typename *)
          uid := ReadUID (line, xx);
          SkipBlanks (line, xx);
          type_name := ID.FromStr (SUBARRAY (line, xx, eol-xx));
          NoteTypeName (s, uid, type_name, cur_unit);
      | 'F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'R', 'Y' =>
          uid := ReadUID (line, xx);
          NoteType (s, uid, file, cur, cur_unit, line[0]);
      | 'O' =>
          uid := ReadUID (line, xx);
          NoteType (s, uid, file, cur, cur_unit, line[0]);
          NoteSubtype (s, uid, Type.ADDRESS_UID);
      | 'P' =>
          uid := ReadUID (line, xx);
          NoteType (s, uid, file, cur, cur_unit, line[0]);
          NoteSubtype (s, uid, Type.REFANY_UID);
      | 'U', 'V' =>
          uid := ReadUID (line, xx);
          super := ReadUID (line, xx);
          NoteType (s, uid, file, cur, cur_unit, line[0]);
          IF (super # 0) THEN NoteSubtype (s, uid, super); END;
      | 'Z' =>
          lhs := ReadUID (line, xx);
          SkipBlanks (line, xx);
          rhs := ReadUID (line, xx);
          NoteRevelation (s, lhs, rhs);
      | '?' => (* builtin type *)
          uid := ReadUID (line, xx);
          SkipBlanks (line, xx);
          type_name := ID.FromStr (SUBARRAY (line, xx, eol-xx));
          NoteType (s, uid, file, cur, cur_unit, line[0]);
          NoteTypeName (s, uid, type_name, cur_unit);
      ELSE (* skip *)
      END;
      INC (cur, len);
    END;
  END ParseWebInfo;

PROCEDURE UnitName (READONLY line: ScanLine;  start, len: INTEGER;
                    intf: BOOLEAN): ID.T =
  CONST CC = ARRAY BOOLEAN OF CHAR { 'm', 'i' };
  VAR xx: ARRAY [0..255] OF CHAR;  n := MIN (NUMBER (xx), len);
  BEGIN
    SUBARRAY (xx, 0, n) := SUBARRAY (line, start, n);
    IF (n < NUMBER (xx)) THEN xx[n] := '.'; INC (n); END;
    IF (n < NUMBER (xx)) THEN xx[n] := CC[intf]; INC (n); END;
    IF (n < NUMBER (xx)) THEN xx[n] := '3'; INC (n); END;
    RETURN ID.FromStr (SUBARRAY (xx, 0, n));
  END UnitName;

PROCEDURE NoteUse (tbl: IntRefTbl.T;  impl, intf: ID.T) =
  VAR ref: REFANY;  ids: IntList.T;
  BEGIN
    IF tbl.get (intf, ref) THEN
      ids := ref;
      WHILE (ids # NIL) DO
        IF (ids.head = impl) THEN RETURN; END;
        ids := ids.tail;
      END;
      ids := ref;
      ids.tail := IntList.Cons (impl, ids.tail);
    ELSE
      EVAL tbl.put (intf, IntList.List1 (impl));
    END;
  END NoteUse;

PROCEDURE NoteTypeName (VAR s: Scan;  uid: INTEGER;  name, home: ID.T) =
  VAR
    tipe := NewType (s, uid);
    t    : Type.T;
    nd   : Node.List;
    ref  : REFANY;
  BEGIN
    (* search for a duplicate *)
    t := tipe.names;
    WHILE (t # NIL) DO
      IF (t.name = name) AND (t.home = home) THEN RETURN END;
      t := t.alias;
    END;

    (* create a new name *)
    t := NEW (Type.T, alias := NIL, name := name, home := home, uid := uid);
    IF (tipe.names # NIL) THEN
      (* preserve the "first" name *)
      t.alias := tipe.names.alias;
      tipe.names.alias := t;
    ELSE
      tipe.names := t;
    END;

    (* register the name in the table *)
    IF s.new.type_names.get (name, ref) THEN
      nd := ref;
      nd.tail := NEW (Node.List, head := t, tail := nd.tail);
    ELSE
      EVAL s.new.type_names.put (name, NEW (Node.List, head := t, tail := NIL));
    END;
  END NoteTypeName;

PROCEDURE NoteType (VAR s: Scan;  uid: INTEGER;  file: TEXT;
                    start: INTEGER;  home: ID.T;  class: CHAR) =
  VAR tipe := NewType (s, uid);
  BEGIN
    IF (tipe.home = ID.NoID) THEN
      tipe.kind        := class;
      tipe.home        := home;
      tipe.info_file   := file;
      tipe.info_offset := start;
    END;
  END NoteType;

PROCEDURE NoteRevelation (VAR s: Scan;  lhs, rhs: INTEGER) =
  VAR info: Type.ObjectInfo;  ref: REFANY;
  BEGIN
    IF s.new.objects.get (rhs, ref) THEN
      info := ref;
      IF (info.concrete # rhs) THEN
        ErrLog.Msg ("?? ", FmtUID (lhs), " == ", FmtUID (rhs));
      END;
      IF (info.opaque = Type.NO_UID) THEN
        info.opaque := lhs;
        EVAL s.new.objects.put (lhs, info);
      ELSIF (info.opaque # lhs) THEN
        ErrLog.Msg ("?? ", FmtUID (lhs), " == ", FmtUID (rhs));
      END;
    ELSE
      info := NEW (Type.ObjectInfo);
      info.opaque := lhs;
      info.concrete := rhs;
      EVAL s.new.objects.put (lhs, info);
      EVAL s.new.objects.put (rhs, info);
    END;
  END NoteRevelation;

PROCEDURE NoteSubtype (VAR s: Scan;  subtype, super: INTEGER) =
  VAR
    sub  := NewType (s, subtype);
    sup  := NewType (s, super);
    sub_info := GetObjInfo (s, sub);
    sup_info := GetObjInfo (s, sup);
  BEGIN
    IF (sub_info.supertype = Type.NO_UID) THEN
      sub_info.supertype := super;
      sub_info.next_peer := sup_info.subtypes;
      sup_info.subtypes  := subtype;
    ELSIF (sub_info.supertype # super) THEN
      ErrLog.Msg ("two super types for ", FmtUID(subtype),
                  " => ", FmtUID (sub_info.supertype) &" and "& FmtUID (super));
    END;
  END NoteSubtype;

PROCEDURE GetObjInfo (VAR s: Scan;  tipe: Type.Info): Type.ObjectInfo =
  VAR ref: REFANY;  info: Type.ObjectInfo;
  BEGIN
    IF s.new.objects.get (tipe.uid, ref) THEN
      info := ref;
    ELSE
      info := NEW (Type.ObjectInfo, concrete := tipe.uid);
      EVAL s.new.objects.put (tipe.uid, info);
    END;
    RETURN info;
  END GetObjInfo;

PROCEDURE NewType (VAR s: Scan;  uid: INTEGER): Type.Info =
  VAR tipe: Type.Info;  ref: REFANY;
  BEGIN
    IF s.new.types.get (uid, ref) THEN
      tipe := ref;
    ELSE
      tipe := NEW (Type.Info, uid := uid, kind := '\000');
      EVAL s.new.types.put (uid, tipe);
    END;
    RETURN tipe;
  END NewType;

(*-------------------------------------------------------------- name map ---*)

PROCEDURE AddPkgNames (VAR s: Scan;  pkg: Pkg.T) =
  BEGIN
    AddDirNames (s, pkg);
  END AddPkgNames;

PROCEDURE AddDirNames (VAR s: Scan;  dir: Dir.T) =
  VAR n := dir.contents;
  BEGIN
    WHILE (n # NIL) DO
      TYPECASE n OF
      | Dir.T (x)     =>  AddDirNames (s, x);
      | Derived.T (x) =>  AddPgmName (s, x);
      | Source.T (x)  =>  AddName (s.new.units, x);
      ELSE (* skip *)
      END;
      n := n.sibling;
    END;
  END AddDirNames;

PROCEDURE AddPgmName (VAR s: Scan;  pgm: Derived.T) =
  BEGIN
    IF (pgm # NIL) AND (pgm.name # ID.NoID) THEN
      IF (pgm.is_pgm)
        THEN AddName (s.new.pgms, pgm);
        ELSE AddName (s.new.libs, pgm);
      END;
    END;
  END AddPgmName;

PROCEDURE AddBuiltinTypes (VAR s: Scan)
  RAISES {Thread.Alerted} =
  BEGIN
    ScanWebInfo (s, Type.BuiltinName, TextRd.New (Type.BuiltinInfo));

    NoteSubtype (s, Type.UNROOT_UID, Type.ADDRESS_UID);
      (* UNTRACED-ROOT <: ADDRESS *)

    NoteSubtype (s, Type.ROOT_UID, Type.REFANY_UID);
      (* ROOT <: REFANY *)

    NoteSubtype (s, Type.NULL_UID, Type.REFANY_UID);
      (* NULL <: REFANY *)

    (*** too messy for the current data structures ****************
    NoteSubtype (s, Type.NULL_UID, Type.ADDRESS_UID);
      (* NULL <: ADDRESS *)
    ***************************************************************)
  END AddBuiltinTypes;

PROCEDURE AddName (tbl: IntRefTbl.T;  n: Node.Named_T) =
  VAR ref: REFANY;  nd: Node.List;
  BEGIN
    IF (n = NIL) THEN
      (* skip *)
    ELSIF tbl.get (n.name, ref) THEN
      nd := ref;
      WHILE (nd # NIL) DO
        IF (nd.head = n) THEN RETURN; END;
        nd := nd.tail;
      END;
      nd := ref;
      nd.tail := NEW (Node.List, head := n, tail := nd.tail);
    ELSE
      EVAL tbl.put (n.name, NEW (Node.List, head := n, tail := NIL));
    END;
  END AddName;

(*------------------------------------------------------------ internal ---*)

PROCEDURE InitDB (VAR x: DataBase) =
  BEGIN
    x.packages    := NEW (IntRefTbl.Default).init ();
    x.libs        := NEW (IntRefTbl.Default).init ();
    x.pgms        := NEW (IntRefTbl.Default).init ();
    x.units       := NEW (IntRefTbl.Default).init ();

    IF (db.types # NIL) THEN
      (* preserve any existing import/export & type information *)
      x.exporters  := db.exporters;
      x.importers  := db.importers;
      x.types      := db.types;
      x.type_names := db.type_names;
      x.objects    := db.objects;
    ELSE
      x.exporters  := NEW (IntRefTbl.Default).init ();
      x.importers  := NEW (IntRefTbl.Default).init ();
      x.types      := NEW (IntRefTbl.Default).init ();
      x.type_names := NEW (IntRefTbl.Default).init ();
      x.objects    := NEW (IntRefTbl.Default).init ();
    END;
  END InitDB;

PROCEDURE ResetDB (VAR x: DataBase) =
  BEGIN
    x.packages    := NIL;
    x.libs        := NIL;
    x.pgms        := NIL;
    x.units       := NIL;
    x.exporters   := NIL;
    x.importers   := NIL;
    x.types       := NIL;
    x.type_names  := NIL;
    x.objects     := NIL;
  END ResetDB;

(*------------------------------------------------------- low-level stuff ---*)

CONST
  MaxPad = 16;
  Blanks = ARRAY [0..MaxPad] OF CHAR { ' ',' ',' ',' ',
           ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '};

PROCEDURE OutWx (VAR s: Scan; s1,s2,s3,s4: TEXT := NIL;  pad := 0)
  RAISES {Thread.Alerted} =
  VAR wx := s.wx;
  BEGIN
    IF wx = NIL THEN RETURN END;
    TRY
      wx.put (s1, s2, s3, s4);
      IF (pad > 0) THEN
        wx.putStr (SUBARRAY (Blanks, 0, pad));
      END;
      wx.flush ();
    EXCEPT Wr.Failure =>
      (* don't abort the scan, just quit trying to do any output *)
      s.wx := NIL;
    END;
  END OutWx;

BEGIN
END BrowserDB.
