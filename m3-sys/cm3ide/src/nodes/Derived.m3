(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Derived;

IMPORT Env, Pathname, Text, Thread, Wr;
IMPORT BrowserDB, Builder, ClassDir, Default, Dir, ErrLog, ID;
IMPORT Node, HTML, OS, Pkg, RegExpr, Source, UserState, Wx;

REVEAL
  T = Tx BRANDED "Derived.T" OBJECT
  OVERRIDES
    class      := Class;
    printname  := PrintName;
    match      := Match;
    iterate    := Iterate;
    next       := Next;
    gen_page   := GenPage;
  END;

VAR
  PreChop  : ARRAY BOOLEAN OF CARDINAL;
  PostChop : ARRAY BOOLEAN OF CARDINAL;
  viewID   := ID.Add ("view");
  runID    := ID.Add ("run");

PROCEDURE Class (t: T): Node.Class =
  TYPE  NC  = Node.Class;
  CONST Map = ARRAY BOOLEAN OF NC { NC.Library, NC.Program };
  BEGIN
    RETURN Map [t.is_pgm];
  END Class;

PROCEDURE PrintName (t: T): TEXT =
  VAR
    nm   := ID.ToText (t.name);
    pre  := PreChop [t.is_pgm];
    post := PostChop [t.is_pgm];
  BEGIN
    RETURN Text.Sub (nm, pre, Text.Length (nm) - pre - post);
  END PrintName;

PROCEDURE Match (t: T;  re: RegExpr.T): BOOLEAN =
  VAR
    nm   := ID.ToText (t.name);
    pre  := PreChop [t.is_pgm];
    post := PostChop [t.is_pgm];
  BEGIN
    RETURN RegExpr.Match (re, nm)
        OR RegExpr.MatchSubstring (re, nm, pre, post);
  END Match;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  VAR x := GenContents (t);
  BEGIN
    s.a := 0;
    s.b := x.cnt;
    s.d := x.elts;
  END Iterate;

PROCEDURE Next (<*UNUSED*> t: T;  VAR s: Node.IteratorState): BOOLEAN =
  VAR n: Node.T;  elts: Node.Array := s.d;
  BEGIN
    WHILE (s.a < s.b) DO
      n := elts [s.a];  INC (s.a);
      IF n.match (s.pattern) THEN
        s.match := n;  RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (action = runID)
      THEN GenRun (t, wx, data);
      ELSE GenView (t, wx, action, data);
    END;
  END GenPage;

PROCEDURE PgmPath (t: T): TEXT =
  VAR file, path: TEXT;
  BEGIN
    IF NOT t.is_pgm THEN RETURN NIL END;
    file := Node.FullPath (t);
    IF OS.IsExecutable (file) THEN RETURN file; END;
    (* but, public programs are usually shipped to the BIN directory! *)
    path := OS.FindExecutable (Pathname.Last (file));
    IF (path # NIL) THEN RETURN path; END;
    (* but, if it's not there, give up. *)
    RETURN file;
  END PgmPath;

PROCEDURE GenView (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    path := Node.FullPath (t);
    x    : Node.Set;
  BEGIN
    GenHeader (t, path, wx);
    IF (action = viewID) THEN
      Pkg.GenActionButtons (t, wx);
      x := GenContents (t);
      HTML.GenChoices (x, wx);
    ELSE
      HTML.NoAction (action, wx);
    END;
    HTML.NoData (data, wx);
    HTML.End (wx);
  END GenView;

PROCEDURE GenRun (t: T;  wx: Wx.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    path   := Node.FullPath (t);
    cmdkey := "RUNCMD|" & path;
    dirkey := "RUNDIR|" & path;
    cmd    := UserState.Get (cmdkey);
    dir    := UserState.Get (dirkey);
  BEGIN
    (* process the incoming form data *)
    WHILE (data # NIL) DO
      IF Text.Equal (data.field, "run-cmd") THEN
        cmd := data.value;
      ELSIF Text.Equal (data.field, "run-dir") THEN
        dir := data.value;
      ELSE
        wx.put ("<STRONG>Unrecognized field: ", data.field, "</STRONG><BR>\n");
      END;
      data := data.next;
    END;

    (* try to ensure there's some sort of reasonable values *)
    IF (cmd = NIL) THEN cmd := PgmPath (t); END;
    IF (dir = NIL) THEN dir := Env.Get ("HOME"); END;
    IF (dir = NIL) THEN dir := Node.FullPath (t.parent); END;

    (* and record them for posterity *)
    UserState.Put (cmdkey, cmd);
    UserState.Put (dirkey, dir);

    GenHeader (t, path, wx);
    Builder.Run (Pkg.Home (t), cmd, dir, wx);
    HTML.End (wx);
  END GenRun;

PROCEDURE GenHeader (t: T;  path: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    HTML.Begin (t, wx);
    Pkg.GenFileNote (path, wx, is_dir := FALSE);
    Pkg.GenBuildNote (t, wx);
    wx.put ("\n");
  END GenHeader;

PROCEDURE FixName (t: T) =
  VAR nm := ID.ToText (t.name);
  BEGIN
    CASE ORD (t.is_pgm) + 2 * ORD (Default.on_unix) OF
    | 0 =>  t.name := ID.Add (nm & ".lib");       (* Win32 library:  foo.lib  *)
    | 1 =>  t.name := ID.Add (nm & ".exe");       (* Win32 program:  foo.exe  *)
    | 2 =>  t.name := ID.Add ("lib" & nm & ".a"); (* Unix library:   libfoo.a *)
    ELSE    (*skip*)                              (* Unix program:   foo      *)
    END;
  END FixName;

PROCEDURE Init () =
  BEGIN
    IF (Default.on_unix) THEN
      (* assume Unix naming conventions *)
      PreChop [FALSE] := 3;  PostChop [FALSE] := 2;  (* Unix library:   libfoo.a *)
      PreChop [TRUE]  := 0;  PostChop [TRUE]  := 0;  (* Unix program:   foo      *)
    ELSE
      (* assume Win32 naming conventions *)
      PreChop [FALSE] := 0;  PostChop [FALSE] := 4;  (* Win32 library:  foo.lib  *)
      PreChop [TRUE]  := 0;  PostChop [TRUE]  := 4;  (* Win32 program:  foo.exe  *)
    END;
  END Init;

PROCEDURE GenContents (t: T): Node.Set =
  VAR x: Node.Set;  c: Node.Class;  pkg: Pkg.T;  path : NamePath;  n: Node.T;
  BEGIN
    (* scan and build the node class virtual directories *)
    FOR k := FIRST (t.seen) TO LAST (t.seen) DO
      IF (t.seen[k]) THEN
        c := Source.NodeClass [k];
        IF (Node.ClassID[c] # ID.NoID) THEN
          Node.Append (x, NEW (ClassDir.T, name := Node.ClassID [c],
                               parent := t, kind := c));
        END;
      END;
    END;

    pkg := Pkg.Home (t);
    FOR i := 0 TO t.n_elts-1 DO
      WITH z = t.contents [i] DO
        path.len := 0;
        AddArcs (path, z.loc.subdir);
        path.arcs [path.len] := z.file;  INC (path.len);
        n := FindSource (z.loc.pkg, path, pkg);
        IF (n # NIL) THEN
          Node.Append (x, n);
        ELSE
          ErrLog.Msg ("Unable to locate source: ", PathToText (z.loc.pkg, path),
                      " for ", Node.FullPath (t));
        END;
      END;
    END;
    
    RETURN x;
  END GenContents;

TYPE
  NamePath = RECORD
    len  : INTEGER;
    arcs : ARRAY [0..19] OF ID.T;
  END;

PROCEDURE AddArcs (VAR path: NamePath;  x: ID.T) =
  CONST BackSlash = '\134';
  VAR
    txt := ID.ToText (x);
    len := Text.Length (txt);
    s0, s1: INTEGER;
    buf: ARRAY [0..99] OF CHAR;
  BEGIN
    <*ASSERT len <= NUMBER (buf) *>
    Text.SetChars (buf, txt);
    s0 := 0;
    WHILE (s0 < len) DO
      s1 := s0;
      WHILE (s1 < len) AND (buf[s1] # '/') AND (buf[s1] # BackSlash) DO
        INC (s1);
      END;
      IF (s0 < s1) THEN
        path.arcs [path.len] := ID.FromStr (SUBARRAY (buf, s0, s1 - s0));
        INC (path.len);
      END;
      s0 := s1 + 1;
    END;
  END AddArcs;

PROCEDURE FindSource (pkg_nm: ID.T;  READONLY path: NamePath;
                      hint: Pkg.T): Node.T =
  VAR n: Node.T;  pkgs: Node.List;  ref: REFANY;
  BEGIN
    (* try the hint *)
    IF (hint # NIL) AND (hint.name = pkg_nm) THEN
      n := FindFile (path, hint);
      IF (n # NIL) THEN RETURN n; END;
    END;

    (* search all the packages with the same name *)
    IF NOT BrowserDB.db.packages.get (pkg_nm, ref) THEN RETURN NIL; END;
    pkgs := ref;
    WHILE (pkgs # NIL) DO
      n := FindFile (path, pkgs.head);
      IF (n # NIL) THEN RETURN n; END;
      pkgs := pkgs.tail;
    END;

    (* failed... *)
    RETURN NIL;
  END FindSource;

PROCEDURE FindFile (READONLY path: NamePath;  dir: Dir.T): Node.T =
  VAR n: Node.Named_T;  nm: ID.T;
  BEGIN
    (* walk through the subdirectories *)
    FOR i := 0 TO path.len-1 DO
      (* find the next subdirectory *)
      nm := path.arcs [i];
      n  := dir.contents;
      LOOP
        IF (n = NIL) THEN RETURN NIL; END;
        IF (n.name = nm) THEN
          IF (i = path.len-1) THEN RETURN n; END;
          IF NOT ISTYPE (n, Dir.T) THEN RETURN NIL; END;
          dir := n; EXIT;
        END;
        n := n.sibling;
      END;
    END;

    (* failed... *)
    RETURN NIL;
  END FindFile;

PROCEDURE PathToText (pkg: ID.T;  READONLY path: NamePath): TEXT =
  VAR p := ID.ToText (pkg);
  BEGIN
    FOR i := 0 TO path.len - 1 DO
      p := HTML.MakeURL (p, ID.ToText (path.arcs[i]));
    END;
    RETURN p;
  END PathToText;

BEGIN
END Derived.
