(* Copyright (C) 1992, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Mon Feb  5 10:49:57 PST 1996 by heydon       *)
(*      modified on Tue May 23 07:48:55 PDT 1995 by kalsow       *)
(*                                                               *)
(* Enhanced by Peter Klein (pk@i3.informatik.rwth-aachen.de) to  *)
(* parse procedure signatures and connect procedure declarations *)
(* in interfaces with their implmentations. -  Mar 7, 1995       *)

MODULE Main;

IMPORT Text, Rd, Wr, TextRd, Thread, Time, Fmt, IntRefTbl, IntList;
IMPORT Process, Params, Lex, OS, FloatMode, Word, IntIntTbl, TextIntTbl;
IMPORT FileWr, OSError, Atom, FmtTime, M3Config, RTParams, RTCollector;
IMPORT FS, RefList, IntSeq, XFormat, RefSeq, TextRefTbl, CharMap, IP;
IMPORT Buf, ID, Wx, MarkUp, CMarkUp, TCPServer, ErrLog, RTutils, RTHeapStats;

CONST
  Title_page = "m3browser.html";
  SLASH      = M3Config.PATH_SEP;
  StartPage  = "<HTML>\n<HEAD>\n";
  StartTitle = "<TITLE>";
  Body       = "<BODY BGCOLOR=\"#ffffff\" VLINK=\"#006633\">\n";
  EndTitle   = "</TITLE>\n</HEAD>\n" & Body;
  IsIndex    = "<ISINDEX>\n";
  EndPage    = "</BODY>\n</HTML>\n";

VAR (* configuration *)
  package_root     := M3Config.PKG_USE;
  server_machine   :  TEXT; (* initialized in "ParseOptions" *)
  server_socket    := 3829;
  server_address   := IP.GetHostAddr();
  accept_address   := server_address;
  accept_maskBits: [0 .. 32] := 0;
  derived_dirs     := IntList.List1 (ID.Add (M3Config.BUILD_DIR));
  n_workers        := 3;
  refresh_interval := 30.0d0; (* minutes *)
  start_time       := Time.Now ();
  title_page := TRUE;
  verbose := FALSE;

TYPE
  TextVec = REF ARRAY OF TEXT;
  IntVec  = REF ARRAY OF INTEGER;

TYPE
  Class = {
      Interface,
      GenInterface,
      Module,
      GenModule,
      HSource,
      CSource
  };

CONST
  ClassTags = ARRAY Class OF TEXT {
    "interfaces",
    "generic interfaces",
    "implementations",
    "generic implementations",
    "C include files",
    "C sources"
  };

  
CONST
  ClassPrefix = ARRAY Class OF TEXT {
    "B",  (* interface *)
    "A",  (* generic interface *)
    "D",  (* impl *)
    "C",  (* generic impl *)
    "F",  (* C include *)
    "E"   (* C source *)
  };

CONST
  ClassTitle = ARRAY Class OF TEXT {
    "Modula-3 Interface: ",
    "Generic Modula-3 Interface: ",
    "Modula-3 Implementation: ",
    "Generic Modula-3 Implementation: ",
    "C include file: ",
    "C source: "
  };

TYPE
  Unit = REF RECORD
    name  : ID.T     := ID.NoID;
    next  : Unit     := NIL;
    set   : UnitSet  := NIL;
    dir   : TEXT     := NIL;
    class : Class    := Class.Interface;
    hidden: BOOLEAN  := FALSE;
  END;

TYPE
  UnitSet = REF RECORD
    pkg           : Pkg     := NIL;
    path          : TEXT    := NIL;
    name          : ID.T    := ID.NoID;
    units         : Unit    := NIL;
    webinfo       : Buf.T   := NIL;
    webinfo_time  : Time.T  := OS.NO_TIME;
    m3export_time : Time.T  := OS.NO_TIME;
    is_pgm        : BOOLEAN := FALSE;
  END;

TYPE
  Pkg = REF RECORD
    name : ID.T      := ID.NoID;
    sets : RefList.T := NIL; (* of UnitSet *)
  END;

CONST
  INTEGER_UID = 16_195c2a74;
  REFANY_UID  = 16_1c1c45e6;
  ADDRESS_UID = 16_08402063;
  ROOT_UID    = 16_9d8fb489;
  UNROOT_UID  = 16_898ea789;
  NULL_UID    = 16_48ec756e;

TYPE
  Type = REF RECORD
    uid       : INTEGER  := 0;
    home      : ID.T     := ID.NoID;
    class     : CHAR     := '\000';
    start     : INTEGER  := 0;
    defn      : Buf.T    := NIL;
    names     : TypeName := NIL;
    super     : Type     := NIL;
    subtypes  : Type     := NIL;
    next_peer : Type     := NIL;
  END;

TYPE
  TypeName = REF RECORD
    next  : TypeName;
    name  : ID.T;
    home  : ID.T;
  END;

TYPE
  DataBase = RECORD
    packages    : IntRefTbl.T; (* package name -> Pkg *)
    libs        : IntRefTbl.T; (* name -> LIST(UnitSet) *)
    pgms        : IntRefTbl.T; (* name -> LIST(UnitSet) *)
    units       : IntRefTbl.T; (* name -> LIST(Unit) *)
    exporters   : IntRefTbl.T; (* name -> LIST(impl name)*)
    importers   : IntRefTbl.T; (* name -> LIST(unit name)*)
    type_ids    : IntRefTbl.T; (* uid ->  Type *)
    type_names  : IntRefTbl.T; (* name -> LIST(Type) *)
    revelations : IntIntTbl.T; (* opaque uid -> concrete uid *)
    opaques     : IntIntTbl.T; (* concrete uid -> opaque uid *)
  END;

VAR
  server      : TCPServer.T := NIL;
  db          : DataBase;
  last_update : Time.T := OS.NO_TIME;
  n_queries   : INTEGER := 0;

PROCEDURE InitDB (VAR x: DataBase) =
  BEGIN
    x.packages    := NEW (IntRefTbl.Default).init ();
    x.libs        := NEW (IntRefTbl.Default).init ();
    x.pgms        := NEW (IntRefTbl.Default).init ();
    x.units       := NEW (IntRefTbl.Default).init ();
    x.exporters   := NEW (IntRefTbl.Default).init ();
    x.importers   := NEW (IntRefTbl.Default).init ();
    x.type_ids    := NEW (IntRefTbl.Default).init ();
    x.type_names  := NEW (IntRefTbl.Default).init ();
    x.revelations := NEW (IntIntTbl.Default).init ();
    x.opaques     := NEW (IntIntTbl.Default).init ();
  END InitDB;

PROCEDURE ResetDB (VAR x: DataBase) =
  BEGIN
    x.packages    := NIL;
    x.libs        := NIL;
    x.pgms        := NIL;
    x.units       := NIL;
    x.exporters   := NIL;
    x.importers   := NIL;
    x.type_ids    := NIL;
    x.type_names  := NIL;
    x.revelations := NIL;
    x.opaques     := NIL;
  END ResetDB;

(*-------------------------------------------------- command line parsing ---*)

PROCEDURE ParseOptions () =
  VAR i := 1;  parm: TEXT;  first_dir := TRUE;
  BEGIN
    WHILE (i < Params.Count) DO
      parm := Params.Get (i); INC (i);
      IF Text.Equal (parm, "-workers") THEN
        parm := Params.Get (i);  INC (i);
        n_workers := GetCard (parm, "number of workers");
      ELSIF Text.Equal (parm, "-port") THEN
        parm := Params.Get (i);  INC (i);
        server_socket := GetCard (parm, "port number");
      ELSIF Text.Equal (parm, "-refresh") THEN
        parm := Params.Get (i);  INC (i);
        refresh_interval := FLOAT (GetCard(parm, "refresh interval"),LONGREAL);
      ELSIF Text.Equal (parm, "-root") THEN
        parm := Params.Get (i);  INC (i);
        package_root := parm;
      ELSIF Text.Equal (parm, "-dir") THEN
        IF (first_dir) THEN derived_dirs := NIL; first_dir := FALSE; END;
        parm := Params.Get (i);  INC (i);
        derived_dirs := IntList.Cons (ID.Add (parm), derived_dirs);
      ELSIF Text.Equal (parm, "-mask") THEN
        parm := Params.Get (i);  INC (i);
        accept_maskBits := MIN(32,MAX(0,GetCard (parm, "mask length")));
      ELSIF Text.Equal (parm, "-notitle") THEN
        title_page := FALSE;
      ELSIF Text.Equal (parm, "-v") THEN
        verbose := TRUE;
      ELSE
        ErrLog.Msg ("Unrecognized option: ", parm);
        Abort ();
      END;
    END;
    TRY server_machine := IP.GetCanonicalByAddr(IP.GetHostAddr()) EXCEPT
      IP.Error => server_machine := NIL
    END;
    IF (server_machine = NIL) THEN
      ErrLog.Msg ("unable to get host machine's name");
      Abort ();
    END;
    derived_dirs := IntList.ReverseD (derived_dirs);
  END ParseOptions;

PROCEDURE GetCard (txt: TEXT;  nm: TEXT): INTEGER =
  VAR i: INTEGER;
  BEGIN
    TRY
      i := Lex.Int (TextRd.New (txt));
      IF (i > 0) THEN RETURN i; END;
    EXCEPT Rd.Failure, Lex.Error, FloatMode.Trap, Thread.Alerted =>
      (*ouch*)
    END;
    ErrLog.Msg ("bad ", nm, " specified: ", txt);
    Abort ();
    RETURN 0;
  END GetCard;

(*------------------------------------------------------------ title page ---*)

PROCEDURE CreateTitlePage (wx: Wx.T;  relative: BOOLEAN) =
  VAR
    server := server_machine & ":" & Fmt.Int (server_socket);
    update := Fmt.LongReal (refresh_interval, Fmt.Style.Auto,prec:=1);
    d      : IntList.T;
    prefix : TEXT := "";
  BEGIN
    IF NOT relative THEN
      prefix := "http://" & server ;
    END;
    Out (wx, StartPage);
    Out (wx, StartTitle, "Modula-3 browser", EndTitle);
    Out (wx, "<H2>Modula-3 browser of ", package_root, "</H2><P>\n");
    Out (wx, "\n<UL>\n");
    Out (wx, "<LI><A HREF=\"", prefix, "/G\">programs</A>\n");
    Out (wx, "<LI><A HREF=\"", prefix, "/0\">libraries</A>\n");
    Out (wx, "<LI><A HREF=\"", prefix, "/1\">interfaces</A>\n");
    Out (wx, "<LI><A HREF=\"", prefix, "/2\">implementations</A>\n");
    Out (wx, "<LI><A HREF=\"", prefix, "/K\">types</A>\n");
    Out (wx, "</UL>\n<P>\n");
    Out (wx, "<HR>\nserver stats:\n<P>\n");
    Out (wx, "<PRE>\n");
    Out (wx, "    server machine:   ", server_machine, "\n");
    Out (wx, "    server started:   ", FmtTime.Long (start_time), "\n");
    Out (wx, "    last update:      ", FmtTime.Long (Time.Now ()), "\n");
    Out (wx, "    update interval:  ", update, " minutes\n");
    Out (wx, "    queries answered: ", Fmt.Int (n_queries), "\n");
    Out (wx, "    packages scanned: ", Fmt.Int (db.packages.size()), "\n");
    Out (wx, "    build dirs:      ");
    d := derived_dirs;
    WHILE (d # NIL) DO
      Out (wx, " ", ID.ToText (d.head));
      d := d.tail;
    END;
    Out (wx, "\n");
    Out (wx, "</PRE>\n");
    Out (wx, EndPage);
  END CreateTitlePage;

PROCEDURE WriteTitlePage () =
  VAR wr: Wr.T;  wx := Wx.New ();
  BEGIN
    CreateTitlePage (wx, relative := FALSE);
    TRY
      wr := FileWr.Open (Title_page);
      Wr.PutText (wr, Wx.ToText (wx));
      Wr.Close (wr);
    EXCEPT
    | Wr.Failure(ec) =>
        ErrLog.Msg ("problem writing \"", Title_page, "\"", ErrMsg (ec));
    | Thread.Alerted =>
        ErrLog.Msg ("interrupted while writing \"", Title_page, "\"");
    | OSError.E(ec) =>
        ErrLog.Msg ("problem writing \"", Title_page, "\"", ErrMsg (ec));
    END;
  END WriteTitlePage;

(*----------------------------------------------- periodic refresh thread ---*)

PROCEDURE Refresh (<*UNUSED*> service: TCPServer.T) =
  BEGIN
    ScanPackages ();
    IF title_page THEN WriteTitlePage (); END;
    RTCollector.Collect ();
  END Refresh;

(*------------------------------------------------------- package browser ---*)

TYPE
  ScanState = RECORD
    pkg_name : ID.T;
    old_pkg  : Pkg;
    new_pkg  : Pkg;
    modified : BOOLEAN;
    dir_name : ID.T;
    new_set  : UnitSet;
    new      : DataBase;
  END;

PROCEDURE ScanPackages () =
  VAR iter: FS.Iterator;  s: ScanState;  now := Time.Now ();  nm: TEXT;
  BEGIN
    IF verbose THEN ErrLog.Msg (" --- ", FmtTime.Long (now)); END;
    last_update    := now;
    s.modified     := FALSE;
    InitDB (s.new);
    AddBuiltinTypes (s);

    TRY
      iter := FS.Iterate (package_root);
      WHILE iter.next (nm) DO
        s.pkg_name := ID.Add (nm);
        ScanPkg (s);
      END;
    EXCEPT OSError.E (ec) =>
      ErrLog.Msg ("trouble scanning packages", ErrMsg (ec));
    END;

    IF (s.modified) THEN
      BuildNameMaps (s);
      db := s.new;
      ErrLog.Msg (" --- ", FmtTime.Long (Time.Now ()));
    END;

    (* make sure the collector has a chance *)
    ResetDB (s.new);
    s.old_pkg := NIL;
    s.new_pkg := NIL;
    s.new_set := NIL;
  END ScanPackages;

PROCEDURE ScanPkg (VAR s: ScanState) =
  VAR ref: REFANY;  d: IntList.T;  u: UnitSet;
  BEGIN
    s.new_pkg := NIL;
    IF db.packages.get (s.pkg_name, ref)
      THEN s.old_pkg := ref;
      ELSE s.old_pkg := NIL;
    END;

    (* scan the derived directories *)
    d := derived_dirs;
    WHILE (d # NIL) DO
      s.dir_name := d.head;
      u := ScanDir (s);
      IF (u # NIL) THEN
        IF (s.new_pkg = NIL) THEN
          s.new_pkg := NEW (Pkg, name := s.pkg_name);
          EVAL s.new.packages.put (s.pkg_name, s.new_pkg);
        END;
        u.pkg := s.new_pkg;
        s.new_pkg.sets := RefList.Cons (u, s.new_pkg.sets);
      END;
      d := d.tail;
    END;
  END ScanPkg;

PROCEDURE ScanDir (VAR s: ScanState): UnitSet =
  VAR
    us_path       := MakePath (ID.ToText (s.pkg_name), ID.ToText (s.dir_name));
    path          := MakePath (package_root, us_path);
    webinfo       := MakePath (path, ".M3WEB");
    webinfo_time  := OS.ModTime (webinfo);
    m3exports     := MakePath (path, ".M3EXPORTS");
    m3export_time := OS.ModTime (m3exports);
    us            : UnitSet := NIL;
    old_set       : UnitSet := NIL;
    x             : RefList.T;
  BEGIN
    IF (m3export_time = OS.NO_TIME) AND (webinfo_time = OS.NO_TIME) THEN
      RETURN NIL;
    END;

    IF (s.old_pkg # NIL) THEN
      (* search for a match in the old package *)
      x := s.old_pkg.sets;
      WHILE (x # NIL) DO
        us := x.head;
        IF Text.Equal (us.path, us_path) THEN  old_set := us; EXIT;  END;
        x := x.tail;
      END;
    END;

    IF (old_set # NIL)
      AND (old_set.webinfo_time >= webinfo_time)
      AND (old_set.m3export_time >= m3export_time) THEN
      RETURN old_set;
    END;

    (* build a new unit set *)
    us := NEW (UnitSet);
    us.path          := us_path;
    us.name          := ID.NoID;
    us.units         := NIL;
    us.webinfo       := NIL;
    us.webinfo_time  := webinfo_time;
    us.m3export_time := m3export_time;
    us.is_pgm        := FALSE;

    s.modified := TRUE;
    s.new_set := us;

    IF (old_set # NIL) AND (old_set.webinfo_time >= webinfo_time)
      THEN us.webinfo := old_set.webinfo;
      ELSE us.webinfo := BufFromFile (webinfo);
    END;

    ScanExports (s, m3exports);
    IF (us.name = ID.NoID) THEN
      us.name := s.pkg_name;
    END;

    IF verbose THEN ErrLog.Msg ("updated ", us.path); END;

    RETURN us;
  END ScanDir;

(*------------------------------------------------------------ .M3EXPORTS ---*)

PROCEDURE BufFromFile (path: TEXT; pad: CARDINAL := 0) : Buf.T =
  BEGIN
    TRY
      RETURN Buf.FromFile (path, NIL, pad);
    EXCEPT
      OSError.E => RETURN NIL;
    END;
  END BufFromFile;

TYPE
  ParseWord = RECORD start, len: INTEGER; END;

VAR
  hidden_id      := ID.Add ("hidden");
  add_intf_id    := ID.Add ("_map_add_interface");
  add_mod_id     := ID.Add ("_map_add_module");
  add_gintf_id   := ID.Add ("_map_add_generic_interface");
  add_gmod_id    := ID.Add ("_map_add_generic_module");
  add_c_id       := ID.Add ("_map_add_c_source");
  add_h_id       := ID.Add ("_map_add_h_source");
  define_lib_id  := ID.Add ("_define_lib");
  define_pgm_id  := ID.Add ("_define_pgm");

PROCEDURE ScanExports (VAR s: ScanState;  m3exports: TEXT) =
  VAR
    cur : INTEGER;
    len : INTEGER;
    n   : INTEGER;
    x   : ARRAY [0..9] OF ParseWord;
    key : ID.T;
    buf := BufFromFile (m3exports);
  BEGIN
    IF (buf = NIL) THEN
      ErrLog.Msg ("unable to read \"", m3exports, "\"");
      RETURN;
    END;

    cur := 0;
    len := NUMBER (buf^);
    WHILE (cur < len) DO
      n := ParseLine (buf, cur, x);
      IF (n > 0) THEN
        key := ParseID (buf, x[0]);
        IF (key = add_intf_id) THEN
          AddUnit (s, Class.Interface, buf, x[1], x[2], x[3], x[4]);
        ELSIF (key = add_mod_id) THEN
          AddUnit (s, Class.Module, buf, x[1], x[2], x[3], x[4]);
        ELSIF (key = add_gintf_id) THEN
         AddUnit (s, Class.GenInterface, buf, x[1], x[2], x[3], x[4]);
        ELSIF (key = add_gmod_id) THEN
          AddUnit (s, Class.GenModule, buf, x[1], x[2], x[3], x[4]);
        ELSIF (key = add_c_id) THEN
          AddUnit (s, Class.CSource, buf, x[1], x[2], x[3], x[4]);
        ELSIF (key = add_h_id) THEN
          AddUnit (s, Class.HSource, buf, x[1], x[2], x[3], x[4]);
        ELSIF (key = define_lib_id) THEN
          s.new_set.is_pgm := FALSE;
          s.new_set.name   := ParseID (buf, x[1]);
        ELSIF (key = define_pgm_id) THEN
          s.new_set.is_pgm := TRUE;
          s.new_set.name   := ParseID (buf, x[1]);
        END;
      END;
    END;
  END ScanExports;

PROCEDURE ParseLine (buf: Buf.T;  VAR cur: INTEGER;
                      VAR x: ARRAY [0..9] OF ParseWord): INTEGER =
  VAR
    eof := NUMBER (buf^);
    len := 0;
    cnt := 0;
    ch  : CHAR;
  BEGIN
    FOR i := FIRST (x) TO LAST (x) DO  x[i].start := 0;  x[i].len := 0;  END;

    ch := buf[cur]; INC (cur);
    WHILE (cur <= eof) AND (ch # '\n') DO
      IF (ch = '%') THEN
        (* comment to end of line *)
        WHILE (cur < eof) AND (ch # '\n') DO ch := buf[cur];  INC (cur);  END;
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
      ch := buf[cur]; INC (cur);
    END;

    IF (len > 0) THEN
      x [cnt].len := len;
      INC (cnt);
      len := 0;
    END;
    RETURN cnt;
  END ParseLine;

PROCEDURE AddUnit (VAR s: ScanState;  class: Class;  buf: Buf.T;
                     READONLY xname, xpkg, xpkg_dir, xhide: ParseWord) =
  VAR
    name    := ParseID (buf, xname);
    pkg     := ParseID (buf, xpkg);
    pkg_dir := ParseID (buf, xpkg_dir);
    hide    := ParseID (buf, xhide);
    d       := s.new_set;
    dir     := MakePath (ID.ToText (pkg), ID.ToText (pkg_dir));
  BEGIN
    d.units := NEW (Unit, name := name, next := d.units, set := d,
                    dir := dir, class := class, hidden := (hide = hidden_id));
  END AddUnit;

PROCEDURE ParseID (buf: Buf.T;  READONLY x: ParseWord): ID.T =
  BEGIN
    RETURN ID.FromStr (SUBARRAY (buf^, x.start, x.len));
  END ParseID;

(*---------------------------------------------------------------- .M3WEB ---*)

PROCEDURE ScanWebInfo (VAR s: ScanState;  buf: Buf.T) =
  VAR
    c         : CHAR;
    is_intf   : BOOLEAN;
    cur       : INTEGER;
    len       : INTEGER;
    eol       : INTEGER;
    cur_file  : ID.T;
    cur_unit  : ID.T;
    unit      : ID.T;
    uid       : INTEGER;
    type_name : ID.T;
    start     : INTEGER;
    lhs, rhs  : INTEGER;
    super     : INTEGER;
  BEGIN
    IF (buf = NIL) THEN RETURN END;

    cur := 0;
    len := NUMBER (buf^);

    (* skip the table of contents *)
    WHILE (cur < len) AND (buf[cur] # '$')  DO INC (cur); END;
    WHILE (cur < len) AND (buf[cur] # '\n') DO INC (cur); END;

    WHILE (cur < len) DO
      c := buf[cur]; INC (cur);
      eol := cur;
      WHILE (eol < len) AND (buf[eol] # '\n') AND (buf[eol] # '\r') DO
        INC (eol);
      END;
      CASE c OF
      | '@' => (* file name  *)
          cur_file := ID.FromStr (SUBARRAY (buf^, cur, eol-cur));
      | 'A' => (* module name *)
          cur_unit := UnitName (buf, cur, eol-cur, FALSE);
          is_intf := FALSE;
      | 'B' => (* interface name *)
          cur_unit := UnitName (buf, cur, eol-cur, TRUE);
          is_intf := TRUE;
      | 'C' => (* import *)
          unit := UnitName (buf, cur, eol-cur, TRUE);
          IF NOT is_intf OR (unit # cur_unit) THEN
            NoteUse (s.new.importers, cur_unit, unit);
          END;
      | 'D' => (* export *)
          unit := UnitName (buf, cur, eol-cur, TRUE);
          IF NOT is_intf OR (unit # cur_unit) THEN
            NoteUse (s.new.exporters, cur_unit, unit);
          END;
      | 'E' => (* typename *)
          uid := ReadUID (buf, cur);
          SkipBlanks (buf, cur);
          type_name := ID.FromStr (SUBARRAY (buf^, cur, eol-cur));
          NoteTypeName (s, uid, type_name, cur_unit);
      | 'F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'R', 'Y' =>
          start := cur-1;
          uid := ReadUID (buf, cur);
          NoteType (s, uid, buf, start, cur_unit, c);
      | 'O' =>
          start := cur-1;
          uid := ReadUID (buf, cur);
          NoteType (s, uid, buf, start, cur_unit, c);
          NoteSubtype (s, uid, ADDRESS_UID);
      | 'P' =>
          start := cur-1;
          uid := ReadUID (buf, cur);
          NoteType (s, uid, buf, start, cur_unit, c);
          NoteSubtype (s, uid, REFANY_UID);
      | 'U', 'V' =>
          start := cur-1;
          uid := ReadUID (buf, cur);
          super := ReadUID (buf, cur);
          NoteType (s, uid, buf, start, cur_unit, c);
          IF (super # 0) THEN NoteSubtype (s, uid, super); END;
      | 'Z' =>
          lhs := ReadUID (buf, cur);
          SkipBlanks (buf, cur);
          rhs := ReadUID (buf, cur);
          NoteRevelation (s, lhs, rhs);
      | '?' => (* builtin type *)
          start := cur-1;
          uid := ReadUID (buf, cur);
          SkipBlanks (buf, cur);
          type_name := ID.FromStr (SUBARRAY (buf^, cur, eol-cur));
          NoteType (s, uid, buf, start, cur_unit, c);
          NoteTypeName (s, uid, type_name, cur_unit);
      ELSE (* skip *)
      END;
      cur := eol;
      WHILE (cur < len) AND ((buf[cur] = '\n') OR (buf[cur] = '\r')) DO
        INC (cur);
      END;
    END;
  END ScanWebInfo;

PROCEDURE UnitName (buf: Buf.T;  start, len: INTEGER;  intf: BOOLEAN): ID.T =
  CONST CC = ARRAY BOOLEAN OF CHAR { 'm', 'i' };
  VAR xx: ARRAY [0..255] OF CHAR;  n := MIN (NUMBER (xx), len);
  BEGIN
    SUBARRAY (xx, 0, n) := SUBARRAY (buf^, start, n);
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

PROCEDURE NoteTypeName (VAR s: ScanState;  uid: INTEGER;  name, home: ID.T) =
  VAR t := NewType (s, uid);  tn: TypeName;  x: RefList.T;  ref: REFANY;
  BEGIN
    (* search for a duplicate *)
    tn := t.names;
    WHILE (tn # NIL) DO
      IF (tn.name = name) AND (tn.home = home) THEN RETURN END;
      tn := tn.next;
    END;

    (* create a new name *)
    tn := NEW (TypeName, next := NIL, name := name, home := home);
    IF (t.names # NIL) THEN
      (* preserve the "first" name *)
      tn.next := t.names.next;
      t.names.next := tn;
    ELSE
      t.names := tn;
    END;

    (* register the name in the table *)
    IF s.new.type_names.get (name, ref) THEN
      x := ref;
      x.tail := RefList.Cons (t, x.tail);
    ELSE
      EVAL s.new.type_names.put (name, RefList.List1 (t));
    END;
  END NoteTypeName;

PROCEDURE NoteType (VAR s: ScanState;  uid: INTEGER;  buf: Buf.T;
                    start: INTEGER;  home: ID.T;  class: CHAR) =
  VAR t := NewType (s, uid);
  BEGIN
    IF (t.home = ID.NoID) THEN
      t.class  := class;
      t.home   := home;
      t.defn   := buf;
      t.start  := start;
    END;
  END NoteType;

PROCEDURE NoteRevelation (VAR s: ScanState;  lhs, rhs: INTEGER) =
  BEGIN
    EVAL s.new.revelations.put (lhs, rhs);
    EVAL s.new.opaques.put (rhs, lhs);
  END NoteRevelation;

PROCEDURE NoteSubtype (VAR s: ScanState;  subtype, super: INTEGER) =
  VAR sub := NewType (s, subtype);  sup := NewType (s, super);
  BEGIN
    IF (sub.super = NIL) THEN
      sub.super := sup;
      sub.next_peer := sup.subtypes;
      sup.subtypes := sub;
    ELSIF (sub.super # sup) THEN
      ErrLog.Msg ("two super types for ", FmtUID(sub.uid),
                    " => ", FmtUID (sub.super.uid) &" and "& FmtUID (sup.uid));
    END;
  END NoteSubtype;

PROCEDURE SuperType (t: Type): Type =
  BEGIN
    IF (t = NIL) THEN
      RETURN NIL;
    ELSIF (t.super # NIL) THEN
      RETURN t.super;
    ELSIF TranslateOpaque (t.uid, t) AND (t.super # NIL) THEN
      RETURN t.super;
    ELSE
      RETURN NIL;
    END;
  END SuperType;

PROCEDURE TranslateOpaque (lhs_uid: INTEGER;  VAR rhs: Type): BOOLEAN =
  VAR rhs_uid: INTEGER;  ref: REFANY;
  BEGIN
    IF db.revelations.get (lhs_uid, rhs_uid)
      AND db.type_ids.get (rhs_uid, ref) THEN
      rhs := ref;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END TranslateOpaque;

PROCEDURE FindOpaque (rhs_uid: INTEGER;  VAR lhs: Type): BOOLEAN =
  VAR lhs_uid: INTEGER;  ref: REFANY;
  BEGIN
    IF db.opaques.get (rhs_uid, lhs_uid)
      AND db.type_ids.get (lhs_uid, ref) THEN
      lhs := ref;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END FindOpaque;

PROCEDURE NewType (VAR s: ScanState;  uid: INTEGER): Type =
  VAR t: Type;  ref: REFANY;
  BEGIN
    IF s.new.type_ids.get (uid, ref) THEN
      t := ref;
    ELSE
      t := NEW (Type, uid := uid, class := '\000');
      EVAL s.new.type_ids.put (uid, t);
    END;
    RETURN t;
  END NewType;

(*-------------------------------------------------------------- name map ---*)

PROCEDURE BuildNameMaps (VAR s: ScanState) =
  VAR
    it   := s.new.packages.iterate ();
    pkg  : Pkg;
    set  : UnitSet;
    unit : Unit;
    x    : RefList.T;
    nm   : INTEGER;
    ref  : REFANY;
  BEGIN
    WHILE it.next (nm, ref) DO
      pkg := ref;
      x := pkg.sets;
      WHILE (x # NIL) DO
        set := x.head;
        IF (set.name # ID.NoID) THEN
          IF (set.is_pgm)
            THEN AddName (s.new.pgms, set.name, set);
            ELSE AddName (s.new.libs, set.name, set);
          END;
        END;
        unit := set.units;
        WHILE (unit # NIL) DO
          AddName (s.new.units, unit.name, unit);
          unit := unit.next;
        END;
        ScanWebInfo (s, set.webinfo);
        x := x.tail;
      END;
    END;

  END BuildNameMaps;

CONST
  BuiltinDesc =
      "$ 0\n"
    & "@**PREDEFINED**\n"
    & "BM3_BUILTIN\n"
    & "?195c2a74 INTEGER\n"
    & "?97e237e2 CARDINAL\n"
    & "?1e59237d BOOLEAN\n"
    & "?08402063 ADDRESS\n"
    & "?56e16863 CHAR\n"
    & "?48e16572 REAL\n"
    & "?94fe32f6 LONGREAL\n"
    & "?9ee024e3 EXTENDED\n"
    & "?48ec756e NULL\n"
    & "?1c1c45e6 REFANY\n"
    & "?00000000 VOID\n"
    & "V9d8fb489 00000000 0 0 0 4\n" (* ROOT = OBJECT END *)
    & "E9d8fb489 ROOT\n"
    & "U898ea789 00000000 0 0 0 4\n" (* UNTRACED ROOT = UNTRACED OBJECT END *)
    & "E898ea789 UNTRACED-ROOT\n"
    & "Y50f86574 1c1c45e6\n" (* TEXT <: REFANY *)
    & "E50f86574 TEXT\n"
    & "Y1541f475 9d8fb489\n" (* MUTEX <: ROOT *)
    & "E1541f475 MUTEX\n"
    ;

VAR(*CONST*) BuiltinBuf := Buf.FromText (BuiltinDesc);

PROCEDURE AddBuiltinTypes (VAR s: ScanState) =
  BEGIN
    ScanWebInfo (s, BuiltinBuf);
    NoteSubtype (s, UNROOT_UID, ADDRESS_UID); (* UNTRACED-ROOT <: ADDRESS *)
    NoteSubtype (s, ROOT_UID,   REFANY_UID);  (* ROOT <: REFANY *)
    NoteSubtype (s, NULL_UID,   REFANY_UID);  (* NULL <: REFANY *)
    (*** too messy for the current data structures ****************
    NoteSubtype (s, NULL_UID,   ADDRESS_UID); (* NULL <: ADDRESS *)
    ***************************************************************)
  END AddBuiltinTypes;

PROCEDURE AddName (tbl: IntRefTbl.T;  nm: ID.T;  val: REFANY) =
  VAR ref: REFANY;  x: RefList.T;
  BEGIN
    IF tbl.get (nm, ref) THEN
      x := ref;
      x.tail := RefList.Cons (val, x.tail);
    ELSE
      EVAL tbl.put (nm, RefList.List1 (val));
    END;
  END AddName;

(*----------------------------------------------------- misc. I/O support ---*)

PROCEDURE ErrMsg (args: OSError.Code): TEXT =
  VAR msg: TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL)
        THEN msg := ": ";
        ELSE msg := msg & " *** ";
      END;
      msg := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    RETURN msg;
  END ErrMsg;

(*--------------------------------------------------- main request server ---*)

PROCEDURE ProcessRequest (<*UNUSED*>service: TCPServer.T;  req: TEXT): TEXT =
  VAR
    len   := Text.Length (req);
    wx    := Wx.New ();
    cmd   : CHAR;
    query : TextVec;
    buf   : Buf.T;
    orig  : TEXT;
    answer: TEXT;
  BEGIN
    orig := req;
    INC (n_queries);
    IF (len < 5) THEN
      ErrLog.Msg ("request too short: ", req);
      RETURN "HTTP/1.0 400 request too short: " & req & "\r\n";
    ELSIF NOT Text.Equal (Text.Sub (req, 0, 5), "GET /") THEN
      ErrLog.Msg ("unknown request: ", req);
      RETURN "HTTP/1.0 400 unknown request: " & req & "\r\n";
    END;

    IF len = 5 THEN req := req & " "; len := 6; END;  (* "GET /" -> "GET / " *)
    cmd := Text.GetChar (req, 5);
    buf := NEW (Buf.T, len - 6);
    Text.SetChars (buf^, Text.Sub (req, 6));
    ParseQuery (buf^, req, query);
    IF Text.Equal (req, "HTTP/1.0") THEN req := ""; END;

    Wx.PutText (wx, "HTTP/1.0 200 ok\r\n");
    IF (cmd # 'Z') THEN Wx.PutText (wx, "Content-type: text/html\n\n"); END;
    CASE cmd OF
    | '0' =>  GenUnitSetList (query, wx, FALSE);
    | '1' =>  GenIntfList (req, query, wx);
    | '2' =>  GenImplList (req, query, wx);
    | '3' =>  GenAnyUnit (req, wx);
    | '4' =>  GenExportUnit (req, wx);
    | '5' =>  GenAnyUnitSet (req, wx, FALSE);
    | '6' =>  GenOneUnit (req, wx);
    | '8' =>  GenOneUnitSet  (req, wx, FALSE);
    | '9' =>  GenUnitSetPrefix (req, query, wx, FALSE);
    | 'A' =>  GenUnitPrefix (req, wx, Class.GenInterface, query);
    | 'B' =>  GenUnitPrefix (req, wx, Class.Interface, query);
    | 'C' =>  GenUnitPrefix (req, wx, Class.GenModule, query);
    | 'D' =>  GenUnitPrefix (req, wx, Class.Module, query);
    | 'E' =>  GenUnitPrefix (req, wx, Class.CSource, query);
    | 'F' =>  GenUnitPrefix (req, wx, Class.HSource, query);
    | 'G' =>  GenUnitSetList (query, wx, TRUE);
    | 'H' =>  GenAnyUnitSet (req, wx, TRUE);
    | 'I' =>  GenOneUnitSet  (req, wx, TRUE);
    | 'J' =>  GenUnitSetPrefix (req, query, wx, TRUE);
    | 'K' =>  GenTypeList (req, query, wx);
    | 'L' =>  GenOneType (req, wx);
    | 'M' =>  GenTypePrefix (req, query, wx);
    | 'N' =>  GenTypeFromUID (req, wx, FALSE);
    | 'O' =>  GenTypeFromUID (req, wx, TRUE);
    | 'P' =>  GenTypeGraph (req, wx);
    | 'Q' =>  GenFlatType (req, wx);
    | 'R' =>  GenImportUnits (req, wx);
    | 'S' =>  GenProcExporterUnit (req, wx);
    | 'X' =>  RTutils.Heap (TRUE, RTutils.HeapPresentation.ByByteCount);
    | 'Y' =>  RTHeapStats.ReportReachable ();
    | 'Z' =>  Wx.PutText (wx, "Content-type: application/edit\n\n");
              Wx.PutText (wx, req);
              Wx.PutText (wx, "\n");
    | '\n', '\r', '\t', ' ' => CreateTitlePage (wx, relative := TRUE);
    ELSE
      req := Text.FromChar (cmd) & req;
      ErrLog.Msg ("unknown request: `GET /", req, "'");
      RETURN "HTTP/1.0 400 unknown request: GET /" & req & "\r\n";
    END;

    answer := Wx.ToText (wx);

    (* make sure the collector has a chance *)
    wx    := NIL;
    query := NIL;
    buf   := NIL;
    orig  := NIL;

    RETURN answer;
  END ProcessRequest;

PROCEDURE ParseQuery (VAR buf   : ARRAY OF CHAR;
                      VAR req   : TEXT;
                      VAR query : TextVec) =

  VAR
    len := NUMBER (buf);
    ch, cx: CHAR;
    start: INTEGER := 0;
    s0, end, n_words: INTEGER;

  PROCEDURE AddWord (a, b: INTEGER) =
    BEGIN
      IF (n_words < NUMBER (query^)) AND (b > a) THEN
        query [n_words] := Text.FromChars (SUBARRAY (buf, a, b-a));
        INC (n_words);
      END;
    END AddWord;

  BEGIN
    req := "";
    query := NIL;
    IF (len <= 0) THEN RETURN; END;

    (* skip leading white space *)
    WHILE (start < len) DO
      ch := buf[start];
      IF (ch # ' ') AND (ch # '\n') AND (ch # '\r') AND (ch # '\t') THEN
        EXIT;
      END;
      INC (start);
    END;

    (* find the end of the request *)
    end := start;
    WHILE (end < len) DO
      ch := buf[end];
      IF (ch = ' ') OR (ch = '?') OR (ch = '\n')
        OR (ch = '\r') OR (ch = '\t') THEN
        req := Text.FromChars (SUBARRAY (buf, start, end - start));
        EXIT;
      END;
      INC (end);
    END;

    IF (ch # '?') THEN RETURN END;

    (* count the words in the query *)
    start := end;  (* == index of '?' *)
    end := start+1;  n_words := 1;
    WHILE (end < len) DO
      ch := buf[end];
      IF (ch = '+') THEN INC (n_words) END;
      INC (end);
    END;

    (* allocate space for the query *)
    query := NEW (TextVec, n_words);

    (* extract the words *)
    INC (start);  s0 := start; end := start; n_words := 0;
    LOOP
      IF (end >= len) THEN
        AddWord (start, s0);
        EXIT;
      END;
      ch := buf[end];
      IF (ch = '+') THEN
        AddWord (start, s0);
        start := end + 1;  s0 := start;  end := start;
      ELSIF (ch = '%') THEN
        (* grab the next two letters and build the ascii character *)
        INC (end);  ch := buf [MIN (len-1, end)];
        INC (end);  cx := buf [MIN (len-1, end)];
        buf [s0] := HexChar (ch, cx);  INC (s0);
        INC (end);
      ELSIF (ch = ' ') OR (ch = '\n') OR (ch = '\r') OR (ch = '\t') THEN
        (* end of string *)
        AddWord (start, s0);
        EXIT;
      ELSE
        (* add the character *)
        buf [s0] := ch;  INC (s0);
        INC (end);
      END;
    END;

    (* make sure the query isn't filled with NILs *)
    IF (n_words <= 0) THEN
      query := NIL;
    ELSIF (n_words < NUMBER (query^)) THEN
      VAR new_query := NEW (TextVec, n_words); BEGIN
        new_query^ := SUBARRAY (query^, 0, n_words);
        query := new_query;
      END;
    END;
  END ParseQuery;

PROCEDURE HexChar (a, b: CHAR): CHAR =
  VAR n := 0;
  BEGIN
    IF    ('0' <= a) AND (a <= '9') THEN  n := ORD(a) - ORD ('0');
    ELSIF ('A' <= a) AND (a <= 'F') THEN  n := ORD(a) - ORD ('A');
    ELSIF ('a' <= a) AND (a <= 'f') THEN  n := ORD(a) - ORD ('a');
    END;
    n := n * 16;
    IF    ('0' <= b) AND (b <= '9') THEN  n := n + ORD(b) - ORD ('0');
    ELSIF ('A' <= b) AND (b <= 'F') THEN  n := n + ORD(b) - ORD ('A');
    ELSIF ('a' <= b) AND (b <= 'f') THEN  n := n + ORD(b) - ORD ('a');
    END;
    RETURN VAL (n, CHAR);
  END HexChar;

(*-------------------------------------------------------------- requests ---*)

PROCEDURE GenUnitSetList (query: TextVec;  wx: Wx.T;  pgm: BOOLEAN) =
  VAR
    it   : IntRefTbl.Iterator;
    x    := NEW (IntSeq.T).init ();
    nm   : INTEGER;
    ref  : REFANY;
    cmd0, cmd1, tag: TEXT;
  BEGIN
    IF (pgm) THEN
      it := db.pgms.iterate (); cmd0 := "H"; cmd1 := "J"; tag := "Programs";
    ELSE
      it := db.libs.iterate (); cmd0 := "5"; cmd1 := "9"; tag := "Libraries";
    END;

    (* collect and count the names *)
    WHILE it.next (nm, ref) DO
      IF (query = NIL) OR QueryMatch (nm, query) THEN
        x.addhi (nm);
      END;
    END;

    (* finally, generate the output *)
    Out (wx, StartPage);
    Out (wx, StartTitle, "Modula-3 ", tag, " in ");
    Out (wx, package_root, EndTitle);
    Out (wx, IsIndex);
    Out (wx, "<H2>", tag, " in ", package_root, "</H2>\n");
    GenDir (cmd0, cmd1, Flatten (x)^, wx, 100);
    Out (wx, EndPage);
  END GenUnitSetList;

PROCEDURE GenUnitSetPrefix (prefix: TEXT;  query: TextVec;  wx: Wx.T;
                            pgm: BOOLEAN) =
  VAR
    it   : IntRefTbl.Iterator;
    nm   : INTEGER;
    ref  : REFANY;
    tag  : TEXT;
    cmd0, cmd1: TEXT;
    x    := NEW (IntSeq.T).init ();
    prefix_len := Text.Length (prefix);
  BEGIN
    IF (pgm) THEN
      it := db.pgms.iterate (); cmd0 := "H"; cmd1 := "J"; tag := "Programs";
    ELSE
      it := db.libs.iterate (); cmd0 := "5"; cmd1 := "9"; tag := "Libraries";
    END;

    (* collect and count the names *)
    WHILE it.next (nm, ref) DO
      IF PrefixMatch (prefix, nm, prefix_len) THEN
        IF (query = NIL) OR QueryMatch (nm, query) THEN
          x.addhi (nm);
        END;
      END;
    END;

    (* finally, generate the output *)
    Out (wx, StartPage, StartTitle);
    Out (wx, "Modula-3 ", tag, " (", prefix, "...)");
    Out (wx, EndTitle, IsIndex);
    Out (wx, "<H2>", tag, " (", prefix, "...) </H2>\n");
    GenDir (cmd0, cmd1, Flatten (x)^, wx, 100);
    Out (wx, EndPage);
  END GenUnitSetPrefix;

PROCEDURE GenIntfList (req: TEXT;  query: TextVec;  wx: Wx.T) =
  VAR limit := 30;
  BEGIN
    IF (req # NIL) AND (Text.Length (req) > 0) THEN
      (* generate the full, flat list *)
      limit := LAST(INTEGER);
    END;
    Out (wx, StartPage);
    Out (wx, StartTitle, "Modula-3 Interfaces", EndTitle);
    Out (wx, IsIndex);
    GenList (wx, Class.Interface, limit, query);
    GenList (wx, Class.GenInterface, limit, query);
    GenList (wx, Class.HSource, limit, query);
    Out (wx, EndPage);
  END GenIntfList;

PROCEDURE GenImplList (req: TEXT;  query: TextVec;  wx: Wx.T) =
  VAR limit := 30;
  BEGIN
    IF (req # NIL) AND (Text.Length (req) > 0) THEN
      (* generate the full, flat list *)
      limit := LAST(INTEGER);
    END;
    Out (wx, StartPage);
    Out (wx, StartTitle, "Modula-3 Implementations", EndTitle);
    Out (wx, IsIndex);
    GenList (wx, Class.Module, limit, query);
    GenList (wx, Class.GenModule, limit, query);
    GenList (wx, Class.CSource, limit, query);
    Out (wx, EndPage);
  END GenImplList;

PROCEDURE GenList (wx: Wx.T;  c: Class;  limit: INTEGER;  query: TextVec) =
  VAR elts := UnitVector (c, NIL, query);
  BEGIN
    IF NUMBER (elts^) = 0 THEN RETURN END;
    Out (wx, "<H2>", ClassTags[c], "</H2>\n");
    GenDir ("3", ClassPrefix [c], elts^, wx, limit);
  END GenList;

PROCEDURE GenUnitPrefix (req: TEXT;  wx: Wx.T;  c: Class;  query: TextVec) =
  VAR elts := UnitVector (c, req, query);
  BEGIN
    Out (wx, StartPage);
    Out (wx, StartTitle, "Modula-3 ", ClassTags[c]);
    Out (wx, " (", req, "...)", EndTitle);
    Out (wx, IsIndex);
    Out (wx, "<H2>", ClassTags[c]);
    Out (wx, " (", req, "...)</H2>\n");
    IF NUMBER (elts^) # 0 THEN
      GenDir ("3", ClassPrefix [c], elts^, wx, 50)
    END;
    Out (wx, EndPage);
  END GenUnitPrefix;

PROCEDURE UnitVector (c: Class;  prefix: TEXT;  query: TextVec): IntVec =
  VAR
    name: INTEGER;
    ref: REFANY;
    prefix_len: INTEGER;
    it := db.units.iterate ();
    x  : RefList.T := NIL;
    u  : Unit;
    z  := NEW (IntSeq.T).init ();
  BEGIN
    IF (prefix # NIL) THEN prefix_len := Text.Length (prefix); END;
    
    (* collect and count the names *)
    WHILE it.next (name, ref) DO
      IF (prefix = NIL) OR PrefixMatch (prefix, name, prefix_len) THEN
        IF (query = NIL) OR QueryMatch (name, query) THEN
          x := ref;
          WHILE (x # NIL) DO
            u := x.head;
            IF (u.class = c) THEN
              z.addhi (u.name);
              EXIT;
            END;
            x := x.tail;
          END;
        END;
      END;
    END;

    RETURN Flatten (z);
  END UnitVector;

PROCEDURE GenAnyUnit (name: TEXT;  wx: Wx.T) =
  VAR x: RefList.T;  ref: REFANY;  u: Unit;  nm := ID.Add (name);
  BEGIN
    IF NOT db.units.get (nm, ref) THEN
      Out (wx, StartPage);
      Out (wx, StartTitle, name, EndTitle);
      Out (wx, "<H2>", name, ":</H2>\n");
      Out (wx, "<STRONG> *unknown* </STRONG>\n");
      Out (wx, EndPage);
      RETURN;
    END;
    x := RemoveDuplicateUnits (ref);
    u := x.head;

    Out (wx, StartPage);
    Out (wx, StartTitle, ClassTitle [u.class], name, EndTitle);
    IF (x.tail # NIL) THEN
      Out (wx, "<H2>", ID.ToText (u.name), ":</H2>\n");
      GenChoices ("6", x, wx);
    ELSE
      Out (wx, "<H2>", u.dir, SLASH, ID.ToText (u.name), ":</H2>\n");
      GenInstances ("6", ref, wx);
      GenExporters (name, wx);
      GenImportLink (nm, name, wx);
      GenUnit (u, wx);
    END;
    Out (wx, EndPage);
  END GenAnyUnit;

PROCEDURE GenOneUnit (req: TEXT;  wx: Wx.T) =
  VAR x: RefList.T;  n_units: INTEGER;  name: TEXT;  dir: TEXT;
      u: Unit; ref: REFANY;  nm: ID.T;
  BEGIN
    SplitReq (req, nm, dir);
    name := ID.ToText (nm);
    IF NOT db.units.get (nm, ref) THEN
      Out (wx, StartPage);
      Out (wx, StartTitle, name, EndTitle);
      Out (wx, "<H2>", name, ":</H2>\n");
      Out (wx, "<STRONG> *unknown* </STRONG>\n");
      Out (wx, EndPage);
      RETURN;
    END;
    x := RemoveDuplicateUnits (ref);

    (* check for multiple units *)
    ScanChoices (x, dir, n_units, u);

    Out (wx, StartPage);
    IF (n_units = 0) THEN
      u := x.head;
      Out (wx, StartTitle, ClassTitle [u.class], name, EndTitle);
      Out (wx, "<H2>", dir, SLASH, name, ":</H2>\n");
      Out (wx, "<STRONG> *deleted* </STRONG>\n");
    ELSE
      Out (wx, StartTitle, ClassTitle [u.class], name, EndTitle);
      Out (wx, "<H2>", u.dir, SLASH, ID.ToText (u.name), ":</H2>\n");
      GenInstances ("6", ref, wx);
      GenExporters (name, wx);
      GenImportLink (nm, name, wx);
      GenUnit (u, wx);
    END;
    Out (wx, EndPage);
  END GenOneUnit;

PROCEDURE RemoveDuplicateUnits (x: RefList.T): RefList.T =
  (* remove duplicate source units.  All units in 'x' are assumed
     to have the same name.  *)
  VAR seen: TextIntTbl.T;  z: RefList.T := NIL;  u, v: Unit;
  BEGIN
    IF (x = NIL) OR (x.tail = NIL) THEN
      (* zero or one element list *)
      RETURN x;
    ELSIF (x.tail.tail = NIL) THEN
      (* two element list *)
      u := x.head;
      v := x.tail.head;
      IF Text.Equal (u.dir, v.dir)
        THEN RETURN x.tail;
        ELSE RETURN x;
      END;
    END;

    (* otherwise, build a table and check each one... *)
    seen := NEW (TextIntTbl.Default).init ();
    WHILE (x # NIL) DO
      u := x.head;
      IF NOT seen.put (u.dir, 0) THEN
        z := RefList.Cons (u, z);
      END;
      x := x.tail;
    END;
    RETURN z;
  END RemoveDuplicateUnits;

PROCEDURE SplitReq (req: TEXT;  VAR name: ID.T;  VAR dir: TEXT) =
  VAR len := Text.Length (req);  j: CARDINAL;
  BEGIN
    j := 0;
    WHILE (j < len) AND Text.GetChar (req, j) # '@' DO INC (j) END;
    IF (j < len) THEN
      name := ID.Add (Text.Sub (req, 0, j));
      dir  := Text.Sub (req, j+1, len - j - 1);
    ELSE
      name := ID.Add (req);
      dir  := "";
    END;
  END SplitReq;

PROCEDURE GenInstances (cmd: TEXT;  x: RefList.T;  wx: Wx.T) =
  CONST LibCmd = ARRAY BOOLEAN OF TEXT { "8", "I" };
  VAR single := FALSE;  u: Unit;  lib: TEXT;  uname: TEXT;
  BEGIN
    IF (x = NIL) THEN RETURN END;
    single := (x.tail = NIL);
    u := x.head;
    uname := ID.ToText (u.name);
    Out (wx, "<H4>", uname, " is contained in:</H4>\n");
    Out (wx, "<UL>\n");
    WHILE (x # NIL) DO
      u := x.head;
      lib := LibName (ID.ToText (u.set.name), u.set.is_pgm);
      Out (wx, "<LI><A HREF=\"/", LibCmd[u.set.is_pgm]);
      Out (wx, ID.ToText (u.set.name), "@", u.set.path, "\">");
      Out (wx, u.set.path, SLASH, lib);
      Out (wx, "</A>: <A HREF=\"/", cmd, uname, "@", u.dir);
      Out (wx, "\">", u.dir, SLASH, uname);
      Out (wx, "</A>\n");
      x := x.tail;
    END;
    Out (wx, "</UL>\n");
  END GenInstances;

PROCEDURE GenExporters (name: TEXT;  wx: Wx.T) =
  VAR ref: REFANY;  id: IntList.T;  txt: TEXT;  nm := ID.Add (name);
  BEGIN
    IF db.exporters.get (nm, ref) THEN
      Out (wx, "<H4>exported by:</H4>\n");
      Out (wx, "<UL>\n");
      id := ref;
      WHILE (id # NIL) DO
        txt := ID.ToText (id.head);
        Out (wx, "<LI><A HREF=\"/3", txt, "\">", txt, "</A>\n");
        id := id.tail;
      END;
      Out (wx, "</UL>\n");
    END;
  END GenExporters;

PROCEDURE GenExportUnit (name: TEXT;  wx: Wx.T) =
  VAR ref: REFANY;  id: IntList.T;  nm := ID.Add (name);
  BEGIN
    (* look up my exporters *)
    IF NOT db.exporters.get (nm, ref) OR (ref = NIL) THEN
      Out (wx, StartPage);
      Out (wx, StartTitle, "Exporters of ", name, EndTitle);
      Out (wx, "<H2>Exporters of ", name, ":</H2>\n");
      Out (wx, "<STRONG> *unknown* </STRONG>\n");
      Out (wx, EndPage);
      RETURN;
    END;
    id := ref;

    IF (id.tail = NIL) THEN
      (* there's a unique exporter *)
      GenAnyUnit (ID.ToText (id.head), wx);
    ELSE
      Out (wx, StartPage);
      Out (wx, StartTitle, "Exporters of ", name, EndTitle);
      Out (wx, "<H2>", name, ":</H2>\n");
      GenExporters (name, wx);
      Out (wx, EndPage);
    END;
  END GenExportUnit;

PROCEDURE GenProcExporterUnit (name: TEXT;  wx: Wx.T) =
  VAR exp: IntList.T;
      units: RefList.T;
      unit: Unit;
      matchingUnit: Unit := NIL;
      ref: REFANY;
      tmpWx: Wx.T;
      interface, procedure: TEXT;
      unitText: TEXT;
      matchingUnitText: TEXT;

  BEGIN
    (* extract interface/procedure name *)
    interface := Text.Sub(name, start := 0,
                   length := Text.FindCharR(name, c:= '.'));
    procedure := Text.Sub(name, start := Text.FindCharR(name, c:= '.') + 1);
    
    (* look up exporters *)
    IF (db.exporters.get(ID.Add(interface), ref)) THEN
      exp := ref;
      (* find the procedure's exporter *)
      WHILE (exp # NIL) DO
        IF (db.units.get(exp.head, ref)) THEN
          units := RemoveDuplicateUnits(ref);
          WHILE (units # NIL) DO
            unit := units.head;
            (* generate the unit into a temporary wx *)
            tmpWx := Wx.New();
            GenUnit(unit, tmpWx);
            unitText := Wx.ToText(tmpWx);
            (* check if the header for the procedure is in the unit *)
            IF CharMap.Substr(unitText, "<A NAME=\"" & procedure & "\">") THEN
              IF matchingUnit # NIL THEN
                (* mulitple units with this header: generate choice list *)
                GenChoicesWithHeaders("6", RemoveDuplicateUnits(ref),
                                      procedure, wx);
                RETURN;
              END;
              matchingUnit := unit;
              matchingUnitText := unitText;
            END;
            units := units.tail;
          END;
          IF matchingUnit # NIL THEN
            (* unique exporter: emit *)
            WITH name = ID.ToText(matchingUnit.name) DO
              Out (wx, StartPage);
              Out (wx, StartTitle, ClassTitle [matchingUnit.class],
                name, EndTitle);
              Out(wx, "<H2>", matchingUnit.dir, SLASH, name, ":</H2>\n");
              GenInstances("6", ref, wx);
              GenExporters(name, wx);
              GenImportLink(matchingUnit.name, name, wx);
              Out(wx, matchingUnitText);
            END;
            RETURN;
          END;
        END;
        exp := exp.tail;
      END;
    END;
    Out(wx, StartTitle, "Implementation of ", procedure, EndTitle);
    Out(wx, "<H2>Implementation of ", procedure, ":</H2>\n");
    Out(wx, "<STRONG> *unknown* </STRONG>\n");
    Out (wx, EndPage);
  END GenProcExporterUnit; 

PROCEDURE GenImportLink (nm: ID.T;  name: TEXT;  wx: Wx.T) =
  VAR ref: REFANY;
  BEGIN
    IF db.importers.get (nm, ref) THEN
      Out (wx, name, " is imported by <A HREF=\"R", name, "\">");
      Out (wx, Fmt.Int (IntList.Length (ref)), " units</A><P>\n");
    END;
  END GenImportLink;

PROCEDURE GenImportUnits (name: TEXT;  wx: Wx.T) =
  VAR ref: REFANY;  id: IntList.T;  nm := ID.Add (name);
  BEGIN
    (* look up my importers *)
    IF NOT db.importers.get (nm, ref) OR (ref = NIL) THEN
      Out (wx, StartPage);
      Out (wx, StartTitle, "Importers of ", name, EndTitle);
      Out (wx, "<H2>Importers of ", name, ":</H2>\n");
      Out (wx, "<STRONG> *unknown* </STRONG>\n");
      Out (wx, EndPage);
    RETURN;
    END;
    id := ref;

    IF (id.tail = NIL) THEN
      (* there's a unique importer *)
      GenAnyUnit (ID.ToText (id.head), wx);
    ELSE
      Out (wx, StartPage);
      Out (wx, StartTitle, "Importers of ", name, EndTitle);
      Out (wx, "<H2>Importers of ", name, ":</H2>\n");
      GenImporters (id, wx);
      Out (wx, EndPage);
    END;
  END GenImportUnits;

PROCEDURE GenImporters (imp: IntList.T;  wx: Wx.T) =
  VAR z := NEW (IntSeq.T).init ();
  BEGIN
    WHILE (imp # NIL) DO
      z.addhi (imp.head);
      imp := imp.tail;
    END;
    GenDir ("3", "3", Flatten (z)^, wx, LAST(INTEGER));
  END GenImporters;

PROCEDURE ScanChoices (x: RefList.T;  dir: TEXT;
                       VAR cnt: INTEGER;  VAR u: Unit) =
  VAR n := 0;  uu: Unit;
  BEGIN
    WHILE (x # NIL) DO
      uu := x.head;
      IF (uu.dir # NIL) AND ((dir = NIL) OR Text.Equal (dir, uu.dir)) THEN
        u := uu;
        INC (n);
      END;
      x := x.tail;
    END;
    cnt := n;
  END ScanChoices;

PROCEDURE GenChoices (cmd: TEXT;  x: RefList.T;  wx: Wx.T) =
  VAR u: Unit;
  BEGIN
    Out (wx, "<H3>Select an instance:</H3>\n");
    Out (wx, "<UL>\n");
    TRY
      WHILE (x # NIL) DO
        (* this one is still current *)
        u := x.head;
        Out (wx, "<LI><A HREF=\"/", cmd, ID.ToText (u.name), "@", u.dir);
        Out (wx, "\">", u.dir, "</A>\n");
        x := x.tail;
      END;
    FINALLY
      Out (wx, "</UL>\n");
    END;
  END GenChoices;

PROCEDURE GenChoicesWithHeaders (cmd: TEXT;  x: RefList.T;  h: TEXT;  wx: Wx.T) =
  VAR u: Unit;
  BEGIN
    Out (wx, "<H3>Select an instance:</H3>\n");
    Out (wx, "<UL>\n");
    TRY
      WHILE (x # NIL) DO
        (* this one is still current *)
        u := x.head;
        Out (wx, "<LI><A HREF=\"/", cmd, ID.ToText (u.name), "@", u.dir);
        Out (wx, "#", h, "\">", u.dir, "</A>\n");
        x := x.tail;
      END;
    FINALLY
      Out (wx, "</UL>\n");
    END;
  END GenChoicesWithHeaders; 

PROCEDURE GenUnit (u: Unit;  wx: Wx.T) =
  VAR file := MakePath (package_root, u.dir, ID.ToText (u.name));  buf: Buf.T;
  BEGIN
    (*
    Out (wx, "<A HREF=\"/Z", file, "\">[edit file]</A>\n<P>\n");
    *)

    buf := BufFromFile (file, pad := 1);
    IF (buf = NIL) THEN
      Out (wx, "<STRONG> unable to open ", file, " </STRONG>");
      RETURN;
    END;

    Out (wx, "<HR>\n");
    IF (u.class = Class.CSource) OR (u.class = Class.HSource)
      THEN CMarkUp.Annotate (buf, wx);
      ELSE MarkUp.Annotate (buf, wx);
    END;
  END GenUnit;

(*------------------------------------------------------------- unit sets ---*)

PROCEDURE GenAnyUnitSet (name: TEXT;  wx: Wx.T;  pgm: BOOLEAN) =
  VAR
    us  : UnitSet;
    ref : REFANY;
    x   : RefList.T;
    nm  := ID.Add (name);
    tbl : IntRefTbl.T;
    tag : TEXT;
    cmd0: TEXT;
  BEGIN
    IF (pgm)
      THEN tbl := db.pgms;  tag := "program";  cmd0 := "I";
      ELSE tbl := db.libs;  tag := "library";  cmd0 := "8";
    END;

    IF NOT tbl.get (nm, ref) THEN
      Out (wx, StartPage, StartTitle);
      Out (wx, "Modula-3 ", tag, ": ", name, EndTitle);
      Out (wx, "<H2>", LibName (name, pgm), ":</H2>\n");
      Out (wx, "<STRONG> *unknown* </STRONG>\n");
      Out (wx, EndPage);
      RETURN;
    END;
    x := ref;
    us := x.head;

    Out (wx, StartPage);
    IF (x.tail # NIL) THEN
      Out (wx, StartTitle, "Modula-3 ", tag, ": ", name);
      Out (wx, EndTitle);
      Out (wx, "<H2>", ID.ToText (us.name), ":</H2>\n");
      GenUnitSetChoices (cmd0, x, wx, pgm);
    ELSE
      Out (wx, StartTitle, "Modula-3 ", tag, ": ", name);
      Out (wx, EndTitle, IsIndex);
      Out (wx, "<H2>", us.path, SLASH);
      Out (wx, LibName (name, pgm), ":</H2>\n");
      GenUnitSetContents (us, wx);
    END;
    Out (wx, EndPage);
  END GenAnyUnitSet;

PROCEDURE GenOneUnitSet (req: TEXT;  wx: Wx.T;  pgm: BOOLEAN) =
  VAR
    us     : UnitSet;
    n_libs : INTEGER;
    nm     : ID.T;
    name   : TEXT;
    dir    : TEXT;
    ref    : REFANY;
    x      : RefList.T;
    tbl    : IntRefTbl.T;
    tag    : TEXT;
    cmd0   : TEXT;
  BEGIN
    IF (pgm)
      THEN tbl := db.pgms;  tag := "program";  cmd0 := "I";
      ELSE tbl := db.libs;  tag := "library";  cmd0 := "8";
    END;

    SplitReq (req, nm, dir);
    name := ID.ToText (nm);
    IF NOT tbl.get (nm, ref) THEN
      Out (wx, StartPage, StartTitle);
      Out (wx, "Modula-3 ", tag, ": ", name, EndTitle);
      Out (wx, "<H2>", LibName (name, pgm), ":</H2>\n");
      Out (wx, "<STRONG> *unknown* </STRONG>\n");
      Out (wx, EndPage);
      RETURN;
    END;
    x := ref;

    (* check for multiple libs *)
    ScanUnitSetChoices (x, dir, pgm, n_libs, us);

    Out (wx, StartPage);
    IF (n_libs = 0) THEN
      Out (wx, StartTitle, "Modula-3 ", tag, ": ", name);
      Out (wx, EndTitle);
      Out (wx, "<H2>", dir, SLASH);
      Out (wx, LibName (name, pgm), ":</H2>\n");
      Out (wx, "<STRONG> *deleted* </STRONG>\n");
    ELSE
      Out (wx, StartTitle, "Modula-3 ", tag, ": ", name);
      Out (wx, EndTitle, IsIndex);
      Out (wx, "<H2>", us.path, SLASH);
      Out (wx, LibName (ID.ToText (us.name), pgm), ":</H2>\n");
      GenUnitSetContents (us, wx);
    END;
    Out (wx, EndPage);
  END GenOneUnitSet;

PROCEDURE ScanUnitSetChoices (x: RefList.T;  dir: TEXT;  pgm: BOOLEAN;
                              VAR cnt: INTEGER; VAR last: UnitSet) =
  VAR n := 0;  us: UnitSet;
  BEGIN
    WHILE (x # NIL) DO
      us := x.head;
      IF (us.path # NIL)
        AND (us.is_pgm = pgm)
        AND ((dir = NIL) OR Text.Equal (dir, us.path)) THEN
        (* this one is still current *)
        last := us;
        INC (n);
      END;
      x := x.tail;
    END;
    cnt  := n;
  END ScanUnitSetChoices;

PROCEDURE GenUnitSetChoices (cmd: TEXT;  x: RefList.T;  wx: Wx.T; pgm: BOOLEAN) =
  VAR us: UnitSet;
  BEGIN
    Out (wx, "<H3>Select an instance:</H3>\n");
    Out (wx, "<UL>\n");
    TRY
      WHILE (x # NIL) DO
        us := x.head;
        IF (us.is_pgm = pgm) THEN
          (* this one is still current *)
          Out (wx, "<LI><A HREF=\"/", cmd, ID.ToText (us.name), "@", us.path);
          Out (wx, "\">", us.path, "</A>\n");
        END;
        x := x.tail;
      END;
    FINALLY
      Out (wx, "</UL>\n");
    END;
  END GenUnitSetChoices;

PROCEDURE GenUnitSetContents (us: UnitSet;  wx: Wx.T) =
  VAR cnts: ARRAY Class OF INTEGER;  u: Unit;
  BEGIN
    (* count the number units in each class *)
    FOR c := FIRST (cnts) TO LAST (cnts) DO cnts[c] := 0; END;
    u := us.units;
    WHILE (u # NIL) DO INC (cnts[u.class]);  u := u.next; END;

    FOR c := FIRST (cnts) TO LAST (cnts) DO
      IF (cnts[c] <= 0) THEN
        (* skip *)
      ELSIF (cnts[c] <= 40) THEN
        Out (wx, "<H3>", ClassTags[c], ":</H3>\n");
        GenShortUnitSetIdList (us, c, wx);
      ELSE
        Out (wx, "<H3>", ClassTags[c], ":</H3>\n");
        GenLongUnitSetIdList (us, c, wx, cnts[c]);
      END;
    END;
  END GenUnitSetContents;

PROCEDURE GenShortUnitSetIdList (us: UnitSet;  c: Class;  wx: Wx.T) =
  VAR u := us.units;  n := 0;  elts: ARRAY [0..39] OF INTEGER;
  BEGIN
    WHILE (u # NIL) DO
      IF (u.class = c) THEN
        elts[n] := u.name;  INC (n);
      END;
      u := u.next;
    END;
    GenDir ("3", ClassPrefix [c], SUBARRAY (elts, 0, n), wx, 30);
  END GenShortUnitSetIdList;

PROCEDURE GenLongUnitSetIdList (us: UnitSet;  c: Class;  wx: Wx.T;  n: INTEGER) =
  VAR u := us.units;  elts := NEW (IntVec, n);
  BEGIN
    n := 0;
    WHILE (u # NIL) DO
      IF (u.class = c) THEN
        elts[n] := u.name;  INC (n);
      END;
      u := u.next;
    END;
    GenDir ("3", ClassPrefix [c], elts^, wx, 30);
  END GenLongUnitSetIdList;

(*----------------------------------------------------------------- types ---*)

PROCEDURE GenTypeList (req: TEXT;  query: TextVec;  wx: Wx.T) =
  VAR
    limit   := 60;
    it      := db.type_names.iterate ();
    ref     : REFANY;
    nm      : INTEGER;
    x       := NEW (IntSeq.T).init ();
    last_nm : INTEGER;
  BEGIN
    IF (req # NIL) AND (Text.Length (req) > 0) THEN
      (* generate the full, flat list *)
      limit := LAST(INTEGER);
    END;

    (* extract the list *)
    WHILE it.next (nm, ref) DO
      IF (query = NIL) OR QueryMatch (nm, query) THEN
        x.addhi (nm);
        last_nm := nm;
      END;
    END;

    IF (x.size() = 1) THEN
      GenOneType (ID.ToText (last_nm), wx);
    ELSE
      Out (wx, StartPage);
      Out (wx, StartTitle, "Modula-3 Types", EndTitle);
      Out (wx, IsIndex);
      Out (wx, "<H2>Modula-3 Types </H2>\n");
      GenDir ("L", "M", Flatten (x)^, wx, limit);
      Out (wx, EndPage);
    END;
  END GenTypeList;

PROCEDURE GenTypePrefix (prefix: TEXT;  query: TextVec;  wx: Wx.T) =
  VAR
    limit := 60;
    it    := db.type_names.iterate ();
    ref   : REFANY;
    nm    : INTEGER;
    x     := NEW (IntSeq.T).init ();
    plen  := Text.Length (prefix);
  BEGIN
    (* extract the list *)
    WHILE it.next (nm, ref) DO
      IF PrefixMatch (prefix, nm, plen) THEN
        IF (query = NIL) OR QueryMatch (nm, query) THEN
          x.addhi (nm);
        END;
      END;
    END;

    Out (wx, StartPage);
    Out (wx, StartTitle, "Modula-3 Types (", prefix, "...)", EndTitle);
    Out (wx, IsIndex);
    Out (wx, "<H2>Modula-3 Types (", prefix, "...)</H2>\n");
    GenDir ("L", "M", Flatten (x)^, wx, limit);
    Out (wx, EndPage);
  END GenTypePrefix;

PROCEDURE GenOneType (name: TEXT;  wx: Wx.T) =
  VAR nm := ID.Add (name);  ref: REFANY;  x: RefList.T;
  BEGIN
    IF NOT db.type_names.get (nm, ref) THEN
      Out (wx, StartPage);
      Out (wx, StartTitle, name, EndTitle);
      Out (wx, "<H2>", name, ":</H2>\n");
      Out (wx, "<STRONG> *unknown* </STRONG>\n");
      Out (wx, EndPage);
      RETURN;
    END;
    x := ref;

    IF (x.tail # NIL) THEN
      Out (wx, StartPage);
      Out (wx, StartTitle, "Modula-3 Type: ", name, EndTitle);
      Out (wx, "<H2>", name, ":</H2>\n");
      GenTypeChoices (x, wx);
      Out (wx, EndPage);
    ELSE
      GenType (x.head, wx, FALSE, name);
    END;
  END GenOneType;

PROCEDURE GenTypeChoices (x: RefList.T;  wx: Wx.T) =
  VAR t: Type;  tn: TypeName;
  BEGIN
    Out (wx, "<H3>Select an instance:</H3>\n");
    Out (wx, "<UL>\n");
    TRY
      WHILE (x # NIL) DO
        t := x.head;
        Out (wx, "<LI><A HREF=\"/N", FmtUID(t.uid), "\">");
        tn := t.names;
        WHILE (tn # NIL) DO
          IF (tn # t.names) THEN Out (wx, ",\n "); END;
          Out (wx, ID.ToText (tn.name), " in ", ID.ToText (tn.home));
          tn := tn.next;
        END;
        Out (wx, "</A>\n");
        x := x.tail;
      END;
    FINALLY
      Out (wx, "</UL>\n");
    END;
  END GenTypeChoices;

PROCEDURE GenTypeFromUID (req: TEXT;  wx: Wx.T;  expanded: BOOLEAN) =
  VAR uid := ScanUID (req);  ref: REFANY;
  BEGIN
    IF NOT db.type_ids.get (uid, ref) THEN
      Out (wx, StartPage);
      Out (wx, StartTitle, "Type &lt;", req, "&gt;", EndTitle);
      Out (wx, "<H2>Type &lt;", req, "&gt;:</H2>\n");
      Out (wx, "<STRONG> *unknown* </STRONG>\n");
      Out (wx, EndPage);
      RETURN;
    END;
    GenType (ref, wx, expanded, NIL);
  END GenTypeFromUID;

PROCEDURE GenTypeGraph (req: TEXT;  wx: Wx.T) =
  VAR
    self     := ScanUID (req);
    name     : TEXT;
    home     : TEXT;
    t        : Type;
    indent   : INTEGER;
    ref      : REFANY;
    cnt      : ARRAY [0..4] OF INTEGER;
    maxDepth : INTEGER;
    total    : INTEGER;
  BEGIN
    GetTypeName (self, name, home, NIL);
    Out (wx, StartPage);
    Out (wx, StartTitle, "Subtype graph for ", name, EndTitle);
    Out (wx, "<H2>Subtype graph for ", name, ":</H2><P>\n");

    IF NOT db.type_ids.get (self, ref) THEN Out (wx, EndPage); RETURN END;
    t := ref;

    (* find out how deep to print the tree *)
    FOR i := FIRST (cnt) TO LAST (cnt) DO cnt[i] := 0 END;
    CountSubtypes (t, 0, cnt);
    maxDepth := 0;    total := 0;
    WHILE (maxDepth <= LAST (cnt)) AND (total + cnt[maxDepth] < 100) DO
      INC (total, cnt[maxDepth]);
      INC (maxDepth);
    END;
    maxDepth := MAX (1, maxDepth - 1);

    Out (wx, "<PRE>\n");
    indent := GenSuperTypes (t, 0, wx);
    GenSubtypes (t, 0, maxDepth, indent, wx);
    Out (wx, "</PRE>\n");
    Out (wx, EndPage);
  END GenTypeGraph;

PROCEDURE CountSubtypes (t: Type;  depth: INTEGER;  VAR cnt: ARRAY OF INTEGER)=
  VAR u, v: Type;
  BEGIN
    IF (depth <= LAST (cnt)) THEN
      u := t.subtypes;
      WHILE (u # NIL) DO
        INC (cnt[depth]);
        v := u;
        EVAL FindOpaque (u.uid, v);
        CountSubtypes (v, depth+1, cnt);
        u := u.next_peer;
      END;
    END;
  END CountSubtypes;

PROCEDURE GenSuperTypes (t: Type;  depth: INTEGER;  wx: Wx.T): INTEGER =
  VAR in: INTEGER;
  BEGIN
    IF (t = NIL) THEN
      RETURN 0;
    ELSIF (depth >= 99) THEN
      Out (wx, "....\n");
      RETURN 3;
    ELSE
      in := GenSuperTypes (SuperType (t), depth+1, wx);
      IF (depth # 0) THEN in := 0 END; (* hack *)
      GenGraphEntry (t, in, 0, wx, (depth = 0));
      RETURN in + 3;
    END;
  END GenSuperTypes;

PROCEDURE GenGraphEntry (t: Type;  indent, depth: INTEGER;  wx: Wx.T;
                         key: BOOLEAN)=
  VAR name, home: TEXT;
  BEGIN
    GetTypeName (t.uid, name, home, NIL);
    Indent (wx, indent);
    FOR i := 1 TO depth DO Out (wx, "|  "); END;
    Out (wx, "<A HREF=\"/N", FmtUID (t.uid), "\">", name, "</A>");
    IF (home # NIL) THEN
      Out (wx, " in <A HREF=\"/3", home,"\">", home, "</A>");
    END;
    IF (key) THEN
      Out (wx, " <==");
    END;
    Out (wx, "\n");
  END GenGraphEntry;

PROCEDURE GenSubtypes (t: Type;  depth, maxDepth, indent: INTEGER;  wx: Wx.T) =
  VAR z: RefSeq.T;  u, v: Type;
  BEGIN
    IF (t = NIL) OR (t.subtypes = NIL) THEN
      RETURN;
    ELSIF (depth >= maxDepth) THEN
      Indent (wx, indent);
      FOR i := 1 TO depth DO Out (wx, "|  "); END;
      Out (wx, "....\n");
      RETURN;
    ELSE
      z := NEW (RefSeq.T).init ();
      u := t.subtypes;
      WHILE (u # NIL) DO
        IF FindOpaque (u.uid, v)
          THEN z.addhi (v);
          ELSE z.addhi (u);
        END;
        u := u.next_peer;
      END;
      GenSubtypeNames (z, depth, maxDepth, indent, wx);
    END;
  END GenSubtypes;

PROCEDURE GenSubtypeNames (z: RefSeq.T;  depth, maxDepth, indent: INTEGER;
                           wx: Wx.T) =
  TYPE XX = REF ARRAY OF RECORD type: Type;  name, home: TEXT; END;
  VAR
    n   := z.size ();
    map := NEW (IntVec, n);
    xx  := NEW (XX, n);

  PROCEDURE CmpTypeName (a, b: INTEGER): [-1..+1] =
    VAR ca, cb: CHAR;
    BEGIN
      WITH xa = xx[a],  xb = xx[b] DO
        ca := Text.GetChar (xa.name, 0);
        cb := Text.GetChar (xb.name, 0);
        IF    (ca # '&') AND (cb = '&') THEN RETURN -1;
        ELSIF (ca = '&') AND (cb # '&') THEN RETURN +1;
        ELSE RETURN CharMap.CmpText (xa.name, xb.name);
        END;
      END;
    END CmpTypeName;

  BEGIN
    (* build the list of names & homes *)
    FOR i := 0 TO n-1 DO
      map[i] := i;
      WITH zz = xx[i] DO
        zz.type := z.get (i);
        GetTypeName (zz.type.uid, zz.name, zz.home, NIL);
      END;
    END;

    Sort (map^, CmpTypeName);

    FOR i := 0 TO n-1 DO
      WITH zz = xx[map[i]] DO
        GenGraphEntry (zz.type, indent, depth, wx, FALSE);
        GenSubtypes (zz.type, depth+1, maxDepth, indent, wx);
      END;
    END;
  END GenSubtypeNames;

PROCEDURE GetTypeName (uid: INTEGER;  VAR(*OUT*)name, home: TEXT;  pref: TEXT)=
  VAR ref: REFANY;  t, u: Type := NIL;
  BEGIN
    IF db.type_ids.get (uid, ref) THEN
      t := ref;
      IF SetTypeName (t, name, home, pref) THEN RETURN END;
    END;
    IF FindOpaque (uid, u) THEN
      t := u;
      IF SetTypeName (t, name, home, pref) THEN RETURN END;
    END;
    name := "&lt;" & FmtUID (uid) & "&gt;";
    home := NIL;
    IF (t # NIL) THEN home := ID.ToText (t.home); END;
  END GetTypeName;

PROCEDURE SetTypeName (t: Type;  VAR(*OUT*)name, home: TEXT;
                       pref: TEXT): BOOLEAN =
  VAR id: ID.T;  tn: TypeName;
  BEGIN
    IF (t.names = NIL) THEN RETURN FALSE; END;

    IF (pref # NIL) THEN
      (* search for a match *)
      id := ID.Add (pref);
      tn := t.names;
      WHILE (tn # NIL) DO
        IF (tn.name = id) THEN
          name := pref;
          home := ID.ToText (tn.home);
          RETURN TRUE;
        END;
        tn := tn.next;
      END;
    END;

    name := ID.ToText (t.names.name);
    home := ID.ToText (t.names.home);
    RETURN TRUE;
  END SetTypeName;

PROCEDURE GenType (t: Type;  wx: Wx.T;  expanded: BOOLEAN;  pref: TEXT) =
  VAR ex: IntRefTbl.T;
  BEGIN
    GenTypeHeader (t, ORD(expanded), pref, wx);
    IF (t.defn = NIL) THEN Out (wx, EndPage); RETURN END;

    ex := NIL;
    IF expanded THEN
      ex := NEW (IntRefTbl.Default).init ();
      EVAL ex.put (t.uid, NIL);
    END;

    Out (wx, "structure:\n");
    Out (wx, "<PRE>\n");
    FormatType (t, ex, wx);
    Out (wx, "</PRE>\n");
    Out (wx, EndPage);
  END GenType;

PROCEDURE FormatType (t: Type;  expanded: IntRefTbl.T;  wx: Wx.T) =
  VAR fmt := XFormat.New (wx);
  BEGIN
    fmt.putText ("  ");
    fmt.begin (0);
    GenTypeExpr (t.defn, t.start, 1, ' ', expanded, fmt, topLevel := TRUE);
    fmt.end ();
    fmt.flush ();
    fmt.close ();
  END FormatType;

TYPE
  ObjEntry = REF RECORD
    next   : ObjEntry := NIL;
    name   : TEXT     := NIL;
    uid    : INTEGER  := 0;
    dfault : TEXT     := NIL;
    hidden : BOOLEAN  := FALSE;
    source : INTEGER  := 0;
  END;

  ObjEntryQueue = RECORD
    head, tail: ObjEntry := NIL;
  END;

  ObjInfo = RECORD
    traced  : BOOLEAN := FALSE;
    fields  : ObjEntryQueue;
    methods : ObjEntryQueue;
    names   : TextRefTbl.T := NIL;
  END;

PROCEDURE GenFlatType (req: TEXT;  wx: Wx.T) =
  VAR uid := ScanUID (req);  ref: REFANY;  t: Type;  info: ObjInfo;
  BEGIN
    IF NOT db.type_ids.get (uid, ref) THEN
      Out (wx, StartPage);
      Out (wx, StartTitle, "Type &lt;", req, "&gt;", EndTitle);
      Out (wx, "<H2>Type &lt;", req, "&gt;:</H2>\n");
      Out (wx, "<STRONG> *unknown* </STRONG>\n");
      Out (wx, EndPage);
      RETURN;
    END;
    t := ref;
    GenTypeHeader (t, 2, NIL, wx);
    IF (t.defn = NIL) THEN Out (wx, EndPage); RETURN END;

    info.names := NEW (TextRefTbl.Default).init ();
    ExtractObject (t, info);

    Out (wx, "structure:\n");
    Out (wx, "<PRE>\n");
    IF (info.fields.head # NIL) OR (info.methods.head # NIL)
      THEN FormatObject (info, wx);
      ELSE FormatType (t, NIL, wx);
    END;
    Out (wx, "</PRE>\n");
    Out (wx, EndPage);
  END GenFlatType;

PROCEDURE ExtractObject (t: Type;  VAR info: ObjInfo) =
  VAR
    defn          : Buf.T;
    start         : INTEGER;
    eof           : INTEGER;
    ch            : CHAR;
    n_fields      : INTEGER;
    n_methods     : INTEGER;
    n_overrides   : INTEGER;
    n_pending     : INTEGER;
    field_source  : INTEGER;
    method_source : INTEGER;
    id, idX       : TEXT;
    entry         : ObjEntry;
    ref           : REFANY;
    rhs           : Type;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    field_source  := t.uid;
    method_source := t.uid;
    IF TranslateOpaque (t.uid, rhs) THEN t := rhs; END;
    ExtractObject (SuperType (t), info);
    defn := t.defn;
    IF (defn = NIL) THEN RETURN END;
    eof := NUMBER (t.defn^);
    start := t.start;
    IF (start >= eof) THEN RETURN END;
    ch := defn [start]; INC (start);
    IF (ch # 'U') AND (ch # 'V') THEN RETURN END;
    IF (ch = 'V') THEN info.traced := TRUE END;

    EVAL ReadUID (defn, start); (* self *)
    EVAL ReadUID (defn, start); (* super type *)
    n_fields    := ReadInt (defn, start); (* # fields *)
    n_methods   := ReadInt (defn, start); (* # methods *)
    n_overrides := ReadInt (defn, start); (* # overrides *)
    start := NextLine (defn, start);
    n_pending := n_fields + n_methods + n_overrides;

    WHILE (start < eof) AND (n_pending > 0) DO

      ch := defn[start];  INC (start);
      CASE ch OF
      | 'L' => (* field *)
           DEC (n_fields);
           DEC (n_pending);
           entry := NEW (ObjEntry);
           entry.name := ReadName (defn, start);
           EVAL ReadInt (defn, start); (* bit offset *)
           EVAL ReadInt (defn, start); (* bit size *)
           entry.uid := ReadUID (defn, start); (* type *)
           entry.source := field_source;  field_source := 0;
           AddObjEntry (info, info.fields, entry);
      | 'W' => (* method *)
           DEC (n_methods);
           DEC (n_pending);
           entry := NEW (ObjEntry);
           entry.name := ReadName (defn, start);
           entry.uid := ReadUID (defn, start); (* type *)
           entry.dfault := ReadBrand (defn, start);
           entry.source := method_source;  method_source := 0;
           AddObjEntry (info, info.methods, entry);
      | 'X' => (* overrides *)
           DEC (n_overrides);
           DEC (n_pending);
           id  := ReadName (defn, start);
           idX := ReadName (defn, start);
           IF info.names.get (id, ref) THEN
             entry := ref;
             entry.dfault := idX;
           END;
      ELSE
        (* skip *)
      END; (* CASE *)
      IF (n_pending > 0) THEN
        start := NextLine (defn, start);
      END;
    END;
  END ExtractObject;

PROCEDURE AddObjEntry (VAR info: ObjInfo;  VAR q: ObjEntryQueue;  e: ObjEntry)=
  VAR ref: REFANY;  old: ObjEntry;
  BEGIN
    IF (q.head = NIL)
      THEN q.head := e;
      ELSE q.tail.next := e;
    END;
    q.tail := e;
    IF info.names.get (e.name, ref) THEN
      old := ref;
      old.hidden := TRUE;
    END;
    EVAL info.names.put (e.name, e);
  END AddObjEntry;

PROCEDURE FormatObject (READONLY info: ObjInfo;  wx: Wx.T) =
  VAR fmt := XFormat.New (wx);  x: ObjEntry;
  BEGIN
    fmt.putText ("  ");
    fmt.begin (2);
    IF (NOT info.traced) THEN fmt.putText ("UNTRACED "); END;
    fmt.putText ("OBJECT");
    fmt.newLine ();
    IF (info.fields.head # NIL) THEN
      fmt.newLine ();
      fmt.align (4, tryOneLine := FALSE);
      x := info.fields.head;
      WHILE (x # NIL) DO
        FormatObjEntry (x, fmt, FALSE);
        x := x.next;
      END;
      fmt.end ();              
    END;
    IF (info.methods.head # NIL) THEN
      fmt.newLine (-2);
      fmt.putText ("METHODS");
      fmt.newLine ();
      fmt.align (4, tryOneLine := FALSE);
      x := info.methods.head;
      WHILE (x # NIL) DO
        FormatObjEntry (x, fmt, TRUE);
        x := x.next;
      END;
      fmt.end ();              
    END;
    fmt.newLine (-2);
    fmt.putText ("END");
    fmt.end ();              
    fmt.flush ();
    fmt.close ();
  END FormatObject;

PROCEDURE FormatObjEntry (x: ObjEntry;  fmt: XFormat.T;  method: BOOLEAN) =
  BEGIN
    fmt.group ();

      fmt.group ();
        fmt.putText (x.name);
        fmt.putChar (' ');
      fmt.end ();

      fmt.group ();
        IF (method) THEN
          GenTypeName (x.uid, NIL, fmt, sig_only := TRUE);
        ELSE
          fmt.putText (": ");
          GenTypeName (x.uid, NIL, fmt);
        END;
        IF (x.dfault = NIL) THEN fmt.putText (";"); END;
      fmt.end ();

      fmt.group ();
        IF (x.dfault # NIL) THEN
          fmt.putText (" := ");
          GenProcRef (fmt, x.dfault);
          fmt.putText (";");
        END;
      fmt.end ();

      fmt.group ();
        IF (x.source # 0) THEN
          fmt.putText (" (* ");
          GenTypeName (x.source, NIL, fmt);
          fmt.putText (" *)");
        END;
        IF (x.hidden) THEN
          fmt.putText (" (*HIDDEN*)");
        END;
      fmt.end ();

    fmt.end ();
  END FormatObjEntry;

PROCEDURE GenTypeHeader (t: Type;  mode: INTEGER;  pref: TEXT;  wx: Wx.T) =
  VAR uid, xid: TEXT;  tn := t.names;  name, home: TEXT;  u: Type;
  BEGIN
    uid := FmtUID (t.uid);
    GetTypeName (t.uid, name, home, pref);

    IF (home # NIL) THEN
      Out (wx, StartPage);
      Out (wx, StartTitle, name, " in ", home, EndTitle);
      Out (wx, "<H2>", name);
      Out (wx, " in <A HREF=\"/3", home, "\">", home, "</A>:</H2><P>\n");
    ELSE
      Out (wx, StartPage);
      Out (wx, StartTitle, name, EndTitle);
      Out (wx, "<H2>", name, ":</H2><P>\n");
    END;
    Out (wx, "(internal uid = &lt;", uid, "&gt;");
    IF TranslateOpaque (t.uid, u) THEN
      xid := FmtUID (u.uid);
      Out (wx, ", revealed = &lt;", xid, "&gt;");
    END;
    IF FindOpaque (t.uid, u) THEN
      xid := FmtUID (u.uid);
      Out (wx, ", opaque = &lt;", xid, "&gt;");
    END;
    Out (wx, ")<P>\n");

    IF (mode # 0) THEN
      Out (wx, "  <A HREF=\"/N", uid, "\">[condensed view]</A>\n");
    END;
    IF (mode # 1) THEN
      Out (wx, "  <A HREF=\"/O", uid, "\">[expanded view]</A>\n");
    END;
    IF (mode # 2) THEN
      IF (t.class = 'V') OR (t.class = 'U') OR
        ((t.class = 'Y') AND TranslateOpaque (t.uid, u)) THEN
        Out (wx, "  <A HREF=\"/Q", uid, "\">[flat view]</A>\n");
      END;
    END;
    IF (t.subtypes # NIL) OR (SuperType (t) # NIL) THEN
      Out (wx, "  <A HREF=\"/P", uid, "\">[subtype graph]</A>\n");
    END;
    Out (wx, "<P>\n");

    IF (tn # NIL) AND (tn.next # NIL) THEN
      Out (wx, "aliases:\n<UL>\n");
      WHILE (tn # NIL) DO
        Out (wx, "<LI>", ID.ToText (tn.name), " in ");
        Out (wx, "<A HREF=\"/3", ID.ToText(tn.home),"\">");
        Out (wx, ID.ToText(tn.home),"</A>\n");
        tn := tn.next;
      END;
      Out (wx, "</UL>\n<P>\n");
      tn := t.names;
    END;
  END GenTypeHeader;

VAR debug := RTParams.IsPresent("debug");

PROCEDURE GenTypeExpr (defn     : Buf.T;
                       start    : INTEGER;
                       count    : INTEGER;
                       kind     : CHAR;
                       expanded : IntRefTbl.T;
                       fmt      : XFormat.T;
                       topLevel : BOOLEAN := FALSE;
                       sig_only : BOOLEAN := FALSE;
                       opaque_id: INTEGER := 0) =
  VAR
    eof        := NUMBER (defn^);
    ch         : CHAR;
    a, b, c, d : INTEGER;
    e, f       : INTEGER;
    id, idX    : TEXT;
    rhs        : Type;
  BEGIN
    WHILE (start < eof) AND (count > 0) DO

      IF (debug) THEN
        fmt.putMarkup ("(");
        fmt.putMarkup (Fmt.Int (start));
        fmt.putMarkup ("*");
        fmt.putMarkup (Fmt.Int (count));
        fmt.putMarkup ("*");
        fmt.putMarkup (Text.FromChar (defn[start]));
        fmt.putMarkup (")");
      END;

      ch := defn[start];  INC (start);
      IF (ch = kind) OR (kind = ' ') THEN
        DEC (count);
        CASE ch OF
        | '?' => (* builtin type *)
             EVAL ReadUID (defn, start);
             id := ReadName (defn, start);
             fmt.putText (id);
        | 'F' => (* array *)
             EVAL ReadUID (defn, start);
             a := ReadUID (defn, start);
             b := ReadUID (defn, start);
             fmt.begin (2);
               fmt.putText ("ARRAY ");
               fmt.break (0);
               GenTypeName (a, expanded, fmt);
               fmt.putChar (' ');
               fmt.break (0);
               fmt.putText ("OF ");
               fmt.break (0);
               GenTypeName (b, expanded, fmt);
             fmt.end ();
        | 'G' => (* open array *)
             EVAL ReadUID (defn, start);
             a := ReadUID (defn, start);
             fmt.begin (2);
               fmt.putText ("ARRAY OF ");
               fmt.break ();
               GenTypeName (a, expanded, fmt);
             fmt.end ();
        | 'H' => (* enum *)
             EVAL ReadUID (defn, start);
             a := ReadInt (defn, start);
             b := NextLine (defn, start);
             fmt.begin (2);
               fmt.putText ("{");
               GenTypeExpr (defn, b, a, 'I', expanded, fmt);
               fmt.putText ("}");
             fmt.end ();
        | 'I' => (* enum elt *)
             id := ReadName (defn, start);
             fmt.break ();
             fmt.putText (id);
             IF (count > 0) THEN fmt.putText (", "); END;
        | 'J' => (* bits for *)
             EVAL ReadUID (defn, start);
             a := ReadInt (defn, start);
             b := ReadUID (defn, start);
             fmt.begin (2);
               fmt.putText ("BITS ");
               fmt.putText (Fmt.Int (a));
               fmt.putText (" FOR ");
               fmt.break ();
               GenTypeName (b, expanded, fmt);
             fmt.end ();
        | 'K' => (* record *)
             EVAL ReadUID (defn, start);  (* self *)
             EVAL ReadInt (defn, start);  (* total size *)
             a := ReadInt (defn, start);  (* # fields *)
             b := NextLine (defn, start);
             fmt.begin (2);
               fmt.putText ("RECORD ");
               IF (topLevel) THEN fmt.newLine () END;
               fmt.unitedBreak ();
               fmt.align (3, tryOneLine := NOT topLevel);
               GenTypeExpr (defn, b, a, 'L', expanded, fmt);
               fmt.end ();
               fmt.unitedBreak (-2);
               fmt.putText ("END ");
             fmt.end (); (* RECORD *)
        | 'L' => (* field *)
             id := ReadName (defn, start);
             EVAL ReadInt (defn, start);
             EVAL ReadInt (defn, start);
             a := ReadUID (defn, start);
             fmt.group ();
               fmt.group ();
                 fmt.putText (id);
                 fmt.putChar (' ');
               fmt.end ();
               fmt.group ();
                 fmt.putText (": ");
               fmt.end ();
               fmt.group ();
                 GenTypeName (a, expanded, fmt);
                 fmt.putText ("; ");
               fmt.end ();
             fmt.end ();
        | 'M' => (* set *)
             EVAL ReadUID (defn, start);
             a := ReadUID (defn, start);
             fmt.begin (2);
               fmt.putText ("SET OF ");
               fmt.break ();
               GenTypeName (a, expanded, fmt);
             fmt.end ();
        | 'N' => (* subrange *)
             EVAL ReadUID (defn, start);
             a   := ReadUID (defn, start);
             id  := ReadName (defn, start);
             idX := ReadName (defn, start);
             fmt.begin (2);
               fmt.putText ("[ ");
               fmt.putText (id);
               fmt.break ();
               fmt.putText (" .. ");
               fmt.putText (idX);
               fmt.putText (" ]");
               IF (a # INTEGER_UID) THEN
                 fmt.putChar (' ');
                 fmt.break ();
                 fmt.putText ("(OF ");
                 GenTypeName (a, expanded, fmt);
                 fmt.putText (")");
               END;
             fmt.end ();
        | 'O', 'P' => (* untraced ref *)
             EVAL ReadUID (defn, start);
             a := ReadUID (defn, start);
             id := ReadBrand (defn, start);
             fmt.begin (2);
               IF (ch = 'O') THEN
                 fmt.putText ("UNTRACED ");
               END;
               IF (id # NIL) THEN
                 fmt.break ();
                 fmt.putText ("BRANDED \"");
                 fmt.putText (id, raw := TRUE);
                 fmt.putText ("\" ");
               END;
               fmt.break ();
               fmt.putText ("REF ");
               fmt.break ();
               GenTypeName (a, expanded, fmt);
             fmt.end ();
        | 'Q' => (* indirect *)
             EVAL ReadUID (defn, start);
             a := ReadUID (defn, start);
             fmt.begin (2);
               fmt.putText ("VAR ");
               fmt.break ();
               GenTypeName (a, expanded, fmt);
             fmt.end ();
        | 'R' => (* procedure *)
             EVAL ReadUID (defn, start);
             a := ReadInt (defn, start);  (* # formals *)
             b := ReadUID (defn, start);  (* return type *)
             c := ReadInt (defn, start);  (* # raises *)
             d := NextLine (defn, start);
             fmt.begin (2);
               IF (NOT sig_only) THEN
                 fmt.putText ("PROCEDURE ");
               END;
               fmt.putText ("(");
               IF (a > 0) THEN
                 fmt.align (3, tryOneLine := TRUE);
                 GenTypeExpr (defn, d, a, 'S', expanded, fmt);
                 fmt.end ();
               END;
               fmt.putText (")");
               IF (b # 0) THEN
                 fmt.break ();
                 fmt.putText (": ");
                 GenTypeName (b, expanded, fmt);
               END;
               IF (c > 0) THEN
                 fmt.break ();
                 fmt.begin (2);
                 fmt.putText (" RAISES {");
                 GenTypeExpr (defn, d, a, 'T', expanded, fmt);
                 fmt.putText ("}");
                 fmt.end ();
               END;
             fmt.end ();
          | 'S' => (* formal *)
             id := ReadName (defn, start);
             a := ReadUID (defn, start);
             fmt.group ();
               fmt.group ();
                 fmt.putText (id);
                 fmt.putChar (' ');
               fmt.end ();
               fmt.group ();
                 fmt.putText (": ");
               fmt.end ();
               fmt.group ();
                 GenTypeName (a, expanded, fmt);
                 IF (count > 0) THEN fmt.putText ("; "); END;
               fmt.end ();
             fmt.end ();
        | 'T' => (* raises *)
             id := ReadName (defn, start);
             fmt.break ();
             fmt.putText (id);
             IF (count > 0) THEN fmt.putText (", "); END;
        | 'U', 'V' => (* untraced obj, obj *)
             a := ReadUID (defn, start); (* self *)
             b := ReadUID (defn, start); (* super type *)
             c := ReadInt (defn, start); (* # fields *)
             d := ReadInt (defn, start); (* # methods *)
             e := ReadInt (defn, start); (* # overrides *)
             EVAL ReadInt (defn, start); (* total field size *)
             f := NextLine (defn, start);
             id := ReadBrand (defn, start);

             fmt.begin (2);
               IF (b # 0) THEN (* super type *)
                 IF (expanded = NIL) THEN
                   GenTypeName (b, expanded, fmt);
                   fmt.putChar (' ');
                 ELSE
                   GenTypeName (b, expanded, fmt, topLevel);
                   fmt.newLine ();
                   fmt.newLine (-2);
                 END;
               ELSE
                 IF (ch = 'U') THEN fmt.putText ("UNTRACED "); END;
               END;
               IF (id # NIL) THEN
                 fmt.break ();
                 fmt.putText ("BRANDED \"");
                 fmt.putText (id, raw := TRUE);
                 fmt.putText ("\" ");
               END;
               fmt.putText ("OBJECT ");
               IF (expanded # NIL) THEN
                 IF NOT GenObjectName (a, fmt) THEN
                   EVAL GenObjectName (opaque_id, fmt);
                 END;
               END;
               IF (c > 0) THEN
                 fmt.unitedBreak ();
                 fmt.align (3, tryOneLine := NOT topLevel);
                 GenTypeExpr (defn, f, c, 'L', expanded, fmt);
                 fmt.end ();
               END;
               IF (d > 0) THEN
                 fmt.unitedBreak (-2);
                 fmt.putText ("METHODS ");
                 fmt.unitedBreak (0);
                 GenTypeExpr (defn, f, d, 'W', expanded, fmt);
               END;
               IF (e > 0) THEN
                 fmt.unitedBreak (-2);
                 fmt.putText ("OVERRIDES ");
                 fmt.unitedBreak (0);
                 fmt.align (3, tryOneLine := NOT topLevel);
                 GenTypeExpr (defn, f, e, 'X', expanded, fmt);
                 fmt.end ();
               END;
               IF (topLevel) THEN fmt.newLine (-2); END;
               fmt.unitedBreak (-2);
               fmt.putText ("END ");
             fmt.end (); (* OBJECT *)
        | 'W' => (* method *)
             id := ReadName (defn, start);
             a := ReadUID (defn, start);
             idX := ReadBrand (defn, start);
             fmt.unitedBreak ();
             fmt.putText (id);
             fmt.putChar (' ');
             fmt.begin ();
               GenTypeName (a, expanded, fmt, sig_only := TRUE);
               IF (idX # NIL) THEN
                 fmt.putText (" := ");
                 GenProcRef (fmt, idX);
               END;
               fmt.putText ("; ");
             fmt.end ();
        | 'X' => (* overrides *)
             id  := ReadName (defn, start);
             idX := ReadName (defn, start);
             fmt.group ();
               fmt.group ();
                 fmt.putText (id);
                 fmt.putChar (' ');
               fmt.end ();
               fmt.group ();
                 fmt.putText (":= ");
               fmt.end ();
               fmt.group ();
                 GenProcRef (fmt, idX);
                 fmt.putText ("; ");
               fmt.end ();
             fmt.end ();
        | 'Y' => (* opaque *)
             a := ReadUID (defn, start); (* self *)
             b := ReadUID (defn, start); (* super *)
             IF TranslateOpaque (a, rhs) THEN
               GenTypeExpr (rhs.defn, rhs.start, 1, ' ',
                   expanded, fmt, topLevel, opaque_id := a);
             ELSE
               fmt.begin (2);
                 fmt.putText ("<: ");
                 GenTypeName (a, expanded, fmt, topLevel);
               fmt.end ();
             END;
        | '@', 'A', 'B', 'C', 'D', 'Z' =>
             INC (count); (* ignore this line *)
        ELSE
          fmt.putMarkup ("(! bad char =\"");
          fmt.putMarkup (Text.FromChar (ch));
          fmt.putMarkup ("\" !)");
        END; (* CASE *)
      END; (* IF ch = kind *)
      IF (count > 0) THEN
        start := NextLine (defn, start);
      END;
    END;
  END GenTypeExpr;

PROCEDURE NextLine (defn: Buf.T;  start: INTEGER): INTEGER =
  VAR eof := NUMBER (defn^);
  BEGIN
    WHILE (start < eof) AND (defn[start] # '\n') DO INC (start); END;
    RETURN start + 1;
  END NextLine;

(**********
PROCEDURE SkipLine (defn: Buf.T;  start: INTEGER;  kind: CHAR): INTEGER =
  VAR eof := NUMBER (defn^);
  BEGIN
    WHILE (start < eof) AND (defn[start] # kind) DO
      start := NextLine (defn, start);
    END;
    RETURN NextLine (defn, start);
  END SkipLine;
**********)

PROCEDURE GenTypeName (uid      : INTEGER;
                       ex       : IntRefTbl.T;
                       fmt      : XFormat.T;
                       topLevel : BOOLEAN := FALSE;
                       sig_only : BOOLEAN := FALSE) =
  VAR t: Type;  ref: REFANY;  old: BOOLEAN;
  BEGIN
    IF NOT db.type_ids.get (uid, ref) THEN
      fmt.putMarkup ("&lt;", 1);
      fmt.putText   (FmtUID (uid));
      fmt.putMarkup ("&gt;", 1);
      RETURN;
    END;
    t := ref;
    IF (ex # NIL)
      AND (t.defn # NIL)
      AND (topLevel = IsRef (t))
      AND NOT ex.get (uid, ref) THEN
      old := ex.put (uid, NIL);
      fmt.group ();
      GenTypeExpr (t.defn, t.start, 1, ' ', ex, fmt, topLevel, sig_only);
      fmt.end ();
      IF NOT old THEN EVAL ex.delete (uid, ref); END;
      RETURN;
    END;
    fmt.group ();
    fmt.putMarkup ("<A HREF=\"/N");
    fmt.putMarkup (FmtUID (uid));
    fmt.putMarkup ("\">");
    IF (t.names # NIL) AND NOT sig_only THEN
      fmt.putText (ID.ToText (t.names.name));
    ELSE
      fmt.putMarkup ("&lt;", 1);
      fmt.putText   (FmtUID (uid));
      fmt.putMarkup ("&gt;", 1);
    END;
    fmt.putMarkup ("</A>");
    fmt.end ();
  END GenTypeName;

PROCEDURE IsRef (t: Type): BOOLEAN =
  BEGIN
    RETURN (t.class = 'P') OR (t.class = 'V')
        OR (t.class = 'O') OR (t.class = 'U')
        OR (t.class = 'Y');
  END IsRef;

PROCEDURE GenObjectName (uid: INTEGER;  fmt: XFormat.T): BOOLEAN =
  VAR t: Type;  ref: REFANY;
  BEGIN
    IF (uid = 0) THEN RETURN FALSE END;
    IF NOT db.type_ids.get (uid, ref) THEN RETURN FALSE END;
    t := ref;
    IF (t.names = NIL) THEN RETURN FALSE END;
    fmt.group ();
    fmt.putText ("(* ");
    fmt.putMarkup ("<A HREF=\"/N");
    fmt.putMarkup (FmtUID (uid));
    fmt.putMarkup ("\">");
    fmt.putText (ID.ToText (t.names.name));
    fmt.putMarkup ("</A>");
    fmt.putText (" *)");
    fmt.end ();
    RETURN TRUE;
  END GenObjectName;

PROCEDURE GenProcRef(fmt: XFormat.T; t: TEXT) =
  VAR dotIndex := Text.FindChar(t, '.'); BEGIN
    fmt.group();
    IF dotIndex = -1 THEN
      fmt.putMarkup(t)
    ELSE
      WITH
    	unit = Text.Sub(t, 0, dotIndex),
    	proc = Text.Sub(t, dotIndex + 1) DO
    	fmt.putMarkup("<A HREF=\"/S" & unit & ".i3." & proc & "#"
    	       & proc & "\">" & t & "</A>");
      END
    END;
    fmt.end();
  END GenProcRef;

PROCEDURE ReadUID (buf: Buf.T;  VAR cursor: INTEGER): INTEGER = 
  VAR eof := NUMBER (buf^);  ch: CHAR;  uid: INTEGER := 0;  digit: INTEGER;
  BEGIN
    SkipBlanks (buf, cursor);
    WHILE (cursor < eof) DO
      ch := buf[cursor];
      IF ('0' <= ch) AND (ch <= '9') THEN
        digit := ORD (ch) - ORD ('0');
      ELSIF ('a' <= ch) AND (ch <= 'f') THEN
        digit := 10 + ORD (ch) - ORD ('a');
      ELSE
        EXIT;
      END;
      INC (cursor);
      uid := Word.LeftShift (uid, 4) + digit;
      uid := Word.And (uid, 16_ffffffff);
    END;
    RETURN uid;
  END ReadUID;

PROCEDURE SkipBlanks (buf: Buf.T;  VAR cur: INTEGER) =
  VAR eof := NUMBER (buf^);
  BEGIN
    WHILE (cur < eof) AND (buf[cur] = ' ') DO INC (cur); END;
  END SkipBlanks;

PROCEDURE ReadName (buf: Buf.T;  VAR cursor: INTEGER): TEXT =
  VAR ch: CHAR;  eof := NUMBER (buf^);  start := cursor;
  BEGIN
    SkipBlanks (buf, cursor);
    start := cursor;
    WHILE (cursor < eof) DO
      ch := buf[cursor];
      IF (ch = ' ') OR (ch = '\n') OR (ch = '\r') THEN EXIT END;
      INC (cursor);
    END;
    RETURN Text.FromChars (SUBARRAY (buf^, start, cursor - start));
  END ReadName;

PROCEDURE ReadInt (buf: Buf.T;  VAR cursor: INTEGER): INTEGER =
  VAR ch: CHAR;  eof := NUMBER (buf^);  val := 0;
  BEGIN
    SkipBlanks (buf, cursor);
    WHILE (cursor < eof) DO
      ch := buf[cursor];
      IF (ch < '0') OR ('9' < ch) THEN EXIT END;
      val := val * 10 + ORD (ch) - ORD ('0');
      INC (cursor);
    END;
    RETURN val;
  END ReadInt;

PROCEDURE ReadBrand (buf: Buf.T; VAR cursor: INTEGER): TEXT =
  VAR eof := NUMBER (buf^);  start: INTEGER;
  BEGIN
    IF (buf[cursor] # ' ') THEN RETURN NIL END;
    INC (cursor);
    start := cursor;
    WHILE (cursor < eof) AND (buf[cursor] # '\r') AND (buf[cursor] # '\n') DO
      INC (cursor);
    END;
    RETURN Text.FromChars (SUBARRAY (buf^, start, cursor - start));
  END ReadBrand;

CONST
  HexDigits = ARRAY [0..15] OF CHAR { '0','1','2','3','4','5','6','7',
                                      '8','9','a','b','c','d','e','f' };

PROCEDURE FmtUID (uid: INTEGER): TEXT =
  VAR buf: ARRAY [0..7] OF CHAR;
  BEGIN
    FOR i := 7 TO 0 BY -1 DO
      buf [i] := HexDigits [Word.And (uid, 16_f)];
      uid := Word.RightShift (uid, 4);
    END;
    RETURN Text.FromChars (buf);
  END FmtUID;

PROCEDURE ScanUID (txt: TEXT): INTEGER =
  VAR cursor := 0;  buf := NEW (Buf.T, Text.Length (txt));
  BEGIN
    Text.SetChars (buf^, txt);
    RETURN ReadUID (buf, cursor);
  END ScanUID;

(*------------------------------------------------------ HTML directories ---*)

(* In principle an HTML front-end will do a good job rendering
   a list of names in <DIR></DIR> brackets.  In practice "xmosaic"
   doesn't.  The following code is intended to compensate. *)

PROCEDURE GenDir (cmd, alt_cmd: TEXT;  VAR names: ARRAY OF INTEGER;
                  wx: Wx.T;  limit: INTEGER) =
  CONST Dir_width = 78; (* max # characters per line *)
  CONST Max_cols  = 6;  (* max # columns per line *)
  CONST Gap       = 2;  (* inter-column gap *)
  CONST Gap_text  = "  ";
  VAR max_len := 0;  n_cols := 1;  width, n_rows, j: CARDINAL;  nm: TEXT;
    prefix_len := 0;  n_names := NUMBER (names);  nm_len: INTEGER;
    counts: IntVec;
  BEGIN
    IF n_names <= 0 THEN RETURN END;
    Sort (names);

    (* find the longest name *)
    FOR i := FIRST (names) TO LAST (names) DO
      max_len := MAX (max_len, Text.Length (ID.ToText (names[i])));
    END;

    (* if there are too many names, collapse the list to prefixes *)
    IF n_names > limit THEN
      prefix_len := FindPrefixes (names, n_names, counts, max_len, limit);
      (* recompute the max length *)
      max_len := MIN (max_len, prefix_len + 3);
      FOR i := 0 TO n_names-1 DO
        IF counts[i] <= 1 THEN
          max_len := MAX (max_len, Text.Length (ID.ToText (names[i])));
        END;
      END;
    END;

    (* compute an approriate layout *)
    max_len := MAX (5, max_len);
    INC (max_len, Gap);
    n_cols := MAX (1, MIN (Dir_width DIV max_len, Max_cols));
    n_rows := (n_names + n_cols - 1) DIV n_cols;
    width  := Dir_width DIV n_cols - Gap;

    Out (wx, "<PRE>\n");
    TRY
      FOR row := 0 TO n_rows-1 DO
        FOR col := 0 TO n_cols-1 DO
          j := col * n_rows + row;
          IF (j < n_names) THEN
            nm := ID.ToText (names [j]);
            nm_len := Text.Length (nm);
            IF (prefix_len # 0) AND (counts[j] > 1) THEN
              IF (nm_len > prefix_len) THEN
                nm := Text.Sub (nm, 0, prefix_len);
                nm_len := prefix_len;
              END;
              Out (wx, "<A HREF=\"/", alt_cmd);
              Out (wx, nm, "\">", nm, "...");
              INC (nm_len, 3);
            ELSE
              Out (wx, "<A HREF=\"/", cmd, nm, "\">", nm);
            END;
            Out (wx, "</A>");
            IF (col # n_cols-1) THEN
              (* pad to the next column *)
              FOR x := 1 TO width - nm_len DO Out (wx, " "); END;
            END;
            Out (wx, Gap_text);
          END;
        END;
        Out (wx, "\n");
      END;
    FINALLY
      Out (wx, "</PRE>\n");
    END;
  END GenDir;

(*--------------------------------------------------- find prefix classes ---*)

PROCEDURE FindPrefixes (VAR names   : ARRAY OF INTEGER;
                        VAR n_names : CARDINAL;
                        VAR counts  : IntVec;
                            max_len : INTEGER;
                            limit   : INTEGER): INTEGER =
  VAR len, n, n0: INTEGER;
      cnts := NEW (IntVec, NUMBER (names));
      cnts0 := NEW (IntVec, NUMBER (names));
      tmp: IntVec;
  BEGIN
    (* find a prefix that generates a non-trivial choice *)
    n := 0;  len := 0;
    WHILE (len <= max_len) AND (n < 2) DO
      INC (len);
      n := CntPrefixes (names, cnts^, len);
    END;

    (* find the largest prefix that's got fewer than limit classes *)
    REPEAT
      n0 := n;
      tmp := cnts0;  cnts0 := cnts;  cnts := tmp;
      INC (len);
      n := CntPrefixes (names, cnts^, len);
    UNTIL (len >= max_len) OR (n > limit);

    (* pick the best size *)
    IF (limit - n0 <= n - limit) THEN
      (* use the shorter prefix *)
      DEC (len);
      cnts := cnts0;
    END;

    (* collapse the list of names *)
    n0 := 0;  counts := cnts;
    FOR i := 0 TO LAST (names) DO
      IF cnts[i] > 0 THEN
        names[n0] := names[i];
        counts[n0] := cnts[i];
        INC (n0);
      END;
    END;

    n_names := n0;
    RETURN len;
  END FindPrefixes;

PROCEDURE CntPrefixes (READONLY names : ARRAY OF INTEGER;
                            VAR cnts  : ARRAY OF INTEGER;
                                len   : INTEGER): INTEGER =
  VAR n_classes := 1;  last_class := 0;  short: BOOLEAN;  class_id, xx: TEXT;
  BEGIN
    class_id := ID.ToText (names[0]);
    short := Text.Length (class_id) < len;
    cnts [0] := 1;
    FOR i := 1 TO LAST (names) DO
      IF PrefixMatch (class_id, names[i], len) THEN
        INC (cnts[last_class]);
        cnts[i] := 0;
        xx := ID.ToText (names[i]);
        IF (short) AND (Text.Length (class_id) < Text.Length (xx)) THEN
          (* use 'i' as the class representitive *)
          cnts[i] := cnts[last_class];
          cnts[last_class] := 0;
          class_id := xx;
          short := Text.Length (class_id) < len;
        END;
      ELSE
        class_id := ID.ToText (names[i]);
        short := Text.Length (class_id) < len;
        cnts[i] := 1;
        last_class := i;
        INC (n_classes);
      END;
    END;
    RETURN n_classes;
  END CntPrefixes;

PROCEDURE PrefixMatch (a: TEXT;  bx: ID.T;  len: INTEGER): BOOLEAN =
  BEGIN
    RETURN CharMap.PrefixMatch (a, ID.ToText (bx), len);
  END PrefixMatch;

PROCEDURE QueryMatch (id: ID.T;  query: TextVec): BOOLEAN =
  VAR nm := ID.ToText (id);
  BEGIN
    IF (query = NIL) THEN RETURN TRUE END;
    FOR i := FIRST (query^) TO LAST (query^) DO
      IF NOT CharMap.Substr (nm, query[i]) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END QueryMatch;

(*--------------------------------------------------------------- sorting ---*)

TYPE  Elem_T = INTEGER;

PROCEDURE Elem_Compare (a, b: INTEGER): [-1 .. +1] =
  BEGIN
    RETURN ID.Compare (a, b);
  END Elem_Compare;

PROCEDURE Sort (VAR a: ARRAY OF Elem_T;  cmp := Elem_Compare) =
  BEGIN
    QuickSort (a, 0, NUMBER (a), cmp);
    InsertionSort (a, 0, NUMBER (a), cmp);
  END Sort;

PROCEDURE QuickSort (VAR a: ARRAY OF Elem_T;  lo, hi: INTEGER;
                     cmp := Elem_Compare) =
  CONST CutOff = 9;
  VAR i, j: INTEGER;  key, tmp: Elem_T;
  BEGIN
    WHILE (hi - lo > CutOff) DO (* sort a[lo..hi) *)

      (* use median-of-3 to select a key *)
      i := (hi + lo) DIV 2;
      IF cmp (a[lo], a[i]) < 0 THEN
        IF cmp (a[i], a[hi-1]) < 0 THEN
          key := a[i];
        ELSIF cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        ELSE
          key := a[lo];  a[lo] := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        END;
      ELSE (* a[lo] >= a[i] *)
        IF cmp (a[hi-1], a[i]) < 0 THEN
          key := a[i];  tmp := a[hi-1];  a[hi-1] := a[lo];  a[lo] := tmp;
        ELSIF cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[lo];  a[lo] := a[i];  a[i] := key;
        ELSE
          key := a[hi-1];  a[hi-1] := a[lo];  a[lo] := a[i];  a[i] := key;
        END;
      END;

      (* partition the array *)
      i := lo+1;  j := hi-2;

      (* find the first hole *)
      WHILE cmp (a[j], key) > 0 DO DEC (j) END;
      tmp := a[j];
      DEC (j);

      LOOP
        IF (i > j) THEN EXIT END;

        WHILE cmp (a[i], key) < 0 DO INC (i) END;
        IF (i > j) THEN EXIT END;
        a[j+1] := a[i];
        INC (i);

        WHILE cmp (a[j], key) > 0 DO DEC (j) END;
        IF (i > j) THEN  IF (j = i-1) THEN  DEC (j)  END;  EXIT  END;
        a[i-1] := a[j];
        DEC (j);
      END;

      (* fill in the last hole *)
      a[j+1] := tmp;
      i := j+2;

      (* then, recursively sort the smaller subfile *)
      IF (i - lo < hi - i)
        THEN  QuickSort (a, lo, i-1, cmp);   lo := i;
        ELSE  QuickSort (a, i, hi, cmp);     hi := i-1;
      END;

    END; (* WHILE (hi-lo > CutOff) *)
  END QuickSort;


PROCEDURE InsertionSort (VAR a: ARRAY OF Elem_T;  lo, hi: INTEGER;
                         cmp := Elem_Compare) =
  VAR j: INTEGER;  key: Elem_T;
  BEGIN
    FOR i := lo+1 TO hi-1 DO
      key := a[i];
      j := i-1;
      WHILE (j >= lo) AND cmp (key, a[j]) < 0 DO
        a[j+1] := a[j];
        DEC (j);
      END;
      a[j+1] := key;
    END;
  END InsertionSort;


(*------------------------------------------------------- low-level stuff ---*)

PROCEDURE Flatten (x: IntSeq.T): IntVec =
  VAR n := x.size ();  elts := NEW (IntVec, n);
  BEGIN
    FOR i := 0 TO n-1 DO
      elts[i] := x.get(i);
    END;
    RETURN elts;
  END Flatten;

PROCEDURE Indent (wx: Wx.T;  indent: INTEGER) =
  BEGIN
    WHILE (indent > 8) DO  Wx.PutText (wx, "        "); DEC (indent, 8);  END;
    WHILE (indent > 0) DO  Wx.PutChar (wx, ' '); DEC (indent);  END;
  END Indent;

PROCEDURE Out (wx: Wx.T;  a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN Wx.PutText (wx, a);
     IF (b # NIL) THEN Wx.PutText (wx, b);
      IF (c # NIL) THEN Wx.PutText (wx, c);
       IF (d # NIL) THEN Wx.PutText (wx, d);
        IF (e # NIL) THEN Wx.PutText (wx, e);
    END END END END END
  END Out;

PROCEDURE Abort () =
  BEGIN
    TCPServer.Abort (server);
    Process.Exit (1);
  END Abort;

PROCEDURE MakePath (a, b, c, d: TEXT := NIL): TEXT =
  VAR path := a;
  BEGIN
    IF (b # NIL) THEN path := path & SLASH & b END;
    IF (c # NIL) THEN path := path & SLASH & c END;
    IF (d # NIL) THEN path := path & SLASH & d END;
    RETURN path;
  END MakePath;

VAR posix : BOOLEAN := Text.Equal (SLASH, "/");

PROCEDURE LibName (nm: TEXT;  pgm: BOOLEAN): TEXT =
  BEGIN
    IF (nm = NIL) THEN
      RETURN "<no name>"
    ELSIF pgm THEN
      IF posix
        THEN RETURN nm;
        ELSE RETURN nm & ".exe";
      END;
    ELSE
      IF posix
        THEN RETURN "lib" & nm & ".a";
        ELSE RETURN nm & ".lib";
      END;
    END;
  END LibName;

(*---------------------------------------------------------------------------*)

BEGIN
  InitDB (db);
  ParseOptions ();
  Refresh (NIL);
  LOOP
    server := TCPServer.Fork (server_socket, n_workers, ProcessRequest,
                              Refresh, refresh_interval * 60.0d0,
                              ErrLog.Note, accept_address, accept_maskBits );
    IF (server = NIL) THEN EXIT END;
    TCPServer.Join (server);
  END;
END Main.
