(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Type;

IMPORT AtomList, IntArraySort, IntRefTbl, Fmt;
IMPORT Rd, RefSeq, Text, TextRd, TextRefTbl, Thread, Wr;

IMPORT BrowserDB, ConfigItem, Default, ErrLog, ID, Node, HTML, OS, Wx, XFormat;
FROM LexMisc IMPORT ReadUID, FmtUID, ReadInt, ReadBrand, ReadName;

REVEAL
  T = Tx BRANDED "Type.T" OBJECT
  OVERRIDES
    class     := Class;
    arcname   := NodeID;
    filename  := NodeName;
    printname := NodeName;
    iterate   := Iterate;
    next      := Next;
    gen_page  := GenPage;
  END;

VAR
  viewID     := ID.Add ("view");
  expandedID := ID.Add ("expanded");
  graphID    := ID.Add ("graph");
  flatID     := ID.Add ("flat");

TYPE
  ScanLine = ARRAY [0..511] OF CHAR;

CONST
  BuiltinUnit = "M3_BUILTIN.i3";

PROCEDURE Class (<*UNUSED*> t: T): Node.Class =
  BEGIN
    RETURN Node.Class.Type;
  END Class;

PROCEDURE NodeName (t: T): TEXT =
  BEGIN
    RETURN ID.ToText (NodeID (t));
  END NodeName;

PROCEDURE NodeID (t: T): ID.T =
  VAR info: Info;  ref: REFANY;
  BEGIN
    IF (t.name # ID.NoID) THEN
      RETURN t.name;
    END;
    IF BrowserDB.db.types.get (t.uid, ref) THEN
      info := ref;
      IF (info.names # NIL) THEN
        RETURN info.names.name;
      END;
    END;
    RETURN ID.Add (FmtUID (t.uid));
  END NodeID;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  (* type nodes are fixed-points => they only return self. *)
  BEGIN
    s.d := t;
  END Iterate;

PROCEDURE Next (<*UNUSED*> t: T;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    s.match := s.d;  s.d := NIL;
    RETURN (s.match # NIL);
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF    (action = viewID)     THEN GenView (t, wx);
    ELSIF (action = flatID)     THEN GenFlatView (t, wx);
    ELSIF (action = expandedID) THEN GenExpandedView (t, wx);
    ELSIF (action = graphID)    THEN GenGraphView (t, wx);
    ELSE GenView (t, wx);  HTML.NoAction (action, wx);
    END;
    HTML.NoData (data, wx);
    HTML.End (wx);
  END GenPage;

PROCEDURE GenView (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR info: Info;
  BEGIN
    info := GenTitle (t, 0, NIL, wx);
    GenType (info, wx, FALSE);
  END GenView;

PROCEDURE GenExpandedView (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR info: Info;
  BEGIN
    info := GenTitle (t, 1, NIL, wx);
    GenType (info, wx, TRUE);
  END GenExpandedView;

PROCEDURE GenFlatView (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR info: Info;
  BEGIN
    info := GenTitle (t, 2, NIL, wx);
    GenFlatType (info, wx);
  END GenFlatView;

PROCEDURE GenGraphView (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR info: Info;
  BEGIN
    info := GenTitle (t, 3, NIL, wx);
    GenTypeGraph (info, wx);
  END GenGraphView;

PROCEDURE GenTitle (t: T;  mode: INTEGER;  pref: TEXT;  wx: Wx.T): Info
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    uid      := FmtUID (t.uid);
    info     : Info  := NIL;
    tn       : T := NIL;
    name     : TEXT;
    home     : TEXT;
    alias    : TEXT;
    u        : Info;
    obj_info : ObjectInfo;
  BEGIN
    GetTypeName (t.uid, name, home, pref);
    EVAL Get (t.uid, info);

    wx.put ("Content-type: text/html\n");
    wx.put ("Location: ", Default.server_href, FmtHREF (info, t.uid), "/\n");
    IF ConfigItem.X[ConfigItem.T.Use_multiple_windows].bool THEN
      wx.put ("Window-target: ", Node.ClassWindow[Node.Class.Type], "\n");
    END;
    wx.put ("\n<HTML>\n<HEAD>\n");
    HTML.GenBase (t, wx);

    IF (home # NIL) AND NOT Text.Equal (home, BuiltinUnit) THEN
      wx.put ("<TITLE>Type ", name, " in ", home);
      wx.put ("</TITLE>\n</HEAD>\n<BODY BGCOLOR=\"#ffffff\">\n<H3>");
      HTML.PutImg (Node.ClassIcon[Node.Class.Type], wx);
      wx.put (" Type ", name);
      wx.put (" in <A HREF=\"/unit/", home, "\">");
      wx.put (home, "</A>:</H3>\n");
    ELSE
      wx.put ("<TITLE>Type ", name);
      wx.put ("</TITLE>\n</HEAD>\n<BODY BGCOLOR=\"#ffffff\">\n<H3>");
      HTML.PutImg (Node.ClassIcon[Node.Class.Type], wx);
      wx.put (" Type ", name, ":</H3>\n");
    END;
    HTML.GenPathFinder (t, wx);
    wx.put ("<P>\n");

    wx.put ("<BR>(internal uid = &lt;", uid, "&gt;"); 
    IF TranslateOpaque (t.uid, u) THEN
      wx.put (", revealed = &lt;", FmtUID (u.uid), "&gt;");
    END;
    IF FindOpaque (t.uid, u) THEN
      wx.put (", opaque = &lt;", FmtUID (u.uid), "&gt;");
    END;
    wx.put (")\n");

    wx.put ("<BR>");
    IF (mode # 0) THEN
      wx.put (" <A HREF=\"[view]\">[condensed view]</A>\n");
    END;
    IF (mode # 1) THEN
      wx.put (" <A HREF=\"[expanded]\">[expanded view]</A>\n");
    END;
    IF (mode # 2) AND (info # NIL) AND IsObject (info) THEN
      wx.put (" <A HREF=\"[flat]\">[flat view]</A>\n");
    END;
    IF GetObjInfo (t.uid, obj_info) THEN
      wx.put (" <A HREF=\"[graph]\">[subtype graph]</A>\n");
    END;

    IF (info # NIL) THEN tn := info.names; END;
    IF (tn # NIL) AND (tn.alias # NIL) THEN
      wx.put ("<P><STRONG>Aliases:</STRONG>\n<MENU>\n");
      WHILE (tn # NIL) DO
        alias := ID.ToText (tn.home);
        IF (home = NIL) OR NOT Text.Equal (home, alias) THEN
          IF Text.Equal (alias, BuiltinUnit) THEN
            wx.put ("<LI>", ID.ToText (tn.name), "\n");
          ELSE
            wx.put ("<LI>", ID.ToText (tn.name), " in ");
            wx.put ("<A HREF=\"/unit/", alias);
            wx.put ("/\">", alias,"</A>\n");
          END;
        END;
        tn := tn.alias;
      END;
      wx.put ("</MENU>\n");
    END;

    wx.flush (); (* force the title to appear *)
    RETURN info;
  END GenTitle;

(*------------------------------------------------- subtype graph ---*)

PROCEDURE GenTypeGraph (info: Info;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    indent   : INTEGER;
    cnt      : ARRAY [0..4] OF INTEGER ;
    maxDepth : INTEGER := 0;
    total    : INTEGER := 0;
  BEGIN
    IF (info = NIL) THEN RETURN END;

    (* find out how deep to print the tree *)
    FOR i := FIRST (cnt) TO LAST (cnt) DO cnt[i] := 0 END;
    CountSubtypes (info, 0, cnt);
    WHILE (maxDepth <= LAST (cnt)) AND (total + cnt[maxDepth] < 100) DO
      INC (total, cnt[maxDepth]);
      INC (maxDepth);
    END;
    maxDepth := MAX (1, maxDepth - 1);

    wx.put ("<PRE>\n");
    indent := GenSuperTypes (info, 0, wx);
    GenSubtypes (info, 0, maxDepth, indent, wx);
    wx.put ("</PRE>\n");
  END GenTypeGraph;

PROCEDURE CountSubtypes (info: Info;  depth: INTEGER;  VAR cnt: ARRAY OF INTEGER) =
  VAR u, v: Info;
  BEGIN
    IF (depth <= LAST (cnt)) AND (info.uid # 0) THEN
      u := Subtypes (info);
      WHILE (u # NIL) DO
        INC (cnt[depth]);
        v := u;
        EVAL FindOpaque (u.uid, v);
        CountSubtypes (v, depth+1, cnt);
        u := NextPeer (u);
      END;
    END;
  END CountSubtypes;

PROCEDURE GenSuperTypes (info: Info;  depth: INTEGER;  wx: Wx.T): INTEGER
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR in: INTEGER;
  BEGIN
    IF (info = NIL) THEN
      RETURN 0;
    ELSIF (depth >= 99) THEN
      wx.put ("....\n");
      RETURN 3;
    ELSE
      in := GenSuperTypes (SuperType (info), depth+1, wx);
      IF (depth # 0) THEN in := 0 END; (* hack *)
      GenGraphEntry (info, in, 0, wx, (depth = 0));
      RETURN in + 3;
    END;
  END GenSuperTypes;

PROCEDURE GenGraphEntry (info: Info;  indent, depth: INTEGER;
                         wx: Wx.T;  key: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR name, home: TEXT;
  BEGIN
    GetTypeName (info.uid, name, home, NIL);
    Indent (wx, indent);
    FOR i := 1 TO depth DO wx.put ("|  "); END;
    wx.put ("<A HREF=\"/", FmtHREF (info, info.uid), "/\">");
    wx.put (name, "</A>");
    IF (home # NIL) AND NOT Text.Equal (home, BuiltinUnit) THEN
      wx.put (" in <A HREF=\"/unit/", home,"\">");
      wx.put (home, "</A>");
    END;
    IF (key) THEN
      wx.put (" <STRONG>&lt;==</STRONG>");
    END;
    wx.put ("\n");
  END GenGraphEntry;

PROCEDURE GenSubtypes (info: Info;  depth, maxDepth, indent: INTEGER;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR z: RefSeq.T;  u, v: Info;
  BEGIN
    IF (info = NIL) THEN RETURN; END;

    IF (depth >= maxDepth) THEN
      Indent (wx, indent);
      FOR i := 1 TO depth DO wx.put ("|  "); END;
      wx.put ("....\n");
      RETURN;
    END;

    u := Subtypes (info);
    IF (u = NIL) THEN RETURN; END;

    z := NEW (RefSeq.T).init ();
    WHILE (u # NIL) DO
      IF FindOpaque (u.uid, v)
        THEN z.addhi (v);
        ELSE z.addhi (u);
      END;
      u := NextPeer (u);
    END;
    GenSubtypeNames (z, depth, maxDepth, indent, wx);
  END GenSubtypes;

PROCEDURE Indent (wx: Wx.T;  indent: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    WHILE (indent > 8) DO  wx.put ("        "); DEC (indent, 8);  END;
    WHILE (indent > 0) DO  wx.putChar (' ');    DEC (indent);     END;
  END Indent;

PROCEDURE GenSubtypeNames (z: RefSeq.T;  depth, maxDepth, indent: INTEGER;
                           wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  TYPE XX = REF ARRAY OF RECORD info: Info;  name, home: TEXT; END;
  VAR
    n   := z.size ();
    map := NEW (REF ARRAY OF INTEGER, n);
    xx  := NEW (XX, n);

  PROCEDURE CmpTypeName (a, b: INTEGER): [-1..+1] =
    VAR ca, cb: CHAR;
    BEGIN
      WITH xa = xx[a],  xb = xx[b] DO
        ca := Text.GetChar (xa.name, 0);
        cb := Text.GetChar (xb.name, 0);
        IF    (ca # '&') AND (cb = '&') THEN RETURN -1;
        ELSIF (ca = '&') AND (cb # '&') THEN RETURN +1;
        ELSE RETURN Text.Compare (xa.name, xb.name);
        END;
      END;
    END CmpTypeName;

  BEGIN
    (* build the list of names & homes *)
    FOR i := 0 TO n-1 DO
      map[i] := i;
      WITH zz = xx[i] DO
        zz.info := z.get (i);
        GetTypeName (zz.info.uid, zz.name, zz.home, NIL);
      END;
    END;

    IntArraySort.Sort (map^, CmpTypeName);

    FOR i := 0 TO n-1 DO
      WITH zz = xx[map[i]] DO
        GenGraphEntry (zz.info, indent, depth, wx, FALSE);
        GenSubtypes (zz.info, depth+1, maxDepth, indent, wx);
      END;
    END;
  END GenSubtypeNames;

(*----------------------------------------------- top-level views ---*)

PROCEDURE GenType (info: Info;  wx: Wx.T;  expanded: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR already_expanded: IntRefTbl.T := NIL;
  BEGIN
    IF (info = NIL) THEN RETURN; END;

    IF expanded THEN
      already_expanded := NEW (IntRefTbl.Default).init ();
      EVAL already_expanded.put (info.uid, NIL);
    END;

    wx.put ("<P><STRONG>Structure:</STRONG>\n");
    wx.flush ();
    wx.put ("<PRE>\n");
    FormatType (info, already_expanded, wx);
    wx.put ("</PRE>\n");
    wx.flush ();
  END GenType;

PROCEDURE FormatType (info: Info;  expanded: IntRefTbl.T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR fmt := XFormat.New (wx);
  BEGIN
    fmt.putText ("  ");
    fmt.begin (0);
    GenTypeExpr (info, expanded, fmt, topLevel := TRUE);
    fmt.end ();
    fmt.flush ();
    fmt.close ();
  END FormatType;

PROCEDURE GenFlatType (info: Info;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR xxx: ObjInfo; 
  BEGIN
    IF (info = NIL) THEN RETURN END;

    xxx.names := NEW (TextRefTbl.Default).init ();
    ExtractObject (info, xxx);

    wx.put ("<P><STRONG>Structure:</STRONG>\n");
    wx.flush ();
    wx.put ("<PRE>\n");
    IF (xxx.fields.head # NIL) OR (xxx.methods.head # NIL)
      THEN FormatObject (xxx, wx);
      ELSE FormatType (info, NIL, wx);
    END;
    wx.put ("</PRE>\n");
    wx.flush ();
  END GenFlatType;

(*------------------------------------------------- object types  ---*)

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

PROCEDURE ExtractObject (info: Info;  VAR xxx: ObjInfo)
  RAISES {Thread.Alerted} =
  VAR rhs: Info;  field_source, method_source: INTEGER;  rd: Rd.T;
  BEGIN
    IF (info = NIL) THEN RETURN END;
    field_source  := info.uid;
    method_source := info.uid;
    IF TranslateOpaque (info.uid, rhs) THEN info := rhs; END;
    ExtractObject (SuperType (info), xxx);
    rd := OpenDesc (info);
    TRY
      TRY
        ExtractThisObject (rd, xxx, field_source, method_source);
      FINALLY
        CloseDesc (rd);
      END;
    EXCEPT Rd.Failure(ec) =>
      Choke ("Trouble reading", info.info_file, ec);
    END;
  END ExtractObject;

PROCEDURE ExtractThisObject (rd: Rd.T;  VAR xxx: ObjInfo;
                             field_source, method_source: INTEGER)
  RAISES {Thread.Alerted, Rd.Failure} =
  VAR
    line          : ScanLine;
    eol           : INTEGER;
    cur           : INTEGER;
    ch            : CHAR;
    n_fields      : INTEGER;
    n_methods     : INTEGER;
    n_overrides   : INTEGER;
    n_pending     : INTEGER;
    id, idX       : TEXT;
    entry         : ObjEntry;
    ref           : REFANY;
  BEGIN
    IF (rd = NIL) THEN RETURN END;

    eol := Rd.GetSubLine (rd, line);
    ch := line[0];  cur := 1;
    IF (ch # 'U') AND (ch # 'V') THEN RETURN END;
    IF (ch = 'V') THEN xxx.traced := TRUE END;

    EVAL ReadUID (line, cur); (* self *)
    EVAL ReadUID (line, cur); (* super type *)
    n_fields    := ReadInt (line, cur); (* # fields *)
    n_methods   := ReadInt (line, cur); (* # methods *)
    n_overrides := ReadInt (line, cur); (* # overrides *)
    n_pending   := n_fields + n_methods + n_overrides;

    WHILE (n_pending > 0) AND NOT Rd.EOF (rd) DO
      eol := Rd.GetSubLine (rd, line);
      ch  := line[0];  cur := 1;
      CASE ch OF

      | 'L' => (* field *)
           DEC (n_fields);
           DEC (n_pending);
           entry := NEW (ObjEntry);
           entry.name := ReadName (line, cur);
           EVAL ReadInt (line, cur); (* bit offset *)
           EVAL ReadInt (line, cur); (* bit size *)
           entry.uid := ReadUID (line, cur); (* type *)
           entry.source := field_source;  field_source := 0;
           AddObjEntry (xxx, xxx.fields, entry);

      | 'W' => (* method *)
           DEC (n_methods);
           DEC (n_pending);
           entry := NEW (ObjEntry);
           entry.name := ReadName (line, cur);
           entry.uid := ReadUID (line, cur); (* type *)
           entry.dfault := ReadBrand (line, cur);
           entry.source := method_source;  method_source := 0;
           AddObjEntry (xxx, xxx.methods, entry);

      | 'X' => (* overrides *)
           DEC (n_overrides);
           DEC (n_pending);
           id  := ReadName (line, cur);
           idX := ReadName (line, cur);
           IF xxx.names.get (id, ref) THEN
             entry := ref;
             entry.dfault := idX;
           END;

      ELSE (* skip *)
      END; (* CASE *)
    END; (* WHILE *)
  END ExtractThisObject;

PROCEDURE AddObjEntry (VAR xxx: ObjInfo;  VAR q: ObjEntryQueue;  e: ObjEntry)=
  VAR ref: REFANY;  old: ObjEntry;
  BEGIN
    IF (q.head = NIL)
      THEN q.head := e;
      ELSE q.tail.next := e;
    END;
    q.tail := e;
    IF xxx.names.get (e.name, ref) THEN
      old := ref;
      old.hidden := TRUE;
    END;
    EVAL xxx.names.put (e.name, e);
  END AddObjEntry;

PROCEDURE FormatObject (READONLY xxx: ObjInfo;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR fmt := XFormat.New (wx);  x: ObjEntry;
  BEGIN
    fmt.putText ("  ");
    fmt.begin (2);
    IF (NOT xxx.traced) THEN fmt.putText ("UNTRACED "); END;
    fmt.putText ("OBJECT");
    fmt.newLine ();
    IF (xxx.fields.head # NIL) THEN
      fmt.newLine ();
      fmt.align (4, tryOneLine := FALSE);
      x := xxx.fields.head;
      WHILE (x # NIL) DO
        FormatObjEntry (x, fmt, FALSE);
        x := x.next;
      END;
      fmt.end ();              
    END;
    IF (xxx.methods.head # NIL) THEN
      fmt.newLine (-2);
      fmt.putText ("METHODS");
      fmt.newLine ();
      fmt.align (4, tryOneLine := FALSE);
      x := xxx.methods.head;
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

PROCEDURE FormatObjEntry (x: ObjEntry;  fmt: XFormat.T;  method: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted} =
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

(*------------------------------------------ expression formatter ---*)

PROCEDURE GenTypeExpr (info     : Info;
                       expanded : IntRefTbl.T;
                       fmt      : XFormat.T;
                       topLevel : BOOLEAN := FALSE;
                       sig_only : BOOLEAN := FALSE;
                       opaque_id: INTEGER := 0)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR rd := OpenDesc (info);   buf: ScanLine;
  BEGIN
    IF (rd = NIL) THEN RETURN END;
    TRY
      TRY
        EmitTypeExpr (rd, buf, 1, ' ', expanded, fmt,
                      topLevel, sig_only, opaque_id);
      FINALLY
        CloseDesc (rd);
      END;
    EXCEPT Rd.Failure(ec) =>
      Choke2 ("Trouble reading", info.info_file, ec, fmt);
    END;
  END GenTypeExpr;

PROCEDURE EmitTypeExpr (rd       : Rd.T;
                    VAR line     : ScanLine;
                        count    : INTEGER;
                        kind     : CHAR;
                        expanded : IntRefTbl.T;
                        fmt      : XFormat.T;
                        topLevel : BOOLEAN := FALSE;
                        sig_only : BOOLEAN := FALSE;
                        opaque_id: INTEGER := 0)
  RAISES {Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR
    eol, cur   : INTEGER;
    ch         : CHAR;
    a, b, c, d : INTEGER;
    e, f       : INTEGER;
    id, idX    : TEXT;
    rhs        : Info;
  BEGIN
    WHILE (count > 0) AND NOT Rd.EOF (rd) DO
      eol := Rd.GetSubLine (rd, line);
      ch := line[0];  cur := 1;
      IF (ch = kind) OR (kind = ' ') THEN
        DEC (count);
        CASE ch OF
        | '?' => (* builtin type *)
             EVAL ReadUID (line, cur);
             id := ReadName (line, cur);
             fmt.putText (id);
        | 'F' => (* array *)
             EVAL ReadUID (line, cur);
             a := ReadUID (line, cur);
             b := ReadUID (line, cur);
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
             EVAL ReadUID (line, cur);
             a := ReadUID (line, cur);
             fmt.begin (2);
               fmt.putText ("ARRAY OF ");
               fmt.break ();
               GenTypeName (a, expanded, fmt);
             fmt.end ();
        | 'H' => (* enum *)
             EVAL ReadUID (line, cur);
             a := ReadInt (line, cur); (* # enum elements *)
             fmt.begin (2);
               fmt.putText ("{");
               EmitTypeExpr (rd, line, a, 'I', expanded, fmt);
               fmt.putText ("}");
             fmt.end ();
        | 'I' => (* enum elt *)
             id := ReadName (line, cur);
             fmt.break ();
             fmt.putText (id);
             IF (count > 0) THEN fmt.putText (", "); END;
        | 'J' => (* bits for *)
             EVAL ReadUID (line, cur);
             a := ReadInt (line, cur);
             b := ReadUID (line, cur);
             fmt.begin (2);
               fmt.putText ("BITS ");
               fmt.putText (Fmt.Int (a));
               fmt.putText (" FOR ");
               fmt.break ();
               GenTypeName (b, expanded, fmt);
             fmt.end ();
        | 'K' => (* record *)
             EVAL ReadUID (line, cur);  (* self *)
             EVAL ReadInt (line, cur);  (* total size *)
             a := ReadInt (line, cur);  (* # fields *)
             fmt.begin (2);
               fmt.putText ("RECORD ");
               IF (topLevel) THEN fmt.newLine () END;
               fmt.unitedBreak ();
               fmt.align (3, tryOneLine := NOT topLevel);
               EmitTypeExpr (rd, line, a, 'L', expanded, fmt);
               fmt.end ();
               fmt.unitedBreak (-2);
               fmt.putText ("END ");
             fmt.end (); (* RECORD *)
        | 'L' => (* field *)
             id := ReadName (line, cur);
             EVAL ReadInt (line, cur);
             EVAL ReadInt (line, cur);
             a := ReadUID (line, cur);
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
             EVAL ReadUID (line, cur);
             a := ReadUID (line, cur);
             fmt.begin (2);
               fmt.putText ("SET OF ");
               fmt.break ();
               GenTypeName (a, expanded, fmt);
             fmt.end ();
        | 'N' => (* subrange *)
             EVAL ReadUID (line, cur);
             a   := ReadUID (line, cur);
             id  := ReadName (line, cur);
             idX := ReadName (line, cur);
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
             EVAL ReadUID (line, cur);
             a := ReadUID (line, cur);
             id := ReadBrand (line, cur);
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
             EVAL ReadUID (line, cur);
             a := ReadUID (line, cur);
             fmt.begin (2);
               fmt.putText ("VAR ");
               fmt.break ();
               GenTypeName (a, expanded, fmt);
             fmt.end ();
        | 'R' => (* procedure *)
             EVAL ReadUID (line, cur);
             a := ReadInt (line, cur);  (* # formals *)
             b := ReadUID (line, cur);  (* return type *)
             c := ReadInt (line, cur);  (* # raises *)
             d := ORD(Rd.Index (rd));
             fmt.begin (2);
               IF (NOT sig_only) THEN
                 fmt.putText ("PROCEDURE ");
               END;
               fmt.putText ("(");
               IF (a > 0) THEN
                 fmt.align (3, tryOneLine := TRUE);
                 Rd.Seek (rd, d);
                 EmitTypeExpr (rd, line, a, 'S', expanded, fmt);
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
                 Rd.Seek (rd, d);
                 EmitTypeExpr (rd, line, a, 'T', expanded, fmt);
                 fmt.putText ("}");
                 fmt.end ();
               END;
             fmt.end ();
          | 'S' => (* formal *)
             id := ReadName (line, cur);
             a := ReadUID (line, cur);
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
             id := ReadName (line, cur);
             fmt.break ();
             fmt.putText (id);
             IF (count > 0) THEN fmt.putText (", "); END;
        | 'U', 'V' => (* untraced obj, obj *)
             a := ReadUID (line, cur); (* self *)
             b := ReadUID (line, cur); (* super type *)
             c := ReadInt (line, cur); (* # fields *)
             d := ReadInt (line, cur); (* # methods *)
             e := ReadInt (line, cur); (* # overrides *)
             EVAL ReadInt (line, cur); (* total field size *)
             id := ReadBrand (line, cur);
             f := ORD(Rd.Index (rd));

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
                 Rd.Seek (rd, f);
                 EmitTypeExpr (rd, line, c, 'L', expanded, fmt);
                 fmt.end ();
               END;
               IF (d > 0) THEN
                 fmt.unitedBreak (-2);
                 fmt.putText ("METHODS ");
                 fmt.unitedBreak (0);
                 Rd.Seek (rd, f);
                 EmitTypeExpr (rd, line, d, 'W', expanded, fmt);
               END;
               IF (e > 0) THEN
                 fmt.unitedBreak (-2);
                 fmt.putText ("OVERRIDES ");
                 fmt.unitedBreak (0);
                 fmt.align (3, tryOneLine := NOT topLevel);
                 Rd.Seek (rd, f);
                 EmitTypeExpr (rd, line, e, 'X', expanded, fmt);
                 fmt.end ();
               END;
               IF (topLevel) THEN fmt.newLine (-2); END;
               fmt.unitedBreak (-2);
               fmt.putText ("END ");
             fmt.end (); (* OBJECT *)
        | 'W' => (* method *)
             id := ReadName (line, cur);
             a := ReadUID (line, cur);
             idX := ReadBrand (line, cur);
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
             id  := ReadName (line, cur);
             idX := ReadName (line, cur);
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
             a := ReadUID (line, cur); (* self *)
             b := ReadUID (line, cur); (* super *)
             IF TranslateOpaque (a, rhs) THEN
               GenTypeExpr (rhs, expanded, fmt, topLevel, opaque_id := a);
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
    END; (* WHILE *)
  END EmitTypeExpr;

PROCEDURE GenTypeName (uid      : INTEGER;
                       ex       : IntRefTbl.T;
                       fmt      : XFormat.T;
                       topLevel : BOOLEAN := FALSE;
                       sig_only : BOOLEAN := FALSE)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ref: REFANY;  old: BOOLEAN;  info: Info;
  BEGIN
    IF NOT Get (uid, info) THEN
      fmt.putMarkup ("&lt;", 1);
      fmt.putText   ("anon: " & FmtUID (uid));
      fmt.putMarkup ("&gt;", 1);
      RETURN;
    END;
    IF (ex # NIL)
      AND (info.info_file # NIL)
      AND (topLevel = IsRef (info))
      AND NOT ex.get (uid, ref) THEN
      old := ex.put (uid, NIL);
      fmt.group ();
      GenTypeExpr (info, ex, fmt, topLevel, sig_only);
      fmt.end ();
      IF NOT old THEN EVAL ex.delete (uid, ref); END;
      RETURN;
    END;
    fmt.group ();
    fmt.putMarkup ("<A HREF=\"/");
    fmt.putMarkup (FmtHREF (info, uid));
    fmt.putMarkup ("/\">");
    IF (info.names # NIL) AND NOT sig_only THEN
      fmt.putText (ID.ToText (info.names.name));
    ELSE
      fmt.putMarkup ("&lt;", 1);
      fmt.putText   ("anon: " & FmtUID (uid));
      fmt.putMarkup ("&gt;", 1);
    END;
    fmt.putMarkup ("</A>");
    fmt.end ();
  END GenTypeName;

PROCEDURE GenObjectName (uid: INTEGER;  fmt: XFormat.T): BOOLEAN
  RAISES {Wr.Failure} =
  VAR info: Info;
  BEGIN
    IF (uid = 0) THEN RETURN FALSE END;
    IF NOT Get (uid, info) THEN RETURN FALSE END;
    IF (info.names = NIL) THEN RETURN FALSE END;
    fmt.group ();
    fmt.putText ("(* ");
    fmt.putMarkup ("<A HREF=\"/");
    fmt.putMarkup (FmtHREF (info, uid));
    fmt.putMarkup ("/\">");
    fmt.putText (ID.ToText (info.names.name));
    fmt.putMarkup ("</A>");
    fmt.putText (" *)");
    fmt.end ();
    RETURN TRUE;
  END GenObjectName;

PROCEDURE GenProcRef (fmt: XFormat.T;  t: TEXT)
  RAISES {Wr.Failure} =
  VAR dotIndex := Text.FindChar(t, '.');  unit, proc: TEXT;
  BEGIN
    fmt.group();
    IF dotIndex = -1 THEN
      fmt.putMarkup(t)
    ELSE
      unit := Text.Sub (t, 0, dotIndex);
      proc := Text.Sub (t, dotIndex + 1);
      fmt.putMarkup(Fmt.F ("<A HREF=\"/exporter/%s.i3/%s#%s\">%s</A>",
                           unit, proc, proc, t));
    END;
    fmt.end();
  END GenProcRef;

(*--------------------------------------------------------- names ---*)

PROCEDURE GetTypeName (uid: INTEGER;  VAR(*OUT*)name, home: TEXT;  pref: TEXT)=
  VAR t, u: Info := NIL;
  BEGIN
    IF Get (uid, t) THEN
      IF SetTypeName (t, name, home, pref) THEN RETURN END;
    END;
    IF FindOpaque (uid, u) THEN
      t := u;
      IF SetTypeName (t, name, home, pref) THEN RETURN END;
    END;
    name := Fmt.F ("&lt;anon: %s&gt;", FmtUID (uid));
    home := NIL;
    IF (t # NIL) THEN home := ID.ToText (t.home); END;
  END GetTypeName;

PROCEDURE SetTypeName (t: Info;  VAR(*OUT*)name, home: TEXT;
                       pref: TEXT): BOOLEAN =
  VAR id: ID.T;  tn: T;
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
        tn := tn.alias;
      END;
    END;

    name := ID.ToText (t.names.name);
    home := ID.ToText (t.names.home);
    RETURN TRUE;
  END SetTypeName;

PROCEDURE FmtHREF (info: Info;  uid: INTEGER): TEXT =
  VAR nd: Node.List;  ref: REFANY;
  BEGIN
    IF (info # NIL) AND (info.names # NIL) THEN
      IF BrowserDB.db.type_names.get (info.names.name, ref) AND (ref # NIL) THEN
        nd := NARROW (ref, Node.List);
        IF (nd.tail = NIL) THEN
          (* the type's name is unique, skip the UID nonsense. *)
          RETURN "type/" & ID.ToText (info.names.name);
        END;
      END;
    END;
    RETURN "type-uid/" & FmtUID (uid);
  END FmtHREF;

(*------------------------------------------ opaque & revelations ---*)

PROCEDURE Get (uid: INTEGER;  VAR info: Info): BOOLEAN =
  VAR ref: REFANY;
  BEGIN
    IF BrowserDB.db.types.get (uid, ref) THEN
      info := ref;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END Get;

PROCEDURE GetObjInfo (uid: INTEGER;  VAR info: ObjectInfo): BOOLEAN =
  VAR ref: REFANY;
  BEGIN
    IF BrowserDB.db.objects.get (uid, ref) THEN
      info := ref;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END GetObjInfo;

PROCEDURE FindOpaque (rhs_uid: INTEGER;  VAR lhs: Info): BOOLEAN =
  VAR obj_info: ObjectInfo;
  BEGIN
    RETURN GetObjInfo (rhs_uid, obj_info)
       AND (obj_info.opaque # 0)
       AND (obj_info.opaque # rhs_uid)
       AND Get (obj_info.opaque, lhs);
  END FindOpaque;

PROCEDURE TranslateOpaque (lhs_uid: INTEGER;  VAR rhs: Info): BOOLEAN =
  VAR obj_info: ObjectInfo;
  BEGIN
    RETURN GetObjInfo (lhs_uid, obj_info)
       AND (obj_info.concrete # lhs_uid)
       AND Get (obj_info.concrete, rhs);
  END TranslateOpaque;

(*------------------------------------------------------ subtypes ---*)

PROCEDURE IsRef (t: Info): BOOLEAN =
  BEGIN
    RETURN (t.kind = 'P') OR (t.kind = 'V')
        OR (t.kind = 'O') OR (t.kind = 'U')
        OR (t.kind = 'Y');
  END IsRef;

PROCEDURE IsObject (t: Info): BOOLEAN =
  VAR u: Info;
  BEGIN
    RETURN (t.kind = 'V')
        OR (t.kind = 'U')
        OR ((t.kind = 'Y') AND TranslateOpaque (t.uid, u));
  END IsObject;

PROCEDURE SuperType (t: Info): Info =
  VAR u: Info;  obj_info: ObjectInfo;
  BEGIN
    IF (t # NIL)
      AND GetObjInfo (t.uid, obj_info)
      AND (obj_info.supertype # 0)
      AND Get (obj_info.supertype, u) THEN
      RETURN u;
    END;
    RETURN NIL;
  END SuperType;

PROCEDURE Subtypes (t: Info): Info =
  VAR u: Info;  obj_info: ObjectInfo;
  BEGIN
    IF (t # NIL)
      AND GetObjInfo (t.uid, obj_info)
      AND (obj_info.subtypes # 0)
      AND Get (obj_info.subtypes, u) THEN
      RETURN u;
    END;
    RETURN NIL;
  END Subtypes;

PROCEDURE NextPeer (t: Info): Info =
  VAR u: Info;  obj_info: ObjectInfo;
  BEGIN
    IF (t # NIL)
      AND GetObjInfo (t.uid, obj_info)
      AND (obj_info.next_peer # 0)
      AND Get (obj_info.next_peer, u) THEN
      RETURN u;
    END;
    RETURN NIL;
  END NextPeer;

(*--------------------------------------------------------- files ---*)

PROCEDURE OpenDesc (info: Info): Rd.T
  RAISES {Thread.Alerted} =
  VAR rd: Rd.T;
  BEGIN
    IF (info.info_file = NIL) THEN RETURN NIL; END;

    IF Text.Equal (info.info_file, BuiltinName)
      THEN rd := TextRd.New (BuiltinInfo);
      ELSE rd := OS.OpenRd (info.info_file);
    END;
    IF (rd = NIL) THEN RETURN NIL; END;
    
    TRY
      Rd.Seek (rd, info.info_offset);
    EXCEPT Rd.Failure (ec) =>
      Choke ("Unable to seek in", info.info_file, ec);
      OS.CloseRd (rd);
      RETURN NIL;
    END;

    RETURN rd;
  END OpenDesc;

PROCEDURE CloseDesc (rd: Rd.T) =
  BEGIN
    OS.CloseRd (rd);
  END CloseDesc;

PROCEDURE Choke (tag: TEXT;  file: TEXT;  ec: AtomList.T) =
  BEGIN
    ErrLog.Msg (tag & " \"", file, "\"", OS.Err (ec));
  END Choke;

PROCEDURE Choke2 (tag: TEXT;  file: TEXT;  ec: AtomList.T;  fmt: XFormat.T)
  RAISES {Wr.Failure} =
  BEGIN
    Choke (tag, file, ec);
    fmt.putMarkup (Fmt.F ("\n<PRE>\n!!%s \"%s\"%s !!\n</PRE>\n",
                          tag, file, OS.Err (ec)));
  END Choke2;

(*------------------------------------------------ initialization ---*)

PROCEDURE Init () =
  BEGIN
  END Init;

BEGIN
END Type.
