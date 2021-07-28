(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Source;

IMPORT IntList, Text, Thread, Wr;
IMPORT BrowserDB, Buf, CMarkUp, Decl, Editor, FileNode, HTML, ID;
IMPORT M3MarkUp, MarkUp, Node, Pkg, RegExpr, Wx;

REVEAL
  T = Tx BRANDED "Source.T" OBJECT
  OVERRIDES
    class      := Class;
    printname  := PrintName;
    match      := Match;
    iterate    := Iterate;
    next       := Next;
    gen_page   := GenPage;
  END;

VAR
  viewID   := ID.Add ("view");
  editID   := ID.Add ("edit");
  clientID := ID.Add ("clients");

CONST
  IsM3 = ARRAY Kind OF BOOLEAN { TRUE,  (* i3 *)
                                 TRUE,  (* m3 *)
                                 TRUE,  (* ig *)
                                 TRUE,  (* mg *)
                                 FALSE, (* c *)
                                 FALSE, (* cpp *)
                                 FALSE, (* h *)
                                 FALSE, (* quake *)
                                 FALSE  (* other *) };

PROCEDURE Class (t: T): Node.Class =
  BEGIN
    RETURN NodeClass [t.kind];
  END Class;

PROCEDURE PrintName (t: T): TEXT =
  VAR nm := ID.ToText (t.name);
  BEGIN
    RETURN Text.Sub (nm, 0, Text.Length (nm) - ExtLen [t.kind]);
  END PrintName;

PROCEDURE Match (t: T;  re: RegExpr.T): BOOLEAN =
  VAR nm := ID.ToText (t.name);
  BEGIN
    RETURN RegExpr.Match (re, nm)
        OR RegExpr.MatchSubstring (re, nm, 0, ExtLen[t.kind]);
  END Match;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  VAR nodes: Node.Set;
  BEGIN
    s.a := 1;  s.b := 0;  s.d := NIL;
    IF IsM3 [t.kind] THEN
      nodes := Decl.FindNodes (t, s.pattern);
      IF (nodes.cnt > 0) THEN
        s.a := 0;
        s.b := nodes.cnt;
        s.d := nodes.elts;
      END;
    END;
  END Iterate;

PROCEDURE Next (<*UNUSED*> t: T;  VAR s: Node.IteratorState): BOOLEAN =
  VAR elts: Node.Array := s.d;
  BEGIN
    IF (s.a < s.b) THEN
      s.match := elts[s.a];  INC (s.a);
      RETURN TRUE;
    END;
    RETURN FALSE;
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    EmitPage (t, wx, action, data, NIL);
  END GenPage;

PROCEDURE EmitPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData;
                    target_decl: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR path := Node.FullPath (t);
  BEGIN
    IF (action = editID) THEN
      Editor.Run (path, "1");
      action := viewID;
    END;

    IF (t.kind = Kind.Other) AND FileNode.EmitFile (t, path, wx) THEN
      RETURN;
    END;

    HTML.Begin (t, wx);
    Pkg.GenFileNote (path, wx, is_dir := FALSE);
    Pkg.GenBuildNote (t, wx);
    wx.put ("\n");

    IF    (action = viewID)   THEN GenView (t, path, wx, target_decl);
    ELSIF (action = clientID) THEN GenClients (t, wx);
    ELSE HTML.NoAction (action, wx);
    END;

    HTML.NoData (data, wx);
    HTML.End (wx);
  END EmitPage;

PROCEDURE GenView (t: T;  path: TEXT;  wx: Wx.T;  target_decl: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    GenExporters (t, wx);
    GenImportLink (t, wx);
    GenBody (t, path, wx, target_decl);
  END GenView;

(*******  "Foo.i3/_EXPORTERS_/Foo.m3"  doesn't work! ***********
PROCEDURE GenExporters (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ref: REFANY;  id: IntList.T;  txt: TEXT;
  BEGIN
    IF BrowserDB.db.exporters.get (t.name, ref) THEN
      wx.put ("<STRONG>Exported&nbsp;by:</STRONG>&nbsp;");
      id := ref;
      WHILE (id # NIL) DO
        txt := ID.ToText (id.head);
        wx.put ("<A HREF=\"", M3MarkUp.Intf_to_Impl_Mark);
        wx.put ("/", txt, "\">");
        wx.put (txt, "</A>");
        id := id.tail;
        IF id # NIL THEN wx.put (",&nbsp;") END;
      END;
      wx.put ("&nbsp;&nbsp;&nbsp;&nbsp; ");
    END;
  END GenExporters;
******************************************************************)

PROCEDURE GenExporters (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ref: REFANY;  id: IntList.T;  txt: TEXT;
  BEGIN
    IF BrowserDB.db.exporters.get (t.name, ref) THEN
      wx.put ("<STRONG>Exported&nbsp;by:</STRONG>&nbsp;");
      id := ref;
      IF (id # NIL) THEN
        wx.put ("<A HREF=\"", M3MarkUp.Intf_to_Impl_Mark, "\">");
        WHILE (id # NIL) DO
          txt := ID.ToText (id.head);
          wx.put (txt);
          id := id.tail;
          IF id # NIL THEN wx.put (",&nbsp;") END;
        END;
        wx.put ("</A>");
      END;
      wx.put ("&nbsp;&nbsp;&nbsp;&nbsp; ");
    END;
  END GenExporters;

PROCEDURE GenImportLink (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ref: REFANY;  cnt: INTEGER;  importers: IntList.T;  units: Node.List;
  BEGIN
    IF BrowserDB.db.importers.get (t.name, ref) THEN
      cnt := 0;
      importers := ref;
      WHILE (importers # NIL) DO
        IF BrowserDB.db.units.get (importers.head, ref) THEN
          units := ref;
          WHILE (units # NIL) DO
            INC (cnt);
            units := units.tail;
          END;
        END;
        importers := importers.tail;
      END;

      wx.put ("<STRONG>Imported&nbsp;by:</STRONG>&nbsp;<A HREF=\"[clients]\">");
      IF (cnt = 1)
        THEN wx.put ("one&nbsp;unit");
        ELSE wx.putInt (cnt);  wx.put ("&nbsp;units");
      END;
      wx.put ("</A>\n");
    END;
  END GenImportLink;

PROCEDURE GenClients (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ref: REFANY;  clients: Node.Set;  importers: IntList.T;  units: Node.List;
  BEGIN
    (* collect up everybody that looks likely... *)
    IF BrowserDB.db.importers.get (t.name, ref) THEN
      importers := ref;
      WHILE (importers # NIL) DO
        IF BrowserDB.db.units.get (importers.head, ref) THEN
          units := ref;
          WHILE (units # NIL) DO
            Node.Append (clients, units.head);
            units := units.tail;
          END;
        END;
        importers := importers.tail;
      END;
    END;

    wx.put ("<P><STRONG>Importing clients:</STRONG>\n");
    HTML.GenChoices (clients, wx);
  END GenClients;

PROCEDURE GenBody (t: T;  path: TEXT;  wx: Wx.T;  target_decl: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR buf := Buf.FromFile (path, pad := 1);
  BEGIN
    IF (buf = NIL) THEN 
      wx.put ("<P><STRONG>Unable to open \"", path, "\"</STRONG>\n");
      RETURN;
    END;

    Pkg.GenActionButtons (t, wx);
    wx.put ("<HR>\n");

    IF IsM3 [t.kind]
      THEN MarkUp.Annotate (buf, wx, NIL, target_decl);
      ELSE CMarkUp.Annotate (buf, wx, NIL);
    END;
  END GenBody;

PROCEDURE Init () =
  BEGIN
  END Init;

BEGIN
END Source.
