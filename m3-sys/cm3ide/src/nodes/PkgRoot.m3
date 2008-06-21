(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE PkgRoot;

IMPORT Thread, Wr;
IMPORT BrowserDB, (** ClassDir,**)  Dir, HTML, ID, Node, Pkg, WebServer, Wx;

REVEAL
  T = Tx BRANDED "PkgRoot.T" OBJECT
  OVERRIDES
    class    := Class;
    filename := FileName;
    iterate  := Iterate;
    next     := Next;
    gen_page := GenPage;
  END;

CONST
  Map = ARRAY BOOLEAN OF Node.Class {
    Node.Class.BrowsePkgRoot, Node.Class.BuildPkgRoot
  };

VAR
  n_roots  : CARDINAL := 0;
  head     : T := NIL;
  tail     : T := NIL;
  viewID   := ID.Add ("view");
  rescanID := ID.Add ("rescan");

PROCEDURE Add (name, path: TEXT;  buildable: BOOLEAN) =
  VAR t := NEW (T, name := ID.Add (name), path := path, buildable := buildable,
                kind := VAL (ORD (Node.FirstPkgRoot) + n_roots, Node.Class));
  BEGIN
    INC (n_roots);
    IF head = NIL
      THEN head := t;
      ELSE tail.sibling := t;
    END;
    tail := t;
    Node.ClassTag    [t.kind] := name;
    Node.ClassID     [t.kind] := t.name;
    Node.ClassPlural [t.kind] := name & " packages";
    (** AddClassEntries (t); **)
  END Add;

(***
PROCEDURE AddClassEntries (t: T) =
  BEGIN
    FOR c := Node.Class.BuildPackage TO Node.Class.BrowsePackage DO
      t.contents := NEW (ClassDir.T, name := Node.ClassID[c], kind := c,
                           parent := t, sibling := t.contents);
    END;
  END AddClassEntries;
***)

PROCEDURE First (): T =
  BEGIN
    RETURN head;
  END First;

PROCEDURE Class (t: T): Node.Class =
  BEGIN
    RETURN Map [t.buildable];
  END Class;

PROCEDURE FileName (t: T): TEXT =
  BEGIN
    RETURN t.path;
  END FileName;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  BEGIN
    s.d := t.contents;
  END Iterate;

PROCEDURE Next (<*UNUSED*> t: T;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    LOOP
      TYPECASE s.d OF
      | NULL =>
          RETURN FALSE;
      | Node.Named_T (n) =>
          s.d := n.sibling;
          IF n.match (s.pattern) THEN
            s.match := n;  RETURN TRUE;
          END;
      ELSE (* boring, skip it *)
          RETURN FALSE;
      END;
    END;
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR path := Node.FullPath (t);  results: Node.Set;  n: Node.Named_T;
  BEGIN
    HTML.Begin (t, wx);
    wx.put ("<P>");
    Pkg.GenFileNote (path, wx, is_dir := TRUE);
    wx.put ("\n");
    Dir.GenReadmeInfo (path, wx);

    wx.put ("<P>\n<TABLE><TR>\n");
    GenButton ("./[rescan]", "Rescan", wx);
    IF (t.buildable) THEN
      GenButton ("/form/new-pkg/", "Create package", wx);
    END;
    wx.put ("</TR></TABLE>\n");

    IF (action = rescanID) THEN
      BrowserDB.ScanRoot (t, wx);
      action := viewID;
    ELSE
      n := t.contents;
      WHILE (n # NIL) DO
        Node.Append (results, n);
(***
        TYPECASE n OF
        | ClassDir.T => (* skip *)
        ELSE            Node.Append (results, n);
        END;
***)
        n := n.sibling;
      END;
      HTML.GenChoices (results, wx);
    END;

    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END GenPage;

PROCEDURE GenButton (url, label: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wx.put ("<TD><FORM method=get action=\"", url, "\">");
    wx.put ("<INPUT TYPE=submit VALUE=\"", label, "\"></FORM></TD>\n");
  END GenButton;

PROCEDURE Init () =
  VAR t := head;
  BEGIN
    WHILE (t # NIL) DO
      WebServer.RegisterRoot (ID.ToText (t.name), t);
      t := t.sibling;
    END;
  END Init;

PROCEDURE Reset () =
  VAR t := head;
  BEGIN
    WHILE (t # NIL) DO
      WebServer.UnregisterRoot (ID.ToText (t.name));
      Node.ClassTag    [t.kind] := NIL;
      Node.ClassID     [t.kind] := ID.NoID;
      Node.ClassPlural [t.kind] := "Packages";
      t := t.sibling;
    END;
    head := NIL;
    tail := NIL;
  END Reset;

BEGIN
END PkgRoot.
