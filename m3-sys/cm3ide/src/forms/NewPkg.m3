(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE NewPkg;

IMPORT ASCII, FileWr, FS, OSError, Text, Thread, Wr;
IMPORT (**Builder,**) Form, HTML, ID, Node, OS, Pkg, PkgRoot, Wx;

PROCEDURE Init () =
  BEGIN
    Form.Register ("new-pkg", DoNewPkg);
    Form.Register ("create-pkg", DoCreatePkg);
  END Init;

PROCEDURE DoNewPkg (self: Node.T;  <*UNUSED*>data: Node.FormData;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR n: PkgRoot.T;  first: BOOLEAN := TRUE;  nm: TEXT;
  BEGIN
    HTML.BeginXX (self, wx, "Create package");
    wx.put ("<HR>\n");
    wx.put ("(Specify the new package's root, name and kind)\n");
    wx.put ("<BLOCKQUOTE>\n");
    wx.put ("<FORM action=\"/form/create-pkg\" method=\"get\">\n");
    wx.put ("<DL>\n");
    wx.put ("<DT><BR><STRONG>Package root to use</STRONG>\n");
    wx.put ("<DD>");

    n := PkgRoot.First ();
    WHILE (n # NIL) DO
      IF n.buildable THEN
        nm := ID.ToText (n.name);
        wx.put ("<INPUT TYPE=radio NAME=\"root\" VALUE=\"", nm, "\"");
        IF first THEN wx.put (" CHECKED=TRUE");  first := FALSE;  END;
        wx.put (">", nm, "</INPUT><BR>\n");
      END;
      n := n.sibling;
    END;

    wx.put ("\n");
    wx.put ("<DT><BR><STRONG>Name of the package</STRONG>\n");
    wx.put ("<DD><INPUT TYPE=text NAME=\"name\">\n");
    wx.put ("\n");
    wx.put ("<DT><BR><STRONG>What kind of a package</STRONG>\n");
    wx.put ("<DD><INPUT TYPE=radio NAME=\"kind\" VALUE=\"pgm\" CHECKED=TRUE>\n");
    wx.put ("      <IMG SRC=\"/rsrc/pgm.gif\">Program</INPUT><BR>\n");
    wx.put ("    <INPUT TYPE=radio NAME=\"kind\" VALUE=\"lib\">\n");
    wx.put ("      <IMG SRC=\"/rsrc/lib.gif\">Library</INPUT>\n");
    wx.put ("</DL>\n");
    wx.put ("<INPUT TYPE=submit VALUE=\"Create New Package\">\n");
    wx.put ("</FORM>\n");
    wx.put ("</BLOCKQUOTE>\n");
    wx.put ("<HR>\n");
    HTML.End (wx);
  END DoNewPkg;

PROCEDURE DoCreatePkg (self: Node.T;  data: Node.FormData;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    root: TEXT := "";
    name: TEXT := "";
    pgm: BOOLEAN := FALSE;
    pkg, module_name: TEXT;
    pkg_node: Node.T;
    pkg_root: PkgRoot.T;
    err_wx := NEW (Wx.T).init (NIL);
  BEGIN
    HTML.BeginXX (self, err_wx, "Create package");

    (* grab the incoming form data *)
    WHILE (data # NIL) DO
      IF Text.Equal (data.field, "root") THEN
        root := data.value;
      ELSIF Text.Equal (data.field, "name") THEN
        name := data.value;
      ELSIF Text.Equal (data.field, "kind") THEN
        pgm := Text.Equal (data.value, "pgm");
      ELSE
        err_wx.put ("<STRONG>Unrecognized field name: ", data.field, "</STRONG><BR>\n");
      END;
      data := data.next;
    END;

    IF RootOK (root, err_wx, pkg_root) AND NameOK (name, err_wx) THEN
      pkg := OS.MakePath (pkg_root.path, name);
      module_name := Upcase (name);
      IF  CreateDirectories (pkg, err_wx)
      AND CreateMakefile (pkg, name, module_name, pgm, err_wx)
      AND ((NOT pgm) OR CreateMain (pkg, module_name, err_wx)) THEN
        pkg_node := FakePkgNode (name, pkg_root);
        TRY
          pkg_node := Pkg.Rescan (pkg_node);
          IF (pkg_node # NIL) THEN
            pkg_node.gen_page (wx, ID.Add ("view"), NIL);
            RETURN;
          END;
        EXCEPT Wr.Failure, Thread.Alerted =>
          err_wx.put ("<STRONG>Unable to scan the new package.</STRONG><BR>\n");
        END;
        (** Builder.Build (pkg_node, pkg, "", wx); **)
      END;
    END;

    (* we had some sort of trouble... *)
    wx.put (err_wx.toText ());
    HTML.End (wx);
  END DoCreatePkg;

PROCEDURE FakePkgNode (name: TEXT;  pkg_root: PkgRoot.T): Pkg.T =
  VAR pkg := NEW (Pkg.T);
  BEGIN
    pkg.name      := ID.Add (name);
    pkg.parent    := pkg_root;
    pkg.scanned   := 0;
    pkg.contents  := NIL;
    RETURN pkg;
  END FakePkgNode;

PROCEDURE RootOK (root: TEXT;  wx: Wx.T;  VAR(*OUT*)pkg_root: PkgRoot.T): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR n := PkgRoot.First ();  nm: ID.T;
  BEGIN
    pkg_root := NIL;
    IF (root = NIL) OR Text.Equal (root, "") THEN
      wx.put ("<P><STRONG>Please specify a package root.</STRONG><BR>\n");
      RETURN FALSE;
    END;

    nm := ID.Add (root);
    WHILE (n # NIL) DO
      IF (nm = n.name) OR Text.Equal (n.path, root) THEN
        pkg_root := n;
        RETURN TRUE;
      END;
      n := n.sibling;
    END;

    wx.put ("<P><STRONG>", root, " is not a known package root.</STRONG><BR>\n");
    RETURN FALSE;
  END RootOK;

PROCEDURE NameOK (name: TEXT;  wx: Wx.T): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR c: CHAR;
  BEGIN
    IF (name = NIL) OR Text.Equal (name, "") THEN
      wx.put ("<P><STRONG>Please specify a name for the package.</STRONG><BR>\n");
      RETURN FALSE;
    END;

    IF NOT Text.GetChar (name, 0) IN ASCII.Letters THEN
      wx.put ("<P><STRONG>Package names must begin with an alphabetic",
              " letter.</STRONG><BR>\n");
      RETURN FALSE;
    END;

    FOR i := 1 TO Text.Length (name) - 1 DO
      c := Text.GetChar (name, i);
      IF NOT c IN ASCII.AlphaNumerics THEN
        wx.put ("<P><STRONG> \"", name, "\" is not a legal module name ",
          " because it contains a '" & Text.FromChar(c) & "'</STRONG><BR>\n");
        RETURN FALSE;
      END;
    END;

    RETURN TRUE;
  END NameOK;

PROCEDURE Upcase (nm: TEXT): TEXT =
  VAR c := Text.GetChar (nm, 0);  cc := ASCII.Upper[c];
  BEGIN
    IF (c = cc) THEN RETURN nm; END;
    RETURN Text.FromChar (cc) & Text.Sub (nm, 1);
  END Upcase;

PROCEDURE CreateDirectories (pkg: TEXT;  wx: Wx.T): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR src := OS.MakePath (pkg, "src");
  BEGIN
    TRY
      wx.put ("Creating a directory for the package: <TT>", pkg, "</TT>...<BR>\n");
      FS.CreateDirectory (pkg);
      wx.put ("Creating a source directory: <TT>", src, "</TT>...<BR>\n");
      FS.CreateDirectory (src);
      RETURN TRUE;
    EXCEPT OSError.E (ec) =>
      wx.put ("  ** failed", OS.Err (ec), "<BR>\n");
      RETURN FALSE;
    END;
  END CreateDirectories;

PROCEDURE CreateMakefile (pkg, name, main: TEXT;  pgm: BOOLEAN;  wx: Wx.T): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR f: Wr.T;  file := OS.MakePath (pkg, "src", "m3makefile");
  BEGIN
    wx.put ("Creating a makefile: <TT>", file, "</TT>...<BR>\n");
    TRY
      f := FileWr.Open (file);

      Wr.PutText (f, "% m3makefile for ");
      Wr.PutText (f, name);
      Wr.PutText (f, Wr.EOL);
      Wr.PutText (f, Wr.EOL);

      Wr.PutText (f, "import (\"libm3\")");
      Wr.PutText (f, Wr.EOL);
      Wr.PutText (f, Wr.EOL);

      IF (pgm) THEN
        Wr.PutText (f, "implementation (\"");
        Wr.PutText (f, main);
        Wr.PutText (f, "\")");
        Wr.PutText (f, Wr.EOL);
        
        Wr.PutText (f, "program (\"");
        Wr.PutText (f, name);
        Wr.PutText (f, "\")");
        Wr.PutText (f, Wr.EOL);
        Wr.PutText (f, Wr.EOL);
      ELSE
        Wr.PutText (f, "library (\"");
        Wr.PutText (f, name);
        Wr.PutText (f, "\")");
        Wr.PutText (f, Wr.EOL);
        Wr.PutText (f, Wr.EOL);
      END;

      Wr.Close (f);
      RETURN TRUE;
    EXCEPT
    | OSError.E (ec) =>
        wx.put ("  ** failed", OS.Err (ec), "<BR>\n");
    | Wr.Failure (ec) =>
        wx.put ("  ** failed", OS.Err (ec), "<BR>\n");
    | Thread.Alerted =>
        wx.put ("  ** interrupted<BR>\n");
    END;
    RETURN FALSE;
  END CreateMakefile;

PROCEDURE CreateMain (pkg, main: TEXT;  wx: Wx.T): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR f: Wr.T;  file := OS.MakePath (pkg, "src", main & ".m3");
  BEGIN
    wx.put ("Creating main program module: <TT>", file, "</TT>...<BR>\n");
    TRY
      f := FileWr.Open (file);

      Wr.PutText (f, "MODULE ");
      Wr.PutText (f, main);
      Wr.PutText (f, " EXPORTS Main;");
      Wr.PutText (f, Wr.EOL);
      Wr.PutText (f, Wr.EOL);

      Wr.PutText (f, "BEGIN");
      Wr.PutText (f, Wr.EOL);

      Wr.PutText (f, "END ");
      Wr.PutText (f, main);
      Wr.PutText (f, ".");
      Wr.PutText (f, Wr.EOL);

      Wr.Close (f);
      RETURN TRUE;
    EXCEPT
    | OSError.E (ec) =>
        wx.put ("  ** failed", OS.Err (ec), "<BR>\n");
    | Wr.Failure (ec) =>
        wx.put ("  ** failed", OS.Err (ec), "<BR>\n");
    | Thread.Alerted =>
        wx.put ("  ** interrupted<BR>\n");
    END;
    RETURN FALSE;
  END CreateMain;

BEGIN
END NewPkg.
