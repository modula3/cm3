(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Dir;

IMPORT FileRd, Rd, Wr, Text, Thread, OSError;
IMPORT ClassDir, Editor, HTML, ID, Node, OS, Pkg, Wx;

REVEAL
  T = Tx BRANDED "Dir.T" OBJECT
  OVERRIDES
    class    := Class;
    iterate  := Iterate;
    next     := Next;
    gen_page := GenPage;
  END;

VAR
  editID := ID.Add ("editmake");
  viewID := ID.Add ("view");

PROCEDURE Class (<*UNUSED*> t: T): Node.Class =
  BEGIN
    RETURN Node.Class.Directory;
  END Class;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  BEGIN
    s.a := 0; (* phase 0 *)
    s.d := t.contents;
    s.e := NIL; (* pushed frames *)
  END Iterate;

PROCEDURE Next (<*UNUSED*> t: T;  VAR s: Node.IteratorState): BOOLEAN =
  TYPE Frame = REF RECORD dir: T;  prev: REFANY;  END;
  VAR fr: Frame;
  BEGIN
    IF (s.a = 0) THEN
      (* phase 0: return the immediate nodes *)
      WHILE s.d # NIL DO
        TYPECASE s.d OF
        | T (n) =>
            s.d := n.sibling;
            s.e := NEW (Frame, dir := n, prev := s.e);
            IF n.match (s.pattern) THEN
              s.match := n;  RETURN TRUE;
            END;
        | Node.Named_T (n) =>
            s.d := n.sibling;
            IF n.match (s.pattern) THEN
              s.match := n;  RETURN TRUE;
            END;
        ELSE (* boring node, skip it *)
        END;
      END; (*WHILE*)
      s.a := 1;
    END; (*IF phase = 0 *)

    (* phase 1: return the non-directory nodes of the subtrees:
                s.a =1, s.d = next node, s.e = pushed frames. *)

    LOOP
      WHILE s.d # NIL DO
        TYPECASE s.d OF
        | T (n) =>
            s.d := n.sibling;
            s.e := NEW (Frame, dir := n, prev := s.e);
        | ClassDir.T (n) =>
            s.d := n.sibling;
        | Node.Named_T (n) =>
            s.d := n.sibling;
            IF n.match (s.pattern) THEN  s.match := n;  RETURN TRUE;  END;
        ELSE (* boring node, skip it *)
        END; (*TYPECASE*)
      END;

      IF (s.e = NIL) THEN RETURN FALSE; END;

      (* pop a frame and continue *)
      fr := NARROW (s.e, Frame);  s.e := fr.prev;
      s.d := fr.dir.contents;
    END;
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR path := Node.FullPath (t);  icon := Node.ClassIcon [t.class()];
  BEGIN
    IF (action = editID) THEN
      Editor.Run (OS.MakePath (path, "m3makefile"), "1");
      action := viewID;
    END;

    (* Title the page *)
    HTML.BeginYY (t, wx, "Directory: ", path);
    wx.put ("<H3>");
    IF (icon # NIL) THEN HTML.PutImg (icon, wx); wx.put (" "); END;
    Pkg.GenFileNote (path, wx, is_dir := TRUE);
    Pkg.GenBuildNote (t, wx);
    wx.put ("</H3>\n");
    HTML.GenPathFinder (t, wx);
    wx.put ("\n");

    Pkg.GenActionButtons (t, wx);
    GenContents (t, wx);
    wx.put ("<HR>\n");
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END GenPage;

PROCEDURE GenContents (t: T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR results: Node.Set;
  BEGIN
    ScanDir (t, 0, results);
    HTML.GenChoices (results, wx);
  END GenContents;

PROCEDURE ScanDir (t: T;  depth: INTEGER;  VAR results: Node.Set) =
  VAR n := t.contents;
  BEGIN
    WHILE (n # NIL) DO
      TYPECASE n OF
      | T (x) =>
          IF (depth = 0) THEN  Node.Append (results, n);  END;
          ScanDir (x, depth + 1, results);
      | ClassDir.T =>
          IF (depth = 0) THEN  Node.Append (results, n);  END;
      | Node.Named_T =>
          Node.Append (results, n);
      END;
      n := n.sibling;
    END;
  END ScanDir;

TYPE
  ReadmeInfo = RECORD file, title, pre, post: TEXT END;

PROCEDURE GenReadmeInfo (path: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  CONST
    Files = ARRAY [0..4] OF ReadmeInfo {
      ReadmeInfo { "index.html",  NIL,  NIL,     NIL      },
      ReadmeInfo { "index.htm",   NIL,  NIL,     NIL      },
      ReadmeInfo { "README.html", NIL,  NIL,     NIL     },
      ReadmeInfo { "README.htm",  NIL,  NIL,     NIL     },
      ReadmeInfo { "README",      NIL,  "<PRE>\n", "\n</PRE>" } };
  BEGIN
    FOR i := FIRST (Files) TO LAST (Files) DO
      IF TryReadme (path, Files[i], wx) THEN RETURN; END;
    END;
    path := OS.MakePath (path, "src");
    FOR i := FIRST (Files) TO LAST (Files) DO
      IF TryReadme (path, Files[i], wx) THEN RETURN; END;
    END;
  END GenReadmeInfo;

PROCEDURE TryReadme (path: TEXT;  READONLY info: ReadmeInfo;  wx: Wx.T): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR rd: Rd.T := NIL;  got_it := FALSE;
  BEGIN
    TRY
      rd := FileRd.Open (OS.MakePath (path, info.file));
      got_it := TRUE;
      TRY
        wx.put ("<P>\n");
        IF (info.title # NIL) THEN
          wx.put ("<STRONG>", info.title, "</STRONG>\n<P>\n");
        END;
        IF (info.pre # NIL) OR (info.post # NIL) THEN
          wx.put (info.pre);
          ProcessRawText (Rd.GetText (rd, LAST (INTEGER)), wx);
          wx.put (info.post);
        ELSE
          wx.put (info.pre, Rd.GetText (rd, LAST (INTEGER)), info.post);
        END;
        wx.put ("\n");
      FINALLY
        Rd.Close (rd);
      END;
    EXCEPT OSError.E, Rd.Failure => (* ignore *)
    END;
    RETURN got_it;
  END TryReadme;

PROCEDURE ProcessRawText (txt: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    len  := Text.Length (txt);
    done := 0;
    c    : CHAR;
  BEGIN
    IF (len <= 0) THEN RETURN END;
    FOR i := 0 TO len-1 DO
      c := Text.GetChar (txt, i);
      IF (c = '<') THEN
        IF (done < i) THEN wx.putSub (txt, done, i-done); END;
        wx.put ("&lt;");
        done := i+1;
      ELSIF (c = '>') THEN
        IF (done < i) THEN wx.putSub (txt, done, i-done); END;
        wx.put ("&gt;");
        done := i+1;
      ELSIF (c = '"') THEN
        IF (done < i) THEN wx.putSub (txt, done, i-done); END;
        wx.put ("&quot;");
        done := i+1;
      ELSIF (c = '&') THEN
        IF (done < i) THEN wx.putSub (txt, done, i-done); END;
        wx.put ("&amp;");
        done := i+1;
      ELSE
        (* vanilla character *)
      END;
    END;
    IF (done < len) THEN wx.putSub (txt, done); END;
  END ProcessRawText;

PROCEDURE Init () =
  BEGIN
  END Init;

BEGIN
END Dir.
