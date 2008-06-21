(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE FileDir;

IMPORT FS, OSError, Thread, Wr;
IMPORT HTML, FileNode, ID, Node, OS, RegExpr, Wx;

REVEAL
  T = Tx BRANDED "FileDir.T" OBJECT
  OVERRIDES
    class    := Class;
    iterate  := Iterate;
    next     := Next;
    gen_page := GenPage;
  END;

PROCEDURE Class (<*UNUSED*> t: T): Node.Class =
  BEGIN
    RETURN Node.Class.RawFile;
  END Class;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  BEGIN
    s.d := RegExpr.SimpleString (s.pattern);
    s.e := NIL;
    IF (s.d = NIL) THEN
      TRY s.e := FS.Iterate (t.path);
      EXCEPT OSError.E => s.e := NIL;
      END;
    END;
  END Iterate;

PROCEDURE Next (t: T;  VAR s: Node.IteratorState): BOOLEAN =
  VAR iter: FS.Iterator;  nm: TEXT;
  BEGIN
    IF (s.d # NIL) THEN
      s.match := BuildNode (t, s.d);
      s.d := NIL;
      RETURN TRUE;
    ELSE
      iter := s.e;
      WHILE (iter # NIL) AND iter.next (nm) DO
        IF RegExpr.Match (s.pattern, nm) THEN
          s.match := BuildNode (t, nm);
          RETURN TRUE;
        END;
      END;
      IF (iter # NIL) THEN iter.close (); END;
    END;

    s.d := NIL;
    s.e := NIL;
    RETURN FALSE;
  END Next;

PROCEDURE BuildNode (t: T;  nm: TEXT): Node.Named_T =
  VAR path := OS.MakePath (t.path, nm);  n: Node.Named_T;
  BEGIN
    IF OS.IsDirectory (path)
      THEN n := NEW (T,          name := ID.Add (nm), path := path);
      ELSE n := NEW (FileNode.T, name := ID.Add (nm), path := path);
    END;
    n.parent := t;
    RETURN n;
  END BuildNode;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR results: Node.Set;  nm: TEXT;  iter: FS.Iterator;
  BEGIN
    TRY
      iter := FS.Iterate (t.path);
      WHILE iter.next (nm) DO
        Node.Append (results, BuildNode (t, nm));
      END;
    EXCEPT OSError.E =>
    END;

    HTML.BeginXX (t, wx, "Directory: ", t.path);
    HTML.GenChoices (results, wx);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END GenPage;

BEGIN
END FileDir.
