(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE ClassDir;

IMPORT Wr, Thread;
IMPORT ID, Node, HTML, Wx;

REVEAL
  T = Tx BRANDED "ClassDir.T" OBJECT
  OVERRIDES
    class     := Class;
    printname := PrintName;
    iterate   := Iterate;
    next      := Next;
    gen_page  := GenPage;
  END;

PROCEDURE Class (<*UNUSED*> t: T): Node.Class =
  BEGIN
    RETURN Node.Class.ClassDir;
  END Class;

PROCEDURE PrintName (t: T): TEXT =
  BEGIN
    RETURN Node.ClassPlural [t.kind];
  END PrintName;

PROCEDURE Iterate (t: T;  VAR s: Node.IteratorState) =
  BEGIN
    t.parent.iterate (s);
  END Iterate;

PROCEDURE Next (t: T;  VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    WHILE t.parent.next (s) DO
      IF (s.match.class () = t.kind) THEN RETURN TRUE; END;
    END;
    RETURN FALSE;
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    path    := Node.FullPath (t.parent);
    results : Node.Set;
    iter    : Node.IteratorState;
  BEGIN
    HTML.BeginXX (t, wx, Node.ClassPlural [t.kind], " of ", path);

    iter.pattern := NIL;
    t.parent.iterate (iter);
    WHILE t.parent.next (iter) DO
      IF iter.match.class () = t.kind THEN
        Node.Append (results, iter.match);
      END;
    END;
    HTML.GenChoices (results, wx);

    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END GenPage;

PROCEDURE Init () =
  BEGIN
  END Init;

BEGIN
END ClassDir.
