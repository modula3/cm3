(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Form;

IMPORT Thread, Wr;
IMPORT HTML, ID, Node, RegExpr, WebServer, Wx;

TYPE
  Info = REF RECORD
    name    : TEXT     := NIL;
    handler : Handler  := NIL;
    next    : Info     := NIL;
    node    : Node.T   := NIL;
  END;

VAR
  root  : Node.T := NIL;
  forms : Info   := NIL;

PROCEDURE Register (name: TEXT;  handler: Handler) =
  BEGIN
    forms := NEW (Info, name := name, handler := handler, next := forms);
    (* no sanity checks... *)
  END Register;

PROCEDURE Init () =
  BEGIN
    root := NEW (FormRoot, name := ID.Add ("form"));
    WebServer.RegisterRoot ("form", root);
  END Init;

(*----------------------------------------------------- form root ---*)

TYPE
  FormRoot = Node.Named_T OBJECT
  OVERRIDES
    class    := FormClass;
    iterate  := FormIterate;
    next     := FormNext;
    gen_page := FormPage;
  END;

PROCEDURE FormClass (<*UNUSED*> self: FormRoot): Node.Class =
  BEGIN
    RETURN Node.Class.Root;
  END FormClass;

PROCEDURE FormIterate (<*UNUSED*> self: FormRoot;
                             VAR s: Node.IteratorState) =
  BEGIN
    s.d := forms;
  END FormIterate;

PROCEDURE FormNext (<*UNUSED*> self: FormRoot;
                          VAR s: Node.IteratorState): BOOLEAN =
  VAR info: Info;
  BEGIN
    WHILE (s.d # NIL) DO
      info := s.d;
      s.d := info.next;
      IF RegExpr.Match (s.pattern, info.name) THEN
        s.match := MakeLeaf (info);
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END FormNext;

PROCEDURE FormPage (self: FormRoot;  wx: Wx.T;
                     action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR info := forms;  results: Node.Set;
  BEGIN
    HTML.BeginXX (self, wx, "Forms");
    WHILE (info # NIL) DO
      Node.Append (results, MakeLeaf (info));
      info := info.next;
    END;
    HTML.GenChoices (results, wx);
    HTML.ViewOnly (action, data, wx);
    HTML.End (wx);
  END FormPage;

(*--------------------------------------------------- form leaves ---*)

TYPE
  FormLeaf = Node.Named_T OBJECT
    info : Info;
  OVERRIDES
    class    := LeafClass;
    iterate  := LeafIterate;
    next     := LeafNext;
    gen_page := LeafPage;
  END;

PROCEDURE MakeLeaf (x: Info): Node.T =
  BEGIN
    IF (x.node = NIL) THEN
      x.node := NEW (FormLeaf, name := ID.Add (x.name),
                     parent := root, info := x);
    END;
    RETURN x.node;
  END MakeLeaf;

PROCEDURE LeafClass (<*UNUSED*> self: FormLeaf): Node.Class =
  BEGIN
    RETURN Node.Class.Root;
  END LeafClass;

PROCEDURE LeafIterate (<*UNUSED*> self: FormLeaf;
                       <*UNUSED*> VAR s: Node.IteratorState) =
  BEGIN
  END LeafIterate;

PROCEDURE LeafNext (<*UNUSED*> self: FormLeaf;
                    <*UNUSED*> VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END LeafNext;

PROCEDURE LeafPage (self: FormLeaf;  wx: Wx.T;
                     action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR x := self.info;
  BEGIN
    x.handler (self, data, wx);
    HTML.ViewOnly (action, NIL, wx);
  END LeafPage;

BEGIN
END Form.
