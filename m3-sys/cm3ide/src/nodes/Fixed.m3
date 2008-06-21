(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE Fixed;

IMPORT Bundle, IntRefTbl, Pathname, Thread, Wr;
IMPORT ConfigItem, ErrLog, ID, Node, HTML, CM3_IDE_Bundle, Roots;
IMPORT Text2, Wx;

REVEAL
  T = Node.Named_T BRANDED "Fixed.T" OBJECT
    is_html : BOOLEAN;
    body    : TEXT;
  OVERRIDES
    class    := Class;
    iterate  := Iterate;
    next     := Next;
    gen_page := GenPage;
  END;

VAR
  mu     := NEW (MUTEX);
  found  := NEW (IntRefTbl.Default).init ();
  rsrcs  := CM3_IDE_Bundle.Get ();

PROCEDURE Find (nm: TEXT): T =
  VAR id := ID.Add (nm);  ref: REFANY;  t: T;
  BEGIN
    LOCK mu DO
      IF found.get (id, ref) THEN RETURN ref; END;
      t := NEW (T, name := id, parent := Roots.ResourceRoot);
      t.is_html := NOT Text2.CIEqual (Pathname.LastExt (nm), "gif");
      t.body := Bundle.Get (rsrcs, nm);
      IF (t.body = NIL) THEN
        ErrLog.Msg ("Unable to locate resource \"", nm, "\"");
        t := NIL; (* darn *)
      END;
      EVAL found.put (id, t);
    END;
    RETURN t;
  END Find;

PROCEDURE Class (<*UNUSED*> t: T): Node.Class =
  BEGIN
    RETURN Node.Class.Resource;
  END Class;

PROCEDURE Iterate (<*UNUSED*> t: T;  <*UNUSED*> VAR s: Node.IteratorState) =
  BEGIN
  END Iterate;

PROCEDURE Next (<*UNUSED*> t: T;  <*UNUSED*> VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;  action: ID.T;  data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF t.is_html THEN
      wx.put ("Content-type: text/html\n");
      (** HTML.GenLocation (t, wx); **)
      IF ConfigItem.X[ConfigItem.T.Use_multiple_windows].bool THEN
        wx.put ("Window-target: ", Node.ClassWindow [Node.Class.Resource], "\n");
      END;
      wx.put ("\n");  (* end of HTTP header *)
      EmitBody (t, wx);
      HTML.ViewOnly (action, data, wx);
    ELSE
      wx.put ("Content-type: image/gif\n\n");
      wx.put (t.body);
    END;
  END GenPage;

CONST
  HeadMarks   = ARRAY OF TEXT { "</HEAD>", "</head>", "<BODY>", "<body>" };
  TitleMarks  = ARRAY OF TEXT { "</H1>","</h1>","</H2>","</h2>","</H3>","</h3>" };
  BaseMarks   = ARRAY OF TEXT { "<BASE", "<base" };
  EndBodyMarks= ARRAY OF TEXT { "</BODY>", "</body>" };

PROCEDURE EmitBody (t: T;  wx: Wx.T)  RAISES {Wr.Failure, Thread.Alerted} =
  VAR base_loc, end_head, end_title, end_body, done: INTEGER;  body := t.body;
  BEGIN
    IF (body = NIL) THEN RETURN END;

    end_head  := FindMark (body, HeadMarks);
    base_loc  := FindMark (body, BaseMarks);
    end_title := FindMark (body, TitleMarks);

    done := 0;
    IF (base_loc < 0) AND (end_head >= 0) THEN
      (* the file doesn't have a <BASE> tag and we know where to put one! *)
      wx.putSub (body, 0, end_head);   done := end_head;
      HTML.GenBase (t, wx, leaf := TRUE);
    END;

    IF (end_title > done) THEN
      (* we found a title, let's add a pathfinder after it *)
      wx.putSub (body, done, end_title + 5 - done);
      done := end_title + 5;
      HTML.GenPathFinder (t, wx);
    END;

    (* added this section to comply w/Farshad Nayeri's copyright legend 
    requirement for the open-source release--RCC, 2008_0618 *)
    end_body := FindMark (body, EndBodyMarks);
    IF (end_body >= done)
    THEN
      wx.putSub (body, done, end_body - done);   done := end_body;
      HTML.GenCopyright (wx);
      (*DebugMsg("Fixed.EmitBody; name=" & ID.ToText(t.name) & ", end_body=" & Fmt.Int(end_body) & ", done=" & Fmt.Int(done));*)
    END; (* if *)

    wx.putSub (body, done);
  END EmitBody;

PROCEDURE FindMark (txt: TEXT;  READONLY marks: ARRAY OF TEXT): INTEGER =
  VAR x: INTEGER;
  BEGIN
    FOR i := FIRST (marks) TO LAST (marks) DO
      x := Text2.FindSubstring (txt, marks[i]);
      IF (x >= 0) THEN RETURN x; END;
    END;
    RETURN -1;
  END FindMark;

PROCEDURE Init () =
  BEGIN
  END Init;

(**** can't find any nodes before the roots are initialized,
      otherwise the get NIL parent links! 
PROCEDURE Init () =
  VAR index: T;
  BEGIN
    index := Find ("start.html");
    IF index = NIL THEN
      ErrLog.Msg ("Unable to locate CM3-IDE resources");
      Process.Exit (1);
    END;

    WebServer.RegisterRoot ("index.html", index);
    WebServer.RegisterRoot ("start.html", index);
  END Init;
*******)

BEGIN
END Fixed.
