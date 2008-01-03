(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Thu Jan  5 22:59:06 PST 1995 by najork *)
(*      modified on Fri Jan 15 09:27:06 PST 1993 by steveg *)
(*      modified on Tue Aug  4 16:51:57 PDT 1992 by kalsow *)
(*      modified on Sun Jul 26 10:25:16 1992 by rustan     *)

MODULE V_Input;

IMPORT PaintOp, GraphVBT, View, ZeusPanel, Text, R2;
IMPORT Parse, ParseViewClass, ParseColor, Thread;

TYPE
  T = ParseViewClass.T OBJECT
        graph    : GraphVBT.T;
        vertices : REF ARRAY OF GraphVBT.Vertex;
        state    : Parse.State;
        length   : INTEGER;
        cursor   : INTEGER;
        font     : GraphVBT.WorldFont;
      OVERRIDES
        oeSetup     := Setup;
        oeScan      := Scan;
        oeNoteError := NoteError
      END;

CONST
  V_Pos = 0.5;

PROCEDURE Setup (t: T;  s: Parse.State) RAISES {Thread.Alerted} =
  VAR
    n_chars : INTEGER;
    v       : REF ARRAY OF GraphVBT.Vertex;
    used    : INTEGER;
    len     : INTEGER;
    scale   : REAL;
    h_pos   : REAL;
    max_tok : INTEGER;
  BEGIN
    (* compute the total # of characters of input *)
    n_chars := 1; (* the terminal space *)
    max_tok := 1;
    FOR i := 0 TO s.n_tokens-1 DO
      (*@@ INC (n_chars, Text.Length (s.tokens[i])); @@*)
      max_tok := MAX (max_tok, Text.Length (s.tokens[i])); (*@@*)
    END;
    INC (max_tok); (* leave a little breathing room *)
    n_chars := (max_tok+1) * (s.n_tokens+1); (*@@*)

    (* initialize the view *)
    t.state  := s;
    t.length := n_chars;
    t.cursor := 0;

    (* remove any old vertices from graph *)
    v := t.vertices;
    IF v # NIL THEN
      LOCK t.graph.mu DO
        FOR i := FIRST (v^) TO LAST (v^) DO v[ i ].remove() END
      END
    END;

    (* add the new vertices *)
    t.vertices := NEW (REF ARRAY OF GraphVBT.Vertex, s.n_tokens + 1);
    FOR i := 0 TO s.n_tokens - 1 DO
      t.vertices[i] := NewVertex (t, s.tokens[i], max_tok, ParseColor.Virgin);
    END;
    (* add an error node *)
    t.vertices[s.n_tokens] := NewVertex (t, " ", max_tok, ParseColor.Clear);

    (* display all vertices in one place *)
    t.graph.redisplay ();

    (* move vertices to correct positions via animation *)
    LOCK t.graph.mu DO
      used := 0;  scale := 1.0 / FLOAT (n_chars + 1);
      FOR i := 0 TO s.n_tokens DO
        (*@@
        IF (i < s.n_tokens)
          THEN len := Text.Length (s.tokens[i]);
          ELSE len := 1;
        END; @@*) len := max_tok + 1;
        h_pos := (FLOAT (used) + 0.5 * FLOAT (len) + 0.5) * scale;
        t.vertices[i].move (R2.T { h_pos, V_Pos }, TRUE);
        INC (used, len);
      END;
    END;

    (* and display the final result *)
    t.graph.animate (0.0, 1.0);
  END Setup;

PROCEDURE NewVertex (t: T;  label: TEXT;  len: INTEGER;
                     c: PaintOp.T): GraphVBT.Vertex =
  VAR nodeSize := R2.T { FLOAT (len) / FLOAT (t.length + 2), 0.5 };
  BEGIN
    RETURN NEW (GraphVBT.Vertex,
                   graph := t.graph,
                   shape := GraphVBT.VertexShape.Rectangle,
                   pos := R2.T { 0.0, V_Pos },
                   size := nodeSize,
                   color := c,
                   label := label,
                   font  := t.font
               ).init()
  END NewVertex;

PROCEDURE Scan (t: T;  <*UNUSED*> token: TEXT) =
  VAR n := t.cursor;
  BEGIN
    LOCK t.graph.mu DO
      IF (n > 0) THEN  t.vertices[n - 1].setColor (ParseColor.Accepted)  END;
      IF (n < t.state.n_tokens) THEN
        t.vertices[n].setColor (ParseColor.Current);
      END;
    END;
    INC (t.cursor);
    t.graph.redisplay ();
  END Scan;

PROCEDURE NoteError (t: T) =
  BEGIN
    LOCK t.graph.mu DO
      t.vertices [t.cursor].setColor (ParseColor.Error);
    END;
    t.graph.redisplay ();
  END NoteError;

PROCEDURE NewInput (): View.T =
  VAR g := NEW (GraphVBT.T).init ();
  BEGIN
    RETURN NEW (T, graph := g, font := g.font(size := 0.03)).init (g);
  END NewInput;

BEGIN
  ZeusPanel.RegisterView (NewInput, "input stream", "Parse");
END V_Input.
