(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Thu Jan  5 22:58:21 PST 1995 by najork     *)
(*      modified on Wed Jan  6 15:41:49 PST 1993 by steveg     *)
(*      modified on Tue Aug  4 16:52:54 PDT 1992 by kalsow     *)

MODULE V_Stack;

IMPORT GraphVBT, R2, View, ZeusPanel;
IMPORT ParseViewClass, Parse, ParseColor, Thread;

TYPE
  T = ParseViewClass.T OBJECT
      graph  : GraphVBT.T;
      vn     : VertexNode := NIL;
      depth  : INTEGER;
      height : REAL;
    OVERRIDES
      oeSetup := Setup;
      oePush  := Push;
      oePop   := Pop;
    END;

  VertexNode = REF RECORD v: GraphVBT.Vertex; next: VertexNode END;

PROCEDURE Setup (t: T; <* UNUSED *> s: Parse.State) =
  VAR vn: VertexNode;
  BEGIN
    (* delete any existing nodes *)
    LOCK t.graph.mu DO
      vn := t.vn;
      WHILE vn # NIL DO
        vn.v.remove();  vn := vn.next
      END;
      t.vn := NIL;
    END;

    t.depth  := 0;
    t.height := 5.0;
    t.graph.redisplay ();
  END Setup;

PROCEDURE Push (t: T;  <*UNUSED*> id: INTEGER;  tag: TEXT) 
    RAISES {Thread.Alerted} =
  VAR
    vn     : VertexNode;
    vertex : GraphVBT.Vertex;
  BEGIN
    (* build the new node *)
    vertex := NEW (GraphVBT.Vertex,
                   graph := t.graph,
                   shape := GraphVBT.VertexShape.Rectangle,
                   pos := R2.T { 0.5, (t.height+0.5) / t.height },
                   size := R2.T { 0.9, 1.0 / (t.height+1.0) },
                   color := ParseColor.Active,
                   label := tag ).init();

    (* remember it *)
    vn := NEW( VertexNode, v := vertex, next := t.vn );
    t.vn := vn;

    (* place the new node *)
    LOCK t.graph.mu DO
      IF FLOAT (t.depth) < t.height THEN
        vn.v.move( R2.T { 0.5, (FLOAT(t.depth)+0.5) / t.height }, TRUE );
      ELSE
        (* rescale nodes *)
        t.height := t.height + 5.0;
        WITH size = R2.T { 0.9, 1.0 / (t.height+1.0) } DO
          FOR h := t.depth TO 0 BY -1 DO
            vn.v.move (R2.T {0.5, (FLOAT(h)+0.5) / t.height}, TRUE);
            vn.v.setSize (size);
            vn := vn.next;
          END;
        END;
        <* ASSERT vn = NIL *>
      END;
    END;

    INC (t.depth);
    t.graph.animate (0.0, 0.5);
  END Push;

PROCEDURE Pop (t: T;  <*UNUSED*>id: INTEGER) RAISES {Thread.Alerted} =
  VAR vn := t.vn;
  BEGIN
    <* ASSERT t.depth # 0 *>
    <* ASSERT t.vn # NIL *>
    t.vn := vn.next;
    DEC( t.depth );
    LOCK t.graph.mu DO
      vn.v.move (R2.T { 0.5, (t.height+0.5) / t.height }, TRUE);
    END;
    t.graph.animate (0.0, 0.5);
    LOCK t.graph.mu DO vn.v.remove () END;
    t.graph.redisplay ();
  END Pop;

(*------------------------------------------------------------------ init ---*)

PROCEDURE New (): View.T =
  VAR w := GraphVBT.WorldRectangle { w := 0.0, e := 1.0, n := 1.0, s := 0.0 };
  VAR g := NEW( GraphVBT.T, world := w).init ();
  BEGIN
    RETURN NEW (T, graph := g).init (g)
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "parse stack", "Parse");
END V_Stack.

