(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Jun 23 11:55:09 PDT 1993 by steveg *)
(*      modified on Fri Aug  7 07:15:28 PDT 1992 by mhb *)

MODULE AnimationPath;

IMPORT GraphVBT, RefList, R2;


REVEAL
  StraightPath = StraightPathPublic BRANDED OBJECT
                   p0, p1: R2.T;
                 OVERRIDES
                   init := StraightInit;
                   pos  := StraightPos;
                 END;

PROCEDURE StraightInit (self: StraightPath; p0, p1: R2.T): T =
  BEGIN
    self.p0 := p0;
    self.p1 := p1;
    RETURN self
  END StraightInit;

PROCEDURE StraightPos (self: StraightPath; t: REAL): R2.T =
  BEGIN
    RETURN R2.Add(self.p0, R2.Scale(t, R2.Sub(self.p1, self.p0)))
  END StraightPos;


REVEAL
  BezierPath = BezierPathPublic BRANDED OBJECT
                 p0, p1, p2, p3: R2.T;
               OVERRIDES
                 init := BezierInit;
                 pos  := BezierPos;
               END;

PROCEDURE BezierInit (self: BezierPath; p0, p1, p2, p3: R2.T):T =
  BEGIN
    self.p0 := p0;
    self.p1 := p1;
    self.p2 := p2;
    self.p3 := p3;
    RETURN self
  END BezierInit;

PROCEDURE BezierPos (self: BezierPath; t: REAL): R2.T =
  BEGIN
    RETURN
      R2.T{
        Cubic(self.p0[0], self.p1[0], self.p2[0], self.p3[0], t),
        Cubic(self.p0[1], self.p1[1], self.p2[1], self.p3[1], t)}
  END BezierPos;


REVEAL
  EdgePath = EdgePathPublic BRANDED OBJECT
               e: GraphVBT.Edge
             OVERRIDES
               init := EdgeInit;
               pos  := EdgePos;
             END;

PROCEDURE EdgeInit (self: EdgePath; e: GraphVBT.Edge): T =
  BEGIN
    self.e := e;
    RETURN self
  END EdgeInit;

PROCEDURE EdgePos (self: EdgePath; t: REAL): R2.T =
  VAR p0, p1, p2, p3: R2.T;
  BEGIN
    IF (self.e.control0 = NIL) OR (self.e.control1 = NIL) THEN
      p0 := self.e.vertex0.pos;
      p1 := self.e.vertex1.pos;
      RETURN R2.Mix(p0, 1.0 - t, p1, t);
    ELSE
      p0 := self.e.vertex0.pos;
      p1 := self.e.control0.pos;
      p2 := self.e.control1.pos;
      p3 := self.e.vertex1.pos;
      RETURN R2.T{Cubic(p0[0], p1[0], p2[0], p3[0], t),
                  Cubic(p0[1], p1[1], p2[1], p3[1], t)}
    END;
  END EdgePos;


PROCEDURE Cubic (c0, c1, c2, c3: REAL; t: REAL): REAL =
  BEGIN
    RETURN c0 + t * (3.0 * (c1 - c0)
                       + t * (3.0 * ((c2 - c1) - (c1 - c0))
                                + t * ((c3 - c0) - 3.0 * (c2 - c1))))
  END Cubic;

TYPE
  Edges = REF ARRAY OF RECORD e: GraphVBT.Edge; l: REAL END;

REVEAL
  MultipleEdgePath = MultipleEdgePathPublic BRANDED OBJECT
               edges: Edges;
               totalLength: REAL;
             OVERRIDES
               init := MultipleEdgeInit;
               pos  := MultipleEdgePos;
             END;

PROCEDURE MultipleEdgeInit (self: MultipleEdgePath; edges: RefList.T): T =
  VAR total := 0.0;
  BEGIN
    self.edges := NEW(Edges, RefList.Length(edges));
    FOR i := 0 TO LAST(self.edges^) DO
      WITH e = edges.head, l = EdgeLength(e) DO
        edges := edges.tail;
        self.edges[i].e := e;
        self.edges[i].l := l;
        total := total + l;
      END;
    END;
    self.totalLength := total;
    RETURN self
  END MultipleEdgeInit;

(* Lyle suggest that for a bezier with control points P, Q, R, S estimate
   the length by the length of the lines connecting: P, (P+Q)/2, (Q+R)/2, S *)
PROCEDURE EdgeLength (e: GraphVBT.Edge): REAL =
  BEGIN
    IF e.control0 = NIL THEN
      RETURN R2.Length(R2.Sub(e.vertex0.pos, e.vertex1.pos))
    ELSE
      WITH p  = e.vertex0.pos,
           q  = e.control0.pos,
           r  = e.control1.pos,
           s  = e.vertex1.pos,
           q1 = R2.T{(p[0] + q[0]) / 2.0, (p[1] + q[1]) / 2.0},
           r1 = R2.T{(r[0] + s[0]) / 2.0, (r[1] + s[1]) / 2.0}  DO
        RETURN R2.Length(R2.Sub(p, q1)) + R2.Length(R2.Sub(q1, r1))
                 + R2.Length(R2.Sub(r1, s))
      END;
    END;
  END EdgeLength;

PROCEDURE MultipleEdgePos (self: MultipleEdgePath; t: REAL): R2.T =
  VAR
    p0, p1, p2, p3: R2.T;
    total, goal   : REAL          := 0.0;
    e             : GraphVBT.Edge;
  BEGIN
    goal := t * self.totalLength;
    FOR i := 0 TO LAST(self.edges^) DO
      WITH l = self.edges[i].l DO
        IF total + l >= goal THEN
          e := self.edges[i].e;
          IF l = 0.0 THEN
            t := 0.0
          ELSE
            t := (goal - total) / l;
          END;
          EXIT;
        END;
        total := total + l;
      END;
    END;
    IF (e.control0 = NIL) OR (e.control1 = NIL) THEN
      p0 := e.vertex0.pos;
      p1 := e.vertex1.pos;
      RETURN R2.Mix(p0, 1.0 - t, p1, t);
    ELSE
      p0 := e.vertex0.pos;
      p1 := e.control0.pos;
      p2 := e.control1.pos;
      p3 := e.vertex1.pos;
      RETURN R2.T{Cubic(p0[0], p1[0], p2[0], p3[0], t),
                  Cubic(p0[1], p1[1], p2[1], p3[1], t)}
    END;
  END MultipleEdgePos;

BEGIN
END AnimationPath.
