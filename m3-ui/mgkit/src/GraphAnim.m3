(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Jun 25 21:02:46 PDT 1993 by steveg *)
(*      modified on Fri Aug  7 07:15:28 PDT 1992 by mhb *)

MODULE GraphAnim;

<* PRAGMA LL *>

IMPORT Animate, AnimationPath, GraphVBT, GraphVBTExtras, RefList,
   Math, Matrix2D, MG, MGV, R2;

TYPE
  RotateAnimation = Animate.T BRANDED OBJECT
    center: GraphVBT.Vertex;
    angle: REAL;
    vertices: RefList.T;
  OVERRIDES
    length := LengthAnim;
    doStep := DoStepRotate
  END;

PROCEDURE LengthAnim (<* UNUSED *> anim: Animate.T;
                      <* UNUSED *> v   : MG.V;
                      <* UNUSED *> mg  : MG.T       ): INTEGER =
  BEGIN
    RETURN 100
  END LengthAnim;

<* LL <+ VBT.mu *>
PROCEDURE DoStepRotate (             anim          : RotateAnimation;
                                     time, timePrev: REAL;
                        <* UNUSED *> v             : MG.V;
                        <* UNUSED *> mg            : MG.T             ) =
  VAR
    vertices                  := anim.vertices;
    vertex  : GraphVBT.Vertex;
    center                    := anim.center.pos;
    m := Matrix2D.Concat3(
           Matrix2D.Translate(-center[0], -center[1]),
           Matrix2D.Rotate((time - timePrev) * -anim.angle * Math.Degree),
           Matrix2D.Translate(center[0], center[1]));
  BEGIN
    LOCK anim.center.graph.mu DO
      WHILE vertices # NIL DO
        vertex := vertices.head;
        vertices := vertices.tail;
        vertex.move(Matrix2D.Transform(m, vertex.pos));
      END;
    END;
  END DoStepRotate;

(* copy l and remove any duplicates.  Very inefficient for big lists... *)
PROCEDURE RemoveDups (l: RefList.T): RefList.T =
  VAR
    res, ll: RefList.T;
    v      : REFANY;
    found  : BOOLEAN;
  BEGIN
    WHILE l # NIL DO
      v := l.head;
      l := l.tail;
      ll := res;
      found := FALSE;
      WHILE ll # NIL DO found := found OR ll.head = v; ll := ll.tail END;
      IF NOT found THEN res := RefList.Cons(v, res); END;
    END;
    RETURN res;
  END RemoveDups;

PROCEDURE Rotate (center  : GraphVBT.Vertex;
                  angle   : REAL;
                  vertices: RefList.T (* of GraphVBT.Vertex *)) =
  VAR mgv := GraphVBTExtras.GetMG(center.graph);
  BEGIN
    IF vertices = NIL THEN RETURN END;
    MGV.AddAnimation(
      mgv, NEW(RotateAnimation, center := center, angle := angle,
               vertices := RemoveDups(vertices)).init(), NIL)
  END Rotate;

TYPE
  MoveAnimation =
    Animate.T BRANDED OBJECT
      graph   : GraphVBT.T;
      path    : AnimationPath.MultipleEdgePath;
      vertices: RefList.T;
      (* cheap cache of last position if animation isn't shared *)
      posPrev : R2.T;
      timePrev: REAL := -1.0;
    OVERRIDES
      length := LengthAnim;
      doStep := DoStepMove
    END;

<* LL <+ VBT.mu *>
PROCEDURE DoStepMove (             anim          : MoveAnimation;
                                   time, timePrev: REAL;
                      <* UNUSED *> v             : MG.V;
                      <* UNUSED *> mg            : MG.T           ) =
  VAR
    vertices                        := anim.vertices;
    pos, posPrev, delta: R2.T;
    vertex             : GraphVBT.Vertex;
  BEGIN
    LOCK anim.graph.mu DO
      pos := anim.path.pos(time);
      IF anim.timePrev = timePrev THEN
        posPrev := anim.posPrev;
      ELSE
        posPrev := anim.path.pos(timePrev);
      END;
      delta := R2.Sub(pos, posPrev);
      WHILE vertices # NIL DO
        vertex := vertices.head;
        vertices := vertices.tail;
        vertex.move(R2.Add(vertex.pos, delta));
      END;
      anim.timePrev := time;
      anim.posPrev := pos;
    END;
  END DoStepMove;

PROCEDURE MoveAlongEdges (edges   : RefList.T (* of GraphVBT.Edge *);
                          vertices: RefList.T (* of GraphVBT.Vertex *)) =
  VAR graph := NARROW(vertices.head, GraphVBT.Vertex).graph;
  BEGIN
    MGV.AddAnimation(
      GraphVBTExtras.GetMG(graph),
      NEW(MoveAnimation, graph := graph,
          path := NEW(AnimationPath.MultipleEdgePath).init(edges),
          vertices := RemoveDups(vertices)).init(), NIL);
  END MoveAlongEdges;

BEGIN
END GraphAnim.
