(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Aug 13 19:49:22 PDT 1993 by heydon                   *)
<* PRAGMA LL *>

MODULE VertexSizeAnim;

IMPORT GraphVBT, GraphVBTExtras, R2, Animate, MG, MGV;

TYPE
  LinearAnimation = Animation BRANDED OBJECT
    v: GraphVBT.Vertex;
    old, delta: R2.T
  METHODS
    init(v: GraphVBT.Vertex; new: R2.T): LinearAnimation := LAInit
  OVERRIDES
    size := LASize
  END;
  (* If "la := NEW(LinearAnimation).init(v, new)", then "la.size(t)" returns
     the size that is a linear interpolation between "v"'s initial size and
     "new". *)

PROCEDURE LAInit(self: LinearAnimation; v: GraphVBT.Vertex; new: R2.T):
  LinearAnimation =
  BEGIN
    self.v := v;
    self.old := v.size;
    self.delta := R2.Sub(new, v.size);
    RETURN self
  END LAInit;

PROCEDURE LASize(self: LinearAnimation; t: REAL): R2.T =
  BEGIN
    RETURN R2.Add(self.old, R2.Scale(t, self.delta))
  END LASize;

TYPE
  MyAnimation = Animate.T OBJECT
    v: GraphVBT.Vertex;
    anim: Animation;
  OVERRIDES
    length := Length;
    doStep := DoStep
  END;

PROCEDURE Length(
    <* UNUSED *> t : MyAnimation;
    <* UNUSED *> v : MG.V;
    <* UNUSED *> mg: MG.T): INTEGER =
  BEGIN
    RETURN LAST(INTEGER);
  END Length;

PROCEDURE DoStep(
    self: MyAnimation;
    time: REAL;
    <* UNUSED *> timePrev: REAL;
    <* UNUSED *> v: MG.V;
    <* UNUSED *> mg: MG.T) =
  BEGIN
    self.v.setSize(self.anim.size(time));
  END DoStep;

PROCEDURE Register(v: GraphVBT.Vertex; anim: Animation) =
<* LL = v.graph.mu *>
  BEGIN
    MGV.AddAnimationLocked(
      GraphVBTExtras.GetMG(v.graph),
      NEW(MyAnimation, v := v, anim := anim).init(),
      NIL);
  END Register;

PROCEDURE Linear(v: GraphVBT.Vertex; newW, newH: REAL) =
<* LL = 0 *>
  BEGIN
    LOCK v.graph.mu DO
      Register(v, NEW(LinearAnimation).init(v, R2.T{newW, newH}))
    END
  END Linear;

BEGIN
END VertexSizeAnim.
