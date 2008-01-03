(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Jul 16 15:51:03 PDT 1993 by steveg *)
(*      modified on Fri Aug  7 07:17:10 PDT 1992 by mhb *)

MODULE PaintOpAnim;

IMPORT Animate AS MGAnimate, GraphVBT, GraphVBTExtras, MG, MGPaintOp, MGV, PaintOp, VBT;

REVEAL
  T = PublicT BRANDED OBJECT
        pntop: PaintOp.T;
      OVERRIDES
        init    := Init;
        set     := Set;
        get     := Get;
        op      := Op;
        animate := Animate;
      END;

PROCEDURE Init (t: T; rgb: RGB): T =
  BEGIN
    t.pntop := MGPaintOp.New(rgb);
    RETURN t;
  END Init;

PROCEDURE Set (t: T; graph: GraphVBT.T; rgb: RGB) =
  BEGIN
    MGPaintOp.Set(VBT.ScreenTypeOf(graph), t.pntop, rgb);
  END Set;

PROCEDURE Get (t: T): RGB =
  BEGIN
    RETURN MGPaintOp.Get(t.pntop);
  END Get;

PROCEDURE Op (t: T): PaintOp.T =
  BEGIN
    RETURN t.pntop
  END Op;

TYPE
  MyAnimation = MGAnimate.T OBJECT
                  t    : T;
                  graph: GraphVBT.T;
                  anim : Animation;
                OVERRIDES
                  length := Length;
                  doStep := DoStep;
                END;

PROCEDURE Length (<* UNUSED *> t : MyAnimation;
                  <* UNUSED *> v : MG.V;
                  <* UNUSED *> mg: MG.T         ): INTEGER =
  BEGIN
    RETURN 30;
  END Length;

PROCEDURE DoStep (             self    : MyAnimation;
                               time    : REAL;
                  <* UNUSED *> timePrev: REAL;
                  <* UNUSED *> v       : MG.V;
                  <* UNUSED *> mg      : MG.T         ) =
  BEGIN
    self.t.set(self.graph, self.anim.rgb(time));
  END DoStep;

PROCEDURE Animate (t: T; graph: GraphVBT.T; animation: Animation) =
  BEGIN
    MGV.AddAnimationLocked(
      GraphVBTExtras.GetMG(graph),
      NEW(MyAnimation, t := t, graph := graph, anim := animation).init(), NIL);
  END Animate;

BEGIN
END PaintOpAnim.
