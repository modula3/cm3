(* $Id$ *)

MODULE ParametricSpline;
IMPORT ParametricSplineRep;
IMPORT IntegrateTrap;
IMPORT LRFunction;

REVEAL 
  T = ParametricSplineRep.T BRANDED Brand OBJECT
  OVERRIDES 
    area := Area;
  END;


<* FATAL IntegrateTrap.NoConvergence *>

TYPE
  IntObj = LRFunction.T OBJECT
    spline : T;
  OVERRIDES
    eval := IntObjEval
  END;

PROCEDURE IntObjEval(self : IntObj; p : LONGREAL) : LONGREAL =
  VAR
    y := self.spline.getParametricPoint(p).y;
    dx_ds := self.spline.xSpline.deriv(p);
  BEGIN
    RETURN y * dx_ds
  END IntObjEval;

(* $$\int_{s=0}^{s=1} y(s) {dx\over ds} \,ds$$ *)
PROCEDURE Area(self : T) : LONGREAL =
  BEGIN
    RETURN IntegrateTrap.IntegrateE(NEW(IntObj, spline := self), 
                                    0.0d0, 1.0d0)
  END Area;

BEGIN END ParametricSpline.
