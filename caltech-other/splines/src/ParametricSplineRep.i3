(* $Id$ *)

(* private interface for use by this package *)
INTERFACE ParametricSplineRep;
IMPORT Spline, ParametricSpline;

TYPE
  T = ParametricSpline.Public BRANDED Brand OBJECT
    xSpline, ySpline : Spline.T;
  END;

REVEAL ParametricSpline.T <: T;

CONST Brand = "ParametricSplineRep";

END ParametricSplineRep.
