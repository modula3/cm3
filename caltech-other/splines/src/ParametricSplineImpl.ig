(* $Id$ *)

GENERIC INTERFACE ParametricSplineImpl(BaseSpline);
IMPORT ParametricSpline;

CONST Brand = "Parametric" & BaseSpline.Brand;

TYPE T <: ParametricSpline.T;

(* An instantiation of a ParametricSpline.T is a two-dimensional *) 
(* parametric spline type.  It uses the square-root formula for *)
(* a good parameterization.  The underlying spline is formed as *)
(* a BaseSpline.T; the BaseSpline.T should be a subtype of Spline.T *)
   

END ParametricSplineImpl.
