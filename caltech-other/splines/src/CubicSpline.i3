(* $Id$ *)

INTERFACE CubicSpline;
IMPORT Spline;

CONST NaturalSpline = 1.0d30;
CONST Brand = "NaturalCubicSpline";

TYPE 
  Coord = Spline.Coord;
  T <: Public;

  (* firstDeriv and lastDeriv set the derivatives at the 
     first and last points of the spline.
     
     If firstDeriv or lastDeriv is set to the constant NaturalSpline,
     then the boundary condition at the corresponding point of the spline
     will be the "natural" b.c., viz. zero second derivative.


     The function must be given in order, i.e., the x's of the coords must
     be in increasing order.
   *)

  Public = Spline.T OBJECT METHODS
    init(READONLY coords : ARRAY OF Coord;
         firstDeriv,lastDeriv := NaturalSpline) : T;
  END;

END CubicSpline.
