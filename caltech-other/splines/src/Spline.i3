(* $Id$ *)

INTERFACE Spline;
IMPORT LRFunction, LRPoint;

(* this interface just defines the basic operations on splines *)
(* an implementation can override the methods or add more *)

TYPE
  Coord = LRPoint.T;

  T <: Public;

  Public = LRFunction.T OBJECT METHODS
    deriv(at : LONGREAL) : LONGREAL; (* take derivative; uses Ridders's
                                        algorithm by default;
                                        implementors are urged to 
                                        provide analytic derivatives if
                                        possible *)
  END;

CONST Brand = "Spline";

END Spline.
