(* $Id$ *)
INTERFACE MagTransform;
IMPORT MagCoord AS Coord;
IMPORT MagPoint, MagRect;
IMPORT IntList;

(* Magic's affine transform *)

TYPE
  Unit = [-1..1];

  T = RECORD 
    a : Unit;
    b : Unit;
    c : Coord.T;
    d : Unit;
    e : Unit;
    f : Coord.T;
  END;

CONST
  Brand = "MagTransform";

  Unitary      = T { 1, 0, 0, 0, 1, 0 };
  CCW          = T { 0,-1, 0, 1, 0, 0 };
  CW           = T { 0, 1, 0,-1, 0, 0 };
  Sideways     = T {-1, 0, 0, 0, 1, 0 };
  Upsidedown   = T { 1, 0, 0, 0,-1, 0 };
  
EXCEPTION ParseError;

PROCEDURE Parse(from : TEXT) : T RAISES { ParseError } ;
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

(* transform objects from one coord sys into another *)
PROCEDURE Point(READONLY a : MagPoint.T; READONLY by : T) : MagPoint.T;
PROCEDURE Rect(READONLY a : MagRect.T; READONLY by : T) : MagRect.T;

PROCEDURE Compose(READONLY super, sub : T) : T;
PROCEDURE Inverse(READONLY a : T) : T;

PROCEDURE Format(READONLY a : T) : TEXT;

TYPE 
  Iterator = OBJECT METHODS
    next(VAR transform : T) : BOOLEAN
  END;
END MagTransform.


