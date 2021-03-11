(* $Id$ *)

INTERFACE MagArrayElemTransform;
IMPORT MagTransform;

TYPE
  T = RECORD 
    transform : MagTransform.T; (* what is the transform *)
    xArray, yArray : BOOLEAN;   (* is the element part of an array in dim? *)
    xIdx, yIdx : INTEGER        (* yes -- index is... *)
  END;

CONST Brand = "MagArrayElemTransform";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END MagArrayElemTransform.
