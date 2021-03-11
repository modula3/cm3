(* $Id$ *)
INTERFACE MagArrayData;
IMPORT MagRect, MagTransform;

EXCEPTION ParseError;
  

TYPE
  T <: REFANY;

CONST Brand = "Magic ArrayData";

PROCEDURE Parse(data : TEXT) : T RAISES { ParseError } ;


PROCEDURE ComputeArrayBBox(READONLY box : MagRect.T; 
                           READONLY transform : MagTransform.T;
                           READONLY arrayData : T) : MagRect.T;

PROCEDURE Format(READONLY t : T) : TEXT;

PROCEDURE FormatForMag(READONLY t : T) : TEXT;

END MagArrayData.


