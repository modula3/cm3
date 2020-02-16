INTERFACE Bbox;
IMPORT Point;
IMPORT Compass;

TYPE
  T = RECORD llx, lly, urx, ury : INTEGER; END;

PROCEDURE Format(READONLY bbox : T) : TEXT;
PROCEDURE IsContained(READONLY bbox : T; READONLY p : Point.T) : BOOLEAN;
PROCEDURE IsContainedWithStep(b : T; 
                              READONLY p : Point.T; 
                              c : REF Compass.Dir) : BOOLEAN;

CONST 
  Brand = "Bbox";

END Bbox.
