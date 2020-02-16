MODULE Bbox;
IMPORT Fmt;
IMPORT Point;
IMPORT Compass;

PROCEDURE Format(READONLY bbox : T) : TEXT =
  BEGIN
    RETURN "(" & Fmt.Int(bbox.llx) & " " & Fmt.Int(bbox.lly) & " " & 
           Fmt.Int(bbox.urx) & " " & Fmt.Int(bbox.ury) & ")"
  END Format;

PROCEDURE IsContained(READONLY b : T; READONLY p : Point.T) : BOOLEAN =
  BEGIN
    RETURN p.h >= b.llx AND p.h <= b.urx AND p.v <= b.ury AND p.v >= b.lly 
  END IsContained;

PROCEDURE IsContainedWithStep(b : T; 
                              READONLY p : Point.T; 
                              c : REF Compass.Dir) : BOOLEAN =
  BEGIN
    IF c # NIL THEN
      CASE c^ OF
      | Compass.Dir.N => INC(b.ury)
      | Compass.Dir.E => INC(b.urx)
      | Compass.Dir.W => DEC(b.llx)
      | Compass.Dir.S => DEC(b.lly)
      END
    END;
    RETURN IsContained(b, p)
  END IsContainedWithStep;
BEGIN END Bbox.
