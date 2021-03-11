GENERIC MODULE Interval(Float);

PROCEDURE Size(i: T): Float.T =
  BEGIN
    RETURN i.hi - i.lo;
  END Size;

PROCEDURE Member(x: Float.T; READONLY a: T): BOOLEAN =
  BEGIN
    RETURN a.lo <= x  AND  x < a.hi;
  END Member;

BEGIN
END Interval.
