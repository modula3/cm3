GENERIC INTERFACE Interval(Float);

(* An interval "a" contains a "Float.T x" if "a.lo <= x  AND  x < a.hi". *)

TYPE
  T = RECORD
    lo, hi: Float.T;
  END;

PROCEDURE Size(i: T): Float.T;
PROCEDURE Member(x: Float.T; READONLY a: T): BOOLEAN;

END Interval.
