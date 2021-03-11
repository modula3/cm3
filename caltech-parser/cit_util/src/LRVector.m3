(* $Id$ *)

MODULE LRVector;
IMPORT Math;

PROCEDURE Norm(v : T) : LONGREAL =
  VAR
    res := 0.0d0;
  BEGIN
    FOR i := FIRST(v^) TO LAST(v^) DO
      res := res + v[i]*v[i]
    END;
    RETURN Math.sqrt(res)
  END Norm;

PROCEDURE Copy(a : T) : T =
  VAR res := NEW(T, NUMBER(a^)); BEGIN res^ := a^; RETURN res END Copy;

BEGIN END LRVector.
