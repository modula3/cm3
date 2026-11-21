(* $Id: LRVector.m3,v 1.1 2001/10/24 18:27:38 penzes Exp $ *)

MODULE LRVector;
IMPORT Math;
IMPORT Word, LongrealType;

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

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    IF a = b THEN
      RETURN TRUE
    ELSIF NUMBER(a^) # NUMBER(b^) THEN
      RETURN FALSE
    ELSE
      RETURN a^ = b^
    END
  END Equal;

PROCEDURE Hash(a : T) : Word.T =
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      res := Word.Times(Word.Plus(res, LongrealType.Hash(a[i])), 16_c0edbabe)
    END;
    RETURN res
  END Hash;

PROCEDURE Compare(a, b : T) : [-1..1] =
  BEGIN
    <*ASSERT NUMBER(a^) = NUMBER(b^)*>
    FOR i := FIRST(a^) TO LAST(a^) DO
      IF a[i] < b[i] THEN
        RETURN -1
      ELSIF b[i] > a[i] THEN
        RETURN +1
      ELSIF a[i] # b[i] THEN
        RETURN -1 (* NaN, Inf, etc. *)
      END
    END;
    RETURN 0
  END Compare;

BEGIN END LRVector.
