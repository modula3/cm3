MODULE PersistentRevelation;

(* A PersistentRevelation is a pair of integers written into object code. *)

PROCEDURE CompareInt(a, b: INTEGER): [-1 .. 1] =
BEGIN
  IF a < b THEN
    RETURN -1;
  ELSIF a > b THEN
    RETURN 1;
  END;
  RETURN 0;
END CompareInt;

PROCEDURE Compare(a, b: T): [-1 .. 1] =
VAR c := CompareInt(a.lhs_id, b.lhs_id);
BEGIN
  IF c = 0 THEN
    c := CompareInt(a.rhs_id, b.rhs_id);
  END;
  RETURN c;
END Compare;

BEGIN
END PersistentRevelation.
