INTERFACE PersistentRevelation;

(* A PersistentRevelation is a pair of integers written into object code. *)

TYPE
  T = RECORD
    lhs_id := 0;
    rhs_id := 0;
  END;

CONST Brand = "PersistentRevelation";

PROCEDURE Compare(a, b: T): [-1 .. 1];

END PersistentRevelation.
