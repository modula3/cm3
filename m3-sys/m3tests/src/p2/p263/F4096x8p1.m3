(* Test First_readable_addr. *)

MODULE F4096x8p1; (* x8 is times 8, is plus *)
IMPORT F0;

PROCEDURE F1(t: REF F0.T4096x8p1): CHAR =
BEGIN
  RETURN t.x0[0];
END F1;

BEGIN
END F4096x8p1.
