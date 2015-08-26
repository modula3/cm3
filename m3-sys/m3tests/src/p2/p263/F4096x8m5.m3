(* Test First_readable_addr. *)

MODULE F4096x8m5; (* x8 is times 8, m is minus *)
IMPORT F0;

PROCEDURE F1(t: REF F0.T4096x8m5): CHAR =
BEGIN
  RETURN t.x0[0];
END F1;

BEGIN
END F4096x8m5.
