(* Test First_readable_addr. *)

MODULE F2;
IMPORT F0;

PROCEDURE F1(t: REF F0.T2): CHAR =
BEGIN
  RETURN t.x0[0];
END F1;

BEGIN
END F2.
