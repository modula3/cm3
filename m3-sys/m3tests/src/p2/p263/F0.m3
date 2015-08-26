(* Test First_readable_addr. *)

MODULE F0;

PROCEDURE F1(t: REF T0): CHAR =
BEGIN
  RETURN t.x0[0];
END F1;

BEGIN
END F0.
