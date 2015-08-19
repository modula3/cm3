(* Test First_readable_addr. *)

MODULE F1;
IMPORT F0;

PROCEDURE F1(t: REF F0.T): CHAR =
BEGIN
  RETURN t.x1;
END F1;

BEGIN
END F1.
