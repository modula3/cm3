(* RETURN of a value outside the procedure's subrange result type. *)
MODULE Main;
PROCEDURE Size (lo, hi: INTEGER): CARDINAL =
  BEGIN
    RETURN hi - lo;
  END Size;
VAR lo := 5;
BEGIN
  EVAL Size (lo, 3);
END Main.
