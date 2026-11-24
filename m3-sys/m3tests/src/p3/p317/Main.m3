UNSAFE MODULE Main;

(* LOOPHOLE of a record value to a scalar, where the record comes from a
   call whose argument needs a range check, or from an element whose
   subscript needs a bounds check.  The check splits the block; MSIR's
   record-to-scalar LOOPHOLE emitted its spill into the block before the
   check, after its branch (failed MSIR verification). *)

IMPORT IO, Fmt;

TYPE R = RECORD v: INTEGER END;

VAR
  n   := 7;
  i   := 2;
  arr := ARRAY [0 .. 3] OF R { R { 1 }, R { 2 }, R { 3 }, R { 4 } };

PROCEDURE Make (a: [0 .. 1000]): R =
  BEGIN
    RETURN R { a };
  END Make;

BEGIN
  IO.Put ("call " & Fmt.Int (LOOPHOLE (Make (n), INTEGER)) & "\n");
  IO.Put ("element " & Fmt.Int (LOOPHOLE (arr[i], INTEGER)) & "\n");
  IO.Put ("sum " & Fmt.Int (LOOPHOLE (Make (n + 1), INTEGER)
                              + LOOPHOLE (arr[i + 1], INTEGER)) & "\n");
END Main.
