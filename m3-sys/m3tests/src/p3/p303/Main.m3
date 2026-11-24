(* Block-local variables with initializers: one in a nested procedure, and
   one in a loop body, re-initialized on every iteration.  With MSIR emitted
   alongside the C backend (@M3m3front-msir), the front end once emitted the
   nested procedure's initialization into the enclosing procedure's C (which
   did not compile) and the loop variable's MSIR initialization once, at
   procedure entry. *)

MODULE Main;
IMPORT IO, Fmt;

PROCEDURE Outer (n: INTEGER): INTEGER =
  VAR sum := 0;
  PROCEDURE Inner (k: INTEGER) =
    BEGIN
      VAR z: INTEGER := k * 10; BEGIN
        INC (sum, z);
      END;
    END Inner;
  BEGIN
    FOR i := 1 TO n DO Inner (i) END;
    RETURN sum;
  END Outer;

BEGIN
  FOR i := 1 TO 3 DO
    VAR x := i - i; BEGIN
      INC (x);
      IO.Put (Fmt.Int (x) & " ");
    END;
  END;
  IO.Put ("| " & Fmt.Int (Outer (3)) & "\n");
END Main.
