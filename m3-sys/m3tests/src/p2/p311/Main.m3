(* Procedures declared in inner blocks of a nested procedure: they use and
   update variables of the inner block, of their enclosing procedure and of
   the procedure enclosing that, including a FOR variable. *)

MODULE Main;
IMPORT IO, Fmt;

PROCEDURE Outer () =
  VAR o := 100;
  PROCEDURE P () =
    BEGIN
      VAR x := 1;
          PROCEDURE Q () = BEGIN INC (x);  INC (o) END Q;
      BEGIN
        x := 5;  Q ();
        IO.Put ("x=" & Fmt.Int (x) & " o=" & Fmt.Int (o) & "\n");
        VAR y := 10;
            PROCEDURE R () = BEGIN INC (y, x) END R;
        BEGIN
          R ();  IO.Put ("y=" & Fmt.Int (y) & "\n");
        END;
      END;
      FOR i := 1 TO 2 DO
        VAR s := 0;
            PROCEDURE S () = BEGIN s := i * 7 END S;
        BEGIN
          S ();  IO.Put ("s=" & Fmt.Int (s) & "\n");
        END;
      END;
    END P;
  BEGIN
    P ();
    IO.Put ("o=" & Fmt.Int (o) & "\n");
  END Outer;

BEGIN
  Outer ();
END Main.
