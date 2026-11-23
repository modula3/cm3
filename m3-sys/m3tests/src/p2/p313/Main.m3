(* <*TRACE*> pragmas on formals, locals with and without initializers, a
   nested procedure's local, a WITH variable and a FOR variable. *)

MODULE Main;
IMPORT IO, Fmt;

PROCEDURE T (name: TEXT;  v: INTEGER) =
  BEGIN
    IO.Put ("trace " & name & " = " & Fmt.Int (v) & "\n");
  END T;

PROCEDURE P (f: INTEGER <*TRACE T*>) =
  VAR a: INTEGER := f + 1 <*TRACE T*>;
  VAR b: [0..9] <*TRACE T*>;
  PROCEDURE Q () =
    VAR c := a * 10 <*TRACE T*>;
    BEGIN IO.Put ("in Q\n");  EVAL c END Q;
  BEGIN
    IO.Put ("in P\n");
    Q ();
    WITH w <*TRACE T*> = a + b DO IO.Put ("in WITH\n");  EVAL w END;
    FOR i <*TRACE T*> := 1 TO 2 DO IO.Put ("loop\n") END;
  END P;

BEGIN
  P (41);
END Main.
