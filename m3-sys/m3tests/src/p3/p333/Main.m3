(* Unreachable statements after RETURN and RAISE, and after a statically
   failing check, compile: MSIR used to append them after the terminator. *)

MODULE Main;
IMPORT IO, Fmt;
TYPE R = RECORD a: BITS 3 FOR [0..7]; b: [0..10] END;  Ref = REF RECORD x: INTEGER END;
EXCEPTION E;
VAR g := 5;  r: R;  rr := NEW(Ref);
PROCEDURE P(): INTEGER =
  VAR x, y: INTEGER := 2;  q: R;  t: Ref := rr;
  BEGIN
    RETURN x;
    x := y;  q := r;  q.a := 3;  y := q.a;  t.x := g;  g := t.x;  IO.Put("dead\n");
  END P;
PROCEDURE Q() RAISES {E} =
  VAR x: INTEGER;  q: R;
  BEGIN
    RAISE E;
    x := g;  q.a := 1;  q := r;
  END Q;
PROCEDURE S() =
  VAR x: [0..10];  q: R;
  BEGIN
    q.b := 11;
    x := g;  q := r;  q.a := 2;
  END S;
BEGIN
  IO.PutInt(P()); IO.Put("\n");
  TRY Q() EXCEPT E => IO.Put("E\n") END;
  IO.Put("ok\n");
END Main.
