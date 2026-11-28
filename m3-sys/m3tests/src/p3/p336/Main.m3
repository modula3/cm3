(* Nested procedures that use a WITH alias of a designator with no byte
   address (B50): a bit-packed record field and a packed array element at a
   dynamic index, through a direct call, through a procedure value (a
   closure), two levels down, and read-only.  Writes through the alias reach
   the field or element and leave its neighbours alone. *)

MODULE Main;
IMPORT IO, Fmt;

TYPE
  In  = BITS 13 FOR RECORD a: BITS 4 FOR [0..15];  b: BITS 9 FOR [-256..255] END;
  Out = RECORD x: BITS 3 FOR [0..7];  f: In;  y: BITS 6 FOR [0..63] END;

VAR
  outs : ARRAY [0..3] OF Out;
  p4   : ARRAY [0..9] OF BITS 4 FOR [0..15];
  bad  := 0;

PROCEDURE Value (what: TEXT;  got, want: INTEGER) =
  BEGIN
    IF got # want THEN
      IO.Put ("FAIL " & what & " = " & Fmt.Int (got) & ", want " & Fmt.Int (want) & "\n");
      INC (bad);
    END;
  END Value;

PROCEDURE Apply (p: PROCEDURE ()) = BEGIN p () END Apply;

PROCEDURE Field () =
  (* a packed record field: direct call, closure, two levels *)
  BEGIN
    WITH w = outs[2].f DO
      PROCEDURE SetA () = BEGIN w.a := 11 END SetA;
      PROCEDURE SetB () = BEGIN w.b := -100 END SetB;
      PROCEDURE Outer () =
        PROCEDURE Inner () = BEGIN INC (w.a) END Inner;
        BEGIN Inner () END Outer;
      BEGIN
        SetA ();
        Apply (SetB);
        Outer ();
      END;
    END;
  END Field;

PROCEDURE Element (i: INTEGER) =
  (* a packed array element at a dynamic index *)
  BEGIN
    WITH e = p4[i] DO
      PROCEDURE Set () = BEGIN e := 9 END Set;
      PROCEDURE Bump () = BEGIN INC (e, 3) END Bump;
      BEGIN
        Set ();
        Apply (Bump);
      END;
    END;
  END Element;

PROCEDURE Read (): INTEGER =
  (* read-only use of the whole packed record *)
  BEGIN
    WITH w = outs[2].f DO
      PROCEDURE Sum (): INTEGER = BEGIN RETURN w.a + w.b END Sum;
      BEGIN
        RETURN Sum ();
      END;
    END;
  END Read;

BEGIN
  outs[2].x := 5;  outs[2].y := 42;
  Field ();
  Value ("outs[2].f.a", outs[2].f.a, 12);
  Value ("outs[2].f.b", outs[2].f.b, -100);
  Value ("outs[2].x", outs[2].x, 5);
  Value ("outs[2].y", outs[2].y, 42);
  Value ("sum", Read (), 12 - 100);

  FOR i := 0 TO 9 DO p4[i] := i END;
  Element (3);  Element (6);
  FOR i := 0 TO 9 DO
    IF i = 3 OR i = 6 THEN
      Value ("p4[" & Fmt.Int (i) & "]", p4[i], 12);
    ELSE
      Value ("p4[" & Fmt.Int (i) & "]", p4[i], i);
    END;
  END;

  IF bad = 0 THEN IO.Put ("OK\n") END;
END Main.
