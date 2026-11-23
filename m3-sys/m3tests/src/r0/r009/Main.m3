(* Assigning a value outside a sub-byte field's range. *)
MODULE Main;
TYPE R = RECORD a: BITS 3 FOR [0..5];  b: BITS 5 FOR [0..31] END;
VAR r: R;  v := 7;
BEGIN
  r.a := v;
END Main.
