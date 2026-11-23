(* INC past the upper bound of a subrange variable. *)
MODULE Main;
VAR d: [0..9] := 8;  k := 2;
BEGIN
  INC (d, k);
END Main.
