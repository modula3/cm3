(* A VALUE argument outside its formal's subrange type. *)
MODULE Main;
PROCEDURE Small (x: [0..9]): INTEGER =
  BEGIN
    RETURN x;
  END Small;
VAR big := 12;
BEGIN
  EVAL Small (big);
END Main.
