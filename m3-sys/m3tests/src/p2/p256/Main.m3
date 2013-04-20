(* test for removing unused functions/declarations *)

MODULE Main;
IMPORT B;

PROCEDURE Main() =
BEGIN
    B.Exported();
END Main;

BEGIN
    Main();
END Main.
