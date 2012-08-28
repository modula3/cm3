(* easy: initializers are actually all snippets in the module begin/end *)

MODULE Main;
PROCEDURE F1(): INTEGER = BEGIN RETURN 1; END F1;
VAR a := F1();
BEGIN
END Main.
