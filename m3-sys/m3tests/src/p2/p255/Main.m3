(* This test case demonstrates a problem
where the frontend doesn't describe all the types
to the backend. And also where it doesn't single-instance
constants that it could/should. *)

MODULE Main;
IMPORT HighlightVBT;

VAR Main_b := 1;

PROCEDURE Main() =
BEGIN
    NEW(HighlightVBT.HighlightVBT_T).init();
    NEW(HighlightVBT.HighlightVBT_T).init();
    NEW(HighlightVBT.HighlightVBT_T).init();
END Main;

BEGIN
    INC(HighlightVBT.HighlightVBT_var_a);
    INC(Main_b);
    Main();
    INC(HighlightVBT.HighlightVBT_var_b);
END Main.
