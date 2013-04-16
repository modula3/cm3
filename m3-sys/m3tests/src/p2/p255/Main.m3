(* This test case demonstrates a problem
where the frontend doesn't describe all the types
to the backend. And also where it doesn't single-instance
constants that it could/should. *)

MODULE Main;
IMPORT HighlightVBT;

PROCEDURE Main() =
BEGIN
    NEW(HighlightVBT.T).init();
END Main;

BEGIN
    Main();
END Main.
