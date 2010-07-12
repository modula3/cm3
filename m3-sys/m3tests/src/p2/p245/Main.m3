(* ../src/text/TextConv.m3:13:0: internal compiler error:
   in estimate_move_cost, at tree-inline.c:3016
*)

MODULE Main;
IMPORT Text;

PROCEDURE F1(READONLY sep: SET OF CHAR) =
  BEGIN
    LOOP
      IF Text.GetChar("A", 0) IN sep THEN
        RETURN;
      END;
    END;
  END F1;

<*NOWARN*>PROCEDURE F2(READONLY sep: SET OF CHAR) =
  BEGIN
    F1(sep);
  END F2;

BEGIN
END Main.
