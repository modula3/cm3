(* ../src/text/TextConv.m3:13:0: internal compiler error:
   in estimate_move_cost, at tree-inline.c:3016
*)

MODULE Main;
IMPORT Text;

PROCEDURE xExplodedItemSize(READONLY sep: SET OF CHAR) =
  BEGIN
    FOR i := 0 TO 1 DO
      IF Text.GetChar("ABC", 0) IN sep THEN
        RETURN;
      END;
    END;
  END xExplodedItemSize;

<*NOWARN*>PROCEDURE xExplodedSize(READONLY sep: SET OF CHAR) =
  BEGIN
    xExplodedItemSize(sep);
  END xExplodedSize;

BEGIN
END Main.
