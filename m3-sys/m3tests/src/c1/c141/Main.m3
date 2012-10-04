MODULE Main;
IMPORT TextExtras, RTIO, ASCII;

PROCEDURE F1() =
VAR pos: CARDINAL := 0;
BEGIN
    RTIO.PutInt(ORD(TextExtras.FindCharSet("abc", ASCII.Asciis - ASCII.Set {' '}, pos)));
    RTIO.PutText(" ");
    RTIO.PutInt(pos);
    RTIO.PutText("\n");
    RTIO.Flush();
END F1;

BEGIN
    F1();
END Main.
