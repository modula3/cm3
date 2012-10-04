MODULE Main;
IMPORT RTIO, ASCII;

PROCEDURE FindCharSet(READONLY charSet: ASCII.Set) : BOOLEAN =
  BEGIN
    RETURN 'a' IN charSet;
  END FindCharSet;

PROCEDURE F1() =
BEGIN
    RTIO.PutInt(ORD(FindCharSet(ASCII.Asciis - ASCII.Set {' '})));
    RTIO.PutText("\n");
    RTIO.Flush();
END F1;

BEGIN
    F1();
END Main.
