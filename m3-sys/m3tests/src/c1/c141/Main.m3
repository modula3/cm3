MODULE Main;
IMPORT RTIO, ASCII, Text;

PROCEDURE FindCharSet(
    t: Text.T;
    READONLY charSet: ASCII.Set;
    VAR index: CARDINAL)
    : BOOLEAN =
  VAR
    i: CARDINAL := index;
    length: CARDINAL := Text.Length(t);
  BEGIN
    REPEAT
      IF Text.GetChar (t, i) IN charSet THEN index := i; RETURN TRUE END;
      INC(i);
    UNTIL i = length;
    index := i;
    RETURN FALSE;
  END FindCharSet;

PROCEDURE F1() =
VAR pos: CARDINAL := 0;
BEGIN
    RTIO.PutInt(ORD(FindCharSet("abc", ASCII.Asciis - ASCII.Set {' '}, pos)));
    RTIO.PutText(" ");
    RTIO.PutInt(pos);
    RTIO.PutText("\n");
    RTIO.Flush();
END F1;

BEGIN
    F1();
END Main.
