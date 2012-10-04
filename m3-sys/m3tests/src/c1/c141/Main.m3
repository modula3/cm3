UNSAFE MODULE Main;
IMPORT RTIO, ASCII;

TYPE BYTE = BITS 8 FOR [0..255];

PROCEDURE F1(READONLY charSet: ASCII.Set) =
CONST size = BITSIZE(charSet) DIV 8;
VAR p := LOOPHOLE(ADR(charSet), REF ARRAY [0..size - 1] OF BYTE);
  BEGIN
    FOR i := 0 TO size - 1 DO
        RTIO.PutHex(p[i]); RTIO.PutText(" ");
    END;
    RTIO.Flush();
  END F1;

BEGIN
    F1(ASCII.Asciis - ASCII.Set {' '});
END Main.
