UNSAFE MODULE Main;
IMPORT RTIO, Text;

TYPE BYTE = BITS 8 FOR [0..255];
TYPE Range = ['a'..'d'];
TYPE Set = SET OF Range;

PROCEDURE NibbleToHexChar(a: BYTE): CHAR =
BEGIN
    IF a <= 9 THEN
        RETURN VAL(a + VAL(ORD('0'), BYTE), CHAR);
    END;
    RETURN VAL(a - 10 + VAL(ORD('A'), BYTE), CHAR);
END NibbleToHexChar;

PROCEDURE ByteToHex2(a: BYTE): TEXT =
VAR buf: ARRAY [0..1] OF CHAR;
BEGIN
    buf[0] := NibbleToHexChar(a DIV 16);
    buf[1] := NibbleToHexChar(a MOD 16);
    RETURN Text.FromChars(SUBARRAY(buf, 0, 2));
END ByteToHex2;

PROCEDURE F1(READONLY charSet: Set) =
CONST size = BITSIZE(charSet) DIV 8;
VAR p := LOOPHOLE(ADR(charSet), REF ARRAY [0..size - 1] OF BYTE);
  BEGIN
    FOR i := 0 TO size - 1 DO
        RTIO.PutText(ByteToHex2(p[i]));
    END;
    RTIO.PutText("\n");
    RTIO.Flush();
  END F1;

BEGIN
    F1(Set{'a'..'d'});
    F1(Set{'a','b'} - Set{'c'});
    F1(Set{'a','b'} + Set{'c'});
END Main.
