INTERFACE Dump;

<* EXTERNAL *> PROCEDURE Dump(type, offset, count, bitsize, bytesize: INTEGER;
                              address: ADDRESS);

<* EXTERNAL *> PROCEDURE Print1(textLiteral:TEXT; decimal: INTEGER);

END Dump.
