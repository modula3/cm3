MODULE Main;

IMPORT I, IO;

VAR
  b: ARRAY [0..1] OF CHAR := SUBARRAY(I.a, 1, 2);
BEGIN
  IO.PutChar(b[0]);
  IO.Put("\nOK\n");
END Main.
