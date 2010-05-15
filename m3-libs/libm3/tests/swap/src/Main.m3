MODULE Main;
FROM Swap IMPORT Swap4;
FROM RTIO IMPORT Flush, PutHex, PutText;

PROCEDURE NL() = BEGIN PutText("\n"); END NL;

BEGIN
  PutHex(16_12F3); NL();
  PutHex(Swap4(16_12F3)); NL();
  NL();
  PutHex(16_1283); NL();
  PutHex(Swap4(16_1283)); NL();
  NL();
  PutHex(16_1273); NL();
  PutHex(Swap4(16_1273)); NL();
  NL();
  Flush();
END Main.
