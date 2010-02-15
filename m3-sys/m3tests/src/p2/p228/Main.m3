(* This test covers sign/zero extension of
 * the return value of functions returning types
 * smaller than 32bits. It is most effective
 * to test it on NT386, esp. with older versions of
 * Visual C++, or hand written assembly.
 *)

MODULE Main;
IMPORT RTIO;
FROM Test IMPORT FInt8, FUInt8, FInt16, FUInt16, FInt32, FUInt32, FInt64, FUInt64;
FROM RTIO IMPORT Flush;

CONST PutI = RTIO.PutInt;
CONST PutL = RTIO.PutLong;
CONST PutT = RTIO.PutText;

PROCEDURE NL() =
BEGIN
  PutT("\n");
END NL;

BEGIN
  PutI(FInt8()); NL();
  PutI(FUInt8()); NL();
  PutI(FInt16()); NL();
  PutI(FUInt16()); NL();
  PutI(FInt32()); NL();
  PutI(FUInt32()); NL();
  PutL(FInt64()); NL();
  PutL(FUInt64()); NL();
  Flush();
END Main.
