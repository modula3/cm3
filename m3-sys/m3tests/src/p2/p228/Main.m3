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
  PutT("FInt8   ");   PutI(FInt8()); NL();
  PutT("FUInt8  ");  PutI(FUInt8()); NL();
  PutT("FInt16  ");  PutI(FInt16()); NL();
  PutT("FUInt16 "); PutI(FUInt16()); NL();
  PutT("FInt32  ");  PutI(FInt32()); NL();
  PutT("FUInt32 "); PutI(FUInt32()); NL();
  PutT("FInt64  ");  PutL(FInt64()); NL();
  PutT("FUInt64 "); PutL(FUInt64()); NL();

  PutT("FInt8   => LONGINT ");   PutL(VAL(FInt8(), LONGINT)); NL();
  PutT("FUInt8  => LONGINT ");  PutL(VAL(FUInt8(), LONGINT)); NL();
  PutT("FInt16  => LONGINT ");  PutL(VAL(FInt16(), LONGINT)); NL();
  PutT("FUInt16 => LONGINT "); PutL(VAL(FUInt16(), LONGINT)); NL();
  PutT("FInt32  => LONGINT ");  PutL(VAL(FInt32(), LONGINT)); NL();
  PutT("FUInt32 => LONGINT "); PutL(VAL(FUInt32(), LONGINT)); NL();
  PutT("FInt64  => LONGINT ");  PutL(VAL(FInt64(), LONGINT)); NL();
  PutT("FUInt64 => LONGINT "); PutL(VAL(FUInt64(), LONGINT)); NL();

  (* PutT("FInt8   => LONGCARD ");   PutL(VAL(FInt8(), LONGCARD)); NL(); *)
  PutT("FUInt8  => LONGCARD ");  PutL(VAL(FUInt8(), LONGCARD)); NL();
  (* PutT("FInt16  => LONGCARD ");  PutL(VAL(FInt16(), LONGCARD)); NL(); *)
  PutT("FUInt16 => LONGCARD "); PutL(VAL(FUInt16(), LONGCARD)); NL();
  (* PutT("FInt32  => LONGCARD ");  PutL(VAL(FInt32(), LONGCARD)); NL(); *)
  (* PutT("FUInt32 => LONGCARD "); PutL(VAL(FUInt32(), LONGCARD)); NL();    bug *)
  (* PutT("FInt64  => LONGCARD ");  PutL(VAL(FInt64(), LONGCARD)); NL(); *)
  (* PutT("FUInt64 => LONGCARD "); PutL(VAL(FUInt64(), LONGCARD)); NL(); *)

  Flush();
END Main.
