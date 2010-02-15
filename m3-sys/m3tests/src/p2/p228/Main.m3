(* This test covers sign/zero extension of
 * the return value of functions returning types
 * smaller than 32bits. It is most effective
 * to test it on NT386, esp. with older versions of
 * Visual C++, or hand written assembly.
 *)

MODULE Main;
IMPORT RTIO;
FROM Test IMPORT NegativeInt8, NegativeUInt8, NegativeInt16, NegativeUInt16, NegativeInt32, NegativeUInt32, NegativeInt64, NegativeUInt64,
                 PositiveInt8, PositiveUInt8, PositiveInt16, PositiveUInt16, PositiveInt32, PositiveUInt32, PositiveInt64, PositiveUInt64;
FROM RTIO IMPORT Flush;

CONST PutI = RTIO.PutInt;
CONST PutL = RTIO.PutLong;
CONST PutT = RTIO.PutText;

PROCEDURE NL() =
BEGIN
  PutT("\n");
END NL;

BEGIN
  PutT("NegativeInt8 "); PutI(NegativeInt8()); NL();
  PutT("NegativeUInt8 "); PutI(NegativeUInt8()); NL();
  PutT("NegativeInt16 "); PutI(NegativeInt16()); NL();
  PutT("NegativeUInt16 "); PutI(NegativeUInt16()); NL();
  PutT("NegativeInt32 "); PutI(NegativeInt32()); NL();
  PutT("NegativeUInt32 "); PutI(NegativeUInt32()); NL();
  PutT("NegativeInt64 "); PutL(NegativeInt64()); NL();
  PutT("NegativeUInt64 "); PutL(NegativeUInt64()); NL();

  PutT("NegativeInt8  => LONGINT "); PutL(VAL(NegativeInt8(), LONGINT)); NL();
  PutT("NegativeUInt8 => LONGINT "); PutL(VAL(NegativeUInt8(), LONGINT)); NL();
  PutT("NegativeInt16 => LONGINT "); PutL(VAL(NegativeInt16(), LONGINT)); NL();
  PutT("NegativeUInt16 => LONGINT "); PutL(VAL(NegativeUInt16(), LONGINT)); NL();
  PutT("NegativeInt32 => LONGINT "); PutL(VAL(NegativeInt32(), LONGINT)); NL();
  PutT("NegativeUInt32 => LONGINT "); PutL(VAL(NegativeUInt32(), LONGINT)); NL();
  PutT("NegativeInt64 => LONGINT "); PutL(VAL(NegativeInt64(), LONGINT)); NL();
  PutT("NegativeUInt64 => LONGINT "); PutL(VAL(NegativeUInt64(), LONGINT)); NL();

  (* PutT("NegativeInt8 => LONGCARD "); PutL(VAL(NegativeInt8(), LONGCARD)); NL(); *)
  PutT("NegativeUInt8 => LONGCARD "); PutL(VAL(NegativeUInt8(), LONGCARD)); NL();
  (* PutT("NegativeInt16 => LONGCARD "); PutL(VAL(NegativeInt16(), LONGCARD)); NL(); *)
  PutT("NegativeUInt16 => LONGCARD "); PutL(VAL(NegativeUInt16(), LONGCARD)); NL();
  (* PutT("NegativeInt32 => LONGCARD "); PutL(VAL(NegativeInt32(), LONGCARD)); NL(); *)
  (* PutT("NegativeUInt32 => LONGCARD "); PutL(VAL(NegativeUInt32(), LONGCARD)); NL(); bug? *)
  (* PutT("NegativeInt64 => LONGCARD "); PutL(VAL(NegativeInt64(), LONGCARD)); NL(); *)
  (* PutT("NegativeUInt64 => LONGCARD "); PutL(VAL(NegativeUInt64(), LONGCARD)); NL(); *)




  PutT("PositiveInt8 "); PutI(PositiveInt8()); NL();
  PutT("PositiveUInt8 "); PutI(PositiveUInt8()); NL();
  PutT("PositiveInt16 "); PutI(PositiveInt16()); NL();
  PutT("PositiveUInt16 "); PutI(PositiveUInt16()); NL();
  PutT("PositiveInt32 "); PutI(PositiveInt32()); NL();
  PutT("PositiveUInt32 "); PutI(PositiveUInt32()); NL();
  PutT("PositiveInt64 "); PutL(PositiveInt64()); NL();
  PutT("PositiveUInt64 "); PutL(PositiveUInt64()); NL();

  PutT("PositiveInt8 => LONGINT "); PutL(VAL(PositiveInt8(), LONGINT)); NL();
  PutT("PositiveUInt8 => LONGINT "); PutL(VAL(PositiveUInt8(), LONGINT)); NL();
  PutT("PositiveInt16 => LONGINT "); PutL(VAL(PositiveInt16(), LONGINT)); NL();
  PutT("PositiveUInt16 => LONGINT "); PutL(VAL(PositiveUInt16(), LONGINT)); NL();
  PutT("PositiveInt32 => LONGINT "); PutL(VAL(PositiveInt32(), LONGINT)); NL();
  PutT("PositiveUInt32 => LONGINT "); PutL(VAL(PositiveUInt32(), LONGINT)); NL();
  PutT("PositiveInt64 => LONGINT "); PutL(VAL(PositiveInt64(), LONGINT)); NL();
  PutT("PositiveUInt64 => LONGINT "); PutL(VAL(PositiveUInt64(), LONGINT)); NL();

  PutT("PositiveInt8  => LONGCARD "); PutL(VAL(PositiveInt8(), LONGCARD)); NL();
  PutT("PositiveUInt8 => LONGCARD "); PutL(VAL(PositiveUInt8(), LONGCARD)); NL();
  PutT("PositiveInt16 => LONGCARD "); PutL(VAL(PositiveInt16(), LONGCARD)); NL();
  PutT("PositiveUInt16 => LONGCARD "); PutL(VAL(PositiveUInt16(), LONGCARD)); NL();
  PutT("PositiveInt32 => LONGCARD "); PutL(VAL(PositiveInt32(), LONGCARD)); NL();
  PutT("PositiveUInt32 => LONGCARD "); PutL(VAL(PositiveUInt32(), LONGCARD)); NL();
  PutT("PositiveInt64 => LONGCARD "); PutL(VAL(PositiveInt64(), LONGCARD)); NL();
  PutT("PositiveUInt64 => LONGCARD "); PutL(VAL(PositiveUInt64(), LONGCARD)); NL();


  Flush();
END Main.
