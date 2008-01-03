MODULE TestBits EXPORTS Test;
(* Arithmetic for Modula-3, see doc for details Abstract: Tests for Utils
   module.

   1/1/96 Harry George Initial version *)

IMPORT Bits AS B;
IMPORT Fmt, Text;

CONST Module = "TestBits.";

PROCEDURE TestWhichEndian (): BOOLEAN =
  CONST ftn = Module & "TestWhichEndian";
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    FOR i := 1 TO 4 DO
      CASE B.WhichEndian() OF
      | -1 => Msg("little endian\n");
      | 0 => Msg("error\n");
      | +1 => Msg("big endian\n");
      END;
    END;
    RETURN result;
  END TestWhichEndian;

PROCEDURE TestFmt (): BOOLEAN =
  CONST
    ftn = Module & "TestFmt";
    x   = 2_1010;
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    Msg("2_1010=" & B.Fmt(x, nbits := 4) & "\n");
    Msg("2_00001010=" & B.Fmt(x, nbits := 8) & "\n");
    Msg("2_000000001010=" & B.Fmt(x, nbits := 12) & "\n");
    Msg("2_0000000000001010=" & B.Fmt(x, nbits := 16) & "\n");

    RETURN result;
  END TestFmt;

PROCEDURE TestReverse (): BOOLEAN =
  CONST
    ftn   = Module & "TestReverse";
    x     = 2_1011;
    nbits = 4;
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    Msg(B.Fmt(x, nbits + 2) & " reverses to "
          & B.Fmt(B.Reverse(x, nbits), nbits + 2) & "\n");
    <* ASSERT B.Reverse(x, nbits) = 2_1101 *>

    RETURN result;
  END TestReverse;

CONST textdata = "the quick brown fox jumped over the lazy dog";
VAR str: ARRAY [0 .. 20] OF CHAR;


PROCEDURE TestHashPJW (): BOOLEAN =
  CONST ftn = Module & "TestHashPJW";
  VAR result := TRUE;

  BEGIN
    Debug(1, ftn, "begin\n");
    Text.SetChars(str, textdata);
    FOR i := FIRST(str) TO LAST(str) - 6 BY 5 DO
      Msg("hash=" & Fmt.Int(B.HashPJW(str, i, i + 5)) & "\n");
    END;
    RETURN result;
  END TestHashPJW;

PROCEDURE TestHashELF (): BOOLEAN =
  CONST ftn = Module & "TestHashELF";
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    Text.SetChars(str, textdata);
    FOR i := FIRST(str) TO LAST(str) - 6 BY 5 DO
      Msg("hash=" & Fmt.Int(B.HashELF(str, i, i + 5)) & "\n");
    END;

    RETURN result;
  END TestHashELF;

PROCEDURE TestBits (): BOOLEAN =
  CONST ftn = Module & "TestBits";
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    NewLine();
    EVAL TestWhichEndian();
    NewLine();
    EVAL TestFmt();
    NewLine();
    EVAL TestReverse();
    NewLine();
    EVAL TestHashPJW();
    NewLine();
    EVAL TestHashELF();
    RETURN result;
  END TestBits;

BEGIN
END TestBits.
