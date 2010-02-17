MODULE Main;
IMPORT RTIO, Word, Long;
FROM RTIO IMPORT Flush;
FROM Compiler IMPORT ThisLine;

(* This test covers various longint and bit operations.
 * Shift, extract, insert, etc.
 *)

(* NOTE: The NT386 backend does constant folding, but not inlining.
 * This test code takes advantage of that and compares
 * the constant folded values against the non-constant folded values.
 * If a differently-optimizing backend is used, that is ok, but the
 * asserts might not be useful.
 *)

(* decrease these for faster runs *)
VAR InsertExtractMaxAB := 33;
VAR InsertExtractMaxMN := 10;

(* turn a constant into not a constant, from m3back's point of view *)
PROCEDURE NotConstL(a: LONGINT): LONGINT =
BEGIN
  RETURN a;
END NotConstL;

PROCEDURE NotConstI(a: INTEGER): INTEGER =
BEGIN
  RETURN a;
END NotConstI;

PROCEDURE NL() =
BEGIN
  PutT("\n");
  (*Flush();*)
END NL;

CONST PutT = RTIO.PutText;
CONST PutI = RTIO.PutInt;
CONST PutH = RTIO.PutHex;
CONST PutL = RTIO.PutLong;
CONST PutLH = RTIO.PutLongHex;

PROCEDURE NotPortableL(<*UNUSED*>a: LONGINT) =
BEGIN
   (* PutL(a); *)
   PutT("NotPortableL");
END NotPortableL;

PROCEDURE NotPortableH(<*UNUSED*>a: INTEGER) =
BEGIN
   (* PutH(a); *)
   PutT("NotPortableH");
END NotPortableH;

PROCEDURE TestInsert() =
VAR result32: CARDINAL := 0;
    result64: LONGINT := 0L;
BEGIN
  FOR a32 := 0 TO InsertExtractMaxAB DO
    FOR b32 := 0 TO InsertExtractMaxAB DO
      FOR m := 0 TO InsertExtractMaxMN DO
        FOR n := 0 TO InsertExtractMaxMN DO
          result32 := Word.Insert(a32, b32, m, n);
          PutT("insert32(a:"); PutH(a32);
          PutT(", b:"); PutH(b32);
          PutT(", m:"); PutH(m);
          PutT(", n:"); PutH(n);
          PutT("):"); PutH(result32);
          NL();
          <* ASSERT (n # 0) OR (result32 = a32) *>
        END
      END
    END
  END;

  FOR a64 := 0L TO VAL(InsertExtractMaxAB, LONGINT) DO
    FOR b64 := 0L TO VAL(InsertExtractMaxAB, LONGINT) DO
      FOR m := 0 TO InsertExtractMaxMN DO
        FOR n := 0 TO InsertExtractMaxMN DO
          result64 := Long.Insert(a64, b64, m, n);
          PutT("insert64(a:"); PutLH(a64);
          PutT(", b:"); PutLH(b64);
          PutT(", m:"); PutH(m);
          PutT(", n:"); PutH(n);
          PutT("):"); PutLH(result64);
          NL();
          <* ASSERT (n # 0) OR (result64 = a64) *>
        END
      END
    END
  END;
  Flush();
END TestInsert;

PROCEDURE TestExtract() =
CONST sign_extend = 0;
VAR result32: CARDINAL := 0;
    result64: LONGINT := 0L;
BEGIN
  FOR a32 := 0 TO InsertExtractMaxAB DO
     FOR m := 0 TO InsertExtractMaxMN DO
      FOR n := 0 TO InsertExtractMaxMN DO
        result32 := Word.Extract(a32, m, n);
        PutT("extract32(value:"); PutH(a32);
        PutT(", m:"); PutH(m);
        PutT(", n:"); PutH(n);
        PutT(", sign_extend:"); PutH(sign_extend);
        PutT("):"); PutH(result32);
        NL();
        <* ASSERT (n # 0) OR (result32 = 0) *>
      END
    END
  END;

  FOR a64 := 0L TO VAL(InsertExtractMaxAB, LONGINT) DO
    FOR m := 0 TO InsertExtractMaxMN DO
      FOR n := 0 TO InsertExtractMaxMN DO
        result64 := Long.Extract(a64, m, n);
        PutT("extract64(value:"); PutLH(a64);
        PutT(", m:"); PutH(m);
        PutT(", n:"); PutH(n);
        PutT(", sign_extend:"); PutH(sign_extend);
        PutT("):"); PutLH(result64);
        NL();
        <* ASSERT (n # 0) OR (result64 = 0L) *>
      END
    END
  END;
  Flush();
END TestExtract;

VAR a := 1234L;
VAR b := 5678L;
VAR c := 1024L * 1024L * 1024L * 8L;
VAR d: INTEGER;
CONST ExectTrue = ARRAY BOOLEAN OF TEXT{"bad\n","good\n"};
CONST ExpectFalse = ARRAY BOOLEAN OF TEXT{"good\n","bad\n"};
CONST FalseTrue = ARRAY BOOLEAN OF TEXT{"false\n","true\n"};

PROCEDURE TestAdd(a, b: LONGINT): LONGINT =
BEGIN
  <* ASSERT (1 + 0) = 1 *>
  <* ASSERT (FIRST(INTEGER) + NotConstI(0 )) = FIRST(INTEGER) *>
  <* ASSERT (FIRST(LONGINT) + NotConstL(0L)) = FIRST(LONGINT) *>
  <* ASSERT (LAST(INTEGER) + NotConstI(0 )) = LAST(INTEGER) *>
  <* ASSERT (LAST(LONGINT) + NotConstL(0L)) = LAST(LONGINT) *>
  RETURN a + b;
END TestAdd;

PROCEDURE TestSub(a, b: LONGINT): LONGINT =
BEGIN
  <* ASSERT (1 - 0) = 1 *>
  <* ASSERT (FIRST(INTEGER) - NotConstI(0 )) = FIRST(INTEGER) *>
  <* ASSERT (FIRST(LONGINT) - NotConstL(0L)) = FIRST(LONGINT) *>
  <* ASSERT (LAST(INTEGER) - NotConstI(0 )) = LAST(INTEGER) *>
  <* ASSERT (LAST(LONGINT) - NotConstL(0L)) = LAST(LONGINT) *>
  RETURN a - b;
END TestSub;

PROCEDURE TestMult(a, b: LONGINT): LONGINT =
BEGIN
  <* ASSERT (1 * 0) = 0 *>
  <* ASSERT (FIRST(INTEGER) * NotConstI(0 )) = 0 *>
  <* ASSERT (FIRST(LONGINT) * NotConstL(0L)) = 0L *>
  <* ASSERT (FIRST(INTEGER) * NotConstI(1 )) = FIRST(INTEGER) *>
  <* ASSERT (FIRST(LONGINT) * NotConstL(1L)) = FIRST(LONGINT) *>
  <* ASSERT (LAST(INTEGER) * NotConstI(0 )) = 0 *>
  <* ASSERT (LAST(LONGINT) * NotConstL(0L)) = 0L *>
  <* ASSERT (LAST(INTEGER) * NotConstI(1 )) = LAST(INTEGER) *>
  <* ASSERT (LAST(LONGINT) * NotConstL(1L)) = LAST(LONGINT) *>
  RETURN a * b;
END TestMult;


PROCEDURE TestDivI(a, b: INTEGER): INTEGER =
  VAR k: INTEGER;
BEGIN
  <* ASSERT (0 DIV 1) = 0 *>
  <* ASSERT (1 DIV 1) = 1 *>
  <* ASSERT (FIRST(INTEGER) DIV NotConstI(1 )) = FIRST(INTEGER) *>
  <* ASSERT (LAST(INTEGER) DIV NotConstI(1 )) = LAST(INTEGER) *>

  FOR i := -1000 TO 1000 DO
    FOR j := -1000 TO 1000 DO
      IF j # 0 THEN
        k := (i DIV j);
        IF ABS(i) <= 10 AND ABS(j) <= 10 THEN
          PutI(i);
          PutT(" DIVI ");
          PutI(j);
          PutT(":");
          PutI(k);
          NL();
        END;
        IF (i < 0) = (j < 0) THEN
          <* ASSERT k >= 0 *>
        ELSE
          <* ASSERT k <= 0 *>
        END;
      END;
    END;
  END;

  RETURN a DIV b;
END TestDivI;

PROCEDURE TestDivL(a, b: LONGINT): LONGINT =
  VAR k: LONGINT;
BEGIN
  <* ASSERT (0L DIV 1L) = 0L *>
  <* ASSERT (1L DIV 1L) = 1L *>
  <* ASSERT (FIRST(LONGINT) DIV NotConstL(1L)) = FIRST(LONGINT) *>
  <* ASSERT (LAST(LONGINT) DIV NotConstL(1L)) = LAST(LONGINT) *>

  FOR i := -1000L TO 1000L DO
    FOR j := -1000L TO 1000L DO
      IF j # 0L THEN
        k := (i DIV j);
        IF ABS(i) <= 10L AND ABS(j) <= 10L THEN
          PutL(i);
          PutT(" DIVL ");
          PutL(j);
          PutT(":");
          PutL(k);
          NL();
        END;
        IF (i < 0L) = (j < 0L) THEN
          <* ASSERT k >= 0L *>
        ELSE
          <* ASSERT k <= 0L *>
        END;
      END;
    END;
  END;

  RETURN a DIV b;
END TestDivL;

PROCEDURE TestModI(a, b: INTEGER): INTEGER =
  VAR k: INTEGER;
BEGIN

  <* ASSERT (0 MOD 1) = 0 *>
  <* ASSERT (1 MOD 1) = 0 *>
  <* ASSERT (FIRST(INTEGER) MOD NotConstI(1 )) = 0 *>
  <* ASSERT (LAST(INTEGER) MOD NotConstI(1 )) = 0 *>

  FOR i := -1000 TO 1000 DO
    FOR j := -1000 TO 1000 DO
      IF j # 0 THEN
        k := (i MOD j);
        IF ABS(i) <= 10 AND ABS(j) <= 10 THEN
          PutI(i);
          PutT(" MODI ");
          PutI(j);
          PutT(":");
          PutI(k);
          NL();
        END;
        <* ASSERT k = i - j * (i DIV j) *>
        IF j < 0 THEN
          <* ASSERT k > j AND k <= 0 *>
        ELSE
          <* ASSERT k < j AND k >= 0 *>
        END;
      END;
    END;
  END;

  RETURN a MOD b;
END TestModI;

PROCEDURE TestModL(a, b: LONGINT): LONGINT =
  VAR k: LONGINT;
BEGIN

  <* ASSERT (0 MOD 1) = 0 *>
  <* ASSERT (1 MOD 1) = 0 *>
  <* ASSERT (0L MOD 1L) = 0L *>
  <* ASSERT (1L MOD 1L) = 0L *>
  <* ASSERT (FIRST(INTEGER) MOD NotConstI(1 )) = 0 *>
  <* ASSERT (FIRST(LONGINT) MOD NotConstL(1L)) = 0L *>
  <* ASSERT (LAST(INTEGER) MOD NotConstI(1 )) = 0 *>
  <* ASSERT (LAST(LONGINT) MOD NotConstL(1L)) = 0L *>

  FOR i := -1000L TO 1000L DO
    FOR j := -1000L TO 1000L DO
      IF j # 0L THEN
        k := (i MOD j);
        IF ABS(i) <= 10L AND ABS(j) <= 10L THEN
          PutL(i);
          PutT(" MODL ");
          PutL(j);
          PutT(":");
          PutL(k);
          NL();
        END;
        <* ASSERT (i # j) OR ((i MOD j) = 0L) *>
        <* ASSERT k = i - j * (i DIV j) *>
        IF j < 0L THEN
          <* ASSERT k > j AND k <= 0L *>
        ELSE
          <* ASSERT k < j AND k >= 0L *>
        END;
      END;
    END;
  END;

  RETURN a MOD b;
END TestModL;

PROCEDURE TestDivUL(a, b: LONGCARD): LONGCARD =
  VAR k: LONGINT;
BEGIN

  <* ASSERT (0 DIV 1) = 0 *>
  <* ASSERT (1 DIV 1) = 1 *>
  <* ASSERT (0L DIV 1L) = 0L *>
  <* ASSERT (1L DIV 1L) = 1L *>
  <* ASSERT (LAST(INTEGER) DIV NotConstI(1 )) = LAST(INTEGER) *>
  <* ASSERT (LAST(LONGINT) DIV NotConstL(1L)) = LAST(LONGINT) *>

  FOR i := 0L TO 10L DO
    FOR j := 1L TO 10L DO
      k := (i DIV j);
      IF i <= 10L AND j <= 10L THEN
        PutL(i);
        PutT(" DIVUL ");
        PutL(j);
        PutT(":");
        PutL(k);
        NL();
      END;
      <* ASSERT k >= 0L *>
    END;
  END;

  RETURN a DIV b;
END TestDivUL;

PROCEDURE TestModUL(a, b: LONGCARD): LONGCARD =
  VAR k: LONGINT;
BEGIN

  <* ASSERT (0 MOD 1) = 0 *>
  <* ASSERT (1 MOD 1) = 0 *>
  <* ASSERT (0L MOD 1L) = 0L *>
  <* ASSERT (1L MOD 1L) = 0L *>
  <* ASSERT (LAST(INTEGER) MOD NotConstI(1 )) = 0 *>
  <* ASSERT (LAST(LONGINT) MOD NotConstL(1L)) = 0L *>

  FOR i := 0L TO 1000L DO
    FOR j := 1L TO 1000L DO
      k := (i DIV j);
      IF i <= 10L AND j <= 10L THEN
        PutL(i);
        PutT(" MODUL ");
        PutL(j);
        PutT(":");
        PutL(k);
        NL();
      END;
      <* ASSERT (i # j) OR ((i MOD j) = 0L) *>
      <* ASSERT k = i - j * (i DIV j) *>
      <* ASSERT k < j AND k >= 0L *>
    END;
  END;

  RETURN a MOD b;
END TestModUL;

PROCEDURE TestLT(a, b: LONGINT): BOOLEAN =
BEGIN
  FOR i := -100L TO 100L DO
    FOR j := (i + 1L) TO 100L DO
      <* ASSERT (i < j) *>
    END;
  END;

  RETURN a < b;
END TestLT;

PROCEDURE TestLTU(a, b: LONGINT): BOOLEAN =
BEGIN
  FOR i := 0L TO 10L DO
    FOR j := (i + 1L) TO 10L DO
      <* ASSERT Long.LT(i, j) *>
    END;
  END;
 
  <* ASSERT Long.LT(0L, -1L) *>
  <* ASSERT Long.LT(LAST(LONGINT), FIRST(LONGINT)) *>

  FOR i := -100L TO 100L DO
    FOR j := -100L TO 100L DO
      IF (i < 0L) # (j < 0L) THEN
        <* ASSERT (i < j) # Long.LT(i, j) *>
        <* ASSERT Word.LT(VAL(i, INTEGER), VAL(j, INTEGER)) = Long.LT(i, j) *>
      END
    END
  END;

  RETURN Long.LT(a, b);
END TestLTU;

PROCEDURE TestEQ(a, b: LONGINT): BOOLEAN =
BEGIN
  FOR i := -10L TO 10L DO
    FOR j := (i + 1L) TO 10L DO
      <* ASSERT NOT (i = j) *>
    END;
  END;
  RETURN a = b;
END TestEQ;

PROCEDURE TestNE(a, b: LONGINT): BOOLEAN =
BEGIN
  FOR i := -10L TO 10L DO
    FOR j := (i + 1L) TO 10L DO
      <* ASSERT (i # j) *>
    END;
  END;
  RETURN a # b;
END TestNE;

(* shifting without constants *)

PROCEDURE TestLeftShiftInteger() =
VAR a := NotConstI(1);
BEGIN
  PutT("\nTestLeftShiftInteger\n");
  FOR i := 0 TO 40 DO
    IF i < 32 THEN
      PutT("1 << "); PutI(i); PutT(":"); PutH(a); PutT("\n");
    ELSE
      PutT("1 << "); PutI(i); PutT(":"); NotPortableH(a); PutT("\n");
    END;
    a := Word.LeftShift(a, NotConstI(1));
  END;
END TestLeftShiftInteger;

PROCEDURE TestLeftShiftLongint() =
VAR a := NotConstL(1L);
BEGIN
  PutT("\nTestLeftShiftLongint\n");
  FOR i := 0 TO 40 DO
    PutT("1 << "); PutI(i); PutT(":"); PutLH(a); PutT("\n");
    a := Long.LeftShift(a, NotConstI(1));
  END;
END TestLeftShiftLongint;

PROCEDURE TestShiftRightInteger() =
VAR a := NotConstI(16_80000000);
BEGIN
  PutT("\nTestShiftRightInteger\n");
  FOR i := 0 TO 40 DO
    PutT("16_80000000 >> "); PutI(i); PutT(":"); PutH(a); PutT("\n");
    a := Word.RightShift(a, NotConstI(1));
  END;
END TestShiftRightInteger;

PROCEDURE TestShiftRightLongint() =
VAR a := NotConstL(FIRST(LONGINT));
BEGIN
  PutT("\nTestShiftRightLongint\n");
  FOR i := 0 TO 40 DO
    PutT("FIRST(LONGINT) >> "); PutI(i); PutT(":"); PutLH(a); PutT("\n");
    a := Long.RightShift(a, NotConstI(1));
  END;
END TestShiftRightLongint;

PROCEDURE TestShiftInteger() =
VAR a := NotConstI(16_8000);
BEGIN
  PutT("\nTestShiftInteger\n");
  FOR i := -30 TO 30 DO
    IF i <= 16 THEN
      PutT("16_8000 << "); PutI(i); PutT(":"); PutH(Word.Shift(a, NotConstI(i))); PutT("\n");
    ELSE
      PutT("16_8000 << "); PutI(i); PutT(":"); NotPortableH(Word.Shift(a, NotConstI(i))); PutT("\n");
    END;
  END;
END TestShiftInteger;

PROCEDURE TestShiftLongint() =
VAR a := NotConstL(16_8000L);
BEGIN
  PutT("\nTestShiftLongint\n");
  FOR i := -30 TO 30 DO
    PutT("16_8000L << "); PutI(i); PutT(":"); PutLH(Long.Shift(a, NotConstI(i))); PutT("\n");
  END;
END TestShiftLongint;

(* shifting by a constant *)

PROCEDURE TestLeftShiftNInteger() =
CONST b = ARRAY OF INTEGER { 1, 2, 3, 16_8000, 16_12345, 16_ABCD1234, 16_ABCD0000 };
VAR a: INTEGER;
BEGIN
  PutT("\nTestLeftShiftNInteger\n");
  FOR i := FIRST(b) TO LAST (b) DO
    a := NotConstI(b[i]);
    PutH(a); PutT(" << 0"); PutT(":"); PutH(Word.LeftShift(a, 0)); PutT("\n");
    <* ASSERT a #           1 OR Word.LeftShift(          1, 0) = Word.LeftShift(a, 0) *>
    <* ASSERT a #           2 OR Word.LeftShift(          2, 0) = Word.LeftShift(a, 0) *>
    <* ASSERT a #           3 OR Word.LeftShift(          3, 0) = Word.LeftShift(a, 0) *>
    <* ASSERT a #     16_8000 OR Word.LeftShift(    16_8000, 0) = Word.LeftShift(a, 0) *>
    <* ASSERT a #    16_12345 OR Word.LeftShift(   16_12345, 0) = Word.LeftShift(a, 0) *>
    <* ASSERT a # 16_ABCD1234 OR Word.LeftShift(16_ABCD1234, 0) = Word.LeftShift(a, 0) *>
    <* ASSERT a # 16_ABCD0000 OR Word.LeftShift(16_ABCD0000, 0) = Word.LeftShift(a, 0) *>

    IF a > 0 AND a < 10 THEN
      PutH(a); PutT(" << 1"); PutT(":"); PutH(Word.LeftShift(a, 1)); PutT("\n");
    ELSE
      PutH(a); PutT(" << 1"); PutT(":"); NotPortableH(Word.LeftShift(a, 1)); PutT("\n");
    END;
    <* ASSERT a #           1 OR Word.LeftShift(          1, 1) = Word.LeftShift(a, 1) *>
    <* ASSERT a #           2 OR Word.LeftShift(          2, 1) = Word.LeftShift(a, 1) *>
    <* ASSERT a #           3 OR Word.LeftShift(          3, 1) = Word.LeftShift(a, 1) *>
    <* ASSERT a #     16_8000 OR Word.LeftShift(    16_8000, 1) = Word.LeftShift(a, 1) *>
    <* ASSERT a #    16_12345 OR Word.LeftShift(   16_12345, 1) = Word.LeftShift(a, 1) *>
    <* ASSERT a # 16_ABCD1234 OR Word.LeftShift(16_ABCD1234, 1) = Word.LeftShift(a, 1) *>
    <* ASSERT a # 16_ABCD0000 OR Word.LeftShift(16_ABCD0000, 1) = Word.LeftShift(a, 1) *>

    IF a > 0 AND a < 10 THEN
      PutH(a); PutT(" << 2"); PutT(":"); PutH(Word.LeftShift(a, 2)); PutT("\n");
    ELSE
      PutH(a); PutT(" << 2"); PutT(":"); NotPortableH(Word.LeftShift(a, 2)); PutT("\n");
    END;
    <* ASSERT a #           1 OR Word.LeftShift(          1, 2) = Word.LeftShift(a, 2) *>
    <* ASSERT a #           2 OR Word.LeftShift(          2, 2) = Word.LeftShift(a, 2) *>
    <* ASSERT a #           3 OR Word.LeftShift(          3, 2) = Word.LeftShift(a, 2) *>
    <* ASSERT a #     16_8000 OR Word.LeftShift(    16_8000, 2) = Word.LeftShift(a, 2) *>
    <* ASSERT a #    16_12345 OR Word.LeftShift(   16_12345, 2) = Word.LeftShift(a, 2) *>
    <* ASSERT a # 16_ABCD1234 OR Word.LeftShift(16_ABCD1234, 2) = Word.LeftShift(a, 2) *>
    <* ASSERT a # 16_ABCD0000 OR Word.LeftShift(16_ABCD0000, 2) = Word.LeftShift(a, 2) *>

    IF a > 0 AND a < 10 THEN
      PutH(a); PutT(" << 3"); PutT(":"); PutH(Word.LeftShift(a, 3)); PutT("\n");
    ELSE
      PutH(a); PutT(" << 3"); PutT(":"); NotPortableH(Word.LeftShift(a, 3)); PutT("\n");
    END;
    <* ASSERT a #           1 OR Word.LeftShift(          1, 3) = Word.LeftShift(a, 3) *>
    <* ASSERT a #           2 OR Word.LeftShift(          2, 3) = Word.LeftShift(a, 3) *>
    <* ASSERT a #           3 OR Word.LeftShift(          3, 3) = Word.LeftShift(a, 3) *>
    <* ASSERT a #     16_8000 OR Word.LeftShift(    16_8000, 3) = Word.LeftShift(a, 3) *>
    <* ASSERT a #    16_12345 OR Word.LeftShift(   16_12345, 3) = Word.LeftShift(a, 3) *>
    <* ASSERT a # 16_ABCD1234 OR Word.LeftShift(16_ABCD1234, 3) = Word.LeftShift(a, 3) *>
    <* ASSERT a # 16_ABCD0000 OR Word.LeftShift(16_ABCD0000, 3) = Word.LeftShift(a, 3) *>

    IF a > 0 AND a < 2 THEN
      PutH(a); PutT(" << 30"); PutT(":"); PutH(Word.LeftShift(a, 30)); PutT("\n");
    ELSE
      PutH(a); PutT(" << 30"); PutT(":"); NotPortableH(Word.LeftShift(a, 30)); PutT("\n");
    END;
    <* ASSERT a #           1 OR Word.LeftShift(          1, 30) = Word.LeftShift(a, 30) *>
    <* ASSERT a #           2 OR Word.LeftShift(          2, 30) = Word.LeftShift(a, 30) *>
    <* ASSERT a #           3 OR Word.LeftShift(          3, 30) = Word.LeftShift(a, 30) *>
    <* ASSERT a #     16_8000 OR Word.LeftShift(    16_8000, 30) = Word.LeftShift(a, 30) *>
    <* ASSERT a #    16_12345 OR Word.LeftShift(   16_12345, 30) = Word.LeftShift(a, 30) *>
    <* ASSERT a # 16_ABCD1234 OR Word.LeftShift(16_ABCD1234, 30) = Word.LeftShift(a, 30) *>
    <* ASSERT a # 16_ABCD0000 OR Word.LeftShift(16_ABCD0000, 30) = Word.LeftShift(a, 30) *>
  END;
END TestLeftShiftNInteger;

PROCEDURE TestLeftShiftNLongint() =
VAR b := ARRAY [0..7] OF LONGINT{1L, 2L, 3L, 16_8000L, 16_12345L, 16_ABCD1234L, 16_ABCD0000L, 16_ABCD00001234L};
VAR a: LONGINT;
BEGIN
  PutT("\nTestLeftShiftNLongint\n");

  FOR i := FIRST(b) TO LAST (b) DO
    a := NotConstL(b[i]);

    PutLH(a); PutT(" << 0"); PutT(":"); PutLH(Long.LeftShift(a, 0)); PutT("\n");
    <* ASSERT a #               1L OR Long.LeftShift(              1L, 0) = Long.LeftShift(a, 0) *>
    <* ASSERT a #               2L OR Long.LeftShift(              2L, 0) = Long.LeftShift(a, 0) *>
    <* ASSERT a #               3L OR Long.LeftShift(              3L, 0) = Long.LeftShift(a, 0) *>
    <* ASSERT a #         16_8000L OR Long.LeftShift(        16_8000L, 0) = Long.LeftShift(a, 0) *>
    <* ASSERT a #        16_12345L OR Long.LeftShift(       16_12345L, 0) = Long.LeftShift(a, 0) *>
    <* ASSERT a #     16_ABCD1234L OR Long.LeftShift(    16_ABCD1234L, 0) = Long.LeftShift(a, 0) *>
    <* ASSERT a #     16_ABCD0000L OR Long.LeftShift(    16_ABCD0000L, 0) = Long.LeftShift(a, 0) *>
    <* ASSERT a # 16_ABCD00001234L OR Long.LeftShift(16_ABCD00001234L, 0) = Long.LeftShift(a, 0) *>

    PutLH(a); PutT(" << 1"); PutT(":"); PutLH(Long.LeftShift(a, 1)); PutT(" \n");
    <* ASSERT a #               1L OR Long.LeftShift(              1L, 1) = Long.LeftShift(a, 1) *>
    <* ASSERT a #               2L OR Long.LeftShift(              2L, 1) = Long.LeftShift(a, 1) *>
    <* ASSERT a #               3L OR Long.LeftShift(              3L, 1) = Long.LeftShift(a, 1) *>
    <* ASSERT a #         16_8000L OR Long.LeftShift(        16_8000L, 1) = Long.LeftShift(a, 1) *>
    <* ASSERT a #        16_12345L OR Long.LeftShift(       16_12345L, 1) = Long.LeftShift(a, 1) *>
    <* ASSERT a #     16_ABCD1234L OR Long.LeftShift(    16_ABCD1234L, 1) = Long.LeftShift(a, 1) *>
    <* ASSERT a #     16_ABCD0000L OR Long.LeftShift(    16_ABCD0000L, 1) = Long.LeftShift(a, 1) *>
    <* ASSERT a # 16_ABCD00001234L OR Long.LeftShift(16_ABCD00001234L, 1) = Long.LeftShift(a, 1) *>

    PutLH(a); PutT(" << 2"); PutT(":"); PutLH(Long.LeftShift(a, 2)); PutT("\n");
    <* ASSERT a #               1L OR Long.LeftShift(              1L, 2) = Long.LeftShift(a, 2) *>
    <* ASSERT a #               2L OR Long.LeftShift(              2L, 2) = Long.LeftShift(a, 2) *>
    <* ASSERT a #               3L OR Long.LeftShift(              3L, 2) = Long.LeftShift(a, 2) *>
    <* ASSERT a #         16_8000L OR Long.LeftShift(        16_8000L, 2) = Long.LeftShift(a, 2) *>
    <* ASSERT a #        16_12345L OR Long.LeftShift(       16_12345L, 2) = Long.LeftShift(a, 2) *>
    <* ASSERT a #     16_ABCD1234L OR Long.LeftShift(    16_ABCD1234L, 2) = Long.LeftShift(a, 2) *>
    <* ASSERT a #     16_ABCD0000L OR Long.LeftShift(    16_ABCD0000L, 2) = Long.LeftShift(a, 2) *>
    <* ASSERT a # 16_ABCD00001234L OR Long.LeftShift(16_ABCD00001234L, 2) = Long.LeftShift(a, 2) *>

    PutLH(a); PutT(" << 3"); PutT(":"); PutLH(Long.LeftShift(a, 3)); PutT("\n");
    <* ASSERT a #               1L OR Long.LeftShift(              1L, 3) = Long.LeftShift(a, 3) *>
    <* ASSERT a #               2L OR Long.LeftShift(              2L, 3) = Long.LeftShift(a, 3) *>
    <* ASSERT a #               3L OR Long.LeftShift(              3L, 3) = Long.LeftShift(a, 3) *>
    <* ASSERT a #         16_8000L OR Long.LeftShift(        16_8000L, 3) = Long.LeftShift(a, 3) *>
    <* ASSERT a #        16_12345L OR Long.LeftShift(       16_12345L, 3) = Long.LeftShift(a, 3) *>
    <* ASSERT a #     16_ABCD1234L OR Long.LeftShift(    16_ABCD1234L, 3) = Long.LeftShift(a, 3) *>
    <* ASSERT a #     16_ABCD0000L OR Long.LeftShift(    16_ABCD0000L, 3) = Long.LeftShift(a, 3) *>
    <* ASSERT a # 16_ABCD00001234L OR Long.LeftShift(16_ABCD00001234L, 3) = Long.LeftShift(a, 3) *>

    PutLH(a); PutT(" << 30"); PutT(":"); PutLH(Long.LeftShift(a, 30)); PutT("\n");
    <* ASSERT a #               1L OR Long.LeftShift(              1L, 30) = Long.LeftShift(a, 30) *>
    <* ASSERT a #               2L OR Long.LeftShift(              2L, 30) = Long.LeftShift(a, 30) *>
    <* ASSERT a #               3L OR Long.LeftShift(              3L, 30) = Long.LeftShift(a, 30) *>
    <* ASSERT a #         16_8000L OR Long.LeftShift(        16_8000L, 30) = Long.LeftShift(a, 30) *>
    <* ASSERT a #        16_12345L OR Long.LeftShift(       16_12345L, 30) = Long.LeftShift(a, 30) *>
    <* ASSERT a #     16_ABCD1234L OR Long.LeftShift(    16_ABCD1234L, 30) = Long.LeftShift(a, 30) *>
    <* ASSERT a #     16_ABCD0000L OR Long.LeftShift(    16_ABCD0000L, 30) = Long.LeftShift(a, 30) *>
    <* ASSERT a # 16_ABCD00001234L OR Long.LeftShift(16_ABCD00001234L, 30) = Long.LeftShift(a, 30) *>

    PutLH(a); PutT(" << 40"); PutT(":"); PutLH(Long.LeftShift(a, 40)); PutT("\n");
    <* ASSERT a #               1L OR Long.LeftShift(              1L, 40) = Long.LeftShift(a, 40) *>
    <* ASSERT a #               2L OR Long.LeftShift(              2L, 40) = Long.LeftShift(a, 40) *>
    <* ASSERT a #               3L OR Long.LeftShift(              3L, 40) = Long.LeftShift(a, 40) *>
    <* ASSERT a #         16_8000L OR Long.LeftShift(        16_8000L, 40) = Long.LeftShift(a, 40) *>
    <* ASSERT a #        16_12345L OR Long.LeftShift(       16_12345L, 40) = Long.LeftShift(a, 40) *>
    <* ASSERT a #     16_ABCD1234L OR Long.LeftShift(    16_ABCD1234L, 40) = Long.LeftShift(a, 40) *>
    <* ASSERT a #     16_ABCD0000L OR Long.LeftShift(    16_ABCD0000L, 40) = Long.LeftShift(a, 40) *>
    <* ASSERT a # 16_ABCD00001234L OR Long.LeftShift(16_ABCD00001234L, 40) = Long.LeftShift(a, 40) *>

    PutLH(a); PutT(" << 50"); PutT(":"); PutLH(Long.LeftShift(a, 50)); PutT("\n");
    <* ASSERT a #               1L OR Long.LeftShift(              1L, 50) = Long.LeftShift(a, 50) *>
    <* ASSERT a #               2L OR Long.LeftShift(              2L, 50) = Long.LeftShift(a, 50) *>
    <* ASSERT a #               3L OR Long.LeftShift(              3L, 50) = Long.LeftShift(a, 50) *>
    <* ASSERT a #         16_8000L OR Long.LeftShift(        16_8000L, 50) = Long.LeftShift(a, 50) *>
    <* ASSERT a #        16_12345L OR Long.LeftShift(       16_12345L, 50) = Long.LeftShift(a, 50) *>
    <* ASSERT a #     16_ABCD1234L OR Long.LeftShift(    16_ABCD1234L, 50) = Long.LeftShift(a, 50) *>
    <* ASSERT a #     16_ABCD0000L OR Long.LeftShift(    16_ABCD0000L, 50) = Long.LeftShift(a, 50) *>
    <* ASSERT a # 16_ABCD00001234L OR Long.LeftShift(16_ABCD00001234L, 50) = Long.LeftShift(a, 50) *>

    PutLH(a); PutT(" << 60"); PutT(":"); PutLH(Long.LeftShift(a, 60)); PutT("\n");
    <* ASSERT a #               1L OR Long.LeftShift(              1L, 60) = Long.LeftShift(a, 60) *>
    <* ASSERT a #               2L OR Long.LeftShift(              2L, 60) = Long.LeftShift(a, 60) *>
    <* ASSERT a #               3L OR Long.LeftShift(              3L, 60) = Long.LeftShift(a, 60) *>
    <* ASSERT a #         16_8000L OR Long.LeftShift(        16_8000L, 60) = Long.LeftShift(a, 60) *>
    <* ASSERT a #        16_12345L OR Long.LeftShift(       16_12345L, 60) = Long.LeftShift(a, 60) *>
    <* ASSERT a #     16_ABCD1234L OR Long.LeftShift(    16_ABCD1234L, 60) = Long.LeftShift(a, 60) *>
    <* ASSERT a #     16_ABCD0000L OR Long.LeftShift(    16_ABCD0000L, 60) = Long.LeftShift(a, 60) *>
    <* ASSERT a # 16_ABCD00001234L OR Long.LeftShift(16_ABCD00001234L, 60) = Long.LeftShift(a, 60) *>
  END;
END TestLeftShiftNLongint;

PROCEDURE TestShiftRightNInteger() =
VAR a := NotConstI(16_80000000);
BEGIN
  PutT("\nTestShiftRightNInteger\n");
  PutT("16_80000000 >> 0"); PutT(":"); PutH(Word.RightShift(a, 0)); PutT("\n");
  <* ASSERT Word.RightShift(16_80000000, 0) = Word.RightShift(a, 0) *>

  PutT("16_80000000 >> 1"); PutT(":"); PutH(Word.RightShift(a, 1)); PutT("\n");
  <* ASSERT Word.RightShift(16_80000000, 1) = Word.RightShift(a, 1) *>

  PutT("16_80000000 >> 2"); PutT(":"); PutH(Word.RightShift(a, 2)); PutT("\n");
  <* ASSERT Word.RightShift(16_80000000, 2) = Word.RightShift(a, 2) *>

  PutT("16_80000000 >> 3"); PutT(":"); PutH(Word.RightShift(a, 3)); PutT("\n");
  <* ASSERT Word.RightShift(16_80000000, 3) = Word.RightShift(a, 3) *>

  PutT("16_80000000 >> 30"); PutT(":"); PutH(Word.RightShift(a, 30)); PutT("\n");
  <* ASSERT Word.RightShift(16_80000000, 30) = Word.RightShift(a, 30) *>

END TestShiftRightNInteger;

PROCEDURE TestShiftRightNLongint() =
VAR a := NotConstL(FIRST(LONGINT));
BEGIN
  PutT("\nTestShiftRightNLongint\n");
  PutT("FIRST(LONGINT) >> 0"); PutT(":"); PutLH(Long.RightShift(a, 0)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 0) = Long.RightShift(a, 0) *>

  PutT("FIRST(LONGINT) >> 1"); PutT(":"); PutLH(Long.RightShift(a, 1)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 1) = Long.RightShift(a, 1) *>

  PutT("FIRST(LONGINT) >> 2"); PutT(":"); PutLH(Long.RightShift(a, 2)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 2) = Long.RightShift(a, 2) *>

  PutT("FIRST(LONGINT) >> 3"); PutT(":"); PutLH(Long.RightShift(a, 3)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 3) = Long.RightShift(a, 3) *>

  PutT("FIRST(LONGINT) >> 30"); PutT(":"); PutLH(Long.RightShift(a, 30)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 30) = Long.RightShift(a, 30) *>

  PutT("FIRST(LONGINT) >> 31"); PutT(":"); PutLH(Long.RightShift(a, 31)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 31) = Long.RightShift(a, 31) *>

  PutT("FIRST(LONGINT) >> 32"); PutT(":"); PutLH(Long.RightShift(a, 32)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 32) = Long.RightShift(a, 32) *>

  PutT("FIRST(LONGINT) >> 33"); PutT(":"); PutLH(Long.RightShift(a, 33)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 33) = Long.RightShift(a, 33) *>

  PutT("FIRST(LONGINT) >> 40"); PutT(":"); PutLH(Long.RightShift(a, 40)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 40) = Long.RightShift(a, 40) *>

  PutT("FIRST(LONGINT) >> 50"); PutT(":"); PutLH(Long.RightShift(a, 50)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 50) = Long.RightShift(a, 50) *>

  PutT("FIRST(LONGINT) >> 60"); PutT(":"); PutLH(Long.RightShift(a, 60)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 60) = Long.RightShift(a, 60) *>

  PutT("FIRST(LONGINT) >> 61"); PutT(":"); PutLH(Long.RightShift(a, 61)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 61) = Long.RightShift(a, 61) *>

  PutT("FIRST(LONGINT) >> 62"); PutT(":"); PutLH(Long.RightShift(a, 62)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 62) = Long.RightShift(a, 62) *>

  PutT("FIRST(LONGINT) >> 63"); PutT(":"); PutLH(Long.RightShift(a, 63)); PutT("\n");
  <* ASSERT Long.RightShift(FIRST(LONGINT), 63) = Long.RightShift(a, 63) *>

END TestShiftRightNLongint;

PROCEDURE TestShiftNInteger() =
VAR a := NotConstI(16_8000);
BEGIN
  PutT("\nTestShiftNInteger\n");
  PutT("16_8000 <<  -1:"); PutH(Word.Shift(a, -1)); PutT("\n");
  <* ASSERT Word.Shift(16_8000, -1) = Word.Shift(a, -1) *>

  PutT("16_8000 <<   1:"); PutH(Word.Shift(a,  1)); PutT("\n");
  <* ASSERT Word.Shift(16_8000, 1) = Word.Shift(a, 1) *>

  PutT("16_8000 << -10:"); PutH(Word.Shift(a, -10)); PutT("\n");
  <* ASSERT Word.Shift(16_8000, -10) = Word.Shift(a, -10) *>

  PutT("16_8000 <<  10:"); PutH(Word.Shift(a,  10)); PutT("\n");
  <* ASSERT Word.Shift(16_8000, 10) = Word.Shift(a, 10) *>

  PutT("16_8000 << -20:"); NotPortableH(Word.Shift(a, -20)); PutT("\n");
  <* ASSERT Word.Shift(16_8000, -20) = Word.Shift(a, -20) *>

  PutT("16_8000 <<  20:"); NotPortableH(Word.Shift(a,  20)); PutT("\n");
  <* ASSERT Word.Shift(16_8000, 20) = Word.Shift(a, 20) *>

  PutT("16_8000 << -30:"); NotPortableH(Word.Shift(a, -30)); PutT("\n");
  <* ASSERT Word.Shift(16_8000, -30) = Word.Shift(a, -30) *>

  PutT("16_8000 <<  30:"); NotPortableH(Word.Shift(a,  30)); PutT("\n");
  <* ASSERT Word.Shift(16_8000, 30) = Word.Shift(a, 30) *>

END TestShiftNInteger;

PROCEDURE TestShiftNLongint() =
VAR a := NotConstL(16_8000L);
BEGIN
  PutT("\nTestShiftNLongint\n");
  PutT("16_8000L <<  -1:"); PutLH(Long.Shift(a, -1)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, -1) = Long.Shift(a, -1) *>

  PutT("16_8000L <<   1:"); PutLH(Long.Shift(a,  1)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, 1) = Long.Shift(a, 1) *>

  PutT("16_8000L << -10:"); PutLH(Long.Shift(a, -10)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, -10) = Long.Shift(a, -10) *>

  PutT("16_8000L <<  10:"); PutLH(Long.Shift(a,  10)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, 10) = Long.Shift(a, 10) *>

  PutT("16_8000L << -20:"); PutLH(Long.Shift(a, -20)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, -20) = Long.Shift(a, -20) *>

  PutT("16_8000L <<  20:"); PutLH(Long.Shift(a,  20)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, 20) = Long.Shift(a, 20) *>

  PutT("16_8000L << -30:"); PutLH(Long.Shift(a, -30)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, -30) = Long.Shift(a, -30) *>

  PutT("16_8000L <<  30:"); PutLH(Long.Shift(a,  30)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, 30) = Long.Shift(a, 30) *>

  PutT("16_8000L << -40:"); PutLH(Long.Shift(a, -40)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, -40) = Long.Shift(a, -40) *>

  PutT("16_8000L <<  40:"); PutLH(Long.Shift(a,  40)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, 40) = Long.Shift(a, 40) *>

  PutT("16_8000L << -50:"); PutLH(Long.Shift(a, -50)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, -50) = Long.Shift(a, -50) *>

  PutT("16_8000L <<  50:"); PutLH(Long.Shift(a,  50)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, 50) = Long.Shift(a, 50) *>

  PutT("16_8000L << -60:"); PutLH(Long.Shift(a, -60)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, -60) = Long.Shift(a, -60) *>

  PutT("16_8000L <<  60:"); PutLH(Long.Shift(a,  60)); PutT("\n");
  <* ASSERT Long.Shift(16_8000L, 60) = Long.Shift(a, 60) *>

END TestShiftNLongint;

(* shifting constant by a constant *)

PROCEDURE TestLeftShiftMNInteger() =
BEGIN
  PutT("\nTestLeftShiftMNInteger\n");
  PutT("1 <<  0"); PutT(":"); PutH(Word.LeftShift(1,  0)); PutT("\n");
  PutT("1 <<  1"); PutT(":"); PutH(Word.LeftShift(1,  1)); PutT("\n");
  PutT("1 <<  2"); PutT(":"); PutH(Word.LeftShift(1,  2)); PutT("\n");
  PutT("1 <<  3"); PutT(":"); PutH(Word.LeftShift(1,  3)); PutT("\n");
  PutT("1 << 30"); PutT(":"); PutH(Word.LeftShift(1, 30)); PutT("\n");
  PutT("1 << 31"); PutT(":"); PutH(Word.LeftShift(1, 31)); PutT("\n");
END TestLeftShiftMNInteger;

PROCEDURE TestLeftShiftMNLongint() =
BEGIN
  PutT("\nTestLeftShiftMNLongint\n");
  PutT("1L <<  0"); PutT(":"); PutLH(Long.LeftShift(1L, 0)); PutT("\n");
  PutT("1L <<  1"); PutT(":"); PutLH(Long.LeftShift(1L, 1)); PutT("\n");
  PutT("1L <<  2"); PutT(":"); PutLH(Long.LeftShift(1L, 2)); PutT("\n");
  PutT("1L <<  3"); PutT(":"); PutLH(Long.LeftShift(1L, 3)); PutT("\n");
  PutT("1L << 30"); PutT(":"); PutLH(Long.LeftShift(1L, 30)); PutT("\n");
  PutT("1L << 40"); PutT(":"); PutLH(Long.LeftShift(1L, 40)); PutT("\n");
  PutT("1L << 50"); PutT(":"); PutLH(Long.LeftShift(1L, 50)); PutT("\n");
  PutT("1L << 60"); PutT(":"); PutLH(Long.LeftShift(1L, 60)); PutT("\n");
END TestLeftShiftMNLongint;

PROCEDURE TestShiftRightMNInteger() =
BEGIN
  PutT("\nTestShiftRightMNInteger\n");
  PutT("16_80000000 >>  0"); PutT(":"); PutH(Word.RightShift(16_80000000,  0)); PutT("\n");
  PutT("16_80000000 >>  1"); PutT(":"); PutH(Word.RightShift(16_80000000,  1)); PutT("\n");
  PutT("16_80000000 >>  2"); PutT(":"); PutH(Word.RightShift(16_80000000,  2)); PutT("\n");
  PutT("16_80000000 >>  3"); PutT(":"); PutH(Word.RightShift(16_80000000,  3)); PutT("\n");
  PutT("16_80000000 >> 30"); PutT(":"); PutH(Word.RightShift(16_80000000, 30)); PutT("\n");
END TestShiftRightMNInteger;

PROCEDURE TestShiftRightMNLongint() =
BEGIN
  PutT("\nTestShiftRightMNLongint\n");
  PutT("FIRST(LONGINT) >>  0"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT),  0)); PutT("\n");
  PutT("FIRST(LONGINT) >>  1"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT),  1)); PutT("\n");
  PutT("FIRST(LONGINT) >>  2"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT),  2)); PutT("\n");
  PutT("FIRST(LONGINT) >>  3"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT),  3)); PutT("\n");
  PutT("FIRST(LONGINT) >> 30"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 30)); PutT("\n");
  PutT("FIRST(LONGINT) >> 31"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 31)); PutT("\n");
  PutT("FIRST(LONGINT) >> 32"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 32)); PutT("\n");
  PutT("FIRST(LONGINT) >> 33"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 33)); PutT("\n");
  PutT("FIRST(LONGINT) >> 40"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 40)); PutT("\n");
  PutT("FIRST(LONGINT) >> 50"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 50)); PutT("\n");
  PutT("FIRST(LONGINT) >> 60"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 60)); PutT("\n");
  PutT("FIRST(LONGINT) >> 61"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 61)); PutT("\n");
  PutT("FIRST(LONGINT) >> 62"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 62)); PutT("\n");
  PutT("FIRST(LONGINT) >> 63"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 63)); PutT("\n");
END TestShiftRightMNLongint;

PROCEDURE TestShiftMNInteger() =
BEGIN
  PutT("\nTestShiftMNInteger\n");
  PutT("16_8000 << -40:"); NotPortableH(Word.Shift(16_8000, -40)); PutT("\n");
  PutT("16_8000 << -30:"); NotPortableH(Word.Shift(16_8000, -30)); PutT("\n");
  PutT("16_8000 << -20:"); NotPortableH(Word.Shift(16_8000, -20)); PutT("\n");
  PutT("16_8000 <<  -1:"); PutH(Word.Shift(16_8000,  -1)); PutT("\n");
  PutT("16_8000 <<   1:"); PutH(Word.Shift(16_8000,   1)); PutT("\n");
  PutT("16_8000 <<  20:"); NotPortableH(Word.Shift(16_8000,  20)); PutT("\n");
  PutT("16_8000 <<  30:"); NotPortableH(Word.Shift(16_8000,  30)); PutT("\n");
  PutT("16_8000 <<  40:"); NotPortableH(Word.Shift(16_8000,  40)); PutT("\n");
END TestShiftMNInteger;

PROCEDURE TestShiftMNLongint() =
BEGIN
  PutT("\nTestShiftMNLongint\n");
  PutT("16_8000L << -40:"); PutLH(Long.Shift(16_8000L, -40)); PutT("\n");
  PutT("16_8000L << -30:"); PutLH(Long.Shift(16_8000L, -30)); PutT("\n");
  PutT("16_8000L << -20:"); PutLH(Long.Shift(16_8000L, -20)); PutT("\n");
  PutT("16_8000L <<  -1:"); PutLH(Long.Shift(16_8000L,  -1)); PutT("\n");
  PutT("16_8000L <<   1:"); PutLH(Long.Shift(16_8000L,   1)); PutT("\n");
  PutT("16_8000L <<  20:"); PutLH(Long.Shift(16_8000L,  20)); PutT("\n");
  PutT("16_8000L <<  30:"); PutLH(Long.Shift(16_8000L,  30)); PutT("\n");
  PutT("16_8000L <<  40:"); PutLH(Long.Shift(16_8000L,  40)); PutT("\n");
END TestShiftMNLongint;

(* shifting constant by a non-constant (not particularly special, except for shifting zero) *)

PROCEDURE TestLeftShiftMInteger() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30 };
BEGIN
  PutT("\nTestLeftShiftMLongint\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT(" 0 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.LeftShift(0, shift[i])); PutT("\n");
    PutT(" 1 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.LeftShift(1, shift[i])); PutT("\n");
    PutT(" 2 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.LeftShift(2, shift[i])); PutT("\n");
    PutT(" 3 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.LeftShift(3, shift[i])); PutT("\n");
  END;
END TestLeftShiftMInteger;

PROCEDURE TestLeftShiftMLongint() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30, 40, 50, 60 };
BEGIN
  PutT("\nTestLeftShiftMLongint\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT(" 0L << "); PutI(shift[i], 2); PutT(":"); PutLH(Long.LeftShift(0L, shift[i])); PutT("\n");
    PutT(" 1L << "); PutI(shift[i], 2); PutT(":"); PutLH(Long.LeftShift(1L, shift[i])); PutT("\n");
    PutT(" 2L << "); PutI(shift[i], 2); PutT(":"); PutLH(Long.LeftShift(2L, shift[i])); PutT("\n");
    PutT(" 3L << "); PutI(shift[i], 2); PutT(":"); PutLH(Long.LeftShift(3L, shift[i])); PutT("\n");
  END;
END TestLeftShiftMLongint;

PROCEDURE TestShiftRightMInteger() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30 };
BEGIN
  PutT("\nTestShiftRightMInteger\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT("       0 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift(       0, shift[i])); PutT("\n");
    PutT(" 16_8000 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift( 16_8000, shift[i])); PutT("\n");
    PutT(" 16_4000 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift( 16_4000, shift[i])); PutT("\n");
    PutT("16_10000 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift(16_10000, shift[i])); PutT("\n");
    PutT("      -1 >> "); PutI(shift[i], 2); PutT(":"); NotPortableH(Word.RightShift(      -1, shift[i])); PutT("\n");
    PutT("      -2 >> "); PutI(shift[i], 2); PutT(":"); NotPortableH(Word.RightShift(      -2, shift[i])); PutT("\n");
    PutT("      -3 >> "); PutI(shift[i], 2); PutT(":"); NotPortableH(Word.RightShift(      -3, shift[i])); PutT("\n");
  END;
END TestShiftRightMInteger;

PROCEDURE TestShiftRightMLongint() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30, 40, 50, 60 };
BEGIN
  PutT("\nTestShiftRightMLongint\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT("       0L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(       0L, shift[i])); PutT("\n");
    PutT(" 16_8000L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift( 16_8000L, shift[i])); PutT("\n");
    PutT(" 16_4000L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift( 16_4000L, shift[i])); PutT("\n");
    PutT("16_10000L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(16_10000L, shift[i])); PutT("\n");
    PutT("      -1L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(      -1L, shift[i])); PutT("\n");
    PutT("      -2L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(      -2L, shift[i])); PutT("\n");
    PutT("      -3L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(      -3L, shift[i])); PutT("\n");
  END;
END TestShiftRightMLongint;

PROCEDURE TestShiftMInteger() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30, -1, -10, -20, -30 };
BEGIN
  PutT("\nTestShiftMInteger\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT("       0 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.Shift(       0, shift[i])); PutT("\n");
    IF ABS(shift[i]) <= 10 THEN
      PutT(" 16_8000 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.Shift( 16_8000, shift[i])); PutT("\n");
      PutT(" 16_4000 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.Shift( 16_4000, shift[i])); PutT("\n");
      PutT("16_10000 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.Shift(16_10000, shift[i])); PutT("\n");
    ELSE
      PutT(" 16_8000 << "); PutI(shift[i], 3); PutT(":"); NotPortableH(Word.Shift( 16_8000, shift[i])); PutT("\n");
      PutT(" 16_4000 << "); PutI(shift[i], 3); PutT(":"); NotPortableH(Word.Shift( 16_4000, shift[i])); PutT("\n");
      PutT("16_10000 << "); PutI(shift[i], 3); PutT(":"); NotPortableH(Word.Shift(16_10000, shift[i])); PutT("\n");
    END
  END;
END TestShiftMInteger;

PROCEDURE TestShiftMLongint() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30, 40, 50, 60, -1, -10, -20, -30, -40, -50, -60 };
BEGIN
  PutT("\nTestShiftMLongint\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT("       0L << "); PutI(shift[i], 3); PutT(":"); PutLH(Long.Shift(       0L, shift[i])); PutT("\n");
    PutT(" 16_8000L << "); PutI(shift[i], 3); PutT(":"); PutLH(Long.Shift( 16_8000L, shift[i])); PutT("\n");
    PutT(" 16_4000L << "); PutI(shift[i], 3); PutT(":"); PutLH(Long.Shift( 16_4000L, shift[i])); PutT("\n");
    PutT("16_10000L << "); PutI(shift[i], 3); PutT(":"); PutLH(Long.Shift(16_10000L, shift[i])); PutT("\n");
  END;
END TestShiftMLongint;

BEGIN
  PutL(a);
  NL();
  PutLH(c);
  NL();

  EVAL Long.Insert(1L, 2L, 3, 4);
  EVAL Long.Extract(1L, 3, 4);

  (* shifting with no constants *)
  TestLeftShiftInteger();
  TestLeftShiftLongint();
  TestShiftRightInteger();
  TestShiftRightLongint();
  TestShiftInteger();
  TestShiftLongint();

  (* shifting by a constant *)
  TestLeftShiftNInteger();
  TestLeftShiftNLongint();
  TestShiftRightNInteger();
  TestShiftRightNLongint();
  TestShiftNInteger();
  TestShiftNLongint();

  (* shifting constant by a constant *)
  TestLeftShiftMNInteger();
  TestLeftShiftMNLongint();
  TestShiftRightMNInteger();
  TestShiftRightMNLongint();
  TestShiftMNInteger();
  TestShiftMNLongint();

  (* shifting constant by a non-constant (not particularly special, except for shifting zero) *)
  TestLeftShiftMInteger();
  TestLeftShiftMLongint();
  TestShiftRightMInteger();
  TestShiftRightMLongint();
  TestShiftMInteger();
  TestShiftMLongint();

  NL();

  PutT("           :"); PutLH(NotConstL(16_123456789L)); NL();
  <* ASSERT Long.Rotate(16_123456789L, 56) = Long.Rotate(NotConstL(16_123456789L), NotConstI(56)) *>

  PutT("     Rotate:"); PutLH(Long.Rotate(NotConstL(16_123456789L), NotConstI(56))); NL();
  <* ASSERT Long.Rotate(16_123456789L, 56) = Long.Rotate(NotConstL(16_123456789L), NotConstI(56)) *>

  PutT("    -Rotate:"); PutLH(Long.Rotate(NotConstL(16_123456789L), -NotConstI(56))); NL();
  <* ASSERT Long.Rotate(16_123456789L, -56) = Long.Rotate(NotConstL(16_123456789L), -NotConstI(56)) *>

  PutT("RightRotate:"); PutLH(Long.RightRotate(NotConstL(16_123456789L), NotConstI(56))); NL();
  <* ASSERT Long.RightRotate(16_123456789L, 56) = Long.RightRotate(NotConstL(16_123456789L), NotConstI(56)) *>

  PutT(" LeftRotate:"); PutLH(Long.LeftRotate(NotConstL(16_123456789L), NotConstI(56))); NL();
  <* ASSERT Long.LeftRotate(16_123456789L, 56) = Long.LeftRotate(NotConstL(16_123456789L), NotConstI(56)) *>

  PutT("      Shift:"); PutLH(Long.Shift(NotConstL(16_123456789L), NotConstI(16))); NL();
  <* ASSERT Long.Shift(16_123456789L, 16) = Long.Shift(NotConstL(16_123456789L), NotConstI(16)) *>

  PutT("     -Shift:"); PutLH(Long.Shift(NotConstL(16_123456789L), -NotConstI(16))); NL();
  <* ASSERT Long.Shift(16_123456789L, -16) = Long.Shift(NotConstL(16_123456789L), -NotConstI(16)) *>

  PutT(" RightShift:"); PutLH(Long.RightShift(NotConstL(16_123456789L), NotConstI(16))); NL();
  <* ASSERT Long.RightShift(16_123456789L, 16) = Long.RightShift(NotConstL(16_123456789L), NotConstI(16)) *>

  PutT("  LeftShift:"); PutLH(Long.LeftShift(NotConstL(16_123456789L), NotConstI(16))); NL();
  <* ASSERT Long.LeftShift(16_123456789L, 16) = Long.LeftShift(NotConstL(16_123456789L), NotConstI(16)) *>

  PutT("        min:"); PutLH(MIN(NotConstL(16_123456789L), NotConstL(16_876543210L))); NL();
  <* ASSERT MIN(16_123456789L, 16_876543210L) = MIN(NotConstL(16_123456789L), NotConstL(16_876543210L)) *>

  PutT("        max:"); PutLH(MAX(NotConstL(16_123456789L), NotConstL(16_876543210L))); NL();
  <* ASSERT MAX(16_123456789L, 16_876543210L) = MAX(NotConstL(16_123456789L), NotConstL(16_876543210L)) *>

  PutT("       100L:"); PutLH(NotConstL(100L)); NL();
  <* ASSERT 100L = NotConstL(100L) *>

  PutT(FalseTrue[           100L  >           0L]); Flush();
  PutT(FalseTrue[           100L  <           0L]); Flush();
  PutT(FalseTrue[          -100L  >           0L]); Flush();
  PutT(FalseTrue[          -100L  <           0L]); Flush();
  PutT(FalseTrue[NotConstL( 100L) > NotConstL(0L)]); Flush();
  PutT(FalseTrue[NotConstL( 100L) < NotConstL(0L)]); Flush();
  PutT(FalseTrue[NotConstL(-100L) > NotConstL(0L)]); Flush();
  PutT(FalseTrue[NotConstL(-100L) < NotConstL(0L)]); Flush();

  <* ASSERT (NotConstL(  100L) > NotConstL(0L)) = TRUE *>
  <* ASSERT (NotConstL(  100L) < NotConstL(0L)) = FALSE *>
  <* ASSERT (NotConstL( -100L) > NotConstL(0L)) = FALSE *>
  <* ASSERT (NotConstL( -100L) < NotConstL(0L)) = TRUE *>

  PutT("        neg:"); PutLH(-NotConstL(100L)); NL();
  <* ASSERT -100L = -NotConstL(100L) *>

  PutT("        neg:"); PutLH(-NotConstL(-NotConstL(100L))); NL();
  <* ASSERT -(-100L) = -NotConstL(-(NotConstL(100L))) *>

  PutT("        ABS:"); PutLH(ABS(NotConstL(-100L))); NL();
  <* ASSERT ABS(-100L) = ABS(NotConstL(-100L)) *>

  PutT("        ABS:"); PutLH(ABS(NotConstL(100L))); NL();
  <* ASSERT ABS(100L) = ABS(NotConstL(100L)) *>

  PutL(a);
  NL();
  Flush();
  a := 1L;
  b := 1L + 2L;
  c := a + b;
  c := a - b;
  PutT(ExectTrue[c = (a - b)]);
  PutT(ExpectFalse[c = (a + b)]);
  PutT(ExectTrue[(a + b) > a]);
  Flush();

  PutT("VAL(-1, LONGINT) ");
  d := -1;
  a := VAL(d, LONGINT);
  PutL(a);
  NL();

  PutT("-1L ");
  a := -1L;
  PutL(a);
  NL();

  a := VAL(LAST(INTEGER), LONGINT);
  NotPortableL(a);
  INC(a);
  NotPortableL(a);
  NL();

  a := VAL(NotConstI(LAST(INTEGER)), LONGINT);
  NotPortableL(a);
  INC(a);
  NotPortableL(a);
  NL();

  a := VAL(FIRST(INTEGER), LONGINT);
  NotPortableL(a);
  NL();

  a := VAL(FIRST(INTEGER), LONGINT);
  NotPortableL(a);
  DEC(a);
  NotPortableL(a);
  NL();

  a := VAL(NotConstI(FIRST(INTEGER)), LONGINT);
  NotPortableL(a);
  DEC(a);
  NotPortableL(a);
  NL();

(*InsertExtractMaxAB := 2;
  InsertExtractMaxMN := 2; *)

  PutT("LINE "); PutI(ThisLine()); PutT(" FIRST(INTEGER): "); PutI(FIRST(INTEGER)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" LAST(INTEGER): "); PutI(LAST(INTEGER)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(FIRST(INTEGER)): "); PutI(NotConstI(FIRST(INTEGER))); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(LAST(INTEGER)): "); PutI(NotConstI(LAST(INTEGER))); NL();
  NL();

  PutT("LINE "); PutI(ThisLine()); PutT(" FIRST(LONGINT): "); PutL(FIRST(LONGINT)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" LAST(LONGINT): "); PutL(LAST(LONGINT)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(FIRST(LONGINT)): "); PutL(NotConstL(FIRST(LONGINT))); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(LAST(LONGINT)): "); PutL(NotConstL(LAST(LONGINT))); NL();
  NL();



  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(INTEGER)  DIV           1 : "); PutI(          FIRST(INTEGER)  DIV 1); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI( FIRST(INTEGER)) DIV           1 : "); PutI(NotConstI(FIRST(INTEGER)) DIV 1); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(INTEGER)  DIV NotConstI(1): "); PutI(          FIRST(INTEGER)  DIV NotConstI(1)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI( FIRST(INTEGER)) DIV NotConstI(1): "); PutI(NotConstI(FIRST(INTEGER)) DIV NotConstI(1)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(INTEGER)  DIV           1 : "); PutI(           LAST(INTEGER)  DIV 1); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(  LAST(INTEGER)) DIV           1 : "); PutI(NotConstI( LAST(INTEGER)) DIV 1); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(INTEGER)  DIV NotConstI(1): "); PutI(           LAST(INTEGER)  DIV NotConstI(1)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(  LAST(INTEGER)) DIV NotConstI(1): "); PutI(NotConstI( LAST(INTEGER)) DIV NotConstI(1)); NL();
  NL();

  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(INTEGER)  DIV           2 : "); PutI(          FIRST(INTEGER)  DIV 2); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI( FIRST(INTEGER)) DIV           2 : "); PutI(NotConstI(FIRST(INTEGER)) DIV 2); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(INTEGER)  DIV NotConstI(2): "); PutI(          FIRST(INTEGER)  DIV NotConstI(2)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI( FIRST(INTEGER)) DIV NotConstI(2): "); PutI(NotConstI(FIRST(INTEGER)) DIV NotConstI(2)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(INTEGER)  DIV           2 : "); PutI(           LAST(INTEGER)  DIV 2); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(  LAST(INTEGER)) DIV           2 : "); PutI(NotConstI( LAST(INTEGER)) DIV 2); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(INTEGER)  DIV NotConstI(2): "); PutI(           LAST(INTEGER)  DIV NotConstI(2)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(  LAST(INTEGER)) DIV NotConstI(2): "); PutI(NotConstI( LAST(INTEGER)) DIV NotConstI(2)); NL();
  NL();

  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(INTEGER)  DIV           3 : "); PutI(          FIRST(INTEGER)  DIV 3); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI( FIRST(INTEGER)) DIV           3 : "); PutI(NotConstI(FIRST(INTEGER)) DIV 3); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(INTEGER)  DIV NotConstI(3): "); PutI(          FIRST(INTEGER)  DIV NotConstI(3)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI( FIRST(INTEGER)) DIV NotConstI(3): "); PutI(NotConstI(FIRST(INTEGER)) DIV NotConstI(3)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(INTEGER)  DIV           3 : "); PutI(           LAST(INTEGER)  DIV 3); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(  LAST(INTEGER)) DIV           3 : "); PutI(NotConstI( LAST(INTEGER)) DIV 3); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(INTEGER)  DIV NotConstI(3): "); PutI(           LAST(INTEGER)  DIV NotConstI(3)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(  LAST(INTEGER)) DIV NotConstI(3): "); PutI(NotConstI( LAST(INTEGER)) DIV NotConstI(3)); NL();
  NL();

  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(INTEGER)  DIV           4 : "); PutI(          FIRST(INTEGER)  DIV 4); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI( FIRST(INTEGER)) DIV           4 : "); PutI(NotConstI(FIRST(INTEGER)) DIV 4); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(INTEGER)  DIV NotConstI(4): "); PutI(          FIRST(INTEGER)  DIV NotConstI(4)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI( FIRST(INTEGER)) DIV NotConstI(4): "); PutI(NotConstI(FIRST(INTEGER)) DIV NotConstI(4)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(INTEGER)  DIV           4 : "); PutI(           LAST(INTEGER)  DIV 4); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(  LAST(INTEGER)) DIV           4 : "); PutI(NotConstI( LAST(INTEGER)) DIV 4); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(INTEGER)  DIV NotConstI(4): "); PutI(           LAST(INTEGER)  DIV NotConstI(4)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstI(  LAST(INTEGER)) DIV NotConstI(4): "); PutI(NotConstI( LAST(INTEGER)) DIV NotConstI(4)); NL();
  NL();
  NL();

  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(LONGINT)  DIV           1L : "); PutL(          FIRST(LONGINT)  DIV 1L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL( FIRST(LONGINT)) DIV           1L : "); PutL(NotConstL(FIRST(LONGINT)) DIV 1L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(LONGINT)  DIV NotConstL(1L): "); PutL(          FIRST(LONGINT)  DIV NotConstL(1L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL( FIRST(LONGINT)) DIV NotConstL(1L): "); PutL(NotConstL(FIRST(LONGINT)) DIV NotConstL(1L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(LONGINT)  DIV           1L : "); PutL(           LAST(LONGINT)  DIV 1L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(  LAST(LONGINT)) DIV           1L : "); PutL(NotConstL( LAST(LONGINT)) DIV 1L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(LONGINT)  DIV NotConstL(1L): "); PutL(           LAST(LONGINT)  DIV NotConstL(1L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(  LAST(LONGINT)) DIV NotConstL(1L): "); PutL(NotConstL( LAST(LONGINT)) DIV NotConstL(1L)); NL();
  NL();

  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(LONGINT)  DIV           2L : "); PutL(          FIRST(LONGINT)  DIV 2L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL( FIRST(LONGINT)) DIV           2L : "); PutL(NotConstL(FIRST(LONGINT)) DIV 2L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(LONGINT)  DIV NotConstL(2L): "); PutL(          FIRST(LONGINT)  DIV NotConstL(2L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL( FIRST(LONGINT)) DIV NotConstL(2L): "); PutL(NotConstL(FIRST(LONGINT)) DIV NotConstL(2L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(LONGINT)  DIV           2L : "); PutL(           LAST(LONGINT)  DIV 2L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(  LAST(LONGINT)) DIV           2L : "); PutL(NotConstL( LAST(LONGINT)) DIV 2L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(LONGINT)  DIV NotConstL(2L): "); PutL(           LAST(LONGINT)  DIV NotConstL(2L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(  LAST(LONGINT)) DIV NotConstL(2L): "); PutL(NotConstL( LAST(LONGINT)) DIV NotConstL(2L)); NL();
  NL();

  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(LONGINT)  DIV           3L : "); PutL(          FIRST(LONGINT)  DIV 3L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL( FIRST(LONGINT)) DIV           3L : "); PutL(NotConstL(FIRST(LONGINT)) DIV 3L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(LONGINT)  DIV NotConstL(3L): "); PutL(          FIRST(LONGINT)  DIV NotConstL(3L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL( FIRST(LONGINT)) DIV NotConstL(3L): "); PutL(NotConstL(FIRST(LONGINT)) DIV NotConstL(3L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(LONGINT)  DIV           3L : "); PutL(           LAST(LONGINT)  DIV 3L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(  LAST(LONGINT)) DIV           3L : "); PutL(NotConstL( LAST(LONGINT)) DIV 3L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(LONGINT)  DIV NotConstL(3L): "); PutL(           LAST(LONGINT)  DIV NotConstL(3L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(  LAST(LONGINT)) DIV NotConstL(3L): "); PutL(NotConstL( LAST(LONGINT)) DIV NotConstL(3L)); NL();
  NL();

  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(LONGINT)  DIV           4L : "); PutL(          FIRST(LONGINT)  DIV 4L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL( FIRST(LONGINT)) DIV           4L : "); PutL(NotConstL(FIRST(LONGINT)) DIV 4L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("            FIRST(LONGINT)  DIV NotConstL(4L): "); PutL(          FIRST(LONGINT)  DIV NotConstL(4L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL( FIRST(LONGINT)) DIV NotConstL(4L): "); PutL(NotConstL(FIRST(LONGINT)) DIV NotConstL(4L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(LONGINT)  DIV           4L : "); PutL(           LAST(LONGINT)  DIV 4L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(  LAST(LONGINT)) DIV           4L : "); PutL(NotConstL( LAST(LONGINT)) DIV 4L); NL();
  PutT("LINE "); PutI(ThisLine()); PutT("             LAST(LONGINT)  DIV NotConstL(4L): "); PutL(           LAST(LONGINT)  DIV NotConstL(4L)); NL();
  PutT("LINE "); PutI(ThisLine()); PutT(" NotConstL(  LAST(LONGINT)) DIV NotConstL(4L): "); PutL(NotConstL( LAST(LONGINT)) DIV NotConstL(4L)); NL();
  NL();

  EVAL TestAdd(1L, 1L);
  EVAL TestSub(1L, 1L);
  EVAL TestMult(1L, 1L);
  EVAL TestDivI(1, 1);
  EVAL TestDivL(1L, 1L);
  EVAL TestDivUL(1L, 1L);
  EVAL TestModI(1, 1);
  EVAL TestModL(1L, 1L);
  EVAL TestModUL(1L, 1L);
  EVAL TestLT(1L, 1L);
  EVAL TestLTU(1L, 1L);
  EVAL TestEQ(1L, 1L);
  EVAL TestNE(1L, 1L);


  TestInsert();
  TestExtract();

  Flush();

END Main.
