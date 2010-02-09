MODULE Main;
IMPORT RTIO, Word, Long;
FROM RTIO IMPORT Flush;


(* decrease these for faster runs *)
VAR insertextract_max_ab := 33;
VAR insertextract_max_mn := 10;

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
END NL;

CONST PutT = RTIO.PutText;
CONST PutI = RTIO.PutInt;
CONST PutH = RTIO.PutHex;
CONST PutL = RTIO.PutLong;
CONST PutLH = RTIO.PutLongHex;

PROCEDURE TestInsert() =
VAR result32: CARDINAL := 0;
    result64: LONGINT := 0L;
BEGIN
  FOR a32 := 0 TO insertextract_max_ab DO
    FOR b32 := 0 TO insertextract_max_ab DO
      FOR m := 0 TO insertextract_max_mn DO
        FOR n := 0 TO insertextract_max_mn DO
          result32 := Word.Insert(a32, b32, m, n);
          PutT("insert32(a:"); PutH(a32);
          PutT(", b:"); PutH(b32);
          PutT(", m:"); PutH(m);
          PutT(", n:"); PutH(n);
          PutT("):"); PutH(result32);
          NL();
          IF n = 0 THEN
            <* ASSERT result32 = a32 *>
          END
        END
      END
    END
  END;

  FOR a64 := 0L TO VAL(insertextract_max_ab, LONGINT) DO
    FOR b64 := 0L TO VAL(insertextract_max_ab, LONGINT) DO
      FOR m := 0 TO insertextract_max_mn DO
        FOR n := 0 TO insertextract_max_mn DO
          result64 := Long.Insert(a64, b64, m, n);
          PutT("insert64(a:"); PutLH(a64);
          PutT(", b:"); PutLH(b64);
          PutT(", m:"); PutH(m);
          PutT(", n:"); PutH(n);
          PutT("):"); PutLH(result64);
          NL();
          IF n = 0 THEN
            <* ASSERT result64 = a64 *>
          END
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
  FOR a32 := 0 TO insertextract_max_ab DO
     FOR m := 0 TO insertextract_max_mn DO
      FOR n := 0 TO insertextract_max_mn DO
        result32 := Word.Extract(a32, m, n);
        PutT("extract32(value:"); PutH(a32);
        PutT(", m:"); PutH(m);
        PutT(", n:"); PutH(n);
        PutT(", sign_extend:"); PutH(sign_extend);
        PutT("):"); PutH(result32);
        NL();
        IF n = 0 THEN
          <* ASSERT result32 = 0 *>
        END
      END
    END
  END;

  FOR a64 := 0L TO VAL(insertextract_max_ab, LONGINT) DO
    FOR m := 0 TO insertextract_max_mn DO
      FOR n := 0 TO insertextract_max_mn DO
        result64 := Long.Extract(a64, m, n);
        PutT("extract64(value:"); PutLH(a64);
        PutT(", m:"); PutH(m);
        PutT(", n:"); PutH(n);
        PutT(", sign_extend:"); PutH(sign_extend);
        PutT("):"); PutLH(result64);
        NL();
        IF n = 0 THEN
          <* ASSERT result64 = 0L *>
        END
      END
    END
  END;
  Flush();
END TestExtract;

(*VAR a := 1234L;
VAR b := 5678L;
VAR c := 1024L * 1024L * 1024L * 8L;*)
VAR a, b, c: LONGINT;
VAR d: INTEGER;
CONST expect_true = ARRAY BOOLEAN OF TEXT{"bad\n","good\n"};
CONST expect_false = ARRAY BOOLEAN OF TEXT{"good\n","bad\n"};
CONST falsetrue = ARRAY BOOLEAN OF TEXT{"false\n","true\n"};

<*UNUSED*>PROCEDURE Add(a, b: LONGINT): LONGINT =
BEGIN
  RETURN a + b;
END Add;

<*UNUSED*>PROCEDURE Sub(a, b: LONGINT): LONGINT =
BEGIN
  RETURN a - b;
END Sub;

<*UNUSED*>PROCEDURE Mult(a, b: LONGINT): LONGINT =
BEGIN
  RETURN a * b;
END Mult;

<*UNUSED*>PROCEDURE Div(a, b: LONGINT): LONGINT =
BEGIN
  RETURN a DIV b;
END Div;

<*UNUSED*>PROCEDURE Rem(a, b: LONGINT): LONGINT =
BEGIN
  RETURN a MOD b;
END Rem;

<*UNUSED*>PROCEDURE DivU(a, b: LONGCARD): LONGCARD =
BEGIN
  RETURN a DIV b;
END DivU;

<*UNUSED*>PROCEDURE RemU(a, b: LONGCARD): LONGCARD =
BEGIN
  RETURN a MOD b;
END RemU;

<*UNUSED*>PROCEDURE LT(a, b: LONGINT): BOOLEAN =
BEGIN
  RETURN a < b;
END LT;

<*UNUSED*>PROCEDURE LTU(a, b: LONGINT): BOOLEAN =
BEGIN
  RETURN Long.LT(a, b);
END LTU;

<*UNUSED*>PROCEDURE EQ(a, b: LONGINT): BOOLEAN =
BEGIN
  RETURN a = b;
END EQ;

<*UNUSED*>PROCEDURE NE(a, b: LONGINT): BOOLEAN =
BEGIN
  RETURN a # b;
END NE;

(* shifting without constants *)

PROCEDURE TestShiftLeftInteger() =
VAR a := NotConstI(1);
BEGIN
  PutT("\nTestShiftLeftInteger\n");
  FOR i := 0 TO 40 DO
    PutT("1 << "); PutI(i); PutT(":"); PutH(a); PutT("\n");
    a := Word.LeftShift(a, NotConstI(1));
  END;
END TestShiftLeftInteger;

PROCEDURE TestShiftLeftLongint() =
VAR a := NotConstL(1L);
BEGIN
  PutT("\nTestShiftLeftLongint\n");
  FOR i := 0 TO 40 DO
    PutT("1 << "); PutI(i); PutT(":"); PutLH(a); PutT("\n");
    a := Long.LeftShift(a, NotConstI(1));
  END;
END TestShiftLeftLongint;

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
    PutT("16_8000 << "); PutI(i); PutT(":"); PutH(Word.Shift(a, NotConstI(i))); PutT("\n");
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

PROCEDURE TestShiftLeftNInteger() =
VAR a := NotConstI(1);
BEGIN
  PutT("\nTestShiftLeftNInteger\n");
  PutT("1 << 0"); PutT(":"); PutH(Word.Shift(a, 0)); PutT("\n");
  <* ASSERT Word.Shift(1, 0) = Word.Shift(a, 0) *>

  PutT("1 << 1"); PutT(":"); PutH(Word.Shift(a, 1)); PutT("\n");
  <* ASSERT Word.Shift(1, 1) = Word.Shift(a, 1) *>

  PutT("1 << 2"); PutT(":"); PutH(Word.Shift(a, 2)); PutT("\n");
  <* ASSERT Word.Shift(1, 2) = Word.Shift(a, 2) *>

  PutT("1 << 3"); PutT(":"); PutH(Word.Shift(a, 3)); PutT("\n");
  <* ASSERT Word.Shift(1, 3) = Word.Shift(a, 3) *>

  PutT("1 << 30"); PutT(":"); PutH(Word.Shift(a, 30)); PutT("\n");
  <* ASSERT Word.Shift(1, 30) = Word.Shift(a, 30) *>

END TestShiftLeftNInteger;

PROCEDURE TestShiftLeftNLongint() =
VAR a := NotConstL(1L);
BEGIN
  PutT("\nTestShiftLeftNLongint\n");

  PutT("1L << 0"); PutT(":"); PutLH(Long.Shift(a, 0)); PutT("\n");
  <* ASSERT Long.Shift(1L, 0) = Long.Shift(a, 0) *>

  PutT("1L << 1"); PutT(":"); PutLH(Long.Shift(a, 1)); PutT("\n");
  <* ASSERT Long.Shift(1L, 1) = Long.Shift(a, 1) *>

  PutT("1L << 2"); PutT(":"); PutLH(Long.Shift(a, 2)); PutT("\n");
  <* ASSERT Long.Shift(1L, 2) = Long.Shift(a, 2) *>

  PutT("1L << 3"); PutT(":"); PutLH(Long.Shift(a, 3)); PutT("\n");
  <* ASSERT Long.Shift(1L, 3) = Long.Shift(a, 3) *>

  PutT("1L << 30"); PutT(":"); PutLH(Long.Shift(a, 30)); PutT("\n");
  <* ASSERT Long.Shift(1L, 30) = Long.Shift(a, 30) *>

  PutT("1L << 40"); PutT(":"); PutLH(Long.Shift(a, 40)); PutT("\n");
  <* ASSERT Long.Shift(1L, 40) = Long.Shift(a, 40) *>

  PutT("1L << 50"); PutT(":"); PutLH(Long.Shift(a, 50)); PutT("\n");
  <* ASSERT Long.Shift(1L, 50) = Long.Shift(a, 50) *>

  PutT("1L << 60"); PutT(":"); PutLH(Long.Shift(a, 60)); PutT("\n");
  <* ASSERT Long.Shift(1L, 60) = Long.Shift(a, 60) *>

END TestShiftLeftNLongint;

PROCEDURE TestShiftRightNInteger() =
VAR a := NotConstI(16_80000000);
BEGIN
  PutT("\nTestShiftRightNInteger\n");
  PutT("16_80000000 >> 0"); PutT(":"); PutH(Word.RightShift(a, 0)); PutT("\n");
  PutT("16_80000000 >> 1"); PutT(":"); PutH(Word.RightShift(a, 1)); PutT("\n");
  PutT("16_80000000 >> 2"); PutT(":"); PutH(Word.RightShift(a, 2)); PutT("\n");
  PutT("16_80000000 >> 3"); PutT(":"); PutH(Word.RightShift(a, 3)); PutT("\n");
  PutT("16_80000000 >> 30"); PutT(":"); PutH(Word.RightShift(a, 30)); PutT("\n");
END TestShiftRightNInteger;

PROCEDURE TestShiftRightNLongint() =
VAR a := NotConstL(FIRST(LONGINT));
BEGIN
  PutT("\nTestShiftRightNLongint\n");
  PutT("FIRST(LONGINT) >> 0"); PutT(":"); PutLH(Long.RightShift(a, 0)); PutT("\n");
  PutT("FIRST(LONGINT) >> 1"); PutT(":"); PutLH(Long.RightShift(a, 1)); PutT("\n");
  PutT("FIRST(LONGINT) >> 2"); PutT(":"); PutLH(Long.RightShift(a, 2)); PutT("\n");
  PutT("FIRST(LONGINT) >> 3"); PutT(":"); PutLH(Long.RightShift(a, 3)); PutT("\n");
  PutT("FIRST(LONGINT) >> 30"); PutT(":"); PutLH(Long.RightShift(a, 30)); PutT("\n");
  PutT("FIRST(LONGINT) >> 31"); PutT(":"); PutLH(Long.RightShift(a, 31)); PutT("\n");
  PutT("FIRST(LONGINT) >> 32"); PutT(":"); PutLH(Long.RightShift(a, 32)); PutT("\n");
  PutT("FIRST(LONGINT) >> 33"); PutT(":"); PutLH(Long.RightShift(a, 33)); PutT("\n");
  PutT("FIRST(LONGINT) >> 40"); PutT(":"); PutLH(Long.RightShift(a, 40)); PutT("\n");
  PutT("FIRST(LONGINT) >> 50"); PutT(":"); PutLH(Long.RightShift(a, 50)); PutT("\n");
  PutT("FIRST(LONGINT) >> 60"); PutT(":"); PutLH(Long.RightShift(a, 60)); PutT("\n");
  PutT("FIRST(LONGINT) >> 61"); PutT(":"); PutLH(Long.RightShift(a, 61)); PutT("\n");
  PutT("FIRST(LONGINT) >> 62"); PutT(":"); PutLH(Long.RightShift(a, 62)); PutT("\n");
  PutT("FIRST(LONGINT) >> 63"); PutT(":"); PutLH(Long.RightShift(a, 63)); PutT("\n");
END TestShiftRightNLongint;

PROCEDURE TestShiftNInteger() =
VAR a := NotConstI(16_8000);
BEGIN
  PutT("\nTestShiftNInteger\n");
  PutT("16_8000 <<  -1:"); PutH(Word.Shift(a, -1)); PutT("\n");
  PutT("16_8000 <<   1:"); PutH(Word.Shift(a,  1)); PutT("\n");
  PutT("16_8000 << -10:"); PutH(Word.Shift(a, -10)); PutT("\n");
  PutT("16_8000 <<  10:"); PutH(Word.Shift(a,  10)); PutT("\n");
  PutT("16_8000 << -20:"); PutH(Word.Shift(a, -20)); PutT("\n");
  PutT("16_8000 <<  20:"); PutH(Word.Shift(a,  20)); PutT("\n");
  PutT("16_8000 << -30:"); PutH(Word.Shift(a, -30)); PutT("\n");
  PutT("16_8000 <<  30:"); PutH(Word.Shift(a,  30)); PutT("\n");
END TestShiftNInteger;

PROCEDURE TestShiftNLongint() =
VAR a := NotConstL(16_8000L);
BEGIN
  PutT("\nTestShiftNLongint\n");
  PutT("16_8000L <<  -1:"); PutLH(Long.Shift(a, -1)); PutT("\n");
  PutT("16_8000L <<   1:"); PutLH(Long.Shift(a,  1)); PutT("\n");
  PutT("16_8000L << -10:"); PutLH(Long.Shift(a, -10)); PutT("\n");
  PutT("16_8000L <<  10:"); PutLH(Long.Shift(a,  10)); PutT("\n");
  PutT("16_8000L << -20:"); PutLH(Long.Shift(a, -20)); PutT("\n");
  PutT("16_8000L <<  20:"); PutLH(Long.Shift(a,  20)); PutT("\n");
  PutT("16_8000L << -30:"); PutLH(Long.Shift(a, -30)); PutT("\n");
  PutT("16_8000L <<  30:"); PutLH(Long.Shift(a,  30)); PutT("\n");
  PutT("16_8000L << -40:"); PutLH(Long.Shift(a, -40)); PutT("\n");
  PutT("16_8000L <<  40:"); PutLH(Long.Shift(a,  40)); PutT("\n");
  PutT("16_8000L << -50:"); PutLH(Long.Shift(a, -50)); PutT("\n");
  PutT("16_8000L <<  50:"); PutLH(Long.Shift(a,  50)); PutT("\n");
  PutT("16_8000L << -60:"); PutLH(Long.Shift(a, -60)); PutT("\n");
  PutT("16_8000L <<  60:"); PutLH(Long.Shift(a,  60)); PutT("\n");
END TestShiftNLongint;

(* shifting constant by a constant *)

PROCEDURE TestShiftLeftMNInteger() =
BEGIN
  PutT("\nTestShiftLeftMNInteger\n");
  PutT("1 << 0"); PutT(":"); PutH(Word.LeftShift(1, 0)); PutT("\n");
  PutT("1 << 1"); PutT(":"); PutH(Word.LeftShift(1, 1)); PutT("\n");
  PutT("1 << 2"); PutT(":"); PutH(Word.LeftShift(1, 2)); PutT("\n");
  PutT("1 << 3"); PutT(":"); PutH(Word.LeftShift(1, 3)); PutT("\n");
  PutT("1 << 30"); PutT(":"); PutH(Word.LeftShift(1, 30)); PutT("\n");
  PutT("1 << 31"); PutT(":"); PutH(Word.LeftShift(1, 31)); PutT("\n");
END TestShiftLeftMNInteger;

PROCEDURE TestShiftLeftMNLongint() =
BEGIN
  PutT("\nTestShiftLeftNLongint\n");
  PutT("1L << 0"); PutT(":"); PutLH(Long.Shift(1L, 0)); PutT("\n");
  PutT("1L << 1"); PutT(":"); PutLH(Long.Shift(1L, 1)); PutT("\n");
  PutT("1L << 2"); PutT(":"); PutLH(Long.Shift(1L, 2)); PutT("\n");
  PutT("1L << 3"); PutT(":"); PutLH(Long.Shift(1L, 3)); PutT("\n");
  PutT("1L << 30"); PutT(":"); PutLH(Long.Shift(1L, 30)); PutT("\n");
  PutT("1L << 40"); PutT(":"); PutLH(Long.Shift(1L, 40)); PutT("\n");
  PutT("1L << 50"); PutT(":"); PutLH(Long.Shift(1L, 50)); PutT("\n");
  PutT("1L << 60"); PutT(":"); PutLH(Long.Shift(1L, 60)); PutT("\n");
END TestShiftLeftMNLongint;

PROCEDURE TestShiftRightMNInteger() =
BEGIN
  PutT("\nTestShiftRightMNInteger\n");
  PutT("16_80000000 >> 0"); PutT(":"); PutH(Word.RightShift(16_80000000, 0)); PutT("\n");
  PutT("16_80000000 >> 1"); PutT(":"); PutH(Word.RightShift(16_80000000, 1)); PutT("\n");
  PutT("16_80000000 >> 2"); PutT(":"); PutH(Word.RightShift(16_80000000, 2)); PutT("\n");
  PutT("16_80000000 >> 3"); PutT(":"); PutH(Word.RightShift(16_80000000, 3)); PutT("\n");
  PutT("16_80000000 >> 30"); PutT(":"); PutH(Word.RightShift(16_80000000, 30)); PutT("\n");
END TestShiftRightMNInteger;

PROCEDURE TestShiftRightMNLongint() =
BEGIN
  PutT("\nTestShiftRightMNLongint\n");
  PutT("FIRST(LONGINT) >> 0"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 0)); PutT("\n");
  PutT("FIRST(LONGINT) >> 1"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 1)); PutT("\n");
  PutT("FIRST(LONGINT) >> 2"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 2)); PutT("\n");
  PutT("FIRST(LONGINT) >> 3"); PutT(":"); PutLH(Long.RightShift(FIRST(LONGINT), 3)); PutT("\n");
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
  PutT("16_8000 << -40:"); PutH(Word.Shift(16_8000, -40)); PutT("\n");
  PutT("16_8000 << -30:"); PutH(Word.Shift(16_8000, -30)); PutT("\n");
  PutT("16_8000 << -20:"); PutH(Word.Shift(16_8000, -20)); PutT("\n");
  PutT("16_8000 << -1:"); PutH(Word.Shift(16_8000, -1)); PutT("\n");
  PutT("16_8000 <<  1:"); PutH(Word.Shift(16_8000,  1)); PutT("\n");
  PutT("16_8000 << 20:"); PutH(Word.Shift(16_8000, 20)); PutT("\n");
  PutT("16_8000 << 30:"); PutH(Word.Shift(16_8000, 30)); PutT("\n");
  PutT("16_8000 << 40:"); PutH(Word.Shift(16_8000, 40)); PutT("\n");
END TestShiftMNInteger;

PROCEDURE TestShiftMNLongint() =
BEGIN
  PutT("\nTestShiftMNLongint\n");
  PutT("16_8000L << -40:"); PutLH(Long.Shift(16_8000L, -40)); PutT("\n");
  PutT("16_8000L << -30:"); PutLH(Long.Shift(16_8000L, -30)); PutT("\n");
  PutT("16_8000L << -20:"); PutLH(Long.Shift(16_8000L, -20)); PutT("\n");
  PutT("16_8000L << -1:"); PutLH(Long.Shift(16_8000L, -1)); PutT("\n");
  PutT("16_8000L <<  1:"); PutLH(Long.Shift(16_8000L,  1)); PutT("\n");
  PutT("16_8000L << 20:"); PutLH(Long.Shift(16_8000L, 20)); PutT("\n");
  PutT("16_8000L << 30:"); PutLH(Long.Shift(16_8000L, 30)); PutT("\n");
  PutT("16_8000L << 40:"); PutLH(Long.Shift(16_8000L, 40)); PutT("\n");
END TestShiftMNLongint;

(* shifting constant by a non-constant (not particularly special, except for shifting zero) *)

PROCEDURE TestShiftLeftMInteger() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30 };
BEGIN
  PutT("\nTestShiftLeftMLongint\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT(" 0 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.LeftShift(0, shift[i])); PutT("\n");
    PutT(" 1 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.LeftShift(1, shift[i])); PutT("\n");
    PutT(" 2 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.LeftShift(2, shift[i])); PutT("\n");
    PutT(" 3 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.LeftShift(3, shift[i])); PutT("\n");
  END;
END TestShiftLeftMInteger;

PROCEDURE TestShiftLeftMLongint() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30, 40, 50, 60 };
BEGIN
  PutT("\nTestShiftLeftMLongint\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT(" 0L << "); PutI(shift[i], 2); PutT(":"); PutLH(Long.LeftShift(0L, shift[i])); PutT("\n");
    PutT(" 1L << "); PutI(shift[i], 2); PutT(":"); PutLH(Long.LeftShift(1L, shift[i])); PutT("\n");
    PutT(" 2L << "); PutI(shift[i], 2); PutT(":"); PutLH(Long.LeftShift(2L, shift[i])); PutT("\n");
    PutT(" 3L << "); PutI(shift[i], 2); PutT(":"); PutLH(Long.LeftShift(3L, shift[i])); PutT("\n");
  END;
END TestShiftLeftMLongint;

PROCEDURE TestShiftRightMInteger() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30 };
BEGIN
  PutT("\nTestShiftRightMInteger\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT("       0 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift(0, shift[i])); PutT("\n");
    PutT(" 16_8000 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift(16_8000, shift[i])); PutT("\n");
    PutT(" 16_4000 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift(16_4000, shift[i])); PutT("\n");
    PutT("16_10000 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift(16_10000, shift[i])); PutT("\n");
    PutT("      -1 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift(-1, shift[i])); PutT("\n");
    PutT("      -2 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift(-2, shift[i])); PutT("\n");
    PutT("      -3 >> "); PutI(shift[i], 2); PutT(":"); PutH(Word.RightShift(-3, shift[i])); PutT("\n");
  END;
END TestShiftRightMInteger;

PROCEDURE TestShiftRightMLongint() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30, 40, 50, 60 };
BEGIN
  PutT("\nTestShiftRightMLongint\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT("       0L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(0L, shift[i])); PutT("\n");
    PutT(" 16_8000L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(16_8000L, shift[i])); PutT("\n");
    PutT(" 16_4000L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(16_4000L, shift[i])); PutT("\n");
    PutT("16_10000L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(16_10000L, shift[i])); PutT("\n");
    PutT("      -1L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(-1L, shift[i])); PutT("\n");
    PutT("      -2L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(-2L, shift[i])); PutT("\n");
    PutT("      -3L >> "); PutI(shift[i], 2); PutT(":"); PutLH(Long.RightShift(-3L, shift[i])); PutT("\n");
  END;
END TestShiftRightMLongint;

PROCEDURE TestShiftMInteger() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30, -1, -10, -20, -30 };
BEGIN
  PutT("\nTestShiftMInteger\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT("       0 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.Shift(0, shift[i])); PutT("\n");
    PutT(" 16_8000 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.Shift(16_8000, shift[i])); PutT("\n");
    PutT(" 16_4000 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.Shift(16_4000, shift[i])); PutT("\n");
    PutT("16_10000 << "); PutI(shift[i], 3); PutT(":"); PutH(Word.Shift(16_10000, shift[i])); PutT("\n");
  END;
END TestShiftMInteger;

PROCEDURE TestShiftMLongint() =
  CONST shift = ARRAY OF INTEGER {1, 10, 20, 30, 40, 50, 60, -1, -10, -20, -30, -40, -50, -60 };
BEGIN
  PutT("\nTestShiftMLongint\n");
  FOR i := FIRST(shift) TO LAST(shift) DO
    PutT("       0L << "); PutI(shift[i], 3); PutT(":"); PutLH(Long.Shift(0L, shift[i])); PutT("\n");
    PutT(" 16_8000L << "); PutI(shift[i], 3); PutT(":"); PutLH(Long.Shift(16_8000L, shift[i])); PutT("\n");
    PutT(" 16_4000L << "); PutI(shift[i], 3); PutT(":"); PutLH(Long.Shift(16_4000L, shift[i])); PutT("\n");
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
  TestShiftLeftInteger();
  TestShiftLeftLongint();
  TestShiftRightInteger();
  TestShiftRightLongint();
  TestShiftInteger();
  TestShiftLongint();

  (* shifting by a constant *)
  TestShiftLeftNInteger();
  TestShiftLeftNLongint();
  TestShiftRightNInteger();
  TestShiftRightNLongint();
  TestShiftNInteger();
  TestShiftNLongint();

  (* shifting constant by a constant *)
  TestShiftLeftMNInteger();
  TestShiftLeftMNLongint();
  TestShiftRightMNInteger();
  TestShiftRightMNLongint();
  TestShiftMNInteger();
  TestShiftMNLongint();

  (* shifting constant by a non-constant (not particularly special, except for shifting zero) *)
  TestShiftLeftMInteger();
  TestShiftLeftMLongint();
  TestShiftRightMInteger();
  TestShiftRightMLongint();
  TestShiftMInteger();
  TestShiftMLongint();


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

  PutT(falsetrue[           100L  >           0L]); Flush();
  PutT(falsetrue[           100L  <           0L]); Flush();
  PutT(falsetrue[          -100L  >           0L]); Flush();
  PutT(falsetrue[          -100L  <           0L]); Flush();
  PutT(falsetrue[NotConstL( 100L) > NotConstL(0L)]); Flush();
  PutT(falsetrue[NotConstL( 100L) < NotConstL(0L)]); Flush();
  PutT(falsetrue[NotConstL(-100L) > NotConstL(0L)]); Flush();
  PutT(falsetrue[NotConstL(-100L) < NotConstL(0L)]); Flush();

  <* ASSERT (NotConstL(  100L) > NotConstL(0L)) = TRUE *>
  <* ASSERT (NotConstL(  100L) < NotConstL(0L)) = FALSE *>
  <* ASSERT (NotConstL( -100L) > NotConstL(0L)) = FALSE *>
  <* ASSERT (NotConstL( -100L) < NotConstL(0L)) = TRUE *>

  PutT("        neg:"); PutLH(-NotConstL(100L)); NL();
  <* ASSERT -100L = -NotConstL(100L) *>

  PutT("        neg:"); PutLH(-NotConstL(-NotConstL(100L))); NL();
  <* ASSERT -(-100L) = -NotConstL(-(NotConstL(100L))) *>

  PutT("        abs:"); PutLH(ABS(NotConstL(-100L))); NL();
  <* ASSERT ABS(-100L) = ABS(NotConstL(-100L)) *>

  PutT("        abs:"); PutLH(ABS(NotConstL(100L))); NL();
  <* ASSERT ABS(100L) = ABS(NotConstL(100L)) *>

  PutL(a);
  NL();
  Flush();
  a := 1L;
  b := 1L + 2L;
  c := a + b;
  c := a - b;
  PutT(expect_true[c = (a - b)]);
  PutT(expect_false[c = (a + b)]);
  PutT(expect_true[(a + b) > a]);
  Flush();

  d := -1;
  a := VAL(d, LONGINT);
  PutL(a);
  NL();

  a := -1L;
  PutL(a);
  NL();

  a := VAL(LAST(INTEGER), LONGINT);
  INC(a);
  PutL(a);
  NL();

  a := VAL(FIRST(INTEGER), LONGINT);
  PutL(a);
  NL();

  a := VAL(FIRST(INTEGER), LONGINT);
  DEC(a);
  PutL(a);
  NL();

  insertextract_max_ab := 2;
  insertextract_max_mn := 2;

  PutL(FIRST(LONGINT));
  NL();

  PutL(NotConstL(FIRST(LONGINT)));
  NL();

  PutL(LAST(LONGINT));
  NL();

  PutL(NotConstL(LAST(LONGINT)));
  NL();

  PutL(NotConstL(LAST(LONGINT)) DIV NotConstL(2L));
  NL();

  PutL(NotConstL(FIRST(LONGINT)) DIV NotConstL(2L));
  NL();

  PutL(NotConstL(FIRST(LONGINT)) DIV NotConstL(2L));
  NL();

  TestInsert();
  TestExtract();
  Flush();

END Main.
