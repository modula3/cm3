MODULE Main;
IMPORT RTIO, Word, Long;
FROM RTIO IMPORT PutText, PutLong, PutHex, PutLongHex, Flush;


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

CONST PutT = PutText;
CONST PutLH = PutLongHex;

PROCEDURE TestInsert() =
VAR result32: CARDINAL := 0;
    result64: LONGINT := 0L;
BEGIN
  FOR a32 := 0 TO insertextract_max_ab DO
    FOR b32 := 0 TO insertextract_max_ab DO
      FOR m := 0 TO insertextract_max_mn DO
        FOR n := 0 TO insertextract_max_mn DO
          result32 := Word.Insert(a32, b32, m, n);
          PutT("insert32(a:"); PutHex(a32);
          PutT(", b:"); PutHex(b32);
          PutT(", m:"); PutHex(m);
          PutT(", n:"); PutHex(n);
          PutT("):"); PutHex(result32);
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
          PutT(", m:"); PutHex(m);
          PutT(", n:"); PutHex(n);
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
        PutT("extract32(value:"); PutHex(a32);
        PutT(", m:"); PutHex(m);
        PutT(", n:"); PutHex(n);
        PutT(", sign_extend:"); PutHex(sign_extend);
        PutT("):"); PutHex(result32);
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
        PutT(", m:"); PutHex(m);
        PutT(", n:"); PutHex(n);
        PutT(", sign_extend:"); PutHex(sign_extend);
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

BEGIN
  PutLong(a);
  NL();
  PutLH(c);
  NL();

  EVAL Long.Insert(1L, 2L, 3, 4);
  EVAL Long.Extract(1L, 3, 4);

  PutT("     Rotate:"); PutLH(Long.Rotate(NotConstL(16_11112222L), NotConstI(40))); NL();
  <* ASSERT Long.Rotate(16_11112222L, 40) = Long.Rotate(NotConstL(16_11112222L), NotConstI(40)) *>

  PutT("    -Rotate:"); PutLH(Long.Rotate(NotConstL(16_111113333L), -NotConstI(40))); NL();
  <* ASSERT Long.Rotate(16_111113333L, -40) = Long.Rotate(NotConstL(16_111113333L), -NotConstI(40)) *>

  PutT("RightRotate:"); PutLH(Long.RightRotate(NotConstL(16_11114444L), NotConstI(40))); NL();
  <* ASSERT Long.RightRotate(16_11114444L, 40) = Long.RightRotate(NotConstL(16_11114444L), NotConstI(40)) *>

  PutT(" LeftRotate:"); PutLH(Long.LeftRotate(NotConstL(16_11115555L), NotConstI(40))); NL();
  <* ASSERT Long.LeftRotate(16_11115555L, 40) = Long.LeftRotate(NotConstL(16_11115555L), NotConstI(40)) *>

  PutT("      Shift:"); PutLH(Long.Shift(NotConstL(16_1000L), NotConstI(2))); NL();
  <* ASSERT Long.Shift(16_1000L, 2) = Long.Shift(NotConstL(16_1000L), NotConstI(2)) *>

  PutT("     -Shift:"); PutLH(Long.Shift(NotConstL(16_1000L), -NotConstI(2))); NL();
  <* ASSERT Long.Shift(16_1000L, -2) = Long.Shift(NotConstL(16_1000L), -NotConstI(2)) *>

  PutT(" RightShift:"); PutLH(Long.RightShift(NotConstL(16_1000L), NotConstI(2))); NL();
  <* ASSERT Long.RightShift(16_1000L, 2) = Long.RightShift(NotConstL(16_1000L), NotConstI(2)) *>

  PutT("  LeftShift:"); PutLH(Long.LeftShift(NotConstL(16_1000L), NotConstI(2))); NL();
  <* ASSERT Long.LeftShift(16_1000L, 2) = Long.LeftShift(NotConstL(16_1000L), NotConstI(2)) *>

  PutT("        min:"); PutLH(MIN(NotConstL(16_1000L), NotConstL(16_876543210L))); NL();
  <* ASSERT MIN(16_1000L, 16_876543210L) = MIN(NotConstL(16_1000L), NotConstL(16_876543210L)) *>

  PutT("        max:"); PutLH(MAX(NotConstL(16_1000L), NotConstL(16_876543210L))); NL();
  <* ASSERT MAX(16_1000L, 16_876543210L) = MAX(NotConstL(16_1000L), NotConstL(16_876543210L)) *>

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

  PutLong(a);
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
  PutLong(a);
  NL();

  a := -1L;
  PutLong(a);
  NL();

  a := VAL(LAST(INTEGER), LONGINT);
  INC(a);
  PutLong(a);
  NL();

  a := VAL(FIRST(INTEGER), LONGINT);
  PutLong(a);
  NL();

  a := VAL(FIRST(INTEGER), LONGINT);
  DEC(a);
  PutLong(a);
  NL();

  insertextract_max_ab := 2;
  insertextract_max_mn := 2;

  PutLong(FIRST(LONGINT));
  NL();

  PutLong(NotConstL(FIRST(LONGINT)));
  NL();

  PutLong(LAST(LONGINT));
  NL();

  PutLong(NotConstL(LAST(LONGINT)));
  NL();

  PutLong(NotConstL(LAST(LONGINT)) DIV NotConstL(2L));
  NL();

  PutLong(NotConstL(FIRST(LONGINT)) DIV NotConstL(2L));
  NL();

  PutLong(NotConstL(FIRST(LONGINT)) DIV NotConstL(2L));
  NL();

  TestInsert();
  TestExtract();
  Flush();

END Main.
