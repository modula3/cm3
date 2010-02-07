MODULE Main;
IMPORT RTIO, Word, Long;

PROCEDURE TestInsert() =
VAR result32: CARDINAL := 0;
    result64: LONGINT := 0L;
BEGIN
  FOR a32 := 0 TO 33 DO
    FOR b32 := 0 TO 33 DO
      FOR m := 0 TO 10 DO
        FOR n := 0 TO 10 DO
          result32 := Word.Insert(a32, b32, m, n);
          RTIO.PutText("insert32(a:"); RTIO.PutHex(a32);
          RTIO.PutText(", b:"); RTIO.PutHex(b32);
          RTIO.PutText(", m:"); RTIO.PutHex(m);
          RTIO.PutText(", n:"); RTIO.PutHex(n);
          RTIO.PutText("):"); RTIO.PutHex(result32);
          RTIO.PutText("\n"); RTIO.Flush();
          IF n = 0 THEN
            <* ASSERT result32 = a32 *>
          END
        END
      END
    END
  END;

  FOR a64 := 0L TO 33L DO
    FOR b64 := 0L TO 33L DO
      FOR m := 0 TO 10 DO
        FOR n := 0 TO 10 DO
          result64 := Long.Insert(a64, b64, m, n);
          RTIO.PutText("insert64(a:"); RTIO.PutLongHex(a64);
          RTIO.PutText(", b:"); RTIO.PutLongHex(b64);
          RTIO.PutText(", m:"); RTIO.PutHex(m);
          RTIO.PutText(", n:"); RTIO.PutHex(n);
          RTIO.PutText("):"); RTIO.PutLongHex(result64);
          RTIO.PutText("\n"); RTIO.Flush();
          IF n = 0 THEN
            <* ASSERT result64 = a64 *>
          END
        END
      END
    END
  END
END TestInsert;

PROCEDURE TestExtract() =
CONST sign_extend = 0;
VAR result32: CARDINAL := 0;
    result64: LONGINT := 0L;
BEGIN
  FOR a32 := 0 TO 33 DO
     FOR m := 0 TO 10 DO
      FOR n := 0 TO 10 DO
        result32 := Word.Extract(a32, m, n);
        RTIO.PutText("extract32(value:"); RTIO.PutHex(a32);
        RTIO.PutText(", m:"); RTIO.PutHex(m);
        RTIO.PutText(", n:"); RTIO.PutHex(n);
        RTIO.PutText(", sign_extend:"); RTIO.PutHex(sign_extend);
        RTIO.PutText("):"); RTIO.PutHex(result32);
        RTIO.PutText("\n"); RTIO.Flush();
        IF n = 0 THEN
          <* ASSERT result32 = 0 *>
        END
      END
    END
  END;

  FOR a64 := 0L TO 33L DO
    FOR m := 0 TO 10 DO
      FOR n := 0 TO 10 DO
        result64 := Long.Extract(a64, m, n);
        RTIO.PutText("extract64(value:"); RTIO.PutLongHex(a64);
        RTIO.PutText(", m:"); RTIO.PutHex(m);
        RTIO.PutText(", n:"); RTIO.PutHex(n);
        RTIO.PutText(", sign_extend:"); RTIO.PutHex(sign_extend);
        RTIO.PutText("):"); RTIO.PutLongHex(result64);
        RTIO.PutText("\n"); RTIO.Flush();
        IF n = 0 THEN
          <* ASSERT result64 = 0L *>
        END
      END
    END
  END
END TestExtract;

(*VAR a := 1234L;
VAR b := 5678L;
VAR c := 1024L * 1024L * 1024L * 8L;*)
VAR a, b, c: LONGINT;
CONST expect_true = ARRAY BOOLEAN OF TEXT{"bad\n","good\n"};
CONST expect_false = ARRAY BOOLEAN OF TEXT{"good\n","bad\n"};

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
  RTIO.PutLong(a);
  RTIO.PutText("\n");
  RTIO.PutLongHex(c);
  RTIO.PutText("\n");

  EVAL Long.Insert(1L, 2L, 3, 4);
  EVAL Long.Extract(1L, 3, 4);

  RTIO.PutLong(a);
  RTIO.PutText("\n");
  RTIO.Flush();
  a := 1L;
  b := 1L + 2L;
  c := a + b;
  c := a - b;
  RTIO.PutText(expect_true[c = (a - b)]);
  RTIO.PutText(expect_false[c = (a + b)]);
  RTIO.PutText(expect_true[(a + b) > a]);
  RTIO.Flush();
  TestInsert();
  TestExtract();
END Main.
