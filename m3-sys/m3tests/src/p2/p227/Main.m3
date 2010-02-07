MODULE Main;
IMPORT RTIO, Word, Long;
FROM RTIO IMPORT PutLong, PutText, PutHex, PutLongHex, Flush;


(* decrease these for faster runs *)
VAR insertextract_max_ab := 33;
VAR insertextract_max_mn := 10;

(* turn a constant into not a constant, from m3back's point of view *)
PROCEDURE NotConstL(a: LONGINT): LONGINT =
BEGIN
  RETURN a;
END NotConstL;

PROCEDURE NL() =
BEGIN
  PutText("\n");
END NL;

PROCEDURE TestInsert() =
VAR result32: CARDINAL := 0;
    result64: LONGINT := 0L;
BEGIN
  FOR a32 := 0 TO insertextract_max_ab DO
    FOR b32 := 0 TO insertextract_max_ab DO
      FOR m := 0 TO insertextract_max_mn DO
        FOR n := 0 TO insertextract_max_mn DO
          result32 := Word.Insert(a32, b32, m, n);
          PutText("insert32(a:"); PutHex(a32);
          PutText(", b:"); PutHex(b32);
          PutText(", m:"); PutHex(m);
          PutText(", n:"); PutHex(n);
          PutText("):"); PutHex(result32);
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
          PutText("insert64(a:"); PutLongHex(a64);
          PutText(", b:"); PutLongHex(b64);
          PutText(", m:"); PutHex(m);
          PutText(", n:"); PutHex(n);
          PutText("):"); PutLongHex(result64);
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
        PutText("extract32(value:"); PutHex(a32);
        PutText(", m:"); PutHex(m);
        PutText(", n:"); PutHex(n);
        PutText(", sign_extend:"); PutHex(sign_extend);
        PutText("):"); PutHex(result32);
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
        PutText("extract64(value:"); PutLongHex(a64);
        PutText(", m:"); PutHex(m);
        PutText(", n:"); PutHex(n);
        PutText(", sign_extend:"); PutHex(sign_extend);
        PutText("):"); PutLongHex(result64);
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
  PutLongHex(c);
  NL();

  EVAL Long.Insert(1L, 2L, 3, 4);
  EVAL Long.Extract(1L, 3, 4);

  PutLong(a);
  NL();
  Flush();
  a := 1L;
  b := 1L + 2L;
  c := a + b;
  c := a - b;
  PutText(expect_true[c = (a - b)]);
  PutText(expect_false[c = (a + b)]);
  PutText(expect_true[(a + b) > a]);
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
