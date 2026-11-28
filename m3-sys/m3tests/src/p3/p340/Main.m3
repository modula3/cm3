(* LONGINT and Long (unsigned 64-bit) comparisons whose high words are equal
   and whose low words differ in their top bit.  On 32-bit targets the low
   words must then be compared as unsigned numbers; the I386 integrated
   back end compared them as signed, so 2147483648L <= 1000L was TRUE, and
   Fmt.LongInt printed -4294967296L as "0".  The values are variables, so
   nothing is folded. *)

MODULE Main;

IMPORT IO, Long, RuntimeError;

TYPE Small = [0L .. 1000L];

VAR
  b := 2147483648L;        (* high word 0, low word 16_80000000 *)
  m := -2147483649L;       (* high word -1, low word 16_7FFFFFFF *)
  a := -4294967296L;       (* high word -1, low word 0 *)
  k := 1000L;  z := 0L;

PROCEDURE P (name: TEXT;  ok: BOOLEAN) =
  BEGIN
    IF ok
      THEN IO.Put (name & ": pass\n");
      ELSE IO.Put (name & ": FAIL\n");
    END;
  END P;

PROCEDURE InSmall (x: LONGINT): BOOLEAN =
  (* the assignment's range check, [0..1000], is an unsigned compare *)
  VAR s: Small;  ok := TRUE;
  BEGIN
    TRY
      s := x;
    EXCEPT RuntimeError.E => ok := FALSE;
    END;
    RETURN ok;
  END InSmall;

BEGIN
  P ("b > 1000 (const)",   b > 1000L);
  P ("b > k (var)",        b > k);
  P ("NOT b <= k",         NOT (b <= k));
  P ("0 <= b (const)",     0L <= b);
  P ("z < b (var)",        z < b);
  P ("b # 0",              b # z);
  P ("m < -1 (const)",     m < -1L);
  P ("NOT m < a",          NOT (m < a));
  P ("a < m",              a < m);
  P ("a <= k",             a <= k);
  P ("MAX (b, k) = b",     MAX (b, k) = b);
  P ("MIN (b, k) = k",     MIN (b, k) = k);
  P ("MIN (a, m) = a",     MIN (a, m) = a);
  P ("Long.LT (k, b)",     Long.LT (k, b));
  P ("Long.GT (b, k)",     Long.GT (b, k));
  P ("Long.LT (b, a)",     Long.LT (b, a));   (* a is 16_FFFFFFFF00000000 *)
  P ("b not in [0..1000]", NOT InSmall (b));
  P ("k in [0..1000]",     InSmall (k));
  P ("a not in [0..1000]", NOT InSmall (a));
END Main.
