(* -OverflowChk (set in this test's m3makefile): INTEGER and LONGINT
   +, -, *, DIV, unary minus, ABS, INC and DEC abort with
   RuntimeError.IntegerOverflow exactly when the result is not
   representable, and Word arithmetic still wraps around.  Every case is
   expressed through FIRST and LAST, so the output is the same for 32- and
   64-bit INTEGER. *)

MODULE Main;

IMPORT IO, RuntimeError, Word;

TYPE Op = {Add, Sub, Mul, Div, Neg, Abs, Inc, Dec};

PROCEDURE Put (name, result: TEXT) =
  BEGIN
    IO.Put (name & ": " & result & "\n");
  END Put;

(* ---- INTEGER ---- *)

PROCEDURE DoInt (k: Op;  a, b: INTEGER): INTEGER =
  VAR r: INTEGER;
  BEGIN
    CASE k OF
    | Op.Add => r := a + b;
    | Op.Sub => r := a - b;
    | Op.Mul => r := a * b;
    | Op.Div => r := a DIV b;
    | Op.Neg => r := -a;
    | Op.Abs => r := ABS (a);
    | Op.Inc => r := a;  INC (r, b);
    | Op.Dec => r := a;  DEC (r, b);
    END;
    RETURN r;
  END DoInt;

PROCEDURE OvfInt (name: TEXT;  k: Op;  a, b: INTEGER := 0) =
  (* k on a and b must overflow *)
  VAR r: INTEGER;
  BEGIN
    TRY
      r := DoInt (k, a, b);
      Put (name, "FAIL: no overflow");
    EXCEPT RuntimeError.E (e) =>
      IF e = RuntimeError.T.IntegerOverflow
        THEN Put (name, "pass");
        ELSE Put (name, "FAIL: " & RuntimeError.Tag (e));
      END;
    END;
  END OvfInt;

PROCEDURE OkInt (name: TEXT;  k: Op;  a, b, expected: INTEGER) =
  (* k on a and b must give expected *)
  VAR r: INTEGER;
  BEGIN
    TRY
      r := DoInt (k, a, b);
      IF r = expected
        THEN Put (name, "pass");
        ELSE Put (name, "FAIL: wrong result");
      END;
    EXCEPT RuntimeError.E (e) =>
      Put (name, "FAIL: " & RuntimeError.Tag (e));
    END;
  END OkInt;

PROCEDURE TestInt () =
  VAR
    F := FIRST (INTEGER);  L := LAST (INTEGER);
    H := L DIV 2;  (* 2^(n-2) - 1 *)
    T := L DIV 3;
    zero := 0;  one := 1;  two := 2;  three := 3;  m1 := -1;
  BEGIN
    OvfInt ("INTEGER LAST + 1", Op.Add, L, one);
    OvfInt ("INTEGER FIRST + -1", Op.Add, F, m1);
    OvfInt ("INTEGER (H+1) + (H+1)", Op.Add, H + one, H + one);
    OkInt  ("INTEGER LAST + FIRST", Op.Add, L, F, m1);
    OkInt  ("INTEGER H + (H+1)", Op.Add, H, H + one, L);

    OvfInt ("INTEGER FIRST - 1", Op.Sub, F, one);
    OvfInt ("INTEGER LAST - -1", Op.Sub, L, m1);
    OvfInt ("INTEGER 0 - FIRST", Op.Sub, zero, F);
    OkInt  ("INTEGER -1 - FIRST", Op.Sub, m1, F, L);
    OkInt  ("INTEGER 0 - LAST", Op.Sub, zero, L, F + one);

    OvfInt ("INTEGER LAST * 2", Op.Mul, L, two);
    OvfInt ("INTEGER FIRST * -1", Op.Mul, F, m1);
    OvfInt ("INTEGER -1 * FIRST", Op.Mul, m1, F);
    OvfInt ("INTEGER (H+1) * 2", Op.Mul, H + one, two);
    OvfInt ("INTEGER 2 * (H+1)", Op.Mul, two, H + one);
    OvfInt ("INTEGER (T+1) * 3", Op.Mul, T + one, three);
    OvfInt ("INTEGER LAST * LAST", Op.Mul, L, L);
    OvfInt ("INTEGER FIRST * FIRST", Op.Mul, F, F);
    OvfInt ("INTEGER (FIRST DIV 2) * -2", Op.Mul, F DIV two, -two);
    OkInt  ("INTEGER FIRST * 1", Op.Mul, F, one, F);
    OkInt  ("INTEGER -1 * LAST", Op.Mul, m1, L, F + one);
    OkInt  ("INTEGER H * 2", Op.Mul, H, two, L - one);
    OkInt  ("INTEGER T * 3", Op.Mul, T, three, L - one);
    OkInt  ("INTEGER (FIRST DIV 2) * 2", Op.Mul, F DIV two, two, F);
    OkInt  ("INTEGER 0 * FIRST", Op.Mul, zero, F, zero);
    OkInt  ("INTEGER FIRST * 0", Op.Mul, F, zero, zero);

    OvfInt ("INTEGER FIRST DIV -1", Op.Div, F, m1);
    OkInt  ("INTEGER FIRST DIV 1", Op.Div, F, one, F);
    OkInt  ("INTEGER LAST DIV -1", Op.Div, L, m1, F + one);

    OvfInt ("INTEGER -FIRST", Op.Neg, F);
    OkInt  ("INTEGER -LAST", Op.Neg, L, zero, F + one);
    OvfInt ("INTEGER ABS (FIRST)", Op.Abs, F);
    OkInt  ("INTEGER ABS (FIRST + 1)", Op.Abs, F + one, zero, L);

    OvfInt ("INTEGER INC (LAST)", Op.Inc, L, one);
    OvfInt ("INTEGER DEC (FIRST)", Op.Dec, F, one);
    OvfInt ("INTEGER INC (FIRST, -1)", Op.Inc, F, m1);
    OkInt  ("INTEGER INC (LAST, -1)", Op.Inc, L, m1, L - one);
  END TestInt;

(* ---- LONGINT ---- *)

PROCEDURE DoLong (k: Op;  a, b: LONGINT): LONGINT =
  VAR r: LONGINT;
  BEGIN
    CASE k OF
    | Op.Add => r := a + b;
    | Op.Sub => r := a - b;
    | Op.Mul => r := a * b;
    | Op.Div => r := a DIV b;
    | Op.Neg => r := -a;
    | Op.Abs => r := ABS (a);
    | Op.Inc => r := a;  INC (r, b);
    | Op.Dec => r := a;  DEC (r, b);
    END;
    RETURN r;
  END DoLong;

PROCEDURE OvfLong (name: TEXT;  k: Op;  a, b: LONGINT := 0L) =
  (* k on a and b must overflow *)
  VAR r: LONGINT;
  BEGIN
    TRY
      r := DoLong (k, a, b);
      Put (name, "FAIL: no overflow");
    EXCEPT RuntimeError.E (e) =>
      IF e = RuntimeError.T.IntegerOverflow
        THEN Put (name, "pass");
        ELSE Put (name, "FAIL: " & RuntimeError.Tag (e));
      END;
    END;
  END OvfLong;

PROCEDURE OkLong (name: TEXT;  k: Op;  a, b, expected: LONGINT) =
  (* k on a and b must give expected *)
  VAR r: LONGINT;
  BEGIN
    TRY
      r := DoLong (k, a, b);
      IF r = expected
        THEN Put (name, "pass");
        ELSE Put (name, "FAIL: wrong result");
      END;
    EXCEPT RuntimeError.E (e) =>
      Put (name, "FAIL: " & RuntimeError.Tag (e));
    END;
  END OkLong;

PROCEDURE TestLong () =
  VAR
    F := FIRST (LONGINT);  L := LAST (LONGINT);
    H := L DIV 2L;  (* 2^(n-2) - 1 *)
    T := L DIV 3L;
    zero := 0L;  one := 1L;  two := 2L;  three := 3L;  m1 := -1L;
  BEGIN
    OvfLong ("LONGINT LAST + 1", Op.Add, L, one);
    OvfLong ("LONGINT FIRST + -1", Op.Add, F, m1);
    OvfLong ("LONGINT (H+1) + (H+1)", Op.Add, H + one, H + one);
    OkLong  ("LONGINT LAST + FIRST", Op.Add, L, F, m1);
    OkLong  ("LONGINT H + (H+1)", Op.Add, H, H + one, L);

    OvfLong ("LONGINT FIRST - 1", Op.Sub, F, one);
    OvfLong ("LONGINT LAST - -1", Op.Sub, L, m1);
    OvfLong ("LONGINT 0 - FIRST", Op.Sub, zero, F);
    OkLong  ("LONGINT -1 - FIRST", Op.Sub, m1, F, L);
    OkLong  ("LONGINT 0 - LAST", Op.Sub, zero, L, F + one);

    OvfLong ("LONGINT LAST * 2", Op.Mul, L, two);
    OvfLong ("LONGINT FIRST * -1", Op.Mul, F, m1);
    OvfLong ("LONGINT -1 * FIRST", Op.Mul, m1, F);
    OvfLong ("LONGINT (H+1) * 2", Op.Mul, H + one, two);
    OvfLong ("LONGINT 2 * (H+1)", Op.Mul, two, H + one);
    OvfLong ("LONGINT (T+1) * 3", Op.Mul, T + one, three);
    OvfLong ("LONGINT LAST * LAST", Op.Mul, L, L);
    OvfLong ("LONGINT FIRST * FIRST", Op.Mul, F, F);
    OvfLong ("LONGINT (FIRST DIV 2) * -2", Op.Mul, F DIV two, -two);
    OkLong  ("LONGINT FIRST * 1", Op.Mul, F, one, F);
    OkLong  ("LONGINT -1 * LAST", Op.Mul, m1, L, F + one);
    OkLong  ("LONGINT H * 2", Op.Mul, H, two, L - one);
    OkLong  ("LONGINT T * 3", Op.Mul, T, three, L - one);
    OkLong  ("LONGINT (FIRST DIV 2) * 2", Op.Mul, F DIV two, two, F);
    OkLong  ("LONGINT 0 * FIRST", Op.Mul, zero, F, zero);
    OkLong  ("LONGINT FIRST * 0", Op.Mul, F, zero, zero);

    OvfLong ("LONGINT FIRST DIV -1", Op.Div, F, m1);
    OkLong  ("LONGINT FIRST DIV 1", Op.Div, F, one, F);
    OkLong  ("LONGINT LAST DIV -1", Op.Div, L, m1, F + one);

    OvfLong ("LONGINT -FIRST", Op.Neg, F);
    OkLong  ("LONGINT -LAST", Op.Neg, L, zero, F + one);
    OvfLong ("LONGINT ABS (FIRST)", Op.Abs, F);
    OkLong  ("LONGINT ABS (FIRST + 1)", Op.Abs, F + one, zero, L);

    OvfLong ("LONGINT INC (LAST)", Op.Inc, L, one);
    OvfLong ("LONGINT DEC (FIRST)", Op.Dec, F, one);
    OvfLong ("LONGINT INC (FIRST, -1)", Op.Inc, F, m1);
    OkLong  ("LONGINT INC (LAST, -1)", Op.Inc, L, m1, L - one);
  END TestLong;

PROCEDURE TestWord () =
  VAR L := LAST (INTEGER);  one := 1;
  BEGIN
    TRY
      IF Word.Plus (L, one) = FIRST (INTEGER)
        THEN Put ("Word.Plus (LAST, 1)", "pass");
        ELSE Put ("Word.Plus (LAST, 1)", "FAIL: wrong result");
      END;
      IF Word.Times (L, 2) = Word.Minus (0, 2)
        THEN Put ("Word.Times (LAST, 2)", "pass");
        ELSE Put ("Word.Times (LAST, 2)", "FAIL: wrong result");
      END;
    EXCEPT RuntimeError.E (e) =>
      Put ("Word arithmetic", "FAIL: " & RuntimeError.Tag (e));
    END;
  END TestWord;

BEGIN
  TestInt ();
  TestLong ();
  TestWord ();
END Main.
