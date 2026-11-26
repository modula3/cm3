(* RAISE with an argument too large for a word (a record, passed to the
   exception's raise procedure by reference), inside a TRY whose body has
   no other call.  With ex_stack, the raise must be an invoke of the
   handler; otherwise the TRY gets no handler and the exception escapes. *)

MODULE Main;
IMPORT Test;

TYPE Pair = RECORD a, b: INTEGER END;

EXCEPTION E (Pair);

VAR g := NEW (REF Pair);

PROCEDURE FromConstructor (): INTEGER =
  BEGIN
    TRY RAISE E (Pair {1, 2}) EXCEPT E (p) => RETURN p.a + p.b END;
  END FromConstructor;

PROCEDURE FromLocal (): INTEGER =
  VAR l := Pair {3, 4};
  BEGIN
    TRY RAISE E (l) EXCEPT E (p) => RETURN p.a + p.b END;
  END FromLocal;

PROCEDURE FromLocalRef (): INTEGER =
  VAR h := NEW (REF Pair, a := 5, b := 6);
  BEGIN
    TRY RAISE E (h^) EXCEPT E (p) => RETURN p.a + p.b END;
  END FromLocalRef;

PROCEDURE FromGlobalRef (): INTEGER =
  BEGIN
    TRY RAISE E (g^) EXCEPT E (p) => RETURN p.a + p.b END;
  END FromGlobalRef;

PROCEDURE Nested (): INTEGER =
  VAR n := 0;
  BEGIN
    TRY
      TRY RAISE E (Pair {7, 8}) FINALLY INC (n) END;
    EXCEPT E (p) => RETURN n + p.a + p.b
    END;
  END Nested;

PROCEDURE Else (): INTEGER =
  BEGIN
    TRY RAISE E (Pair {9, 10}) EXCEPT ELSE RETURN 19 END;
  END Else;

BEGIN
  g.a := 7;  g.b := 8;
  Test.checkI (FromConstructor (), 3);
  Test.checkI (FromLocal (), 7);
  Test.checkI (FromLocalRef (), 11);
  Test.checkI (FromGlobalRef (), 15);
  Test.checkI (Nested (), 16);
  Test.checkI (Else (), 19);
  TRY RAISE E (Pair {11, 12}) EXCEPT E (p) => Test.checkI (p.a + p.b, 23) END;
  Test.done ();
END Main.
