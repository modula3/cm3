(* TRY/EXCEPT, TRY/FINALLY and LOCK: RETURN of a value and of a record
   through FINALLY, EXIT through FINALLY, EXCEPT ELSE catching RETURN, a
   FINALLY whose RETURN replaces a pending exception, an exception argument
   of type TEXT, and a re-raise through an enclosing FINALLY.  Exercises the
   MSIR exception scopes (D26) in both MSIR backends: lowered to LLVM
   (MSIRObj) and to M3CG (@M3m3front-msir @M3msircg). *)

MODULE Main;
IMPORT IO, Fmt;
EXCEPTION E (INTEGER);  F;  G (TEXT);
TYPE R = RECORD a, b: INTEGER; c: TEXT END;
VAR log := "";  mu := NEW (MUTEX);

PROCEDURE L (t: TEXT) = BEGIN log := log & t END L;

PROCEDURE Raise (n: INTEGER) RAISES {E, F, G} =
  BEGIN
    IF n = 0 THEN RAISE F END;
    IF n < 0 THEN RAISE G ("g" & Fmt.Int (-n)) END;
    RAISE E (n);
  END Raise;

PROCEDURE RetThroughFinally (n: INTEGER): INTEGER =
  BEGIN
    TRY
      IF n > 0 THEN RETURN n * 10 END;
      L ("b");
    FINALLY
      L ("f");
    END;
    RETURN -1;
  END RetThroughFinally;

PROCEDURE RecThroughFinally (n: INTEGER): R =
  BEGIN
    TRY
      TRY
        RETURN R {n, n + 1, "r" & Fmt.Int (n)};
      FINALLY
        L ("i");
      END;
    FINALLY
      L ("o");
    END;
  END RecThroughFinally;

PROCEDURE ExitThroughFinally (): INTEGER =
  VAR i := 0;
  BEGIN
    LOOP
      TRY
        INC (i);
        IF i = 3 THEN EXIT END;
      FINALLY
        L (Fmt.Int (i));
      END;
    END;
    RETURN i;
  END ExitThroughFinally;

PROCEDURE ElseCatchesReturn (n: INTEGER): INTEGER =
  BEGIN
    TRY
      IF n = 1 THEN RETURN 100 END;
      IF n = 2 THEN Raise (5) END;
    EXCEPT
    ELSE L ("else");
    END;
    RETURN n;
  END ElseCatchesReturn;

PROCEDURE FinallyReplaces (): INTEGER =
  BEGIN
    TRY
      TRY
        Raise (1);
      FINALLY
        RETURN 42;
      END;
    EXCEPT E => RETURN -1;
    END;
  END FinallyReplaces;

PROCEDURE Locked (n: INTEGER) RAISES {E, F, G} =
  BEGIN
    LOCK mu DO
      IF n # 0 THEN Raise (n) END;
      L ("l");
    END;
  END Locked;

PROCEDURE ArgText () =
  BEGIN
    TRY Raise (-3) EXCEPT G (t) => L (t) END;
  END ArgText;

PROCEDURE Reraise () RAISES {E} =
  BEGIN
    TRY
      TRY Raise (9) EXCEPT F => L ("?") END;
    FINALLY
      L ("rf");
    END;
  END Reraise;

BEGIN
  L (Fmt.Int (RetThroughFinally (2)));  L (Fmt.Int (RetThroughFinally (0)));  L ("|");
  VAR r := RecThroughFinally (7); BEGIN L (Fmt.Int (r.a + r.b) & r.c) END;  L ("|");
  L (Fmt.Int (ExitThroughFinally ()));  L ("|");
  FOR i := 0 TO 3 DO L (Fmt.Int (ElseCatchesReturn (i)) & ",") END;  L ("|");
  L (Fmt.Int (FinallyReplaces ()));  L ("|");
  TRY Locked (0); Locked (4) EXCEPT E (k) => L ("E" & Fmt.Int (k)) END;
  LOCK mu DO L ("relock") END;  L ("|");
  ArgText ();  L ("|");
  TRY Reraise () EXCEPT E (k) => L ("E" & Fmt.Int (k)) END;  L ("|");
  FOR i := 0 TO 3 DO
    TRY
      TRY Raise (i) FINALLY L ("f") END;
    EXCEPT
    | E (k) => L ("E" & Fmt.Int (k));
    | F     => L ("F");
    END;
  END;
  IO.Put (log & "\n");
END Main.
