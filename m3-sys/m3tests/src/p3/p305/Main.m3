MODULE Main;

(* TRY with a named handler and an ELSE: an EXIT or RETURN in the body is
   caught by the ELSE clause.  On ex_stack targets the handler tests used
   to read the exception through the EXIT/RETURN marker and crash. *)

IMPORT IO, Fmt;
EXCEPTION E; F;

PROCEDURE Ret (): INTEGER =
  BEGIN
    TRY RETURN 1;
    EXCEPT E => RETURN 2;
    ELSE RETURN 3;
    END;
  END Ret;

PROCEDURE RetFall (): INTEGER =
  BEGIN
    TRY RETURN 1;
    EXCEPT E => RETURN 2;
    ELSE
    END;
    RETURN 4;
  END RetFall;

PROCEDURE Raise (x: INTEGER): TEXT =
  BEGIN
    TRY
      IF x = 1 THEN RAISE E END;
      IF x = 2 THEN RAISE F END;
      IF x = 3 THEN RETURN "r" END;
      RETURN "n";
    EXCEPT E => RETURN "e";
    ELSE RETURN "else";
    END;
  END Raise;

PROCEDURE Inner (x: INTEGER): TEXT =
  (* the inner TRY forwards unmatched exceptions to the outer one *)
  BEGIN
    TRY
      TRY
        IF x = 1 THEN RAISE E END;
        IF x = 2 THEN RAISE F END;
        IF x = 3 THEN RETURN "r" END;
      EXCEPT F => RETURN "f";
      END;
      RETURN "n";
    EXCEPT E => RETURN "e";
    ELSE RETURN "else";
    END;
  END Inner;

VAR i, n := 0;
BEGIN
  IO.Put (Fmt.Int (Ret ()) & " " & Fmt.Int (RetFall ()) & "\n");
  WHILE i < 3 DO
    INC (i);
    TRY
      IF i = 2 THEN RAISE E END;
      EXIT;
    EXCEPT E => IO.Put ("E,");
    ELSE INC (n); IO.Put ("else,");
    END;
  END;
  IO.Put (Fmt.Int (n) & "\n");
  FOR x := 0 TO 3 DO IO.Put (Raise (x) & " ") END; IO.Put ("\n");
  FOR x := 0 TO 3 DO IO.Put (Inner (x) & " ") END; IO.Put ("\n");
END Main.
