(* NARROW (x, NULL) checks that x is NIL, for traced and untraced x;
   the front end used to skip the check, since NULL is not traced. *)

MODULE Main;
IMPORT IO, RuntimeError;
VAR r: REFANY := NIL;  u: UNTRACED REF INTEGER := NIL;  n := 0;
PROCEDURE Try(name: TEXT; x: REFANY) =
  BEGIN
    TRY EVAL NARROW(x, NULL); IO.Put(name & " ok\n")
    EXCEPT RuntimeError.E(e) => INC(n); IO.Put(name & " faults: " & RuntimeError.Tag(e) & "\n")
    END;
  END Try;
BEGIN
  Try("NIL", r);
  Try("REF", NEW(REF INTEGER));
  Try("TEXT", "t");
  TRY EVAL NARROW(u, NULL); IO.Put("untraced NIL ok\n") EXCEPT RuntimeError.E => INC(n); IO.Put("untraced NIL faults\n") END;
  u := NEW(UNTRACED REF INTEGER);
  TRY EVAL NARROW(u, NULL); IO.Put("untraced ok\n") EXCEPT RuntimeError.E => INC(n); IO.Put("untraced faults\n") END;
  IF n = 3 THEN IO.Put("OK\n") ELSE IO.Put("WRONG\n") END;
END Main.
