(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT done;

IMPORT Wr, Rd, Stdio, TextRd;
<*FATAL ANY*>

VAR  rd: Rd.T;

PROCEDURE FinAffiche () =
BEGIN
  Wr.PutChar (Stdio.stdout, '\n');
END FinAffiche;

PROCEDURE Foo (AfficheReste: PROCEDURE ()): BOOLEAN =
  VAR c: CHAR;

  PROCEDURE Affiche () =
  BEGIN
    Wr.PutChar (Stdio.stdout, c);
    AfficheReste ()
  END Affiche;

BEGIN
  TRY 
    c := Rd.GetChar (rd);
  EXCEPT
    | Rd.EndOfFile => IF AfficheReste # FinAffiche THEN AfficheReste (); END;
                      RETURN (FALSE); END;
  
  IF c = '\n' THEN
    AfficheReste ();
  ELSE
    EVAL Foo (Affiche); END;

  RETURN (TRUE);
END Foo;

BEGIN
  rd := TextRd.New ("This is a test...");
  WHILE Foo (FinAffiche) DO END;

  done ();
END Main.
