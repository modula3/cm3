(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: more of the Thread interface *)

MODULE Main;

IMPORT Fmt, Wr, Thread, Stdio, Text, Time, ThreadF, Rd;
FROM Test IMPORT msg, done;

TYPE
  T = Thread.Closure BRANDED "p007 T" OBJECT
	id: Text.T; 
	pause: INTEGER := 1;
        limit: INTEGER := 50;
	END;

PROCEDURE Task1 (self: T): REFANY RAISES {} =
  VAR i: TEXT;
  BEGIN
    FOR j := 1 TO self.limit DO 
      msg ("Enter something");
      TRY 
        i := Rd.GetLine (Stdio.stdin);
        msg ("thanks for " & i & "$$$");
      EXCEPT 
      | Rd.EndOfFile => msg ("you don\'t want to speak, eh ?");
                        RETURN NIL; END; END;
    RETURN NIL;
  END Task1;

PROCEDURE Task2 (self: T): REFANY RAISES {} =
  BEGIN
    FOR i := 1 TO self.limit DO
      FOR i := 'A' TO 'Z' DO
       Wr.PutText (Stdio.stdout, Text.FromChar (i)); 
       Time.LongPause (self.pause); END;
      Wr.PutText (Stdio.stdout, "\n"); END;
    RETURN NIL;
  END Task2;

VAR
  t1, t2: T;
  th1, th2: Thread.T;

BEGIN

t1 := NEW (T, id := "A ", pause := 2, limit :=  3, apply := Task1);
t2 := NEW (T, id := "B ", pause := 1, limit :=  1, apply := Task2);

th1 := Thread.Fork (t1);
th2 := Thread.Fork (t2);

EVAL Thread.Join (th1);
EVAL Thread.Join (th2);

done ();

END Main.
