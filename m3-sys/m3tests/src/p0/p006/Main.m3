(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: more of the Thread interface *)

MODULE Main;

IMPORT Fmt, Wr, Thread, Stdio;
<*FATAL ANY*>

TYPE
  T = Thread.Closure BRANDED "T" OBJECT
        inc, limit: INTEGER; 
	me, other: INTEGER; 
	bad: BOOLEAN;
	END;

  A = Thread.Mutex BRANDED "A" OBJECT
	myTurn: INTEGER;
	done:   Thread.Condition;
        val:    INTEGER; 
       METHODS 
	Wait (c: Thread.Condition) := Thread.Wait; END;

VAR 
  task1, task2, task3: T;
  t1, t2, t3      : Thread.T;

  com: A;        

PROCEDURE task (self: T) : REFANY RAISES {} =
BEGIN
  LOOP
    LOCK com DO
      TRY
        WHILE (NOT self.bad) AND (com.myTurn # self.me) DO
          com.Wait (com.done); END;
        INC (com.val, self.inc);
        com.myTurn := self.other;
        Wr.PutText (Stdio.stdout, Fmt.Int (self.me) & "> Changing val to " &
			        Fmt.Int (com.val) & "\n"); 
        IF com.val > self.limit THEN
    	  Wr.PutText (Stdio.stdout, Fmt.Int (self.me) & "> Done.\n");
	  RETURN NIL; END; 
      FINALLY
        Thread.Broadcast (com.done); END; END; END;
END task;

BEGIN

task1 := NEW (T, apply := task, me := 1, other := 2, inc := 3, limit := 100);
task2 := NEW (T, apply := task, me := 2, other := 3, inc := 7, limit := 100);
task3 := NEW (T, apply := task, me := 3, other := 1, inc := 17, limit := 100);

com := NEW (A);
com.done := NEW (Thread.Condition);

com.myTurn := 1;
com.val := 12;
Wr.PutText (Stdio.stdout, "R> val = " & Fmt.Int (com.val) & "\n");

t1 := Thread.Fork (task1);
t2 := Thread.Fork (task2);
t3 := Thread.Fork (task3);

EVAL Thread.Join (t1);
EVAL Thread.Join (t2);
EVAL Thread.Join (t3);

Wr.PutText (Stdio.stdout, "R> val = " & Fmt.Int (com.val) & "\n");

Wr.Close (Stdio.stdout);
END Main.




