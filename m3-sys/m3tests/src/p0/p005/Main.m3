(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: basics of the Thread interface *)

MODULE Main;

IMPORT Fmt, Wr, Thread, Stdio;
FROM Test IMPORT checkI, done;
<*FATAL ANY*>

TYPE
  T = Thread.Closure BRANDED "T" OBJECT
        inc: INTEGER; END;

VAR 
  task1, task2: T;
  t1, t2      : Thread.T;
  i: INTEGER;
  m: Thread.Mutex;

PROCEDURE task (self: T) : REFANY RAISES {} =
BEGIN
 LOCK m DO
   i := i + self.inc;
   Wr.PutText (Stdio.stdout, "Changing i to " & Fmt.Int (i) & "\n");  END;
 RETURN NIL;
END task;

BEGIN

m := NEW (MUTEX);
task1 := NEW (T, apply := task, inc := 3);
task2 := NEW (T, apply := task, inc := 7);

i := 12;
Wr.PutText (Stdio.stdout, "i = " & Fmt.Int (i) & "\n");

t1 := Thread.Fork (task1);
t2 := Thread.Fork (task2);

EVAL Thread.Join (t1);
EVAL Thread.Join (t2);

Wr.PutText (Stdio.stdout, "i = " & Fmt.Int (i) & "\n");
checkI (i, 22);

done ();

END Main.




