(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: more of the Thread interface *)

UNSAFE MODULE Main;

IMPORT Thread, ThreadF, RTIO;

TYPE
  T = Thread.Closure BRANDED "p007 T" OBJECT
	id: INTEGER; 
        limit: INTEGER := 15;
        thread: Thread.T;
      OVERRIDES
	apply := Task; END;

  A = MUTEX BRANDED "p007 common" OBJECT
	first, last, next, limit: INTEGER;
	done:   Thread.Condition;
        count:  INTEGER := 0;
       METHODS 
	Wait (c: Thread.Condition) := Thread.Wait; END;

VAR
  com: A;        
  stop: Thread.Condition;

PROCEDURE Txt (t: TEXT) =
  BEGIN
    ThreadF.SuspendOthers ();
    RTIO.PutText (t);
    ThreadF.ResumeOthers ();
  END Txt;
PROCEDURE Int (i: INTEGER;  width: INTEGER;  pad: TEXT) =
  BEGIN
    ThreadF.SuspendOthers ();
    RTIO.PutInt (i, width);
    RTIO.PutText (pad);
    ThreadF.ResumeOthers ();
  END Int;

(*******
PROCEDURE Task (self: T) : REFANY RAISES {} =
VAR next: T;
BEGIN
  LOOP
    TRY
      LOCK com DO
        WHILE (com.next # self.id) DO
          com.Wait (com.done); END;

        Int (self.id, 0, "#\n");
        DEC (self.limit);

        IF self.limit <= 0 THEN 
	  IF (self.id = com.limit) THEN
	    com.next := 0;
	  ELSE
    	    com.first := self.id + 1;
    	    com.next := com.first; END;
	  RETURN NIL;  
	
        ELSIF (self.id = com.last) THEN
          INC (com.count);
          (*Txt ("\n");*) Int (com.count, 5, "####\n");
	  IF (self.id # com.limit) THEN
       	    com.last := self.id + 1;
            next := NEW (T, id := com.last, limit := 15);
	    next.thread :=  Thread.Fork (next);
          END;
	  com.next := com.first;
        ELSE
	  com.next := self.id + 1;
        END; END;
    FINALLY
      Thread.Broadcast (com.done);
    END; END; 
END Task;
*****)

PROCEDURE Task (self: T) : REFANY RAISES {} =
VAR next: T;  done := FALSE;
BEGIN
  WHILE NOT done DO
    LOCK com DO
      WHILE (com.next # self.id) DO com.Wait (com.done); END;

      Int (self.id, 0, " ");
      DEC (self.limit);

      IF self.limit <= 0 THEN 
        IF (self.id = com.limit) THEN
          com.next := 0;
        ELSE
          com.first := self.id + 1;
          com.next := com.first;
        END;
        done := TRUE;
        
      ELSIF (self.id = com.last) THEN
        INC (com.count);
        Txt ("\n"); Int (com.count, 5, ": ");
        IF (self.id # com.limit) THEN
          com.last := self.id + 1;
          next := NEW (T, id := com.last, limit := 15);
          next.thread :=  Thread.Fork (next);
        END;
        com.next := com.first;
      ELSE
        com.next := self.id + 1;
      END;
    END; (*LOCK*)
    Thread.Broadcast (com.done);
  END;
  RETURN NIL;
END Task;

VAR 
  t: T;
  th: Thread.T;

BEGIN

stop := NEW (Thread.Condition);

com := NEW (A, limit := 2000);
com.done := NEW (Thread.Condition);
com.first := 1;
com.next := 1;
com.last := 1;
t := NEW (T, id := 1, limit := 15);

INC (com.count);
Int (com.count, 5, ": ");

th := Thread.Fork (t);
t.thread := th;
Thread.Broadcast (com.done);

LOOP
  LOCK com DO
    WHILE (com.next # 0) DO
      com.Wait (com.done); END;
    EXIT; END; END;

Txt("\nDone.\n");
RTIO.Flush ();

END Main.
