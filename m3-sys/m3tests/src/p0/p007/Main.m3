(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: more of the Thread interface *)

UNSAFE MODULE Main;

IMPORT Thread;
FROM RTIO IMPORT PutInt, PutText, Flush;

TYPE
  T = Thread.Closure BRANDED "p007 T" OBJECT
    id: INTEGER; 
    limit: INTEGER := 15;
  OVERRIDES
    apply := Task;
  END;

VAR
  first, last, next: INTEGER := 1;
  limit := 2000;
  c := NEW(Thread.Condition);
  m := NEW(Thread.Mutex);

CONST Pad = 5;

VAR count := 0;
PROCEDURE Inc() =
  BEGIN
    INC(count);
    PutText("\n");
    PutInt(count, Pad);
    PutText(": ");
    Flush();
  END Inc;

PROCEDURE Task (self: T) : REFANY RAISES {} =
BEGIN
  LOOP
    LOCK m DO
      WHILE next # self.id DO Thread.Wait (m, c); END;
      PutInt(self.id, Pad);
      DEC (self.limit);
      IF self.limit <= 0 THEN 
        IF self.id = limit THEN
          next := 0;
        ELSE
          first := self.id + 1;
          next := first;
        END;
        Thread.Broadcast(c);
        RETURN NIL;
      ELSIF self.id = last THEN
        Inc();
        IF self.id # limit THEN
          last := self.id + 1;
          EVAL Thread.Fork(NEW(T, id := last, limit := 15));
        END;
        next := first;
        Thread.Broadcast(c);
      ELSE
        next := self.id + 1;
        Thread.Broadcast(c);
      END;
    END;
  END;
END Task;

BEGIN
  LOCK m DO
    Inc();
    EVAL Thread.Fork(NEW(T, id := 1, limit := 15));
    Thread.Broadcast(c);
    WHILE next # 0 DO Thread.Wait(m, c) END;
    PutText("\nDone.\n");
    Flush();
  END;
END Main.
