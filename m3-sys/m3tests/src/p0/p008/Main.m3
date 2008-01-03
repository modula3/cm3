(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: still more of Thread (alerts) *)

MODULE Main;

IMPORT Thread;
FROM Test IMPORT msg, done;

TYPE
  T = Thread.Closure BRANDED "p008 T" OBJECT father: Thread.T; END;

VAR
  m : MUTEX;
  cl: T;
  c : Thread.Condition;
  th: Thread.T;

PROCEDURE TestAlert (self: T): REFANY RAISES {} =
BEGIN
  WHILE NOT Thread.TestAlert () DO
    IF self.father # NIL THEN
      Thread.Alert (self.father); END; END;
  RETURN NIL;
END TestAlert;


BEGIN

m := NEW (MUTEX);
c := NEW (Thread.Condition);

msg ("--- TestAlert");

cl := NEW (T, apply := TestAlert, father := NIL);
th := Thread.Fork (cl);

Thread.Alert (th);
EVAL Thread.Join (th);

msg ("--- AlertWait");

cl := NEW (T, apply := TestAlert, father := Thread.Self ());
th := Thread.Fork (cl);

TRY 
  LOCK m DO
    Thread.AlertWait (m, c); END;
EXCEPT
  Thread.Alerted => END;

Thread.Alert (th);
EVAL Thread.Join (th);

msg ("--- AlertJoin");

cl := NEW (T, apply := TestAlert, father := Thread.Self ());
th := Thread.Fork (cl);

TRY
  EVAL Thread.AlertJoin (th);
EXCEPT 
  | Thread.Alerted => END;

done ();

END Main.
