(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
UNSAFE MODULE RTMachine;

IMPORT Uthread;
FROM Upthread IMPORT pthread_t;
FROM Uucontext IMPORT stack_t;
FROM Ctypes IMPORT int;

PROCEDURE SuspendThread (t: pthread_t) =
  BEGIN
    WITH res = Uthread.suspend(t) DO <*ASSERT res = 0*> END;
  END SuspendThread;

PROCEDURE RestartThread (t: pthread_t) =
  BEGIN
    WITH res = Uthread.continue(t) DO <*ASSERT res = 0*> END;
  END RestartThread;

PROCEDURE GetState(t: pthread_t; VAR sp: ADDRESS; VAR state: ThreadState) =
  VAR
    flag: int;
    lwp: Uthread.lwpid_t;
    ss: stack_t;
  BEGIN
    WITH res = Uthread.getstate(t, flag, lwp, ss, state) DO
      <*ASSERT res = 0*>
    END;
    sp := LOOPHOLE(state.sp, ADDRESS);
  END GetState;

BEGIN
END RTMachine.
