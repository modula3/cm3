(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Upthread;

FROM Ctypes IMPORT int;

TYPE
  pthread_t = ADDRESS;
  destructor_t = PROCEDURE(arg: ADDRESS);
  start_routine_t = PROCEDURE(arg: ADDRESS): ADDRESS;

<*EXTERNAL pthread_detach*> PROCEDURE detach (thread: pthread_t): int;
<*EXTERNAL pthread_self*> PROCEDURE self (): pthread_t;
<*EXTERNAL pthread_equal*> PROCEDURE equal (t1, t2: pthread_t): int;
<*EXTERNAL pthread_yield*> PROCEDURE yield (): int;
<*EXTERNAL pthread_kill*> PROCEDURE kill (thread: pthread_t; sig: int): int;

END Upthread.
