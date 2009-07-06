(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Upthread;

FROM Ctypes IMPORT int;

TYPE
  pthread_t = ADDRESS;
  destructor_t = PROCEDURE(arg: ADDRESS);
  start_routine_t = PROCEDURE(arg: ADDRESS): ADDRESS;

<*EXTERNAL Upthread__detach*> PROCEDURE detach (thread: pthread_t): int;
<*EXTERNAL Upthread__self*> PROCEDURE self (): pthread_t;
<*EXTERNAL Upthread__equal*> PROCEDURE equal (t1, t2: pthread_t): int;
<*EXTERNAL Upthread__kill*> PROCEDURE kill (thread: pthread_t; sig: int): int;

END Upthread.
