(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Upthread;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT size_t;
IMPORT Usysdep;

TYPE
  pthread_t = Usysdep.pthread_t;
  pthread_mutex_t = Usysdep.pthread_mutex_t;
  pthread_cond_t = Usysdep.pthread_cond_t;
  pthread_key_t = Usysdep.pthread_key_t;

  destructor_t = PROCEDURE(arg: ADDRESS);
  start_routine_t = PROCEDURE(arg: ADDRESS): ADDRESS;

<*EXTERNAL "Upthread_PTHREAD_MUTEX_INITIALIZER"*> VAR PTHREAD_MUTEX_INITIALIZER : pthread_mutex_t;
<*EXTERNAL "Upthread_PTHREAD_COND_INITIALIZER"*> VAR PTHREAD_COND_INITIALIZER : pthread_cond_t;

<*EXTERNAL pthread_detach*> PROCEDURE detach (thread: pthread_t): int;
<*EXTERNAL pthread_self*> PROCEDURE self (): pthread_t;
<*EXTERNAL pthread_equal*> PROCEDURE equal (t1, t2: pthread_t): int;
<*EXTERNAL pthread_yield*> PROCEDURE yield (): int;
<*EXTERNAL pthread_mutex_init*> PROCEDURE mutex_init (VAR mutex: pthread_mutex_t; attr: ADDRESS := NIL): int;
<*EXTERNAL pthread_mutex_destroy*> PROCEDURE mutex_destroy (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_lock*> PROCEDURE mutex_lock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_unlock*> PROCEDURE mutex_unlock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_cond_init*> PROCEDURE cond_init (VAR cond: pthread_cond_t; attr: ADDRESS := NIL): int;
<*EXTERNAL pthread_cond_destroy*> PROCEDURE cond_destroy (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_wait*> PROCEDURE cond_wait (VAR cond: pthread_cond_t; VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_cond_signal*> PROCEDURE cond_signal (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_key_create*> PROCEDURE key_create (VAR key: pthread_key_t; destructor: destructor_t): int;
<*EXTERNAL pthread_key_delete*> PROCEDURE key_delete (key: pthread_key_t): int;
<*EXTERNAL pthread_getspecific*> PROCEDURE getspecific (key: pthread_key_t): ADDRESS;
<*EXTERNAL pthread_setspecific*> PROCEDURE setspecific (key: pthread_key_t; value: ADDRESS): int;
<*EXTERNAL pthread_kill*> PROCEDURE kill (thread: pthread_t; sig: int): int;

END Upthread.
