(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Upthread;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT size_t;
IMPORT Usysdep;

TYPE
  pthread_t = Usysdep.pthread_t;
  pthread_attr_t = Usysdep.pthread_attr_t;
  pthread_mutex_t = Usysdep.pthread_mutex_t;
  pthread_cond_t = Usysdep.pthread_cond_t;
  (* pthread_mutexattr_t = Usysdep.pthread_mutexattr_t; *)
  (* pthread_condattr_t = Usysdep.pthread_condattr_t; *)
  pthread_key_t = Usysdep.pthread_key_t;

  destructor_t = Usysdep.destructor_t;
  start_routine_t = Usysdep.start_routine_t;

CONST
  PTHREAD_MUTEX_INITIALIZER = Usysdep.PTHREAD_MUTEX_INITIALIZER;
  PTHREAD_COND_INITIALIZER = Usysdep.PTHREAD_COND_INITIALIZER;

<*EXTERNAL pthread_create*> PROCEDURE create (VAR pthread: pthread_t; READONLY attr: pthread_attr_t; start_routine: start_routine_t; arg: ADDRESS): int;
<*EXTERNAL pthread_detach*> PROCEDURE detach (thread: pthread_t): int;
<*EXTERNAL pthread_self*> PROCEDURE self (): pthread_t;
<*EXTERNAL pthread_equal*> PROCEDURE equal (t1, t2: pthread_t): int;
<*EXTERNAL pthread_attr_init*> PROCEDURE attr_init (VAR attr: pthread_attr_t): int;
<*EXTERNAL pthread_attr_destroy*> PROCEDURE attr_destroy (VAR attr: pthread_attr_t): int;
<*EXTERNAL pthread_attr_getstacksize*> PROCEDURE attr_getstacksize (READONLY attr: pthread_attr_t; VAR stacksize: size_t): int;
<*EXTERNAL pthread_attr_setstacksize*> PROCEDURE attr_setstacksize (VAR attr: pthread_attr_t; stacksize: size_t): int;
<*EXTERNAL pthread_yield*> PROCEDURE yield (): int;
(*<*EXTERNAL pthread_mutex_init*> PROCEDURE mutex_init (VAR mutex: pthread_mutex_t; attr: UNTRACED REF pthread_mutexattr_t): int;*)
  <*EXTERNAL pthread_mutex_init*> PROCEDURE mutex_init (VAR mutex: pthread_mutex_t; attr: ADDRESS := NIL): int;
<*EXTERNAL pthread_mutex_destroy*> PROCEDURE mutex_destroy (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_lock*> PROCEDURE mutex_lock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_unlock*> PROCEDURE mutex_unlock (VAR mutex: pthread_mutex_t): int;
(*<*EXTERNAL pthread_cond_init*> PROCEDURE cond_init (VAR cond: pthread_cond_t; attr: UNTRACED REF pthread_condattr_t): int;*)
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
