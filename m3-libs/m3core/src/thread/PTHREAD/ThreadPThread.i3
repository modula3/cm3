(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(*---------------------------------------------------------------------------*)

UNSAFE INTERFACE ThreadPThread;

FROM Ctypes IMPORT int;
FROM Cstddef IMPORT size_t;
FROM Utime IMPORT struct_timespec;

TYPE
  pthread_t = ADDRESS;
  pthread_mutex_t = ADDRESS;
  pthread_cond_t = ADDRESS;

(*---------------------------------------------------------------------------*)

PROCEDURE SignalHandler(sig: int);

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__SIG_SUSPEND"*>
(*CONST*) VAR SIG_SUSPEND: int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__SetupHandlers"*>
PROCEDURE SetupHandlers();

(*---------------------------------------------------------------------------*)

(* the semaphore is implied *)

<*EXTERNAL "ThreadPThread__sem_wait"*>
PROCEDURE sem_wait (): int;
<*EXTERNAL "ThreadPThread__sem_post"*>
PROCEDURE sem_post (): int;

<*EXTERNAL "ThreadPThread__sem_getvalue"*>
PROCEDURE sem_getvalue (VAR value: int): int;

(*---------------------------------------------------------------------------*)

(* the signal set is implied *)

<*EXTERNAL "ThreadPThread__sigsuspend"*>
PROCEDURE sigsuspend (): int;

(*---------------------------------------------------------------------------*)

(* pthread_create but replace attr with stackSize so that attr need not be known to Modula-3 *)

<*EXTERNAL "ThreadPThread__thread_create"*>
PROCEDURE thread_create(VAR pthread: pthread_t; stackSize: size_t;
                        start_routine: PROCEDURE(arg: ADDRESS): ADDRESS; arg: ADDRESS): int;

<*EXTERNAL "ThreadPThread__pthread_detach"*>
PROCEDURE pthread_detach(thread: pthread_t): int;

<*EXTERNAL "ThreadPThread__pthread_self"*>
PROCEDURE pthread_self(): pthread_t;

<*EXTERNAL "ThreadPThread__pthread_equal"*>
PROCEDURE pthread_equal(t1, t2: pthread_t): int;

<*EXTERNAL "ThreadPThread__pthread_kill"*>
PROCEDURE pthread_kill(t: pthread_t; sig: int): int;

(*---------------------------------------------------------------------------*)

(* static mutexes and conditions *)

<*EXTERNAL "ThreadPThread__activeMu"*> VAR activeMu: pthread_mutex_t;
<*EXTERNAL "ThreadPThread__slotsMu"*>  VAR slotsMu: pthread_mutex_t;
<*EXTERNAL "ThreadPThread__initMu"*>   VAR initMu: pthread_mutex_t;
<*EXTERNAL "ThreadPThread__perfMu"*>   VAR perfMu: pthread_mutex_t;
<*EXTERNAL "ThreadPThread__heapMu"*>   VAR heapMu: pthread_mutex_t;
<*EXTERNAL "ThreadPThread__heapCond"*> VAR heapCond: pthread_cond_t;

(* thread local "activation" *)

<*EXTERNAL "ThreadPThread__pthread_key_create_activations"*>
PROCEDURE pthread_key_create_activations(): int;

<*EXTERNAL "ThreadPThread__pthread_setspecific_activations"*>
PROCEDURE pthread_setspecific_activations(value: ADDRESS): int;

<*EXTERNAL "ThreadPThread__pthread_getspecific_activations"*>
PROCEDURE pthread_getspecific_activations(): ADDRESS;

(*---------------------------------------------------------------------------*)

(* support for dynamically allocated mutexes and condition variables *)

<*EXTERNAL "ThreadPThread__pthread_mutex_new"*>
PROCEDURE pthread_mutex_new():pthread_mutex_t;

<*EXTERNAL "ThreadPThread__pthread_mutex_delete"*>
PROCEDURE pthread_mutex_delete(a:pthread_mutex_t);

<*EXTERNAL ThreadPThread__pthread_mutex_lock*>
PROCEDURE pthread_mutex_lock(mutex: pthread_mutex_t):int;

<*EXTERNAL ThreadPThread__pthread_mutex_unlock*>
PROCEDURE pthread_mutex_unlock(mutex: pthread_mutex_t):int;

<*EXTERNAL "ThreadPThread__pthread_cond_new"*>
PROCEDURE pthread_cond_new():pthread_mutex_t;

<*EXTERNAL "ThreadPThread__pthread_cond_delete"*>
PROCEDURE pthread_cond_delete(a:pthread_cond_t);

<*EXTERNAL ThreadPThread__pthread_cond_wait*>
PROCEDURE pthread_cond_wait(cond: pthread_cond_t; mutex: pthread_mutex_t):int;

<*EXTERNAL ThreadPThread__pthread_cond_timedwait*>
PROCEDURE pthread_cond_timedwait(cond: pthread_cond_t;
                                 mutex: pthread_mutex_t;
                                 READONLY abs: struct_timespec):int;

<*EXTERNAL ThreadPThread__pthread_cond_signal*>
PROCEDURE pthread_cond_signal(cond: pthread_cond_t):int;

<*EXTERNAL ThreadPThread__pthread_cond_broadcast*>
PROCEDURE pthread_cond_broadcast(cond: pthread_cond_t):int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__Nanosleep"*>
PROCEDURE Nanosleep (READONLY req: struct_timespec; VAR rem: struct_timespec): int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__SuspendThread"*>
PROCEDURE SuspendThread (t: pthread_t): BOOLEAN;

<*EXTERNAL "ThreadPThread__RestartThread"*>
PROCEDURE RestartThread (t: pthread_t): BOOLEAN;

<*EXTERNAL "ThreadPThread__ProcessLive"*>
PROCEDURE ProcessLive(start, end: ADDRESS;
                      p: PROCEDURE(start, stop: ADDRESS));

<*EXTERNAL "ThreadPThread__ProcessStopped"*>
PROCEDURE ProcessStopped (t: pthread_t; start, end: ADDRESS;
                          p: PROCEDURE(start, end: ADDRESS));

<*EXTERNAL "ThreadPThread__SaveRegsInStack"*>
PROCEDURE SaveRegsInStack (): ADDRESS;

(*---------------------------------------------------------------------------*)

END ThreadPThread.
