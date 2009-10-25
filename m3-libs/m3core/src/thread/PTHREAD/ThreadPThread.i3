(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(*---------------------------------------------------------------------------*)

UNSAFE INTERFACE ThreadPThread;

FROM Ctypes IMPORT int;
FROM Cstddef IMPORT size_t;
FROM Upthread IMPORT pthread_t, start_routine_t;
FROM Utime IMPORT struct_timespec;

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
                        start_routine: start_routine_t; arg: ADDRESS): int;

(*---------------------------------------------------------------------------*)

(* implement the statically allocated mutexes, condition variables, and
thread locals. These are wrappers to:
 pthread_mutex_lock
 pthread_mutex_unlock
 pthread_cond_broadcast
 pthread_cond_wait
 pthread_key_create
 pthread_setspecific
 pthread_getspecific
 
where the parameters are all implied, and are indicated
by the last part of the function name.
This reduces platform specific code as it removes
the need for the Modula-3 code to define the static mutexes and condition variable(s).
*)

(* mutex "active" *)

<*EXTERNAL "ThreadPThread__pthread_mutex_lock_active"*>
PROCEDURE pthread_mutex_lock_active():int;

<*EXTERNAL "ThreadPThread__pthread_mutex_unlock_active"*>
PROCEDURE pthread_mutex_unlock_active():int;


(* mutex "slot" *)

<*EXTERNAL "ThreadPThread__pthread_mutex_lock_slots"*>
PROCEDURE pthread_mutex_lock_slots():int;

<*EXTERNAL "ThreadPThread__pthread_mutex_unlock_slots"*>
PROCEDURE pthread_mutex_unlock_slots():int;


(* mutex "init" *)

<*EXTERNAL "ThreadPThread__pthread_mutex_lock_init"*>
PROCEDURE pthread_mutex_lock_init():int;

<*EXTERNAL "ThreadPThread__pthread_mutex_unlock_init"*>
PROCEDURE pthread_mutex_unlock_init():int;


(* mutex "perf" *)

<*EXTERNAL "ThreadPThread__pthread_mutex_lock_perf"*>
PROCEDURE pthread_mutex_lock_perf():int;

<*EXTERNAL "ThreadPThread__pthread_mutex_unlock_perf"*>
PROCEDURE pthread_mutex_unlock_perf():int;


(* mutex "heap" *)

<*EXTERNAL "ThreadPThread__pthread_mutex_lock_heap"*>
PROCEDURE pthread_mutex_lock_heap():int;

<*EXTERNAL "ThreadPThread__pthread_mutex_unlock_heap"*>
PROCEDURE pthread_mutex_unlock_heap():int;


(* condition variable "heap" *)

<*EXTERNAL "ThreadPThread__pthread_cond_broadcast_heap"*>
PROCEDURE pthread_cond_broadcast_heap():int;

<*EXTERNAL "ThreadPThread__pthread_cond_wait_heap"*>
PROCEDURE pthread_cond_wait_heap():int;


(* thread local "activation" *)

<*EXTERNAL "ThreadPThread__pthread_key_create_activations"*>
PROCEDURE pthread_key_create_activations(): int;

<*EXTERNAL "ThreadPThread__pthread_setspecific_activations"*>
PROCEDURE pthread_setspecific_activations(value: ADDRESS): int;

<*EXTERNAL "ThreadPThread__pthread_getspecific_activations"*>
PROCEDURE pthread_getspecific_activations(): ADDRESS;

(*---------------------------------------------------------------------------*)

(* support for dynamically allocated mutexes and condition variables *)

TYPE
    pthread_mutex_t = ADDRESS;
    pthread_cond_t = ADDRESS;

<*EXTERNAL "ThreadPThread__pthread_mutex_new"*>
PROCEDURE pthread_mutex_new():pthread_mutex_t;

<*EXTERNAL "ThreadPThread__pthread_mutex_delete"*>
PROCEDURE pthread_mutex_delete(a:pthread_mutex_t);

<*EXTERNAL*>
PROCEDURE pthread_mutex_lock(mutex: pthread_mutex_t):int;

<*EXTERNAL*>
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

<*EXTERNAL "ThreadPThread__ProcessRegisters"*>
PROCEDURE ProcessRegisters(p: PROCEDURE(start, stop: ADDRESS)): ADDRESS;

<*EXTERNAL "ThreadPThread__ProcessState"*>
PROCEDURE ProcessState (t: pthread_t; sp: ADDRESS;
                        p: PROCEDURE(start, stop: ADDRESS)): ADDRESS;

(*---------------------------------------------------------------------------*)

END ThreadPThread.
