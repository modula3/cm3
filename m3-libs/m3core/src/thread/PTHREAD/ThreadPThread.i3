(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(*---------------------------------------------------------------------------*)

UNSAFE INTERFACE ThreadPThread;

FROM Ctypes IMPORT const_int, int, void_star;
FROM Cstddef IMPORT size_t;

TYPE
  (* These are opaque C references (not necessarily UNTRACED REF ADDRESS) *)
  pthread_t = UNTRACED BRANDED REF ADDRESS;
  pthread_mutex_t = UNTRACED BRANDED REF ADDRESS;
  pthread_cond_t = UNTRACED BRANDED REF ADDRESS;
  Activation <: ADDRESS; (* untraced thread stated stored in thread local *)
  const_pthread_mutex_t = pthread_mutex_t;
  const_pthread_cond_t = pthread_cond_t;

(*---------------------------------------------------------------------------*)

PROCEDURE SignalHandler (sig: int; context: ADDRESS);

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__SIG_SUSPEND"*>
VAR SIG_SUSPEND: const_int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__InitC"*>
PROCEDURE InitC(top: ADDRESS);

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
PROCEDURE sigsuspend ();

(*---------------------------------------------------------------------------*)

(* pthread_create but replace attr with stackSize so that attr need not be known to Modula-3 *)

TYPE start_routine_t = PROCEDURE(arg: ADDRESS): ADDRESS;

<*EXTERNAL "ThreadPThread__thread_create"*>
PROCEDURE thread_create(stackSize: size_t; start_routine: start_routine_t; arg: void_star): int;

<*EXTERNAL ThreadPThread__pthread_detach_self*>
PROCEDURE pthread_detach_self(t: pthread_t): int;

<*EXTERNAL ThreadPThread__pthread_self*>
PROCEDURE pthread_self(): pthread_t;

<*EXTERNAL "ThreadPThread__pthread_kill"*>
PROCEDURE pthread_kill(t: pthread_t; sig: int): int;

(*---------------------------------------------------------------------------*)

(* static mutexes and conditions *)

<*EXTERNAL "ThreadPThread__activeMu"*> VAR activeMu: const_pthread_mutex_t;
<*EXTERNAL "ThreadPThread__slotsMu"*>  VAR slotsMu: const_pthread_mutex_t;
<*EXTERNAL "ThreadPThread__initMu"*>   VAR initMu: const_pthread_mutex_t;
<*EXTERNAL "ThreadPThread__perfMu"*>   VAR perfMu: const_pthread_mutex_t;
<*EXTERNAL "ThreadPThread__heapMu"*>   VAR heapMu: const_pthread_mutex_t;
<*EXTERNAL "ThreadPThread__heapCond"*> VAR heapCond: const_pthread_cond_t;

(* thread local "activation" *)

<*EXTERNAL ThreadPThread__SetActivation*>
PROCEDURE SetActivation(value: Activation);

<*EXTERNAL ThreadPThread__GetActivation*>
PROCEDURE GetActivation(): Activation;

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
PROCEDURE pthread_cond_new(): pthread_cond_t;

<*EXTERNAL "ThreadPThread__pthread_cond_delete"*>
PROCEDURE pthread_cond_delete(cond: pthread_cond_t);

<*EXTERNAL ThreadPThread__pthread_cond_wait*>
PROCEDURE pthread_cond_wait(cond: pthread_cond_t; mutex: pthread_mutex_t):int;

<*EXTERNAL ThreadPThread__pthread_cond_timedwait*>
PROCEDURE pthread_cond_timedwait(cond: pthread_cond_t;
                                 mutex: pthread_mutex_t;
                                 abs: LONGREAL(*Time.T*)):int;

<*EXTERNAL ThreadPThread__pthread_cond_signal*>
PROCEDURE pthread_cond_signal(cond: pthread_cond_t):int;

<*EXTERNAL ThreadPThread__pthread_cond_broadcast*>
PROCEDURE pthread_cond_broadcast(cond: pthread_cond_t):int;

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__Nanosleep"*>
PROCEDURE Nanosleep(nanoseconds: INTEGER);

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__SuspendThread"*>
PROCEDURE SuspendThread (t: pthread_t): BOOLEAN;

<*EXTERNAL "ThreadPThread__RestartThread"*>
PROCEDURE RestartThread (t: pthread_t): BOOLEAN;

TYPE ProcessThreadStack = PROCEDURE(start, limit: ADDRESS);

<*EXTERNAL "ThreadPThread__ProcessLive"*>
PROCEDURE ProcessLive (bottom: ADDRESS; p: ProcessThreadStack);

<*EXTERNAL "ThreadPThread__ProcessStopped"*>
PROCEDURE ProcessStopped (t: pthread_t; bottom, context: ADDRESS; p: ProcessThreadStack);
(*---------------------------------------------------------------------------*)
(* coroutine support *)

PROCEDURE SetCoStack(toStack     : ADDRESS;
                     (* address of StackState record we're going TO *)
                     
                     topOfStack : ADDRESS
                     (* address of top of stack we're coming FROM,
                        must have the context pushed before being passed here,
                        so that we have the register state on the stack *));
  (* to denote that we have/are about to switch stacks on a coroutine switch.

  NOTE THAT:        
  
  when the stack disagrees with the execution context is a critical 
  section for GC 

  therefore, clients MUST call IncInCritical before SetCoStack.

  The pattern is:

  IncInCritical()
  SetCoStack(...)
  (* jump to new context *)
  (* running in new context *)
  DecInCritical()
*)
  
PROCEDURE GetStackState() : ADDRESS;
  (* current stack state *)

PROCEDURE GetCurStackBase() : ADDRESS;
  (* threading library's idea of what our current stack base is 
     --- useful for assertions, if nothing else *)

PROCEDURE CreateStackState(base : ADDRESS; context : ADDRESS) : ADDRESS;
  (* create a new stack state *)

PROCEDURE DisposeStack(stack : ADDRESS);
  (* destroy a stack state *)

PROCEDURE IncInCritical();
  (* increment inCritical semaphore for my thread *)

PROCEDURE DecInCritical();
  (* deccrement inCritical semaphore for my thread *)

(*---------------------------------------------------------------------------*)

<*EXTERNAL "ThreadPThread__Solaris"*>
PROCEDURE Solaris(): BOOLEAN;

PROCEDURE AtForkPrepareOutsideFork(); (* called from C *)

(*---------------------------------------------------------------------------*)

END ThreadPThread.
