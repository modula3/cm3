(*
 * Copyright (c) 1993, 1996, 1998 by Sun Microsystems, Inc.
 * All Rights Reserved
 *)

INTERFACE Uthread;

FROM Uucontext IMPORT stack_t, gregset_t;
FROM Ctypes IMPORT unsigned_int, long, int;
FROM Utypes IMPORT size_t;

(*
 * thread.h:
 * definitions needed to use the thread interface except synchronization.
 * use <synch.h> for thread synchronization.
 *)

TYPE
  thread_t = unsigned_int;
  thread_key_t = unsigned_int;
  lwpid_t = unsigned_int;

TYPE start_func_t = PROCEDURE(arg: ADDRESS): ADDRESS;
<*EXTERNAL thr_create*>
PROCEDURE create (stack_base: ADDRESS;
                  stack_size: size_t;
                  start_func: start_func_t;
                  arg: ADDRESS;
                  flags: long;
                  VAR new_thread_ID: thread_t): int;
<*EXTERNAL thr_join*>
PROCEDURE join (thread: thread_t;
                VAR departed: thread_t;
                VAR status: ADDRESS): int;
<*EXTERNAL thr_setconcurrency*>
PROCEDURE setconcurrency (concurrency: int): int;
<*EXTERNAL thr_getconcurrency*>
PROCEDURE getconcurrency (): int;
<*EXTERNAL thr_exit*>
PROCEDURE exit (status: ADDRESS);
<*EXTERNAL thr_self*>
PROCEDURE self (): thread_t;
<*EXTERNAL thr_main*>
PROCEDURE main (): int;
<*EXTERNAL thr_kill*>
PROCEDURE kill (thread: thread_t;
                sig: int): int;
<*EXTERNAL thr_suspend*>
PROCEDURE suspend (thread: thread_t): int;
<*EXTERNAL thr_continue*>
PROCEDURE continue (thread: thread_t): int;
<*EXTERNAL thr_yield*>
PROCEDURE yield ();
<*EXTERNAL thr_setprio*>
PROCEDURE setprio (thread: thread_t; priority: int): int;
<*EXTERNAL thr_getprio*>
PROCEDURE thr_getprio (thread: thread_t; VAR priority: int): int;
TYPE destructor_t = PROCEDURE(value: ADDRESS);
<*EXTERNAL thr_keycreate*>
PROCEDURE keycreate (VAR key: thread_key_t;
                     destructor: destructor_t): int;
<*EXTERNAL thr_setspecific*>
PROCEDURE setspecific (key: thread_key_t;
                       value: ADDRESS): int;
<*EXTERNAL thr_getspecific*>
PROCEDURE getspecific (key: thread_key_t;
                       VAR value: ADDRESS): int;
<*EXTERNAL thr_min_stack*>
PROCEDURE min_stack(): size_t;

(*
 * thread flags (one word bit mask)
 *)
(*
 * POSIX.1c Note:
 * THR_BOUND is defined same as PTHREAD_SCOPE_SYSTEM in <pthread.h>
 * THR_DETACHED is defined same as PTHREAD_CREATE_DETACHED in <pthread.h>
 * Any changes in these definitions should be reflected in <pthread.h>
 *)
CONST
  THR_BOUND     = 16_00000001;		 (* = PTHREAD_SCOPE_SYSTEM *)
  THR_NEW_LWP   = 16_00000002;
  THR_DETACHED  = 16_00000040;		 (* = PTHREAD_CREATE_DETACHED *)
  THR_SUSPENDED = 16_00000080;
  THR_DAEMON    = 16_00000100;

(*
 * The available register states returned by thr_getstate().
 *)
CONST
  TRS_VALID = 0;
  TRS_NONVOLATILE = 1;
  TRS_LWPID = 2;
  TRS_INVALID = 3;

<*EXTERNAL thr_getstate*>
PROCEDURE getstate(thread: thread_t; VAR flag: int; VAR lwp: lwpid_t;
                   VAR ss: stack_t; VAR rs: gregset_t): int;

END Uthread.
