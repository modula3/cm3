(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Upthread;

FROM Ctypes IMPORT long, char, unsigned_long, int;
FROM Utypes IMPORT size_t;
FROM Usignal IMPORT sigset_t;
FROM Utime IMPORT struct_timespec;

(* <pthread_impl.h> *)

(*
 * Internal implementation details
 *)

(* This whole header file will disappear, so don't depend on it... *)

(*
 * [Internal] data structure signatures
 *)
CONST
  PTHREAD_MUTEX_SIG_init = 16_32AAABA7;
  PTHREAD_COND_SIG_init  = 16_3CB0B1BB;
  PTHREAD_ONCE_SIG_init  = 16_30B1BCBA;
(*
 * POSIX scheduling policies
 *)
CONST
  SCHED_OTHER = 1;
  SCHED_FIFO  = 4;
  SCHED_RR    = 2;

  SCHED_PARAM_SIZE = 4;

(* <sys/types.h> *)
TYPE
  pthread_handler = PROCEDURE(arg: ADDRESS);
  struct_pthread_handler_rec = RECORD
    routine: pthread_handler;		 (* routine to call *)
    arg: ADDRESS;			 (* argument to pass *)
    next: UNTRACED REF struct_pthread_handler_rec;
  END;

CONST
  PTHREAD_SIZE            = 596;
  PTHREAD_ATTR_SIZE       =  36;
  PTHREAD_MUTEXATTR_SIZE  =   8;
  PTHREAD_MUTEX_SIZE      =  40;
  PTHREAD_CONDATTR_SIZE   =   4;
  PTHREAD_COND_SIZE       =  24;
  PTHREAD_ONCE_SIZE       =   4;
  PTHREAD_RWLOCK_SIZE     = 124;
  PTHREAD_RWLOCKATTR_SIZE =  12;

TYPE
  struct_opaque_pthread_t = RECORD
    sig: long;
    opaque: ARRAY [0..PTHREAD_SIZE-1] OF char;
  END;
  pthread_t = UNTRACED REF struct_opaque_pthread_t;

  struct_opaque_pthread_attr_t = RECORD
    sig: long;
    opaque: ARRAY [0..PTHREAD_ATTR_SIZE-1] OF char;
  END;
  pthread_attr_t = struct_opaque_pthread_attr_t;

  struct_opaque_pthread_mutexattr_t = RECORD
    sig: long;
    opaque: ARRAY [0..PTHREAD_MUTEXATTR_SIZE-1] OF char;
  END;
  pthread_mutexattr_t = struct_opaque_pthread_mutexattr_t;

  struct_opaque_pthread_mutex_t = RECORD
    sig: long := PTHREAD_MUTEX_SIG_init;
    opaque := ARRAY [0..PTHREAD_MUTEX_SIZE-1] OF char { 0, ..};
  END;
  pthread_mutex_t = struct_opaque_pthread_mutex_t;

  struct_opaque_pthread_condattr_t = RECORD
    sig: long;
    opaque: ARRAY [0..PTHREAD_CONDATTR_SIZE-1] OF char;
  END;
  pthread_condattr_t = struct_opaque_pthread_condattr_t;

  struct_opaque_pthread_cond_t = RECORD
    sig: long := PTHREAD_COND_SIG_init;
    opaque := ARRAY [0..PTHREAD_COND_SIZE-1] OF char { 0, ..};
  END;
  pthread_cond_t = struct_opaque_pthread_cond_t;
  
  struct_opaque_pthread_rwlockattr_t = RECORD
    sig: long;
    opaque: ARRAY [0..PTHREAD_RWLOCKATTR_SIZE-1] OF char;
  END;
  pthread_rwlockattr_t = struct_opaque_pthread_rwlockattr_t;
  
  struct_opaque_pthread_rwlock_t = RECORD
    sig: long;
    opaque: ARRAY [0..PTHREAD_RWLOCK_SIZE-1] OF char;
  END;
  pthread_rwlock_t = struct_opaque_pthread_rwlock_t;

  struct_opaque_pthread_once_t = RECORD
    sig: long := PTHREAD_ONCE_SIG_init;
    opaque := ARRAY [0..PTHREAD_ONCE_SIZE-1] OF char { 0, ..};
  END;
  pthread_once_t = struct_opaque_pthread_once_t;

  pthread_key_t = unsigned_long;

  struct_sched_param = RECORD
    sched_priority: int;
    opaque: ARRAY [0..SCHED_PARAM_SIZE-1] OF char;
  END;

(* <pthread.h> *)

(*
 * Thread attributes
 *)
CONST
  PTHREAD_CREATE_JOINABLE = 1;
  PTHREAD_CREATE_DETACHED = 2;

  PTHREAD_INHERIT_SCHED  = 1;
  PTHREAD_EXPLICIT_SCHED = 2;

  PTHREAD_CANCEL_ENABLE = 16_01; (* Cancel takes place at next cancellation point *)
  PTHREAD_CANCEL_DISABLE = 16_00; (* Cancel postponed *)
  PTHREAD_CANCEL_DEFERRED = 16_02; (* Cancel waits until cancellation point *)
  PTHREAD_CANCEL_ASYNCHRONOUS = 16_00; (* Cancel occurs immediately *)

  (* We only support PTHREAD_SCOPE_SYSTEM *)
  PTHREAD_SCOPE_SYSTEM  = 1;
  PTHREAD_SCOPE_PROCESS = 2;

  (* We only support PTHREAD_PROCESS_PRIVATE *)
  PTHREAD_PROCESS_SHARED  = 1;
  PTHREAD_PROCESS_PRIVATE = 2;

(*
 * Mutex protocol attributes
 *)
CONST
  PTHREAD_PRIO_NONE    = 0;
  PTHREAD_PRIO_INHERIT = 1;
  PTHREAD_PRIO_PROTECT = 2;

(*
 * Mutex type attributes
 *)
CONST
  PTHREAD_MUTEX_NORMAL     = 0;
  PTHREAD_MUTEX_ERRORCHECK = 1;
  PTHREAD_MUTEX_RECURSIVE  = 2;
  PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL;

(*
 * Mutex variables
 *)
CONST
  PTHREAD_MUTEX_INITIALIZER =
    pthread_mutex_t { PTHREAD_MUTEX_SIG_init,
                      ARRAY [0..PTHREAD_MUTEX_SIZE-1] OF char {0, .. } };

(*
 * Condition variables
 *)
CONST
  PTHREAD_COND_INITIALIZER =
    pthread_cond_t { PTHREAD_COND_SIG_init,
                     ARRAY [0..PTHREAD_COND_SIZE-1] OF char {0, .. } };

(*
 * Initialization control (once) variables
 *)
CONST
  PTHREAD_ONCE_INITIALIZER =
    pthread_once_t { PTHREAD_ONCE_SIG_init,
                     ARRAY [0..PTHREAD_ONCE_SIZE-1] OF char {0, .. } };

(*
 * Prototypes for all PTHREAD interfaces
 *)
<*EXTERNAL pthread_attr_destroy*>
PROCEDURE attr_destroy (VAR attr: pthread_attr_t): int;
<*EXTERNAL pthread_attr_getdetachstate*>
PROCEDURE attr_getdetachstate (READONLY attr: pthread_attr_t;
                               VAR detachstate: int): int;
<*EXTERNAL pthread_attr_getinheritsched*>
PROCEDURE attr_getinheritsched (READONLY attr: pthread_attr_t;
                                VAR inheritsched: int): int;
<*EXTERNAL pthread_attr_getschedparam*>
PROCEDURE attr_getschedparam (READONLY attr: pthread_attr_t;
                              VAR param: struct_sched_param): int;
<*EXTERNAL pthread_attr_getschedpolicy*>
PROCEDURE attr_getschedpolicy (READONLY attr: pthread_attr_t;
                               VAR policy: int): int;
<*EXTERNAL pthread_attr_getstackaddr*>
PROCEDURE attr_getstackaddr (READONLY attr: pthread_attr_t;
                             VAR stackaddr: ADDRESS): int;
<*EXTERNAL pthread_attr_getstacksize*>
PROCEDURE attr_getstacksize (READONLY attr: pthread_attr_t;
                             VAR stacksize: size_t): int;
<*EXTERNAL pthread_attr_getstack*>
PROCEDURE attr_getstack (READONLY attr: pthread_attr_t;
                         VAR stackaddr: ADDRESS; VAR stacksize: size_t): int;
<*EXTERNAL pthread_attr_getguardsize*>
PROCEDURE attr_getguardsize (READONLY attr: pthread_attr_t;
                             VAR guardsize: size_t): int;
<*EXTERNAL pthread_attr_init*>
PROCEDURE attr_init (VAR attr: pthread_attr_t): int;
<*EXTERNAL pthread_attr_setdetachstate*>
PROCEDURE attr_setdetachstate (VAR attr: pthread_attr_t;
                               detachstate: int): int;
<*EXTERNAL pthread_attr_setinheritsched*>
PROCEDURE attr_setinheritsched (VAR attr: pthread_attr_t;
                                inheritsched: int): int;
<*EXTERNAL pthread_attr_setschedparam*>
PROCEDURE attr_setschedparam (VAR attr: pthread_attr_t;
                              READONLY param: struct_sched_param): int;
<*EXTERNAL pthread_attr_setschedpolicy*>
PROCEDURE attr_setschedpolicy (VAR attr: pthread_attr_t; policy: int): int;
<*EXTERNAL pthread_attr_setstackaddr*>
PROCEDURE attr_setstackaddr (VAR attr: pthread_attr_t;
                             stackaddr: ADDRESS): int;
<*EXTERNAL pthread_attr_setstacksize*>
PROCEDURE attr_setstacksize (VAR attr: pthread_attr_t; stacksize: size_t): int;
<*EXTERNAL pthread_attr_setguardsize*>
PROCEDURE attr_setguardsize (VAR attr: pthread_attr_t; guardsize: size_t): int;
<*EXTERNAL pthread_attr_setstack*>
PROCEDURE attr_setstack (VAR attr: pthread_attr_t;
                         stackaddr: ADDRESS; stacksize: size_t): int;
<*EXTERNAL pthread_cancel*>
PROCEDURE cancel (thread: pthread_t): int;
<*EXTERNAL pthread_setcancelstate*>
PROCEDURE setcancelstate (state: int; VAR oldstate: int): int;
<*EXTERNAL pthread_setcanceltype*>
PROCEDURE setcanceltype (type: int; VAR oldtype: int): int;
<*EXTERNAL pthread_testcancel*>
PROCEDURE testcancel ();
<*EXTERNAL pthread_cond_broadcast*>
PROCEDURE cond_broadcast (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_destroy*>
PROCEDURE cond_destroy (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_init*>
PROCEDURE cond_init (VAR cond: pthread_cond_t;
                     READONLY attr: pthread_condattr_t): int;
<*EXTERNAL pthread_cond_signal*>
PROCEDURE cond_signal (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_wait*>
PROCEDURE cond_wait (VAR cond: pthread_cond_t;
                     VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_cond_timedwait*>
PROCEDURE cond_timedwait (VAR cond: pthread_cond_t;
                          VAR mutex: pthread_mutex_t;
                          READONLY abstime: struct_timespec): int;
<*EXTERNAL pthread_condattr_init*>
PROCEDURE condattr_init (VAR attr: pthread_condattr_t): int;
<*EXTERNAL pthread_condattr_destroy*>
PROCEDURE condattr_destroy (VAR attr: pthread_condattr_t): int;
<*EXTERNAL pthread_condattr_getpshared*>
PROCEDURE condattr_getpshared (READONLY attr: pthread_condattr_t;
                               VAR pshared: int): int;
<*EXTERNAL pthread_condattr_setpshared*>
PROCEDURE condattr_setpshared (VAR attr: pthread_condattr_t;
                               pshared: int): int;
TYPE start_routine_t = PROCEDURE(arg: ADDRESS): ADDRESS;
<*EXTERNAL pthread_create*>
PROCEDURE create (VAR pthread: pthread_t;
                  READONLY attr: pthread_attr_t;
                  start_routine: start_routine_t;
                  arg: ADDRESS): int;
<*EXTERNAL pthread_detach*>
PROCEDURE detach (thread: pthread_t): int;
<*EXTERNAL pthread_equal*>
PROCEDURE equal (t1, t2: pthread_t): int;
<*EXTERNAL pthread_exit*>
PROCEDURE exit (value_ptr: ADDRESS);
<*EXTERNAL pthread_kill*>
PROCEDURE kill (thread: pthread_t; sig: int): int;
<*EXTERNAL pthread_sigmask*>
PROCEDURE sigmask (how: int; READONLY set: sigset_t; VAR oset: sigset_t): int;
<*EXTERNAL pthread_getschedparam*>
PROCEDURE getschedparam (thread: pthread_t;
                         VAR policy: int;
                         VAR param: struct_sched_param): int;
<*EXTERNAL pthread_join*>
PROCEDURE join (thread: pthread_t; VAR value_ptr: ADDRESS): int;
<*EXTERNAL pthread_mutex_destroy*>
PROCEDURE mutex_destroy (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_getprioceiling*>
PROCEDURE mutex_getprioceiling(READONLY mutex: pthread_mutex_t;
                               VAR prioceiling: int): int;
<*EXTERNAL pthread_mutex_init*>
PROCEDURE mutex_init (VAR mutex: pthread_mutex_t;
                      READONLY attr: pthread_mutexattr_t): int;
<*EXTERNAL pthread_mutex_lock*>
PROCEDURE mutex_lock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_setprioceiling*>
PROCEDURE mutex_setprioceiling (VAR mutex: pthread_mutex_t;
                                prioceiling: int;
                                VAR old_prioceiling: int): int;
<*EXTERNAL pthread_mutex_trylock*>
PROCEDURE mutex_trylock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_unlock*>
PROCEDURE mutex_unlock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutexattr_destroy*>
PROCEDURE mutexattr_destroy (VAR attr: pthread_mutexattr_t): int;
<*EXTERNAL pthread_mutexattr_getprioceiling*>
PROCEDURE mutexattr_getprioceiling (READONLY attr: pthread_mutexattr_t;
                                    VAR prioceiling: int): int;
<*EXTERNAL pthread_mutexattr_getprotocol*>
PROCEDURE pthread_mutexattr_getprotocol (READONLY attr: pthread_mutexattr_t;
                                         VAR protocol: int): int;
<*EXTERNAL pthread_mutexattr_getpshared*>
PROCEDURE mutexattr_getpshared (READONLY attr: pthread_mutexattr_t;
                                VAR pshared: int): int;
<*EXTERNAL pthread_mutexattr_gettype*>
PROCEDURE mutexattr_gettype (READONLY attr: pthread_mutexattr_t;
                             VAR type: int): int;
<*EXTERNAL pthread_mutexattr_init*>
PROCEDURE mutexattr_init (VAR attr: pthread_mutexattr_t): int;
<*EXTERNAL pthread_mutexattr_setprioceiling*>
PROCEDURE mutexattr_setprioceiling (VAR attr: pthread_mutexattr_t;
                                    prioceiling: int): int;
<*EXTERNAL pthread_mutexattr_setprotocol*>
PROCEDURE mutexattr_setprotocol (VAR attr: pthread_mutexattr_t;
                                 protocol: int): int;
<*EXTERNAL pthread_mutexattr_settype*>
PROCEDURE mutexattr_settype (VAR attr: pthread_mutexattr_t; type: int): int;
<*EXTERNAL pthread_mutexattr_setpshared*>
PROCEDURE mutexattr_setpshared (VAR attr: pthread_mutexattr_t;
                                pshared: int): int;
TYPE init_routine_t = PROCEDURE();
<*EXTERNAL pthread_once*>
PROCEDURE once (VAR once_control: pthread_once_t;
                init_routine: init_routine_t): int;
<*EXTERNAL pthread_self*>
PROCEDURE self (): pthread_t;
<*EXTERNAL pthread_setschedparam*>
PROCEDURE setschedparam (thread: pthread_t; policy: int;
                         READONLY param: struct_sched_param): int;
TYPE destructor_t = PROCEDURE(arg: ADDRESS);
<*EXTERNAL pthread_key_create*>
PROCEDURE key_create (VAR key: pthread_key_t;
                      destructor: destructor_t): int;
<*EXTERNAL pthread_key_delete*>
PROCEDURE key_delete (key: pthread_key_t): int;
<*EXTERNAL pthread_setspecific*>
PROCEDURE setspecific (key: pthread_key_t; value: ADDRESS): int;
<*EXTERNAL pthread_getspecific*>
PROCEDURE getspecific (key: pthread_key_t): ADDRESS;
<*EXTERNAL pthread_attr_getscope*>
PROCEDURE attr_getscope (READONLY attr: pthread_attr_t;
                         VAR contentionscope: int): int;
<*EXTERNAL pthread_attr_setscope*>
PROCEDURE attr_setscope (VAR attr: pthread_attr_t; contentionscope:int): int;
<*EXTERNAL pthread_getconcurrency*>
PROCEDURE getconcurrency (): int;
<*EXTERNAL pthread_setconcurrency*>
PROCEDURE setconcurrency (concurrency: int): int;
<*EXTERNAL pthread_rwlock_destroy*>
PROCEDURE rwlock_destroy (VAR rwlock: pthread_rwlock_t): int;
<*EXTERNAL pthread_rwlock_init*>
PROCEDURE rwlock_init (VAR rwlock: pthread_rwlock_t;
                       READONLY attr: pthread_rwlockattr_t): int;
<*EXTERNAL pthread_rwlock_rdlock*>
PROCEDURE rwlock_rdlock (VAR rwlock: pthread_rwlock_t): int;
<*EXTERNAL pthread_rwlock_tryrdlock*>
PROCEDURE wrlock_tryrdlock (VAR rwlock: pthread_rwlock_t): int;
<*EXTERNAL pthread_rwlock_wrlock*>
PROCEDURE rwlock_wrlock (VAR rwlock: pthread_rwlock_t): int;
<*EXTERNAL pthread_rwlock_trywrlock*>
PROCEDURE rwlock_trywrlock (VAR rwlock: pthread_rwlock_t): int;
<*EXTERNAL pthread_rwlock_unlock*>
PROCEDURE rwlock_unlock (VAR rwlock: pthread_rwlock_t): int;
<*EXTERNAL pthread_rwlockattr_init*>
PROCEDURE rwlockattr_init (VAR attr: pthread_rwlockattr_t): int;
<*EXTERNAL pthread_rwlockattr_destroy*>
PROCEDURE rwlockattr_destroy (VAR attr: pthread_rwlockattr_t): int;
<*EXTERNAL pthread_rwlockattr_getpshared*>
PROCEDURE rwlockattr_getpshared (READONLY attr: pthread_rwlockattr_t;
                                 VAR pshared: int): int;
<*EXTERNAL pthread_rwlockattr_setpshared*>
PROCEDURE rwlockattr_setpshared (VAR attr: pthread_rwlockattr_t;
                                 pshared: int): int;

(* returns non-zero if pthread_create or cthread_fork have been called *)
<*EXTERNAL pthread_is_threaded_np*>
PROCEDURE is_threaded_np (): int;

(* returns non-zero if the current thread is the main thread *)
<*EXTERNAL pthread_main_np*>
PROCEDURE main_np (): int;

TYPE mach_port_t = ADDRESS;

(* return the mach thread bound to the pthread *)
<*EXTERNAL pthread_mach_thread_np*>
PROCEDURE mach_thread_np (thread: pthread_t): mach_port_t;
<*EXTERNAL pthread_get_stacksize_np*>
PROCEDURE get_stacksize_np (thread: pthread_t): size_t;
<*EXTERNAL pthread_get_stackaddr_np*>
PROCEDURE get_stackaddr_np (thread: pthread_t): ADDRESS;

(* Like pthread_cond_signal(), but only wake up the specified pthread *)
<*EXTERNAL pthread_cond_signal_thread_np*>
PROCEDURE cond_signal_thread_np (VAR cond: pthread_cond_t;
                                 thread: pthread_t);

(* Like pthread_cond_timedwait, but use a relative timeout *)
<*EXTERNAL pthread_cond_timedwait_relative_np*>
PROCEDURE cond_timedwait_relative_np (VAR cond: pthread_cond_t;
                                      VAR mutex: pthread_mutex_t;
                                      READONLY reltime: struct_timespec): int;

(* Like pthread_create(), but leaves the thread suspended *)
<*EXTERNAL pthread_create_suspended_np*>
PROCEDURE create_suspended_np (VAR thread: pthread_t;
                               READONLY attr: pthread_attr_t;
                               start_routine: start_routine_t;
                               arg: ADDRESS): int;

<*EXTERNAL pthread_yield_np*>
PROCEDURE yield_np ();

<*EXTERNAL thread_suspend*>
PROCEDURE thread_suspend (mach_thread: mach_port_t): int;
<*EXTERNAL thread_resume*>
PROCEDURE thread_resume (mach_thread: mach_port_t): int;

END Upthread.
