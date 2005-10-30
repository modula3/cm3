(*
 * Copyright (c) 1997-2001 by Sun Microsystems, Inc.
 * All rights reserved.
 *)

INTERFACE Upthread;

IMPORT Uthread;
FROM Ctypes IMPORT int, char;
FROM Utypes IMPORT size_t;
FROM Utime IMPORT struct_timespec;
FROM Usignal IMPORT sigset_t;

VAR
  PTHREAD_STACK_MIN: CARDINAL;

(* <sys/types.h> *)
TYPE
  pthread_t = Uthread.thread_t;
  pthread_key_t = Uthread.thread_key_t;

CONST
  SIZEOF_PTHREAD_ATTR_T        =  4;
  SIZEOF_PTHREAD_MUTEX_T       = 24;
  SIZEOF_PTHREAD_MUTEXATTR_T   =  4;
  SIZEOF_PTHREAD_COND_T        = 16;
  SIZEOF_PTHREAD_CONDATTR_T    =  4;
  SIZEOF_PTHREAD_RWLOCK_T      = 64;
  SIZEOF_PTHREAD_RWLOCKATTR_T  =  4;
  SIZEOF_PTHREAD_ONCE_T        = 32;

TYPE
  pthread_attr_t = RECORD
    data: ARRAY[1..SIZEOF_PTHREAD_ATTR_T] OF char;
  END;
  pthread_mutex_t = RECORD
    data: ARRAY[1..SIZEOF_PTHREAD_MUTEX_T] OF char;
  END;
  pthread_mutexattr_t = RECORD
    data: ARRAY[1..SIZEOF_PTHREAD_MUTEXATTR_T] OF char;
  END;
  pthread_cond_t = RECORD
    data: ARRAY [1..SIZEOF_PTHREAD_COND_T] OF char;
  END;
  pthread_condattr_t = RECORD
    data: ARRAY [1..SIZEOF_PTHREAD_CONDATTR_T] OF char;
  END;
  pthread_rwlock_t = RECORD
    data: ARRAY [1..SIZEOF_PTHREAD_RWLOCK_T] OF char;
  END;
  pthread_rwlockattr_t = RECORD
    data: ARRAY [1..SIZEOF_PTHREAD_RWLOCKATTR_T] OF char;
  END;
  pthread_once_t = RECORD
    data: ARRAY [1..SIZEOF_PTHREAD_ONCE_T] OF char;
  END;

(* <sched.h> *)
TYPE
  struct_sched_param = RECORD
    sched_priority: int;	    (* process execution scheduling priority *)
    sched_nicelim: int;		    (* nice value limit for SCHED_OTHER policy *)
    sched_nice: int;		    (* nice value for SCHED_OTHER policy *)
    sched_pad: ARRAY [1..6] OF int; (* pad to the same size as pcparms_t of *)
				    (* sys/priocntl.h *)
				    (*	sizeof(sched_priority) +	*)
				    (*	sizeof(pcparms_t.pc_clparms)	*)
  END;

(*
 * Thread related attribute values defined as in thread.h.
 * These are defined as bit pattern in thread.h.
 * Any change here should be reflected in thread.h.
 *)
CONST
  (* detach *)
  PTHREAD_CREATE_DETACHED = 16_40;       (* = THR_DETACHED *)
  PTHREAD_CREATE_JOINABLE = 0;
  (* scope *)
  PTHREAD_SCOPE_SYSTEM = 16_01;		 (* = THR_BOUND *)
  PTHREAD_SCOPE_PROCESS = 0;

(*
 * Other attributes which are not defined in thread.h
 *)
CONST
  (* inherit *)
  PTHREAD_INHERIT_SCHED = 1;
  PTHREAD_EXPLICIT_SCHED = 0;

(*
 * Value of process-shared attribute
 * These are defined as values defined in sys/synch.h
 * Any change here should be reflected in sys/synch.h.
 *)
CONST
  PTHREAD_PROCESS_SHARED = 1;		 (* = USYNC_PROCESS *)
  PTHREAD_PROCESS_PRIVATE = 0;		 (* = USYNC_THREAD *)
  DEFAULT_TYPE = PTHREAD_PROCESS_PRIVATE;

(*
 * mutex types
 * keep these in synch which sys/synch.h lock flags
 *)
CONST
  PTHREAD_MUTEX_NORMAL = 16_0;
  PTHREAD_MUTEX_ERRORCHECK = 16_2;
  PTHREAD_MUTEX_RECURSIVE = 16_4;
  PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL;

(*
 * Mutex protocol values. Keep these in synch with sys/synch.h lock types.
 *)
CONST
  PTHREAD_PRIO_NONE = 16_0;
  PTHREAD_PRIO_INHERIT = 16_10;
  PTHREAD_PRIO_PROTECT = 16_20;

(*
 * Mutex robustness attribute values. The robustness attribute is a
 * Solaris specific extension to support robust mutexes. Note the _NP suffix
 * to indicate these are not part of the current POSIX spec (POSIX 1003.1 1996),
 * but are platform specific non-portable extensions. Keep these in synch
 * with sys/synch.h lock types.
 *)
CONST
  PTHREAD_MUTEX_STALL_NP = 16_0;
  PTHREAD_MUTEX_ROBUST_NP = 16_40;

(*
 * macros - default initializers defined as in synch.h
 * Any change here should be reflected in synch.h.
 *
 * NOTE:
 * Make sure that any change in the macros is consistent with the definition
 * of the corresponding types in sys/types.h (e.g. PTHREAD_MUTEX_INITIALIZER
 * should be consistent with the definition for pthread_mutex_t).
 *)
CONST
  PTHREAD_MUTEX_INITIALIZER =
    pthread_mutex_t { ARRAY [1..SIZEOF_PTHREAD_MUTEX_T] OF char {0, .. } };
  PTHREAD_COND_INITIALIZER =
    pthread_cond_t { ARRAY [1..SIZEOF_PTHREAD_COND_T] OF char {0, .. } };
  PTHREAD_RWLOCK_INITIALIZER =
    pthread_rwlock_t { ARRAY [1..SIZEOF_PTHREAD_RWLOCK_T] OF char {0, .. } };
  PTHREAD_ONCE_INITIALIZER =
    pthread_once_t { ARRAY [1..SIZEOF_PTHREAD_ONCE_T] OF char {0, .. } };

(* cancellation type and state *)
CONST
  PTHREAD_CANCEL_ENABLE = 16_00;
  PTHREAD_CANCEL_DISABLE = 16_01;
  PTHREAD_CANCEL_DEFERRED = 16_00;
  PTHREAD_CANCEL_ASYNCHRONOUS = 16_02;
  PTHREAD_CANCELED = -19;

(* pthread_once related values *)
CONST
  PTHREAD_ONCE_NOTDONE = 0;
  PTHREAD_ONCE_DONE = 1;
  PTHREAD_ONCE_INIT = pthread_once_t { ARRAY [1..SIZEOF_PTHREAD_ONCE_T] OF char {0, .. } };

(*
 * function prototypes - thread related calls
 *)

<*EXTERNAL pthread_attr_init*>
PROCEDURE attr_init (VAR attr: pthread_attr_t): int;
<*EXTERNAL pthread_attr_destroy*>
PROCEDURE attr_destroy (VAR attr: pthread_attr_t): int;
<*EXTERNAL pthread_attr_setstacksize*>
PROCEDURE attr_setstacksize (VAR attr: pthread_attr_t; stacksize: size_t): int;
<*EXTERNAL pthread_attr_getstacksize*>
PROCEDURE attr_getstacksize (READONLY attr: pthread_attr_t;
                             VAR stacksize: size_t): int;
<*EXTERNAL pthread_attr_setstackaddr*>
PROCEDURE attr_setstackaddr (VAR attr: pthread_attr_t;
                             stackaddr: ADDRESS): int;
<*EXTERNAL pthread_attr_getstackaddr*>
PROCEDURE attr_getstackaddr (READONLY attr: pthread_attr_t;
                             VAR stackaddr: ADDRESS): int;
<*EXTERNAL pthread_attr_setdetachstate*>
PROCEDURE attr_setdetachstate (VAR attr: pthread_attr_t;
                               detachstate: int): int;
<*EXTERNAL pthread_attr_getdetachstate*>
PROCEDURE attr_getdetachstate (READONLY attr: pthread_attr_t;
                               VAR detachstate: int): int;
<*EXTERNAL pthread_attr_setscope*>
PROCEDURE attr_setscope (VAR attr: pthread_attr_t; contentionscope:int): int;
<*EXTERNAL pthread_attr_getscope*>
PROCEDURE attr_getscope (READONLY attr: pthread_attr_t;
                         VAR contentionscope: int): int;
<*EXTERNAL pthread_attr_setinheritsched*>
PROCEDURE attr_setinheritsched (VAR attr: pthread_attr_t;
                                inheritsched: int): int;
<*EXTERNAL pthread_attr_getinheritsched*>
PROCEDURE attr_getinheritsched (READONLY attr: pthread_attr_t;
                                VAR inheritsched: int): int;
<*EXTERNAL pthread_attr_setschedpolicy*>
PROCEDURE attr_setschedpolicy (VAR attr: pthread_attr_t; policy: int): int;
<*EXTERNAL pthread_attr_getschedpolicy*>
PROCEDURE attr_getschedpolicy (READONLY attr: pthread_attr_t;
                               VAR policy: int): int;
<*EXTERNAL pthread_attr_setschedparam*>
PROCEDURE attr_setschedparam (VAR attr: pthread_attr_t;
                              READONLY param: struct_sched_param): int;
<*EXTERNAL pthread_attr_getschedparam*>
PROCEDURE attr_getschedparam (READONLY attr: pthread_attr_t;
                              VAR param: struct_sched_param): int;
TYPE start_routine_t = PROCEDURE(arg: ADDRESS): ADDRESS;
<*EXTERNAL pthread_create*>
PROCEDURE create (VAR pthread: pthread_t;
                  READONLY attr: pthread_attr_t;
                  start_routine: start_routine_t;
                  arg: ADDRESS): int;
TYPE init_routine_t = PROCEDURE();
<*EXTERNAL pthread_once*>
PROCEDURE once (VAR once_control: pthread_once_t;
                init_routine: init_routine_t): int;
<*EXTERNAL pthread_join*>
PROCEDURE join (thread: pthread_t; VAR value_ptr: ADDRESS): int;
<*EXTERNAL pthread_detach*>
PROCEDURE detach (thread: pthread_t): int;
<*EXTERNAL pthread_exit*>
PROCEDURE exit (value_ptr: ADDRESS);
<*EXTERNAL pthread_cancel*>
PROCEDURE cancel (thread: pthread_t): int;
<*EXTERNAL pthread_setschedparam*>
PROCEDURE setschedparam (thread: pthread_t; policy: int;
                         READONLY param: struct_sched_param): int;
<*EXTERNAL pthread_getschedparam*>
PROCEDURE getschedparam (thread: pthread_t;
                         VAR policy: int;
                         VAR param: struct_sched_param): int;
<*EXTERNAL pthread_setcancelstate*>
PROCEDURE setcancelstate (state: int; VAR oldstate: int): int;
<*EXTERNAL pthread_setcanceltype*>
PROCEDURE setcanceltype (type: int; VAR oldtype: int): int;
<*EXTERNAL pthread_testcancel*>
PROCEDURE testcancel ();
<*EXTERNAL pthread_equal*>
PROCEDURE equal (t1, t2: pthread_t): int;
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
<*EXTERNAL pthread_self*>
PROCEDURE self (): pthread_t;

(*
 * function prototypes - synchronization related calls
 *)
<*EXTERNAL pthread_mutexattr_init*>
PROCEDURE mutexattr_init (VAR attr: pthread_mutexattr_t): int;
<*EXTERNAL pthread_mutexattr_destroy*>
PROCEDURE mutexattr_destroy (VAR attr: pthread_mutexattr_t): int;
<*EXTERNAL pthread_mutexattr_setpshared*>
PROCEDURE mutexattr_setpshared (VAR attr: pthread_mutexattr_t;
                                pshared: int): int;
<*EXTERNAL pthread_mutexattr_getpshared*>
PROCEDURE mutexattr_getpshared (READONLY attr: pthread_mutexattr_t;
                                VAR pshared: int): int;
<*EXTERNAL pthread_mutexattr_setprotocol*>
PROCEDURE mutexattr_setprotocol (VAR attr: pthread_mutexattr_t;
                                 protocol: int): int;
<*EXTERNAL pthread_mutexattr_getprotocol*>
PROCEDURE mutexattr_getprotocol (READONLY attr: pthread_mutexattr_t;
                                 VAR protocol: int): int;
<*EXTERNAL pthread_mutexattr_setprioceiling*>
PROCEDURE mutexattr_setprioceiling (VAR attr: pthread_mutexattr_t;
                                    prioceiling: int): int;
<*EXTERNAL pthread_mutexattr_getprioceiling*>
PROCEDURE mutexattr_getprioceiling (READONLY attr: pthread_mutexattr_t;
                                    VAR prioceiling: int): int;
<*EXTERNAL pthread_mutexattr_setrobust_np*>
PROCEDURE mutexattr_setrobust_np (VAR attr: pthread_mutexattr_t;
                                  robustness: int): int;
<*EXTERNAL pthread_mutexattr_getrobust_np*>
PROCEDURE mutexattr_getrobust_np (READONLY attr: pthread_mutexattr_t;
                                  VAR robustness: int): int;
<*EXTERNAL pthread_mutex_init*>
PROCEDURE mutex_init (VAR mutex: pthread_mutex_t;
                      READONLY attr: pthread_mutexattr_t): int;
<*EXTERNAL pthread_mutex_consistent_np*>
PROCEDURE mutex_consistent_np (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_destroy*>
PROCEDURE mutex_destroy (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_lock*>
PROCEDURE mutex_lock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_unlock*>
PROCEDURE mutex_unlock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_trylock*>
PROCEDURE mutex_trylock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_setprioceiling*>
PROCEDURE mutex_setprioceiling (VAR mutex: pthread_mutex_t;
                                prioceiling: int;
                                VAR old_ceiling: int): int;
<*EXTERNAL pthread_mutex_getprioceiling*>
PROCEDURE mutex_getprioceiling (READONLY mutex: pthread_mutex_t;
                                VAR prioceiling: int): int;
<*EXTERNAL pthread_condattr_init*>
PROCEDURE condattr_init (VAR attr: pthread_condattr_t): int;
<*EXTERNAL pthread_condattr_destroy*>
PROCEDURE condattr_destroy (VAR attr: pthread_condattr_t): int;
<*EXTERNAL pthread_condattr_setpshared*>
PROCEDURE condattr_setpshared (VAR attr: pthread_condattr_t;
                               pshared: int): int;
<*EXTERNAL pthread_condattr_getpshared*>
PROCEDURE condattr_getpshared (READONLY attr: pthread_condattr_t;
                               VAR pshared: int): int;
<*EXTERNAL pthread_cond_init*>
PROCEDURE cond_init (VAR cond: pthread_cond_t;
                     READONLY attr: pthread_condattr_t): int;
<*EXTERNAL pthread_cond_destroy*>
PROCEDURE cond_destroy (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_broadcast*>
PROCEDURE cond_broadcast (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_signal*>
PROCEDURE cond_signal (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_wait*>
PROCEDURE cond_wait (VAR cond: pthread_cond_t;
                     VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_cond_timedwait*>
PROCEDURE cond_timedwait (VAR cond: pthread_cond_t;
                          VAR mutex: pthread_mutex_t;
                          READONLY abstime: struct_timespec): int;
<*EXTERNAL pthread_cond_reltimedwait_np*>
PROCEDURE cond_reltimedwait_np (VAR cond: pthread_cond_t;
                                VAR mutex: pthread_mutex_t;
                                READONLY reltime: struct_timespec): int;
<*EXTERNAL pthread_attr_getguardsize*>
PROCEDURE attr_getguardsize (READONLY attr: pthread_attr_t;
                             VAR guardsize: size_t): int;
<*EXTERNAL pthread_attr_setguardsize*>
PROCEDURE attr_setguardsize (VAR attr: pthread_attr_t; guardsize: size_t): int;
<*EXTERNAL pthread_getconcurrency*>
PROCEDURE getconcurrency (): int;
<*EXTERNAL pthread_setconcurrency*>
PROCEDURE setconcurrency (concurrency: int): int;
<*EXTERNAL pthread_mutexattr_settype*>
PROCEDURE mutexattr_settype (VAR attr: pthread_mutexattr_t; type: int): int;
<*EXTERNAL pthread_mutexattr_gettype*>
PROCEDURE mutexattr_gettype (READONLY attr: pthread_mutexattr_t;
                             VAR type: int): int;
<*EXTERNAL pthread_rwlock_init*>
PROCEDURE rwlock_init (VAR rwlock: pthread_rwlock_t;
                       READONLY attr: pthread_rwlockattr_t): int;
<*EXTERNAL pthread_rwlock_destroy*>
PROCEDURE rwlock_destroy (VAR rwlock: pthread_rwlock_t): int;
<*EXTERNAL pthread_rwlock_rdlock*>
PROCEDURE rwlock_rdlock (VAR rwlock: pthread_rwlock_t): int;
<*EXTERNAL pthread_rwlock_tryrdlock*>
PROCEDURE rwlock_tryrdlock (VAR rwlock: pthread_rwlock_t): int;
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

(* <signal.h> *)

<*EXTERNAL pthread_kill*>
PROCEDURE kill (thread: pthread_t; sig: int): int;
<*EXTERNAL pthread_sigmask*>
PROCEDURE sigmask (how: int; READONLY set: sigset_t; VAR oset: sigset_t): int;

END Upthread.
