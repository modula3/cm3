(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Upthread;

FROM Ctypes IMPORT char, int, unsigned_int, void_star;
FROM Utypes IMPORT size_t;
FROM Usignal IMPORT sigset_t;
FROM Utime IMPORT struct_timespec;

(* <bits/pthreadtypes.h> *)

CONST
  SIZEOF_PTHREAD_ATTR_T        = 36;
  SIZEOF_PTHREAD_MUTEX_T       = 24;
  SIZEOF_PTHREAD_MUTEXATTR_T   =  4;
  SIZEOF_PTHREAD_COND_T        = 48;
  SIZEOF_PTHREAD_COND_COMPAT_T = 12;
  SIZEOF_PTHREAD_CONDATTR_T    =  4;
  SIZEOF_PTHREAD_RWLOCK_T      = 32;
  SIZEOF_PTHREAD_RWLOCKATTR_T  =  8;
  SIZEOF_PTHREAD_BARRIER_T     = 20;
  SIZEOF_PTHREAD_BARRIERATTR_T =  4;

(* Thread identifiers.  The structure of the attribute type is not
   exposed on purpose.  *)
TYPE
  pthread_t = void_star;
  pthread_attr_t = RECORD
    data: ARRAY[1..SIZEOF_PTHREAD_ATTR_T] OF char;
  END;

(* Data structures for mutex handling.  The structure of the attribute
   type is not exposed on purpose.  *)
TYPE
  pthread_mutex_t = RECORD
    data: ARRAY[1..SIZEOF_PTHREAD_MUTEX_T] OF char;
  END;
  pthread_mutexattr_t = RECORD
    data: ARRAY[1..SIZEOF_PTHREAD_MUTEXATTR_T] OF char;
  END;

(* Data structure for conditional variable handling.  The structure of
   the attribute type is not exposed on purpose.  *)
TYPE
  pthread_cond_t = RECORD
    data: ARRAY [1..SIZEOF_PTHREAD_COND_T] OF char;
  END;
  pthread_condattr_t = RECORD
    data: ARRAY [1..SIZEOF_PTHREAD_CONDATTR_T] OF char;
  END;

(* Keys for thread-specific data *)
TYPE
  pthread_key_t = unsigned_int;

(* Once-only execution *)
TYPE
  pthread_once_t = RECORD
    data: int;
  END;

(* Data structure for read-write lock variable handling.  The
   structure of the attribute type is not exposed on purpose.  *)
TYPE
  pthread_rwlock_t = RECORD
    data: ARRAY [1..SIZEOF_PTHREAD_RWLOCK_T] OF char;
  END;
  pthread_rwlockattr_t = RECORD
    data: ARRAY [1..SIZEOF_PTHREAD_RWLOCKATTR_T] OF char;
  END;

(* <bits/sched.h> *)
TYPE
  struct_sched_param = RECORD
    sched_priority: int;
  END;

(* <pthread.h> *)

(* Mutex initializers.  *)
CONST
  PTHREAD_MUTEX_INITIALIZER =
    pthread_mutex_t { ARRAY [1..SIZEOF_PTHREAD_MUTEX_T] OF char {0, .. } };

(* Read-write lock initializers.  *)
CONST
  PTHREAD_RWLOCK_INITIALIZER =
    pthread_rwlock_t { ARRAY [1..SIZEOF_PTHREAD_RWLOCK_T] OF char {0, .. } };

(* Conditional variable handling.  *)
CONST
  PTHREAD_COND_INITIALIZER =
    pthread_cond_t { ARRAY [1..SIZEOF_PTHREAD_COND_T] OF char {0, .. } };

(* Single execution handling.  *)
CONST
  PTHREAD_ONCE_INITIALIZER = pthread_once_t { 0 };

TYPE start_routine_t = PROCEDURE(arg: ADDRESS): ADDRESS;
<*EXTERNAL pthread_create*>
PROCEDURE create (VAR pthread: pthread_t;
                  READONLY attr: pthread_attr_t;
                  start_routine: start_routine_t;
                  arg: ADDRESS): int;

<*EXTERNAL pthread_exit*>
PROCEDURE exit (value_ptr: ADDRESS);

<*EXTERNAL pthread_join*>
PROCEDURE join (thread: pthread_t; VAR value_ptr: ADDRESS): int;

<*EXTERNAL pthread_detach*>
PROCEDURE detach (thread: pthread_t): int;

(* Obtain the identifier of the current thread.  *)
<*EXTERNAL pthread_self*>
PROCEDURE self (): pthread_t;

(* Compare two thread identifiers.  *)
<*EXTERNAL pthread_equal*>
PROCEDURE equal (t1, t2: pthread_t): int;

(* Thread attribute handling.  *)
<*EXTERNAL pthread_attr_init*>
PROCEDURE attr_init (VAR attr: pthread_attr_t): int;
<*EXTERNAL pthread_attr_destroy*>
PROCEDURE attr_destroy (VAR attr: pthread_attr_t): int;
<*EXTERNAL pthread_attr_getdetachstate*>
PROCEDURE attr_getdetachstate (READONLY attr: pthread_attr_t;
                               VAR detachstate: int): int;
<*EXTERNAL pthread_attr_setdetachstate*>
PROCEDURE attr_setdetachstate (VAR attr: pthread_attr_t;
                               detachstate: int): int;
<*EXTERNAL pthread_attr_getguardsize*>
PROCEDURE attr_getguardsize (READONLY attr: pthread_attr_t;
                             VAR guardsize: size_t): int;
<*EXTERNAL pthread_attr_setguardsize*>
PROCEDURE attr_setguardsize (VAR attr: pthread_attr_t; guardsize: size_t): int;
<*EXTERNAL pthread_attr_getschedparam*>
PROCEDURE attr_getschedparam (READONLY attr: pthread_attr_t;
                              VAR param: struct_sched_param): int;
<*EXTERNAL pthread_attr_setschedparam*>
PROCEDURE attr_setschedparam (VAR attr: pthread_attr_t;
                              READONLY param: struct_sched_param): int;
<*EXTERNAL pthread_attr_getschedpolicy*>
PROCEDURE attr_getschedpolicy (READONLY attr: pthread_attr_t;
                               VAR policy: int): int;
<*EXTERNAL pthread_attr_setschedpolicy*>
PROCEDURE attr_setschedpolicy (VAR attr: pthread_attr_t; policy: int): int;
<*EXTERNAL pthread_attr_getinheritsched*>
PROCEDURE attr_getinheritsched (READONLY attr: pthread_attr_t;
                                VAR inheritsched: int): int;
<*EXTERNAL pthread_attr_setinheritsched*>
PROCEDURE attr_setinheritsched (VAR attr: pthread_attr_t;
                                inheritsched: int): int;
<*EXTERNAL pthread_attr_getscope*>
PROCEDURE attr_getscope (READONLY attr: pthread_attr_t;
                         VAR contentionscope: int): int;
<*EXTERNAL pthread_attr_setscope*>
PROCEDURE attr_setscope (VAR attr: pthread_attr_t; contentionscope:int): int;
<*EXTERNAL pthread_attr_getstackaddr*>
PROCEDURE attr_getstackaddr (READONLY attr: pthread_attr_t;
                             VAR stackaddr: ADDRESS): int;
<*EXTERNAL pthread_attr_setstackaddr*>
PROCEDURE attr_setstackaddr (VAR attr: pthread_attr_t;
                             stackaddr: ADDRESS): int;
<*EXTERNAL pthread_attr_getstacksize*>
PROCEDURE attr_getstacksize (READONLY attr: pthread_attr_t;
                             VAR stacksize: size_t): int;
<*EXTERNAL pthread_attr_setstacksize*>
PROCEDURE attr_setstacksize (VAR attr: pthread_attr_t; stacksize: size_t): int;
<*EXTERNAL pthread_attr_getstack*>
PROCEDURE attr_getstack (READONLY attr: pthread_attr_t;
                         VAR stackaddr: ADDRESS; VAR stacksize: size_t): int;
<*EXTERNAL pthread_attr_setstack*>
PROCEDURE attr_setstack (VAR attr: pthread_attr_t;
                         stackaddr: ADDRESS; stacksize: size_t): int;

(* Functions for scheduling control.  *)
<*EXTERNAL pthread_setschedparam*>
PROCEDURE setschedparam (thread: pthread_t; policy: int;
                         READONLY param: struct_sched_param): int;
<*EXTERNAL pthread_getschedparam*>
PROCEDURE getschedparam (thread: pthread_t;
                         VAR policy: int;
                         VAR param: struct_sched_param): int;
<*EXTERNAL pthread_setschedprio*>
PROCEDURE setschedprio (thread: pthread_t; prio: int): int;

<*EXTERNAL pthread_getconcurrency*>
PROCEDURE getconcurrency (): int;
<*EXTERNAL pthread_setconcurrency*>
PROCEDURE setconcurrency (concurrency: int): int;

<*EXTERNAL pthread_yield*>
PROCEDURE yield (): int;

(* Functions for handling initialization.  *)
TYPE init_routine_t = PROCEDURE();
<*EXTERNAL pthread_once*>
PROCEDURE once (VAR once_control: pthread_once_t;
                init_routine: init_routine_t): int;

(* Functions for handling cancellation. *)
<*EXTERNAL pthread_setcancelstate*>
PROCEDURE setcancelstate (state: int; VAR oldstate: int): int;
<*EXTERNAL pthread_setcanceltype*>
PROCEDURE setcanceltype (type: int; VAR oldtype: int): int;
<*EXTERNAL pthread_cancel*>
PROCEDURE cancel (thread: pthread_t): int;
<*EXTERNAL pthread_testcancel*>
PROCEDURE testcancel ();

(* Mutex handling.  *)
<*EXTERNAL pthread_mutex_init*>
PROCEDURE mutex_init (VAR mutex: pthread_mutex_t;
                      READONLY attr: pthread_mutexattr_t): int;
<*EXTERNAL pthread_mutex_destroy*>
PROCEDURE mutex_destroy (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_trylock*>
PROCEDURE mutex_trylock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_lock*>
PROCEDURE mutex_lock (VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_mutex_unlock*>
PROCEDURE mutex_unlock (VAR mutex: pthread_mutex_t): int;

(* Functions for handling mutex attributes.  *)
<*EXTERNAL pthread_mutexattr_init*>
PROCEDURE mutexattr_init (VAR attr: pthread_mutexattr_t): int;
<*EXTERNAL pthread_mutexattr_destroy*>
PROCEDURE mutexattr_destroy (VAR attr: pthread_mutexattr_t): int;
<*EXTERNAL pthread_mutexattr_getpshared*>
PROCEDURE mutexattr_getpshared (READONLY attr: pthread_mutexattr_t;
                                VAR pshared: int): int;
<*EXTERNAL pthread_mutexattr_setpshared*>
PROCEDURE mutexattr_setpshared (VAR attr: pthread_mutexattr_t;
                                pshared: int): int;
<*EXTERNAL pthread_mutexattr_gettype*>
PROCEDURE mutexattr_gettype (READONLY attr: pthread_mutexattr_t;
                             VAR type: int): int;
<*EXTERNAL pthread_mutexattr_settype*>
PROCEDURE mutexattr_settype (VAR attr: pthread_mutexattr_t; type: int): int;
<*EXTERNAL pthread_mutex_getprioceiling*>
PROCEDURE mutex_getprioceiling(READONLY mutex: pthread_mutex_t;
                               VAR prioceiling: int): int;
<*EXTERNAL pthread_mutexattr_setprioceiling*>
PROCEDURE mutexattr_setprioceiling (VAR attr: pthread_mutexattr_t;
                                    prioceiling: int): int;
<*EXTERNAL pthread_mutexattr_getprotocol*>
PROCEDURE pthread_mutexattr_getprotocol (READONLY attr: pthread_mutexattr_t;
                                         VAR protocol: int): int;
<*EXTERNAL pthread_mutexattr_setprotocol*>
PROCEDURE mutexattr_setprotocol (VAR attr: pthread_mutexattr_t;
                                 protocol: int): int;

(* Functions for handling read-write locks.  *)
<*EXTERNAL pthread_rwlock_init*>
PROCEDURE rwlock_init (VAR rwlock: pthread_rwlock_t;
                       READONLY attr: pthread_rwlockattr_t): int;
<*EXTERNAL pthread_rwlock_destroy*>
PROCEDURE rwlock_destroy (VAR rwlock: pthread_rwlock_t): int;
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

(* Functions for handling read-write lock attributes.  *)
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

(* Functions for handling conditional variables.  *)
<*EXTERNAL pthread_cond_init*>
PROCEDURE cond_init (VAR cond: pthread_cond_t;
                     READONLY attr: pthread_condattr_t): int;
<*EXTERNAL pthread_cond_destroy*>
PROCEDURE cond_destroy (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_signal*>
PROCEDURE cond_signal (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_broadcast*>
PROCEDURE cond_broadcast (VAR cond: pthread_cond_t): int;
<*EXTERNAL pthread_cond_wait*>
PROCEDURE cond_wait (VAR cond: pthread_cond_t;
                     VAR mutex: pthread_mutex_t): int;
<*EXTERNAL pthread_cond_timedwait*>
PROCEDURE cond_timedwait (VAR cond: pthread_cond_t;
                          VAR mutex: pthread_mutex_t;
                          READONLY abstime: struct_timespec): int;

(* Functions for handling condition variable attributes.  *)
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

(* Functions for handling thread-specific data.  *)
TYPE destructor_t = PROCEDURE(arg: ADDRESS);
<*EXTERNAL pthread_key_create*>
PROCEDURE key_create (VAR key: pthread_key_t;
                      destructor: destructor_t): int;
<*EXTERNAL pthread_key_delete*>
PROCEDURE key_delete (key: pthread_key_t): int;
<*EXTERNAL pthread_getspecific*>
PROCEDURE getspecific (key: pthread_key_t): ADDRESS;
<*EXTERNAL pthread_setspecific*>
PROCEDURE setspecific (key: pthread_key_t; value: ADDRESS): int;

(* <bits/sigthread.h> *)

(* Functions for handling signals. *)
<*EXTERNAL pthread_sigmask*>
PROCEDURE sigmask (how: int; READONLY set: sigset_t; VAR oset: sigset_t): int;
<*EXTERNAL pthread_kill*>
PROCEDURE kill (thread: pthread_t; sig: int): int;

END Upthread.
