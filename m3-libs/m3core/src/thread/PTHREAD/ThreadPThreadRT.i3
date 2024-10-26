(* Copyright (C) 2024 Peter McKinna.  All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

UNSAFE INTERFACE ThreadPThreadRT;

(* Realtime thread support *)

FROM ThreadPThread IMPORT pthread_t, pthread_mutex_t;
FROM Ctypes IMPORT int;

TYPE
  (* These are opaque C references (not necessarily UNTRACED REF
     ADDRESS) *)
  pthread_attr_t = UNTRACED BRANDED REF ADDRESS;
  pthread_mutexattr_t = UNTRACED BRANDED REF ADDRESS;
  cpu_set_t = UNTRACED BRANDED REF ADDRESS;

  sched_param_t = RECORD sched_priority: int;  END;

(* Scheduling *)

<* EXTERNAL ThreadPThread__pthread_setschedparam *>
PROCEDURE pthread_setschedparam
  (t: pthread_t; policy: int; VAR param: sched_param_t): int;

<* EXTERNAL ThreadPThread__pthread_getschedparam *>
PROCEDURE pthread_getschedparam
  (t: pthread_t; VAR policy: int; VAR param: sched_param_t): int;

<* EXTERNAL ThreadPThread__pthread_setschedprio *>
PROCEDURE pthread_setschedprio (t: pthread_t; priority: int): int;

(* Affinity *)

<* EXTERNAL ThreadPThread__pthread_setaffinity_np *>
PROCEDURE pthread_setaffinity_np
  (t: pthread_t; cpuSetSize: int; cpuSet: cpu_set_t): int;

<* EXTERNAL ThreadPThread__pthread_getaffinity_np *>
PROCEDURE pthread_getaffinity_np
  (t: pthread_t; cpuSetSize: int; cpuSet: cpu_set_t): int;

(* Configured cores *)

<* EXTERNAL ThreadPThread__get_conf_cores *>
PROCEDURE get_conf_cores (): int;

(* Online cores *)

<* EXTERNAL ThreadPThread__get_online_cores *>
PROCEDURE get_online_cores (): int;

<* EXTERNAL ThreadPThread__alloc_cpuset *>
PROCEDURE alloc_cpuset (num_cpus: int; VAR size: int): cpu_set_t;

<* EXTERNAL ThreadPThread__free_cpuset *>
PROCEDURE free_cpuset (cpuSet: cpu_set_t);

<* EXTERNAL ThreadPThread__zero_cpuset *>
PROCEDURE zero_cpuset (size: int; cpuSet: cpu_set_t);

<* EXTERNAL ThreadPThread__in_cpuset *>
PROCEDURE in_cpuset (cpu: int; size: int; cpuSet: cpu_set_t): int;

<* EXTERNAL ThreadPThread__add_core_to_cpuset *>
PROCEDURE add_core_to_cpuset (core_id: int; size: int; cpuSet: cpu_set_t);

(* max min priority *)

<* EXTERNAL ThreadPThread__max_priority *>
PROCEDURE max_priority (policy: int): int;

<* EXTERNAL ThreadPThread__min_priority *>
PROCEDURE min_priority (policy: int): int;

(* Mutex support *)

<* EXTERNAL "ThreadPThread__pthread_mutex_rt" *>
PROCEDURE pthread_mutex_rt (protocol: int): pthread_mutex_t;

<* EXTERNAL ThreadPThread__pthread_mutex_getprioceiling *>
PROCEDURE pthread_mutex_getprioceiling
  (mutex: pthread_mutex_t; VAR prioceiling: int): int;

<* EXTERNAL ThreadPThread__pthread_mutex_setprioceiling *>
PROCEDURE pthread_mutex_setprioceiling
  (mutex: pthread_mutex_t; prioceiling: int; VAR oldCeiling: int): int;

END ThreadPThreadRT.
