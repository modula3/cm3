(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking       *)
(*      modified on Mon Oct 24 15:45:17 PDT 1994 by kalsow        *)
(*      modified on Wed Mar 25 16:45:58 PST 1992 by muller        *)

INTERFACE Uresource;

FROM Ctypes IMPORT int, long, unsigned_long;
IMPORT Utime;

(*** <sys/resource.h> ***)

CONST
  PRIO_MIN = -20;
  PRIO_MAX = 20;

  PRIO_PROCESS = 0;
  PRIO_PGRP    = 1;
  PRIO_USER    = 2;

(* Resource utilization information. *)

CONST
  RUSAGE_SELF     = 0;
  RUSAGE_CHILDREN = -1;

TYPE
  struct_rusage_star = UNTRACED REF struct_rusage;
  struct_rusage = RECORD
    ru_utime    : Utime.struct_timeval;  (* user time used *)
    ru_stime    : Utime.struct_timeval;  (* system time used *)
    ru_maxrss   : long;            (* XXX: 0 *)
    ru_ixrss    : long;            (* XXX: 0 *)
    ru_idrss    : long;            (* XXX: sum of rm_asrss *)
    ru_isrss    : long;            (* XXX: 0 *)
    ru_minflt   : long;            (* any page faults not requiring I/O *)
    ru_majflt   : long;            (* any page faults requiring I/O *)
    ru_nswap    : long;            (* swaps *)
    ru_inblock  : long;            (* block input operations *)
    ru_oublock  : long;            (* block output operations *)
    ru_msgsnd   : long;            (* messages sent *)
    ru_msgrcv   : long;            (* messages received *)
    ru_nsignals : long;            (* signals received *)
    ru_nvcsw    : long;            (* voluntary context switches *)
    ru_nivcsw   : long;            (* involuntary " *)
  END;


(* Resource limits *)

CONST
  RLIMIT_CPU   = 0;          (* cpu time in milliseconds *)
  RLIMIT_FSIZE = 1;          (* maximum file size *)
  RLIMIT_DATA  = 2;          (* data size *)
  RLIMIT_STACK = 3;          (* stack size *)
  RLIMIT_CORE  = 4;          (* core file size *)
  RLIMIT_NOFILE= 5;          (* maximum descriptor index + 1 *)
  RLIMIT_VMEM  = 6;          (* maximum mapped memory *)
  RLIMIT_AS    = RLIMIT_VMEM;

  RLIMIT_NLIMITS = 7;          (* number of resource limits *)

  RLIM_INFINITY = 16_7fffffff;

TYPE
  rlim_t = unsigned_long;

  struct_rlimit = RECORD
    rlim_cur: rlim_t;     (* current (soft) limit *)
    rlim_max: rlim_t;     (* maximum value for rlim_cur *)
  END;


(*** getpriority(2), setpriority(2) - get/set program scheduling priority ***)

<*EXTERNAL*> PROCEDURE getpriority (which, who: int): int;
<*EXTERNAL*> PROCEDURE setpriority (which, who, prio: int): int;


(*** getrlimit(2), setrlimit(2) - control maximum system resource
                                  consumption ***)

<*EXTERNAL*> PROCEDURE getrlimit (resource: int; VAR rlp: struct_rlimit): int;
<*EXTERNAL*> PROCEDURE setrlimit (resource: int; VAR rlp: struct_rlimit): int;



(*** getrusage(2) - get information about resource utilization ***)

<*EXTERNAL*> PROCEDURE getrusage (who: int; rus: struct_rusage_star): int;


(*** nice(3) - set program priority ***)

<*EXTERNAL*> PROCEDURE nice (incr: int): int;


END Uresource.
