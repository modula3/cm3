(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Wed Mar 25 16:45:57 PST 1992 by muller    *)
(* ow 03.10.1994                                              *)

INTERFACE Uresource;

FROM Ctypes IMPORT int, long;
FROM Utypes IMPORT rlim_t, fixpt_t;
FROM Utime IMPORT struct_timeval;

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
  struct_rusage = RECORD
    ru_utime: struct_timeval;  (* user time used *)
    ru_stime: struct_timeval;  (* system time used *)
    ru_maxrss: long;
    ru_ixrss: long;            (* integral shared text size *)
    ru_idrss: long;            (* integral unshared data " *)
    ru_isrss: long;            (* integral unshared stack " *)
    ru_minflt: long;           (* page reclaims *)
    ru_majflt: long;           (* page faults *)
    ru_nswap: long;            (* swaps *)
    ru_inblock: long;          (* block input operations *)
    ru_oublock: long;          (* block output operations *)
    ru_msgsnd: long;           (* messages sent *)
    ru_msgrcv: long;           (* messages received *)
    ru_nsignals: long;         (* signals received *)
    ru_nvcsw: long;            (* voluntary context switches *)
    ru_nivcsw: long;           (* involuntary " *)
  END;
  struct_rusage_star = UNTRACED REF struct_rusage;

(* Resource limits *)

CONST
  RLIMIT_CPU     = 0;			 (* cpu time in milliseconds *)
  RLIMIT_FSIZE   = 1;			 (* maximum file size *)
  RLIMIT_DATA    = 2;			 (* data size *)
  RLIMIT_STACK   = 3;			 (* stack size *)
  RLIMIT_CORE    = 4;			 (* core file size *)
  RLIMIT_RSS     = 5;			 (* resident set size *)
  RLIMIT_MEMLOCK = 6;			 (* locked-in-memory address space *)
  RLIMIT_NPROC   = 7;			 (* number of processes *)
  RLIMIT_NOFILE  = 8;			 (* number of open files *)
  
  RLIM_NLIMITS   = 9;			 (* number of resource limits *)

  RLIM_INFINITY	= 16_7fffffff;

TYPE
  struct_rlimit = RECORD
    rlim_cur: rlim_t;			 (* current (soft) limit *)
    rlim_max: rlim_t;			 (* maximum value for rlim_cur *)
  END;

  (* Load average structure. *)
  struct_loadavg = RECORD
    ldavg: ARRAY [0..2] OF fixpt_t;
    fscale: long;
  END;

<*EXTERNAL*> PROCEDURE getpriority (which, who: int): int;
<*EXTERNAL*> PROCEDURE getrlimit   (res: int; VAR r: struct_rlimit): int;
<*EXTERNAL*> PROCEDURE getrusage   (who: int; VAR r: struct_rusage): int;
<*EXTERNAL*> PROCEDURE setpriority (which, who, prio: int): int;
<*EXTERNAL*> PROCEDURE setrlimit   (res: int; READONLY r: struct_rlimit): int;

END Uresource.
