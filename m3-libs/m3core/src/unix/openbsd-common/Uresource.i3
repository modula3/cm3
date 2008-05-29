(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uresource;

FROM Ctypes IMPORT int, long;
IMPORT Utime;
FROM Utypes IMPORT rlim_t, id_t;

CONST
  PRIO_MIN = -20;
  PRIO_MAX = 20;
  PRIO_PROCESS = 0;
  PRIO_PGRP    = 1;
  PRIO_USER    = 2;
  RUSAGE_SELF     = 0;
  RUSAGE_CHILDREN = -1;
  RLIMIT_CPU   = 0;
  RLIMIT_FSIZE = 1;
  RLIMIT_DATA  = 2;
  RLIMIT_STACK = 3;
  RLIMIT_CORE  = 4;
  RLIMIT_RSS   = 5;
  RLIMIT_MEMLOCK = 6;
  RLIMIT_NPROC   = 7;
  RLIMIT_OFILE   = 8;
  RLIM_NLIMITS = 9;
  (* RLIM_INFINITY	= (((rlim_t)1 << 63) - 1); *)

TYPE
  struct_rusage = RECORD
    ru_utime: Utime.struct_timeval;
    ru_stime: Utime.struct_timeval;
    ru_maxrss : long;
    ru_ixrss : long;
    ru_idrss : long;
    ru_isrss : long;
    ru_minflt : long;
    ru_majflt : long;
    ru_nswap : long;
    ru_inblock : long;
    ru_oublock : long;
    ru_msgsnd : long;
    ru_msgrcv : long;
    ru_nsignals : long;
    ru_nvcsw : long;
    ru_nivcsw : long;
  END;
  struct_rusage_star = UNTRACED REF struct_rusage;

TYPE
  struct_rlimit = RECORD
    rlim_cur: rlim_t;
    rlim_max: rlim_t;
  END;

<*EXTERNAL*> PROCEDURE getpriority (which: int; who: id_t): int;
<*EXTERNAL*> PROCEDURE setpriority (which: int; who: id_t; prio: int): int;
<*EXTERNAL*> PROCEDURE getrlimit (resource: int; VAR rlp: struct_rlimit): int;
<*EXTERNAL*> PROCEDURE setrlimit (resource: int; VAR rlp: struct_rlimit): int;
<*EXTERNAL*> PROCEDURE getrusage (who: int; VAR rus: struct_rusage): int;
<*EXTERNAL*> PROCEDURE nice (incr: int): int;

END Uresource.
