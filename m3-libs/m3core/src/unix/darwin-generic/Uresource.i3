(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Wed Mar 25 16:45:57 PST 1992 by muller    *)
(* ow 03.10.1994                                              *)

INTERFACE Uresource;

FROM Ctypes IMPORT int, long;
FROM Utypes IMPORT int64_t, id_t;
FROM Utime IMPORT struct_timeval;

(*** <sys/resource.h> ***)

(*
 * Resource limit type (low 63 bits, excluding the sign bit)
 *)
TYPE
  rlim_t = int64_t;

(*****
 * PRIORITY
 *)

(*
 * Possible values of the first parameter to getpriority()/setpriority(),
 * used to indicate the type of the second parameter.
 *)
CONST
  PRIO_PROCESS = 0;
  PRIO_PGRP    = 1;
  PRIO_USER    = 2;

(*
 * Range limitations for the value of the third parameter to setpriority().
 *)
CONST
  PRIO_MIN = -20;
  PRIO_MAX = 20;




(*****
 * RESOURCE USAGE
 *)

(*
 * Possible values of the first parameter to getrusage(), used to indicate
 * the scope of the information to be returned.
 *)
(* Resource utilization information. *)
CONST
  RUSAGE_SELF     = 0;			 (* Current process information *)
  RUSAGE_CHILDREN = -1;			 (* Current process' children *)

(*
 * A structure representing an accounting of resource utilization.  The
 * address of an instance of this structure is the second parameter to
 * getrusage().
 *
 * Note: All values other than ru_utime and ru_stime are implementaiton
 *       defined and subject to change in a future release.  Their use
 *       is discouraged for standards compliant programs.
 *)
TYPE
  struct_rusage = RECORD
    ru_utime: struct_timeval;		 (* user time used *)
    ru_stime: struct_timeval;		 (* system time used *)
    (*
     * Informational aliases for source compatibility with programs
     * that need more information than that provided by standards,
     * and which do not mind being OS-dependent.
     *)
    ru_maxrss: long;			 (* max resident set size *)
    ru_ixrss: long;			 (* integral shared memory size *)
    ru_idrss: long;			 (* integral unshared data " *)
    ru_isrss: long;			 (* integral unshared stack " *)
    ru_minflt: long;			 (* page reclaims *)
    ru_majflt: long;			 (* page faults *)
    ru_nswap: long;			 (* swaps *)
    ru_inblock: long;			 (* block input operations *)
    ru_oublock: long;			 (* block output operations *)
    ru_msgsnd: long;			 (* messages sent *)
    ru_msgrcv: long;			 (* messages received *)
    ru_nsignals: long;			 (* signals received *)
    ru_nvcsw: long;			 (* voluntary context switches *)
    ru_nivcsw: long;			 (* involuntary " *)
  END;
  struct_rusage_star = UNTRACED REF struct_rusage;

(*****
 * RESOURCE LIMITS
 *)

(*
 * Symbolic constants for resource limits; since all limits are representable
 * as a type rlim_t, we are permitted to define RLIM_SAVED_* in terms of
 * RLIM_INFINITY.
 *)
CONST
  RLIM_INFINITY	= 16_7fffffff;		 (* no limit *)
  RLIM_SAVED_MAX = RLIM_INFINITY;	 (* Unrepresentable hard limit *)
  RLIM_SAVED_CUR = RLIM_INFINITY;	 (* Unrepresentable soft limit *)

(*
 * Possible values of the first parameter to getrlimit()/setrlimit(), to
 * indicate for which resource the operation is being performed.
 *)
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

(*
 * A structure representing a resource limit.  The address of an instance
 * of this structure is the second parameter to getrlimit()/setrlimit().
 *)
TYPE
  struct_rlimit = RECORD
    rlim_cur: rlim_t;			 (* current (soft) limit *)
    rlim_max: rlim_t;			 (* maximum value for rlim_cur *)
  END;

<*EXTERNAL*> PROCEDURE getpriority (which: int; who: id_t): int;
<*EXTERNAL*> PROCEDURE getrlimit   (res: int; VAR r: struct_rlimit): int;
<*EXTERNAL*> PROCEDURE getrusage   (who: int; VAR r: struct_rusage): int;
<*EXTERNAL*> PROCEDURE setpriority (which: int; who: id_t; prio: int): int;
<*EXTERNAL*> PROCEDURE setrlimit   (res: int; READONLY r: struct_rlimit): int;

END Uresource.
