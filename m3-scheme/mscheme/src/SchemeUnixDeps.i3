(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Wed Mar 25 16:45:57 PST 1992 by muller    *)
(* ow 03.10.1994                                              *)

INTERFACE SchemeUnixDeps;

FROM Ctypes IMPORT int, long, char_star;
IMPORT Pathname;

(* this file should also be replaced *)

(*** <sys/resource.h> ***)

(* Resource utilization information. *)

CONST
  RUSAGE_SELF     = 0;
  RUSAGE_CHILDREN = -1;

TYPE
  struct_timeval = RECORD tv_sec, tv_usec : INTEGER END;

  struct_rusage = RECORD
    ru_utime: struct_timeval;  (* user time used *)
    ru_stime: struct_timeval;  (* system time used *)
    ru_maxrss: long;
    ru_ixrss: long;            (* integral shared text size *)
    (* Unsupported in Linux 1.0:
    ru_ismrss: long;           (* integral shared memory size*)
    ******************************)
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

(*** getrusage(2) - get information about resource utilization ***)

<*EXTERNAL*> PROCEDURE getrusage (who: int; VAR rus: struct_rusage): int;

PROCEDURE GetCurrentUser() : TEXT RAISES { Error };
  (* string name of current user *)

PROCEDURE GetHomeDir(user : TEXT) : Pathname.T RAISES { Error };
  (* home directory of specified user *)

EXCEPTION Error;
  
<*EXTERNAL SchemeUnixDeps__getCurrentUserWrapper*>
PROCEDURE getCurrentUserWrapper() : char_star;

<*EXTERNAL SchemeUnixDeps__getErrno*>
PROCEDURE GetErrno() : INTEGER;

<*EXTERNAL SchemeUnixDeps__getHomeDirWrapper*>
PROCEDURE getHomeDirWrapper(user : char_star) : char_star;

END SchemeUnixDeps.
