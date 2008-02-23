(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Fri Feb 24 15:06:47 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:38:05 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Wed Mar 25 16:45:57 PST 1992 by muller            *)

INTERFACE Uresource;

FROM Ctypes IMPORT int, long;
IMPORT Utime;

(*** <sys/resource.h> ***)

(* Resource utilization information. *)

CONST
  RUSAGE_SELF     = 0;

TYPE
  struct_rusage = RECORD
    ru_utime: Utime.struct_timeval;  (* user time used *)
    ru_stime: Utime.struct_timeval;  (* system time used *)
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

(*** getrusage(2) - get information about resource utilization ***)

<*EXTERNAL*> PROCEDURE getrusage (who: int; VAR rus: struct_rusage): int;

END Uresource.
