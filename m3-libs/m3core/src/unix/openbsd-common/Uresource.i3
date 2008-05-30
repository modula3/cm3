(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uresource;

FROM Ctypes IMPORT int, long;
FROM Utime IMPORT struct_timeval;

CONST
  RUSAGE_SELF     = 0;

TYPE
  struct_rusage = RECORD
    ru_utime: struct_timeval;
    ru_stime: struct_timeval;
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

<*EXTERNAL*> PROCEDURE getrusage (who: int; VAR rus: struct_rusage): int;

END Uresource.
