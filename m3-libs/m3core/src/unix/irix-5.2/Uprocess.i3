(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Oct  5 15:40:13 PDT 1994 by ericv         *)
(*      modified on Fri Mar 16 12:35:23 1990 by muller        *)

INTERFACE Uprocess;

FROM Utypes IMPORT pid_t;
FROM Ctypes IMPORT int;

(*** <unistd.h> ***)

(*** getpid(2), getppid(2) - get process identification ***)
<*EXTERNAL*> PROCEDURE getpid (): pid_t;
<*EXTERNAL*> PROCEDURE getppid (): pid_t;

(*** getpgrp(2) - get process group -- BSD compatible ***)
<*EXTERNAL getpgid *> PROCEDURE getpgrp (pid: pid_t): pid_t;

(*** setpgrp(2) - set process group -- BSD compatible ***)
<*EXTERNAL setpgid *> PROCEDURE setpgrp (pid, pgrp: pid_t): int;

END Uprocess.
