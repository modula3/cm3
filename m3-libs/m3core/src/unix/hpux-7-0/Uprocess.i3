(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jun 27 15:51:41 1990 by piet@cs.ruu.nl *)
(*      modified on Fri Mar 16 12:35:23 1990 by muller        *)

INTERFACE Uprocess;

FROM Ctypes IMPORT int;


(*** getpgrp(2) - get process group ***)

<*EXTERNAL*> PROCEDURE getpgrp (): int;

<*EXTERNAL*> PROCEDURE getpgrp2 (pid: int): int;


(*** getpid(2), getppid(2) - get process identification ***)

<*EXTERNAL*> PROCEDURE getpid (): int;
<*EXTERNAL*> PROCEDURE getppid (): int;


(*** setpgrp - set process group ***)

<*EXTERNAL*> PROCEDURE setpgrp (pid, pgrp: int): int;


END Uprocess.
