(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Sun May 27 05:39:11 1990 by muller        *)

INTERFACE Uprocess;

FROM Ctypes IMPORT int;


(*** getpgrp(2) - get process group ***)

<*EXTERNAL*> PROCEDURE getpgrp (pid: int): int;


(*** getpid(2), getppid(2) - get process identification ***)

<*EXTERNAL*> PROCEDURE getpid (): int;
<*EXTERNAL*> PROCEDURE getppid (): int;


(*** setpgrp - set process group ***)

<*EXTERNAL*> PROCEDURE setpgrp (): int;



END Uprocess.
