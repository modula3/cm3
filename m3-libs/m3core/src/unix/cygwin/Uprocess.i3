(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Mar 16 12:35:23 1990 by muller        *)

INTERFACE Uprocess;

FROM Ctypes IMPORT int;

(*** getpid(2) - get process identification ***)

<*EXTERNAL*> PROCEDURE getpid (): int;

END Uprocess.

