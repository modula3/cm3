(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usched;

FROM Ctypes IMPORT int;

(*** <sched.h> ***)

<*EXTERNAL sched_yield*>
PROCEDURE yield (): int;
<*EXTERNAL sched_get_priority_min*>
PROCEDURE get_priority_min (policy: int): int;
<*EXTERNAL sched_get_priority_max*>
PROCEDURE get_priority_max (policy: int): int;

END Usched.
