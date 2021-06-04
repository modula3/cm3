(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uprocess;

FROM Utypes IMPORT pid_t;

<*EXTERNAL "Uprocess__getpid"*>PROCEDURE getpid (): pid_t;

END Uprocess.
