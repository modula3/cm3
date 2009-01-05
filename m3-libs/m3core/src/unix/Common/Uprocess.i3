(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Uprocess;

FROM Ctypes IMPORT int;

PROCEDURE getpid (): int;

END Uprocess.
