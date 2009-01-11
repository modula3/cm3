(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> UNSAFE INTERFACE Usem;

FROM Ctypes IMPORT int;

TYPE 
(* This type is not declared correctly. It is only instantiated in C code. *)
  sem_t = RECORD END;

<*EXTERNAL sem_wait*> PROCEDURE wait (VAR sem: sem_t): int;
<*EXTERNAL sem_post*> PROCEDURE post (VAR sem: sem_t): int;
<*EXTERNAL sem_getvalue*> PROCEDURE getvalue (VAR sem: sem_t; VAR value: int): int;

END Usem.
