(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Usem;

FROM Ctypes IMPORT int, unsigned;

TYPE
 (* 32bit: 16 bytes with 32 bit alignment *)
 (* 64bit: 32 bytes with 64 bit alignment *)
 (* which works out to be 4 INTEGERs either way *)
  struct_sem = RECORD
    opaque : ARRAY [0..3] OF INTEGER;
  END;
  sem_t = UNTRACED REF struct_sem;
  sem_t_star = UNTRACED REF sem_t;

<*EXTERNAL sem_init*> PROCEDURE init (VAR sem: sem_t; pshared: int; value: unsigned): int;
<*EXTERNAL sem_wait*> PROCEDURE wait (VAR sem: sem_t): int;
<*EXTERNAL sem_post*> PROCEDURE post (VAR sem: sem_t): int;
<*EXTERNAL sem_getvalue*> PROCEDURE getvalue (VAR sem: sem_t; VAR value: int): int;

END Usem.
