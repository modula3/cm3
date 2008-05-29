(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

UNSAFE INTERFACE Usem;

FROM Ctypes IMPORT int, unsigned;

CONST
  PZERO = 22;
  SEM_A = 8_0200;
  SEM_R = 8_0400;
  SEM_UNDO = 8_010000;
  GETPID = 4;
  GETVAL = 5;
  GETALL = 6;
  GETNCNT = 3;
  GETZCNT = 7;
  SETVAL = 8;
  SETALL = 9;
  SEMMNI = 10;
  SEMMNS = 60;
  SEMMNU = 30;
  SEMMSL = SEMMNS;
  SEMOPM = 100;
  SEMUME = 10;
  SEMVMX = 32767;
  SEMAEM = 16384;

TYPE 
  struct_sem = RECORD
    opaque : ARRAY [0..2] OF int;
  END;
  sem_t = UNTRACED REF struct_sem;
  sem_t_star = UNTRACED REF sem_t;

(* sem_open, sem_close, sem_unlink are in semaphore.h but man pages say they are not
implemented and always fail *)

<*EXTERNAL sem_init*> PROCEDURE init (VAR sem: sem_t; pshared: int; value: unsigned): int;
<*EXTERNAL sem_destroy*> PROCEDURE destroy (VAR sem: sem_t): int;
<*EXTERNAL sem_trywait*> PROCEDURE trywait (VAR sem: sem_t): int;
<*EXTERNAL sem_wait*> PROCEDURE wait (VAR sem: sem_t): int;
<*EXTERNAL sem_post*> PROCEDURE post (VAR sem: sem_t): int;
<*EXTERNAL sem_getvalue*> PROCEDURE getvalue (VAR sem: sem_t; VAR value: int): int;

END Usem.
