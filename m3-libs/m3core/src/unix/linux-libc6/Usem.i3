(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Fri Apr 30 14:44:50 PDT 1993 by muller    *)
(*      modified on Wed Dec 23 17:44:54 PST 1992 by jdd       *)

UNSAFE INTERFACE Usem;

FROM Ctypes IMPORT char, int, unsigned_int, const_char_star;
FROM Utypes IMPORT mode_t;

(*** <bits/semaphore.h> ***)

CONST
  SIZEOF_SEM_T = 16;

(* Value returned if `sem_open' failed.  *)
CONST
  SEM_FAILED = 0;

(* Maximum value the semaphore can have.  *)
CONST
  SEM_VALUE_MAX = 2147483647;

TYPE
  sem_t = RECORD
    data: ARRAY[1..SIZEOF_SEM_T] OF char;
  END;
  sem_t_star = UNTRACED REF sem_t;

(*** <semaphore.h> ***)

<*EXTERNAL sem_init*>
PROCEDURE init (VAR sem: sem_t; pshared: int; value: unsigned_int): int;
<*EXTERNAL sem_destroy*>
PROCEDURE destroy (VAR sem: sem_t): int;
<*EXTERNAL sem_open*>
PROCEDURE open (name: const_char_star; flags: int): sem_t_star;
<*EXTERNAL sem_open*>
PROCEDURE open_create (name: const_char_star;
                       flags: int;
                       mode: mode_t;
                       value: unsigned_int): sem_t_star;
<*EXTERNAL sem_close*>
PROCEDURE close (VAR sem: sem_t): int;
<*EXTERNAL sem_unlink*>
PROCEDURE unlink (name: const_char_star): int;
<*EXTERNAL sem_trywait*>
PROCEDURE trywait (VAR sem: sem_t): int;
<*EXTERNAL sem_wait*>
PROCEDURE wait (VAR sem: sem_t): int;
<*EXTERNAL sem_post*>
PROCEDURE post (VAR sem: sem_t): int;
<*EXTERNAL sem_getvalue*>
PROCEDURE getvalue (VAR sem: sem_t; VAR value: int): int;

END Usem.
