(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Dec 23 17:44:54 PST 1992 by jdd           *)
(*      modified on Thu Jun 28 20:51:36 1990 by muller        *)

INTERFACE Usem;

FROM Ctypes IMPORT char, int, unsigned_int, const_char_star;
FROM Utypes IMPORT mode_t;

(*** <semaphore.h> ***)

CONST
  SIZEOF_SEM_T = 48;

TYPE
  sem_t = RECORD
    data: ARRAY[1..SIZEOF_SEM_T] OF char;
  END;
  sem_t_star = UNTRACED REF sem_t;

CONST
  SEM_FAILED = -1;

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
<*EXTERNAL sem_wait*>
PROCEDURE wait (VAR sem: sem_t): int;
<*EXTERNAL sem_trywait*>
PROCEDURE trywait (VAR sem: sem_t): int;
<*EXTERNAL sem_post*>
PROCEDURE post (VAR sem: sem_t): int;
<*EXTERNAL sem_getvalue*>
PROCEDURE getvalue (VAR sem: sem_t; VAR value: int): int;

END Usem.
