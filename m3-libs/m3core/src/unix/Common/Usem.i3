(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(*
aix, netbsd, osf1, hpux, ibm-4-3, irix
 no sem_t, no functions declared that Modula-3 pthreads uses
 There is struct_sem = 64 bits with 16 bit alignment but is it the same thing?

freebsd4
 sem_t = 32 bytes with 32bit alignment (8 32bit integers, not confirmed)

darwin
 sem_t = int

solaris
 sem_t = 48 bytes with 64bit alignment

linux-64
 sem_t = 32 bytes with 64bit alignment (4 pointer-sized integers)

linux-32
 sem_t = 16 bytes with 32bit alignment (4 pointer-sized integers)

openbsd
 sem_t = 12 bytes with 32bit alignment (3 32bit integers; not confirmed)

cygwin
 a pointer -- though never used

Modula-3 library code never embeds sem_t in a system-defined larger type.
There is one global sem_t in pthreads code.
Over-allocating it is wasteful, but ok.

use the largest size and largest alignment
  => 48 bytes with 64bit alignment

NOTE: in some of these cases, e.g. openbsd, there might be
an extra level of indirection, which means sem_t just = pointer.
However the "winner", Solaris, is correct.
Linux is correct.
*)

<*EXTERNAL*> UNSAFE INTERFACE Usem;

FROM Ctypes IMPORT int, unsigned;

TYPE 
  sem_t = RECORD
    opaque : ARRAY[0..5] OF LONGINT;
  END;

<*EXTERNAL sem_init*> PROCEDURE init (VAR sem: sem_t; pshared: int; value: unsigned): int;
<*EXTERNAL sem_wait*> PROCEDURE wait (VAR sem: sem_t): int;
<*EXTERNAL sem_post*> PROCEDURE post (VAR sem: sem_t): int;
<*EXTERNAL sem_getvalue*> PROCEDURE getvalue (VAR sem: sem_t; VAR value: int): int;

END Usem.
