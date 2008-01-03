(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 11 16:01:48 PDT 1992 by muller                   *)

INTERFACE Umman;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT caddr_t, size_t, off_t;

(*** sys/mman.h ***)

CONST
(* protections are chosen from these bits, or-ed together *)
  PROT_NONE  = 0;		(* no access to these pages *)
  PROT_READ  = 16_1;		(* pages can be read *)
  PROT_WRITE = 16_2;		(* pages can be written *)
  PROT_EXEC  = 16_4;		(* pages can be executed *)

(* flags contain sharing type, mapping type, and options *)

(* mapping visibility: choose either SHARED or PRIVATE *)
  MAP_SHARED  = 16_1;		(* share changes *)
  MAP_PRIVATE = 16_2;		(* changes are private *)

(* mapping region: choose either FILE or ANONYMOUS *)
  MAP_FILE    = 16_00;		(* map from a file *)
  MAP_ANONYMOUS = 16_10;	(* map an unnamed region *)
  MAP_ANON    = 16_10;		(* map an unnamed region *)
  MAP_TYPE    = 16_f0;		(* the type of the region *)

(* mapping placement: choose either FIXED or VARIABLE *)
  MAP_FIXED   = 16_100;		(* map addr must be exactly as specified *)
  MAP_VARIABLE = 16_0;		(* system can place new region *)

(* advice to madvise *)
  MADV_NORMAL     = 0;		(* no further special treatment *)
  MADV_RANDOM     = 1;		(* expect random page references *)
  MADV_SEQUENTIAL = 2;		(* expect sequential page references *)
  MADV_WILLNEED   = 3;		(* will need these pages *)
  MADV_DONTNEED   = 4;		(* dont need these pages *)
  MADV_SPACEAVAIL = 5;		(* ensure that resources are available *)

(* msem conditions and structure *)
TYPE msemaphore = RECORD
  msem_state: int;		(* The semaphore is locked if non-zero. *)
  msem_wanted: int;		(* Processes are waiting on the semaphore. *)
END;

CONST
  MSEM_UNLOCKED = 0;		(* Initialize the semaphore to unlocked *)
  MSEM_LOCKED = 1;		(* Initialize the semaphore to locked *)
  MSEM_IF_NOWAIT = 2;		(* Do not wait if semaphore is locked *)
  MSEM_IF_WAITERS = 3;		(* Unlock only if there are waiters *)

  MS_ASYNC        = 1;		(* Asynchronous cache flush *)
  MS_SYNC         = 3;		(* Synchronous cache flush *)
  MS_INVALIDATE   = 4;		(* Invalidate cached pages *)

<*EXTERNAL*>
PROCEDURE madvise (addr: caddr_t; len: size_t; behav: int): int;

<*EXTERNAL*>
PROCEDURE mmap (addr: caddr_t; len: size_t; prot,flags,fd: int; off: off_t)
  : caddr_t;

<*EXTERNAL*>
PROCEDURE msync (addr: caddr_t; len: size_t; flags: int): int;

<*EXTERNAL*>
PROCEDURE munmap (addr: caddr_t; len: size_t): int;

<*EXTERNAL*>
PROCEDURE mprotect (addr: caddr_t; len: size_t; prot: int): int;

<*EXTERNAL*>
PROCEDURE mincore (addr: caddr_t; len: size_t; vec: ADDRESS): int;

<*EXTERNAL*>
PROCEDURE msem_init (sem: REF msemaphore; initial_value: int): REF msemaphore;

<*EXTERNAL*>
PROCEDURE msem_lock (sem: REF msemaphore; condition: int): int;

<*EXTERNAL*>
PROCEDURE msem_remove (sem: REF msemaphore): int;

<*EXTERNAL*>
PROCEDURE msem_unlock (sem: REF msemaphore; condition: int): int;

END Umman.
