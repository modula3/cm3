(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Apr  7 18:35:01 PDT 1993 by muller                   *)

INTERFACE Umman;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT caddr_t, size_t, off_t;

(*** sys/mman.h ***)

CONST
  PROT_NONE  = 0;
  PROT_READ  = 16_1;
  PROT_WRITE = 16_2;
  PROT_EXEC  = 16_4;

  MAP_SHARED  = 1;
  MAP_PRIVATE = 2;

  MAP_FILE    = 16_00;
  MAP_ANONYMOUS = 16_10;
  MAP_ANON      = 16_10;
  MAP_TYPE      = 16_F0;

  MAP_FIXED   = 16_100;
  MAP_VARIABLE = 16_00;

  MAP_HASSEMAPHORE = 16_0200;
  MAP_INHERIT      = 16_0400;
  MAP_UNALIGNED    = 16_0800;

  MADV_NORMAL     = 0;
  MADV_RANDOM     = 1;
  MADV_SEQUENTIAL = 2;
  MADV_WILLNEED   = 3;
  MADV_DONTNEED   = 4;
  MADV_SPACEAVAIL = 5;

TYPE
  msemaphore = RECORD
    msem_state: int;
    msem_wanted: int; END;
  msemaphore_star = UNTRACED REF msemaphore;

CONST
  MSEM_UNLOCKED =   0;
  MSEM_LOCKED = 1;
  MSEM_IF_NOWAIT = 2;
  MSEM_IF_WAITERS = 3;

  MS_ASYNC = 1;
  MS_SYNC = 3;
  MS_INVALIDATE = 4;


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
PROCEDURE mvalid (addr: caddr_t; len: size_t; prot: int): int;

<*EXTERNAL*>
PROCEDURE msem_init (sem: msemaphore_star; initial_value: int);

<*EXTERNAL*>
PROCEDURE msem_lock (sem: msemaphore_star; condition: int): int;

<*EXTERNAL*>
PROCEDURE msem_remove (sem: msemaphore_star): int;

<*EXTERNAL*>
PROCEDURE msem_unlock (sem: msemaphore_star; condition: int): int;

(* shm_open and shm_unlink have no man pages *)

END Umman.
