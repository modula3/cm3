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
  PROT_NONE  = 0;
  PROT_READ  = 16_1;
  PROT_WRITE = 16_2;
  PROT_EXEC  = 16_4;

  MAP_SHARED  = 1;
  MAP_PRIVATE = 2;
  MAP_TYPE    = 16_f;

  MAP_FIXED   = 16_10;
  MAP_RENAME  = 16_20;
  MAP_NORESERVE = 16_40;

  MAP_NEW  = 16_80000000;

  MADV_NORMAL     = 0;
  MADV_RANDOM     = 1;
  MADV_SEQUENTIAL = 2;
  MADV_WILLNEED   = 3;
  MADV_DONTNEED   = 4;

  MS_ASYNC        = 16_1;
  MS_INVALIDATE   = 16_2;

  MC_SYNC         = 1;
  MC_LOCK         = 2;
  MC_UNLOCK       = 3;
  MC_ADVISE       = 4;
  MC_LOCKAS       = 5;
  MC_UNLOCKAS     = 6;

  MCL_CURRENT     = 16_1;
  MCL_FUTURE      = 16_2;
   
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


END Umman.
