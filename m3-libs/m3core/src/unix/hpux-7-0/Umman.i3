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
  PROT_KERNEL= 16_8;

  MAP_SHARED    = 16_01;
  MAP_PRIVATE   = 16_02;
  MAP_FIXED     = 16_04;
  MAP_VARIABLE  = 16_08;
  MAP_ANONYMOUS = 16_10;
  MAP_FILE      = 16_20;

  MADV_NORMAL     = 0;
  MADV_RANDOM     = 1;
  MADV_SEQUENTIAL = 2;
  MADV_WILLNEED   = 3;
  MADV_DONTNEED   = 4;
  MADV_SPACEAVAIL = 5;

  MS_SYNC         = 16_1;
  MS_ASYNC        = 16_2;
  MS_INVALIDATE   = 16_4;

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

END Umman.
