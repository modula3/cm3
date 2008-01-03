(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 30 13:38:11 PDT 1992 by muller                   *)

INTERFACE Umman;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT caddr_t, size_t, off_t;

(*** sys/mman.h ***)

CONST
  PROT_READ  = 16_1;
  PROT_WRITE = 16_2;
  PROT_EXEC  = 16_4;

  MAP_SHARED  = 1;
  MAP_PRIVATE = 2;

  MAP_FIXED   = 16_100;

  MADV_NORMAL     = 0;
  MADV_RANDOM     = 1;
  MADV_SEQUENTIAL = 2;
  MADV_WILLNEED   = 3;
  MADV_DONTNEED   = 4;

<*EXTERNAL*>
PROCEDURE mmap (addr: caddr_t; len: size_t; prot,flags,fd: int; 
                off: off_t): caddr_t;

<*EXTERNAL*>
PROCEDURE munmap (addr: caddr_t; len: size_t): caddr_t;

<*EXTERNAL*>
PROCEDURE mprotect (addr: caddr_t; len, prot: int): int;

END Umman.
