(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking                  *)
(*      modified on Sat Jul 11 20:44:19 PDT 1992 by muller                   *)

INTERFACE Umman;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT caddr_t, size_t, off_t;

(*** sys/mman.h ***)

  (*
    Protections are chosen from these bits, or-ed together.
    Note - not all implementations literally provide all possible
    combinations.  PROT_WRITE is often implemented as (PROT_READ |
    PROT_WRITE) and (PROT_EXECUTE as PROT_READ | PROT_EXECUTE).
    However, no implementation will permit a write to succeed
    where PROT_WRITE has not been set.  Also, no implementation will
    allow any access to succeed where prot is specified as PROT_NONE.
  *)
CONST
  PROT_NONE  = 0;			 (* pages cannot be accessed *)
  PROT_READ  = 1;			 (* pages can be read *)
  PROT_WRITE = 2;			 (* pages can be written *)
  PROT_EXEC  = 4;			 (* pages can be executed *)

  (* sharing types:  must choose either SHARED or PRIVATE *)
  MAP_SHARED  = 1;			 (* share changes *)
  MAP_PRIVATE = 2;			 (* changes are private *)
  MAP_TYPE    = 16_F;			 (* mask for share type *)

  (* other flags to mmap (or-ed in to MAP_SHARED or MAP_PRIVATE *)
  MAP_FIXED     = 16_10;		 (* user assigns address *)
  MAP_NORESERVE = 16_40;		 (* don't reserve needed swap area *)
  MAP_ANON      = 16_100;		 (* map anonymous pages directly *)
  MAP_ANONYMOUS = MAP_ANON;

  (* advice to madvise *)
  MADV_NORMAL     = 0;			 (* no further special treatment *)
  MADV_RANDOM     = 1;			 (* expect random page references *)
  MADV_SEQUENTIAL = 2;			 (* expect sequential page refs *)
  MADV_WILLNEED   = 3;			 (* will need these pages *)
  MADV_DONTNEED   = 4;			 (* don't need these pages *)

  (* flags to msync *)
  MS_OLDSYNC    = 0;
  MS_SYNC       = 4;			 (* wait for msync *)
  MS_ASYNC      = 1;			 (* return immediately *)
  MS_INVALIDATE = 2;			 (* invalidate caches *)

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

(* shm_open and shm_unlink have no man pages *)

END Umman.
