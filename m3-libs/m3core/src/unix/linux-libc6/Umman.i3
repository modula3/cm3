(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 24 15:00:28 PST 1995 by kalsow                   *)
(*      modified on Tue Feb 14 20:21:44 GMT 1995 by rrw1000@cam.ac.uk        *)
(*      modified on Fri Apr 30 14:41:23 PDT 1993 by muller                   *)

INTERFACE Umman;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT caddr_t, size_t, off_t;

(*** sys/mman.h ***)

CONST
  PROT_READ  = 16_1; (* page can be read *)
  PROT_WRITE = 16_2; (* page can be written *)
  PROT_EXEC  = 16_4; (* page can be executed *)
  PROT_NONE  = 16_0; (* page can not be accessed *)

  MAP_SHARED    = 16_01; (* Share changes *)
  MAP_PRIVATE   = 16_02; (* Changes are private *)
  MAP_TYPE      = 16_0f; (* Mask for type of mapping *)
  MAP_FIXED     = 16_10; (* Interpret addr exactly *)
  MAP_ANONYMOUS = 16_20; (* don't use a file *)


  MAP_GROWSDOWN  = 16_0100; (* stack-like segment *)
  MAP_DENYWRITE  = 16_0800; (* ETXTBSY *)
  MAP_EXECUTABLE = 16_1000; (* mark it as an executable *)
  MAP_LOCKED     = 16_2000; (* pages are locked *)
  MAP_NORESERVE  = 16_4000; (* don't check for reservations *)

  MS_ASYNC        = 1; (* sync memory asynchronosly *)
  MS_INVALIDATE   = 2; (* invalidate the caches *)
  MS_SYNC         = 4; (* synchronous memory sync *)

  MCL_CURRENT     = 1; (* lock all current mappings *)
  MCL_FUTURE      = 2; (* lock all future mappings *)
   
  MADV_NORMAL     = 16_0; (* default page-in behavior *)
  MADV_RANDOM     = 16_1; (* page-in minimum required *)
  MADV_SEQUENTIAL = 16_2; (* read-ahead aggressively *)
  MADV_WILLNEED   = 16_3; (* pre-fault pages *)
  MADV_DONTNEED   = 16_4; (* discard these pages *)

  (* compatibility flags *)
  MAP_ANON = MAP_ANONYMOUS;
  MAP_FILE = 0;

<*EXTERNAL*>
PROCEDURE mmap (addr: caddr_t; len: size_t; prot,flags,fd: int; off: off_t)
  : caddr_t;

<*EXTERNAL*>
PROCEDURE munmap (addr: caddr_t; len: size_t): int;

<*EXTERNAL*>
PROCEDURE mremap(addr : caddr_t; len : size_t; new : size_t; flags : int) : caddr_t;

<*EXTERNAL*>
PROCEDURE mprotect(addr: caddr_t; len: size_t; prot: int): int;

<*EXTERNAL*>
PROCEDURE msync(addr: caddr_t; len: size_t; flags: int): int;

<*EXTERNAL*>
PROCEDURE mlock(addr: caddr_t; len: size_t): int;

<*EXTERNAL*>
PROCEDURE munlock(addr: caddr_t; len: size_t): int;

<*EXTERNAL*>
PROCEDURE mlockall(flags: int): int;

<*EXTERNAL*>
PROCEDURE munlockall(): int;

END Umman.
