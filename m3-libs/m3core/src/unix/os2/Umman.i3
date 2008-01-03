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
  PROT_NONE  = 0;
  PROT_READ  = 16_1;
  PROT_WRITE = 16_2;
  PROT_EXEC  = 16_4;

  MAP_SHARED  = 1;
  MAP_PRIVATE = 2;
  MAP_TYPE    = 16_f;

  MAP_FIXED   = 16_10;
  MAP_RENAME  = 16_20; (* Doesn't appear to be supported any more - rrw *)
  MAP_ANONYMOUS = 16_20;
  MAP_NORESERVE = 16_40; (* Neither does this - rrw *)

  MAP_GROWSDOWN = 16_400; (* Stack-like segment *)
  MAP_DENYWRITE = 16_800; (* ETXTBSY *)
  MAP_EXECUTABLE = 16_1000; (* Mark it as an executable *)

  (* Up to & including MDV_DONTNEED appears not to be used any more *)
  MAP_NEW  = 16_80000000;

  MADV_NORMAL     = 0;
  MADV_RANDOM     = 1;
  MADV_SEQUENTIAL = 2;
  MADV_WILLNEED   = 3;
  MADV_DONTNEED   = 4;

  MS_ASYNC        = 16_1;
  MS_INVALIDATE   = 16_2;
  MS_SYNC         = 16_4; (* Synchronous memory sync., apparently *)

  (* The following appear not to exist any more either - rrw *)
  MC_SYNC         = 1;
  MC_LOCK         = 2;
  MC_UNLOCK       = 3;
  MC_ADVISE       = 4;
  MC_LOCKAS       = 5;
  MC_UNLOCKAS     = 6;

  MCL_CURRENT     = 16_1;
  MCL_FUTURE      = 16_2;
   
<*EXTERNAL*>
PROCEDURE mmap (addr: caddr_t; len: size_t; prot,flags,fd: int; off: off_t)
  : caddr_t;

<*EXTERNAL*>
PROCEDURE munmap (addr: caddr_t; len: size_t): int;

END Umman.
