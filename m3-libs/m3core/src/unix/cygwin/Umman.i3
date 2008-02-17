(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 24 15:00:28 PST 1995 by kalsow                   *)
(*      modified on Tue Feb 14 20:21:44 GMT 1995 by rrw1000@cam.ac.uk        *)
(*      modified on Fri Apr 30 14:41:23 PDT 1993 by muller                   *)

INTERFACE Umman;

FROM Ctypes IMPORT int, void_star (*, const_void_star*);
FROM Utypes IMPORT size_t, off_t;

(*** sys/mman.h ***)

CONST
(*
  PROT_NONE  = 0;
*)
  PROT_READ  = 16_1;
  PROT_WRITE = 16_2;
(*
  PROT_EXEC  = 16_4;

  MAP_FILE    = 0;
  MAP_SHARED  = 1;
*)
  MAP_PRIVATE = 2;
(*
  MAP_TYPE    = 16_f;

  MAP_FIXED   = 16_10;
*)
  MAP_ANON = 16_20;
(*
  MAP_NORESERVE = 16_4000; 
  MAP_AUTOGROW  = 16_8000; 
*)

(*
  MAP_FAILED = LOOPHOLE(-1, void_star);

  MS_ASYNC        = 16_1;
  MS_INVALIDATE   = 16_2;
  MS_SYNC         = 16_4; (* Synchronous memory sync., apparently *)
*)

<*EXTERNAL*>
PROCEDURE mmap (addr: void_star; len: size_t; prot,flags,fd: int; off: off_t)
  : void_star;

(*
<*EXTERNAL*>
PROCEDURE munmap (addr: void_star; len: size_t): int;

<*EXTERNAL*>
PROCEDURE mprotect(addr: void_star; len: size_t; prot: int): int;

<*EXTERNAL*>
PROCEDURE msync(addr: void_star; len: size_t; flags: int): int;

<*EXTERNAL*>
PROCEDURE mlock(addr: const_void_star; len: size_t): int;

<*EXTERNAL*>
PROCEDURE munlock(addr: const_void_star; len: size_t): int;
*)

END Umman.
