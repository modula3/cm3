(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Oct  6 11:02:30 PDT 1994 by ericv                    *)
(*      modified on Sat Jul 11 20:44:19 PDT 1992 by muller                   *)

INTERFACE Umman;

FROM Word IMPORT Or;
FROM Ctypes IMPORT int, void_star;
FROM Utypes IMPORT size_t, off_t;

(*** sys/mman.h ***)

(* mmap flags and protections *)

CONST
  MAP_SHARED	 = 16_1;	(* share changes *)
  MAP_PRIVATE	 = 16_2;	(* changes are private *)
  MAP_TYPE	 = 16_f;	(* mask for mapping type *)

  MAP_FIXED	 = 16_010;	(* interpret addr exactly *)
  MAP_RENAME	 = 16_020;	(* assign page to file *)
  MAP_AUTOGROW	 = 16_040;	(* file grows with store access *)
  MAP_LOCAL	 = 16_080;	(* separate copies made on fork/sproc *)
  MAP_AUTORESRV	 = 16_100;	(* logical swap reserved on demand *)

  PROT_NONE	 = 16_0;	(* page can not be accessed *)
  PROT_READ	 = 16_1;	(* page can be read *)
  PROT_WRITE	 = 16_2;	(* page can be written *)
  PROT_EXECUTE	 = 16_4;	(* page can be executed *)
  PROT_EXEC	 = 16_4;	(* SVR4 SVID name for PROT_EXECUTE *)

  PROC_TEXT	 = Or(PROT_EXEC, PROT_READ);
  PROC_DATA	 = Or(PROT_READ, PROT_WRITE);
  SHARED           = 16_10;
  PRIVATE          = 16_20;

(*	msync flags
*)
  MS_SYNC	 = 16_0;	(* wait for msync *)
  MS_ASYNC	 = 16_1;	(* return immediately *)
  MS_INVALIDATE	 = 16_2;	(* invalidate mappings *)

(*	advice to madvise
*)
  MADV_NORMAL	 = 0;		(* no further special treatment *)
  MADV_RANDOM	 = 1;		(* expect random page references *)
  MADV_SEQUENTIAL = 2;		(* expect sequential page references *)
  MADV_WILLNEED	 = 3;		(* will need these pages *)
  MADV_DONTNEED	 = 4;		(* don't need these pages *)

(* flags to memcntl *)
  MC_SYNC          = 1;               (* sync with backing store *)
  MC_LOCK          = 2;               (* lock pages in memory *)
  MC_UNLOCK        = 3;               (* unlock pages from memory *)
  MC_ADVISE        = 4;               (* give advice to management *)
  MC_LOCKAS        = 5;               (* lock address space in memory *)
  MC_UNLOCKAS      = 6;               (* unlock address space from memory *)

(* flags to mlockall *)
  MCL_CURRENT      = 16_1;             (* lock current mappings *)
  MCL_FUTURE       = 16_2;             (* lock future mappings *)


<*EXTERNAL*>
PROCEDURE mmap (addr: void_star; len: size_t; prot,flags,fd: int; off: off_t)
  : void_star;

<*EXTERNAL*>
PROCEDURE msync (addr: void_star; len: size_t; flags: int): int;

<*EXTERNAL*>
PROCEDURE munmap (addr: void_star; len: size_t): int;

<*EXTERNAL*>
PROCEDURE madvise (addr: void_star; len: size_t; behav: int): int;

<*EXTERNAL*>
PROCEDURE mprotect (addr: void_star; len: size_t; prot: int): int;

END Umman.
