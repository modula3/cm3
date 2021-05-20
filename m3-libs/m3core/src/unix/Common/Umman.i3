(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

INTERFACE Umman;

FROM Ctypes IMPORT int, void_star;
FROM Cstddef IMPORT size_t;
FROM Utypes IMPORT off_t;

TYPE caddr_t = ADDRESS;

(* This first set very portable. *)
<*EXTERNAL Umman__PROT_NONE*> VAR PROT_NONE: int;
<*EXTERNAL Umman__PROT_READ*> VAR PROT_READ: int;
<*EXTERNAL Umman__PROT_WRITE*> VAR PROT_WRITE: int;

(* This second set not tested across multiple platforms. *)
<*EXTERNAL Umman__PROT_EXEC*> VAR PROT_EXEC: int;
<*EXTERNAL Umman__MAP_SHARED*> VAR MAP_SHARED: int;
<*EXTERNAL Umman__MAP_PRIVATE*> VAR MAP_PRIVATE: int;
<*EXTERNAL Umman__MAP_FIXED*> VAR MAP_FIXED: int;
<*EXTERNAL Umman__MAP_RENAME*> VAR MAP_RENAME: int;
<*EXTERNAL Umman__MAP_NORESERVE*> VAR MAP_NORESERVE: int;
<*EXTERNAL Umman__MAP_HASSEMAPHORE*> VAR MAP_HASSEMAPHORE: int;
<*EXTERNAL Umman__MAP_STACK*> VAR MAP_STACK: int;
<*EXTERNAL Umman__MAP_NOSYNC*> VAR MAP_NOSYNC: int;
<*EXTERNAL Umman__MAP_FILE*> VAR MAP_FILE: int;
<*EXTERNAL Umman__MAP_ANON*> VAR MAP_ANON: int;
<*EXTERNAL Umman__MAP_NOCORE*> VAR MAP_NOCORE: int;

<*EXTERNAL Umman__mprotect*>PROCEDURE mprotect (addr: caddr_t; len: size_t; prot:int): int;
<*EXTERNAL Umman__mmap*>    PROCEDURE mmap     (addr: caddr_t; len: size_t; prot, flags, fd: int; off: off_t): void_star;
<*EXTERNAL Umman__munmap*>  PROCEDURE munmap  ( addr: caddr_t; len: size_t): int;

END Umman.
