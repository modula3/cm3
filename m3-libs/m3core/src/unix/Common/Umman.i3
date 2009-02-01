(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

<*EXTERNAL*> INTERFACE Umman;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT size_t;

(*** sys/mman.h ***)

(*CONST*)
<*EXTERNAL "Uman_PROT_NONE"*> VAR PROT_NONE: int;
<*EXTERNAL "Uman_PROT_READ"*> VAR PROT_READ: int;
<*EXTERNAL "Uman_PROT_WRITE"*> VAR PROT_WRITE: int;

PROCEDURE mprotect (addr: ADDRESS; len: size_t; prot:int): int;

END Umman.
