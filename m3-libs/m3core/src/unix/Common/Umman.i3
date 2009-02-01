(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

INTERFACE Umman;

FROM Ctypes IMPORT int;
FROM Cstddef IMPORT size_t;

(*CONST*)
VAR
  PROT_NONE: int;
  PROT_READ: int;
  PROT_WRITE: int;

<*EXTERNAL*> PROCEDURE mprotect (addr: ADDRESS; len: size_t; prot:int): int;

END Umman.
