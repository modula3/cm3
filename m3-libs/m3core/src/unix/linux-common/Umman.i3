(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

INTERFACE Umman;

FROM Ctypes IMPORT int, void_star;
FROM Utypes IMPORT size_t, off_t;

CONST
  PROT_READ  = 1;
  PROT_WRITE = 2;
  MAP_PRIVATE = 2;
  MAP_ANON = 16_20;

<*EXTERNAL "mmap64"*> PROCEDURE mmap (addr: void_star; len: size_t; prot, flags, fd: int; off: off_t) : void_star;

END Umman.
