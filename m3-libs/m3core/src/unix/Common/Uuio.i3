(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Uuio;

FROM Ctypes IMPORT int, void_star, const_void_star;
FROM Cstddef IMPORT size_t, ssize_t;

PROCEDURE read (d: int; buf: void_star; nbytes: size_t): ssize_t;
PROCEDURE write (d: int; buf: const_void_star; nbytes: size_t): ssize_t;

END Uuio.
