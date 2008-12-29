(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uuio;

FROM Ctypes IMPORT int, void_star, const_void_star;
FROM Cstddef IMPORT size_t, ssize_t;

<*EXTERNAL*> PROCEDURE read (d: int; buf: void_star; nbytes: size_t): ssize_t;
<*EXTERNAL*> PROCEDURE write (d: int; buf: const_void_star; nbytes: size_t): ssize_t;

END Uuio.
