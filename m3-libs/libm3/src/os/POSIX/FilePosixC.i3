(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)

INTERFACE FilePosixC;

FROM Ctypes IMPORT int;
  
(* 1 for TRUE, 0 for FALSE, -1 for error (in errno) *)
<*EXTERNAL*> PROCEDURE RegularFileLockC(fd: int): INTEGER;

(* return value from fnctl; <0 for failure, error in errno *)
<*EXTERNAL*> PROCEDURE RegularFileUnlockC(fd: int): INTEGER;

END FilePosixC.
