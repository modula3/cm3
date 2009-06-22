(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)

INTERFACE FilePosixC;

FROM Ctypes IMPORT int;
  
(* 1 for TRUE, 0 for FALSE, -1 for error (in errno) *)
<*EXTERNAL "FilePosixC__RegularFileLock"*> PROCEDURE RegularFileLock(fd: int): INTEGER;

(* return value from fcntl; <0 for failure, error in errno *)
<*EXTERNAL "FilePosixC__RegularFileUnlock"*> PROCEDURE RegularFileUnlock(fd: int): INTEGER;

END FilePosixC.
