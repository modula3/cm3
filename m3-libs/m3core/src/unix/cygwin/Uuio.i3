(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uuio;

FROM Ctypes IMPORT int, char_star, void_star;

TYPE
  struct_iovec = RECORD
    iov_base: void_star;
    iov_len: int;
  END;
  struct_iovec_star = UNTRACED REF struct_iovec;

<*EXTERNAL*> PROCEDURE read (d: int; buf: char_star; nbytes: int): int;
<*EXTERNAL*> PROCEDURE write (d: int; buf: char_star; nbytes: int): int;

END Uuio.
