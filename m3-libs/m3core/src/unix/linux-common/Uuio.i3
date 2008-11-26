(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uuio;

FROM Ctypes IMPORT int, void_star, const_void_star;
FROM Cstddef IMPORT size_t;
FROM Utypes IMPORT ssize_t;

TYPE
  struct_iovec = RECORD
    iov_base: void_star;
    iov_len: size_t;
  END;
  struct_iovec_star = UNTRACED REF struct_iovec;

<*EXTERNAL*> PROCEDURE read (d: int; buf: void_star; nbytes: size_t): ssize_t;
<*EXTERNAL*> PROCEDURE write (d: int; buf: const_void_star; nbytes: size_t): ssize_t;

END Uuio.
