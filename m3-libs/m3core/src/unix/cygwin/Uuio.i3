(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Apr 29 15:40:46 PDT 1994 by kalsow    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Wed Mar  4 11:53:52 PST 1992 by muller    *)

INTERFACE Uuio;

FROM Ctypes IMPORT int, char_star, void_star;

(*** sys/uio.h ***)

TYPE

TYPE
  struct_iovec = RECORD
    iov_base: void_star;
    iov_len: int;
  END;
  struct_iovec_star = UNTRACED REF struct_iovec;

(*** read, readv(2) - read from a file ***)

<*EXTERNAL*> PROCEDURE read (d: int; buf: char_star; nbytes: int): int;

(*** write, writev(2) - write on a file ***)

<*EXTERNAL*> PROCEDURE write (d: int; buf: char_star; nbytes: int): int;

END Uuio.
