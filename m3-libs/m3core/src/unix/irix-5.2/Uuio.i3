(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Oct  5 10:19:08 PDT 1994 by ericv         *)
(*      modified on Sat Jun 27 18:03:33 PDT 1992 by muller        *)

INTERFACE Uuio;

FROM Ctypes IMPORT int, char_star, short;
FROM Utypes IMPORT caddr_t, ssize_t, off_t, daddr_t;

(*** sys/uio.h ***)

TYPE

TYPE
  struct_iovec = RECORD
    iov_base: caddr_t;
    iov_len: ssize_t;
  END;
  struct_iovec_star = UNTRACED REF struct_iovec;

  struct_uio = RECORD
    uio_iov: struct_iovec_star;
    uio_iovcnt: int;
    uio_offset: off_t;
    uio_segflg: short;
    uio_fmode: short;
    uio_limit: off_t;
    uio_resid: ssize_t;
    uio_blkno: daddr_t;
  END;

  uio_rw = {UIO_READ, UIO_WRITE};

(*
 * Segment flag values (should be enum).
 *)

CONST
  UIO_NOSPACE  = -1;              (* from user data space *)
  UIO_USERSPACE =	0;		(* from user data space *)
  UIO_SYSSPACE =	1;		(* from system space *)
  UIO_USERISPACE =	2;		(* from user I space *)


(*** read, readv(2) - read from a file ***)

<*EXTERNAL*> PROCEDURE read (d: int; buf: char_star; nbytes: int): int;
<*EXTERNAL*> PROCEDURE readv (d: int; iov: struct_iovec_star;
                              iovcnt: int): int;

(*** write, writev(2) - write on a file ***)

<*EXTERNAL*> PROCEDURE write (d: int; buf: char_star; nbytes: int): int;
<*EXTERNAL*> PROCEDURE writev (d: int; iov: struct_iovec_star;
                               ioveclen: int): int;

END Uuio.
