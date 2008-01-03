(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Jun 29 09:25:11 1990 by piet@cs.ruu.nl *)
(*      modified on Fri May  4 21:11:56 1990 by muller        *)

INTERFACE Uuio;

FROM Ctypes IMPORT int, char_star;
FROM Utypes IMPORT caddr_t;

(*** sys/uio.h ***)

TYPE
  struct_iovec = RECORD
    iov_base: caddr_t;
    iov_len: int;
  END;
  struct_iovec_star = UNTRACED REF struct_iovec;

  struct_uio = RECORD
    uio_iov: struct_iovec_star;
    uio_iovcnt: int;
    uio_offset: int;
    uio_seg: int;
    uio_resid: int;
    uio_fpflags: int;
  END;

  uio_rw = {UIO_READ, UIO_WRITE};

(*
 * Segment flag values (should be enum).
 *)

CONST
  UIO_USERSPACE =	0;		(* from user data space *)
  UIO_SYSSPACE =	1;		(* from system space *)
  UIO_USERISPACE =	2;		(* from user I space *)
  UIOSEG_USER =		0;
  UIOSEG_KERNEL =	1;
  UIOSEG_INSTR =	2;
  UIOSEG_LBCOPY =	3;

CONST
  MAX_IOVEC = 		16;		(* maximum length of io vectors *)
  MAXIOV    =    MAX_IOVEC;


(*** read, readv(2) - read from a file ***)

<*EXTERNAL*> PROCEDURE read (d: int; buf: char_star; nbytes: int): int;
<*EXTERNAL*> PROCEDURE readv (d: int; iov: struct_iovec_star;
                              iovcnt: int): int;

(*** write, writev(2) - write on a file ***)

<*EXTERNAL*> PROCEDURE write (d: int; buf: char_star; nbytes: int): int;
<*EXTERNAL*> PROCEDURE writev (d: int; iov: struct_iovec_star;
                               ioveclen: int): int;

END Uuio.
