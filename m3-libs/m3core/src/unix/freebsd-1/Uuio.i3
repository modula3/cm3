(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Oct 17 10:34:55 PDT 1994 by kalsow    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Wed Mar  4 11:53:52 PST 1992 by muller    *)
(* ow 30.09.1994 *)

INTERFACE Uuio;

FROM Ctypes IMPORT int, char_star, void_star;

(*** sys/uio.h ***)


TYPE
  struct_iovec = RECORD
    iov_base: void_star;
    iov_len: int;
  END;
  struct_iovec_star = UNTRACED REF struct_iovec;

  (* There's no corresponding structure to struct_uio - be careful *)
  struct_uio = RECORD
    uio_iov: struct_iovec_star;
    uio_iovcnt: int;
    uio_offset: int;
    uio_resid: int;
    uio_segflg: int;
    uio_flag: int;
    uio_procp: void_star;
  END;

  uio_rw = {UIO_READ, UIO_WRITE};

(*
 * Segment flag values (should be enum).
 *)

CONST
  (* There appear to be no corresponing definitions for any of these
     constants - use with caution. *)
  UIO_USERSPACE =	0;		(* from user data space *)
  UIO_SYSSPACE =	1;		(* from system space *)
  UIO_USERISPACE =	2;		(* from user I space *)

CONST
  MAX_IOVEC = 		1024;		(* maximum length of io vectors *)



(*** read, readv(2) - read from a file ***)

<*EXTERNAL*> PROCEDURE read (d: int; buf: char_star; nbytes: int): int;
<*EXTERNAL*> PROCEDURE readv (d: int; iov: struct_iovec_star;
                              iovcnt: int): int;

(*** write, writev(2) - write on a file ***)

<*EXTERNAL*> PROCEDURE write (d: int; buf: char_star; nbytes: int): int;
<*EXTERNAL*> PROCEDURE writev (d: int; iov: struct_iovec_star;
                               ioveclen: int): int;

END Uuio.
