(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uerror;

FROM Ctypes IMPORT int;

(* CONST *)
VAR
  EACCES: int;
  EADDRINUSE: int;
  EADDRNOTAVAIL: int;
  EAGAIN: int;
  EALREADY: int;
  EBADF: int;
  ECHILD: int;
  ECONNABORTED: int;
  ECONNREFUSED: int;
  ECONNRESET: int;
  EDOM: int;
  EEXIST: int;
  EHOSTDOWN: int;
  EHOSTUNREACH: int;
  EINPROGRESS: int;
  EINTR: int;
  EINVAL: int;
  EIO: int;
  EISCONN: int;
  EISDIR: int;
  EMFILE: int;
  ENAMETOOLONG: int;
  ENETDOWN: int;
  ENETRESET: int;
  ENETUNREACH: int;
  ENFILE: int;
  ENOENT: int;
  ENOEXEC: int;
  ENOMEM: int;
  ENOTEMPTY: int;
  ENOTSOCK: int;
  EPERM: int;
  EPIPE: int;
  ERANGE: int;
  ETIMEDOUT: int;
  EWOULDBLOCK: int;

CONST
  Max = 151; (* approx *)

END Uerror.
