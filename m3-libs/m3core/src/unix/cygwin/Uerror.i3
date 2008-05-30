(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uerror;

CONST
  EPERM = 1;
  ENOENT = 2;
  EINTR = 4;
  EIO = 5;
  ENOEXEC = 8;
  EBADF = 9;
  ECHILD = 10;
  EAGAIN = 11;
  ENOMEM = 12;
  EACCES = 13;
  EEXIST = 17;
  EISDIR = 21;
  EINVAL = 22;
  ENFILE = 23;
  EMFILE = 24;
  EPIPE = 32;
  EDOM = 33;
  ERANGE = 34;
  ENAMETOOLONG = 91;
  ENOTEMPTY = 90;
  EWOULDBLOCK = EAGAIN;
  ENOTSOCK = 108;
  EADDRINUSE = 112;
  EADDRNOTAVAIL = 125;
  ENETDOWN = 115;
  ENETUNREACH = 114;
  ENETRESET = 126;
  ECONNABORTED = 113;
  ECONNRESET = 104;
  EISCONN = 127;
  ETIMEDOUT = 116;
  ECONNREFUSED = 111;
  EHOSTDOWN = 117;
  EHOSTUNREACH = 118;
  EINPROGRESS = 119;
  EALREADY = 120;

(* Extension by mjordan *)
CONST
  Max = 140; (* should be exported from Uerror *)
  
END Uerror.

