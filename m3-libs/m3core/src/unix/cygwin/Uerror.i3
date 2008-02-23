(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Mon Jan  5 01:05:58 GMT 1998 by rrw               *)
(*      modified on Fri Feb 24 14:53:26 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:02:55 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Thu Jul 21 00:00:00 1994 by sims@usa.acsys.com    *)
(*      modified on Thu Nov 22 05:20:45 1990 by muller                *)

INTERFACE Uerror;

(*** <errno.h> ***)

CONST
  EPERM           = 1;    (* Not owner / operation not permitted  *)
  ENOENT          = 2;    (* No such file or directory *)
  EINTR           = 4;    (* Interrupted system call *)
  EIO             = 5;    (* I/O error *)
  ENOEXEC         = 8;    (* Exec format error *)
  EBADF           = 9;    (* Bad file number *)
  ECHILD          = 10;   (* No children *)
  EAGAIN          = 11;   (* No more processes *)
  ENOMEM          = 12;   (* Not enough core *)
  EACCES          = 13;   (* Permission denied *)
  EEXIST          = 17;   (* File exists *)
  EISDIR          = 21;   (* Is a directory *)
  EINVAL          = 22;   (* Invalid argument *)
  ENFILE          = 23;   (* File table overflow *)
  EMFILE          = 24;   (* Too many open files *)
  EPIPE           = 32;   (* Broken pipe *)

   (* math software *)
  EDOM            = 33;   (* Argument too large *)
  ERANGE          = 34;   (* Result too large *)

  ENAMETOOLONG    = 91;   (* File name too long *)
  ENOTEMPTY       = 90;   (* Directory not empty *)

   (* Non-blocking I/O  and IPC errors *)
  EWOULDBLOCK     = EAGAIN;   (* Operation would block *)

  (* ipc/network software *)
        (* argument errors *)
  ENOTSOCK        = 108;   (* Socket operation on non-socket *)
  EADDRINUSE      = 112;   (* Address already in use *)
  EADDRNOTAVAIL   = 125;   (* Can't assign requested address *)

   (* operational errors *)
  ENETDOWN        = 115;   (* Network is down *)
  ENETUNREACH     = 114;   (* Network is unreachable *)
  ENETRESET       = 126;   (* Network dropped connection because of reset *)
  ECONNABORTED    = 113;   (* Software caused connection abort *)
  ECONNRESET      = 104;   (* Connection reset by peer *)
  EISCONN         = 127;   (* Transport endpoint is already connected *)
  ETIMEDOUT       = 116;   (* Connection timed out *)
  ECONNREFUSED    = 111;   (* Connection refused *)
  EHOSTDOWN       = 117;   (* Host is down *)
  EHOSTUNREACH    = 118;   (* No route to host *)
  EINPROGRESS     = 119;   (* Operation now in progress *)
  EALREADY        = 120;   (* Operation already in progress *)

(* Extension by mjordan *)
CONST
  Max = 140; (* should be exported from Uerror *)
  
END Uerror.
