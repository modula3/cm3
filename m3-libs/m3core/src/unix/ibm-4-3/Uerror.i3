(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Tue Jun  4 22:27:54 1991 by muller    *)

INTERFACE Uerror;

FROM Ctypes IMPORT int, char_star;

(*** <errno.h> ***)

CONST
  EPERM   = 1;                 (* Not owner *)
  ENOENT  = 2;                 (* No such file or directory *)
  ESRCH   = 3;                 (* No such process *)
  EINTR   = 4;                 (* Interrupted system call *)
  EIO     = 5;                 (* I/O error *)
  ENXIO   = 6;                 (* No such device or address *)
  E2BIG   = 7;                 (* Arg list too long *)
  ENOEXEC = 8;                 (* Exec format error *)
  EBADF   = 9;                 (* Bad file number *)
  ECHILD  = 10;                (* No children *)
  EAGAIN  = 11;                (* No more processes *)
  ENOMEM  = 12;                (* Not enough core *)
  EACCES  = 13;                (* Permission denied *)
  EFAULT  = 14;                (* Bad address *)
  ENOTBLK = 15;                (* Block device required *)
  EBUSY   = 16;                (* Mount device busy *)
  EEXIST  = 17;                (* File exists *)
  EXDEV   = 18;                (* Cross-device link *)
  ENODEV  = 19;                (* No such device *)
  ENOTDIR = 20;                (* Not a directory*)
  EISDIR  = 21;                (* Is a directory *)
  EINVAL  = 22;                (* Invalid argument *)
  ENFILE  = 23;                (* File table overflow *)
  EMFILE  = 24;                (* Too many open files *)
  ENOTTY  = 25;                (* Not a typewriter *)
  ETXTBSY = 26;                (* Text file busy *)
  EFBIG   = 27;                (* File too large *)
  ENOSPC  = 28;                (* No space left on device *)
  ESPIPE  = 29;                (* Illegal seek *)
  EROFS   = 30;                (* Read-only file system *)
  EMLINK  = 31;                (* Too many links *)
  EPIPE   = 32;                (* Broken pipe *)

  (* math software *)
  EDOM    = 33;                (* Argument too large *)
  ERANGE  = 34;                (* Result too large *)

 (* non-blocking and interrupt i/o *)
  EWOULDBLOCK  = 35;           (* Operation would block *)
  EINPROGRESS  = 36;           (* Operation now in progress *)
  EALREADY     = 37;           (* Operation already in progress *)

  (* ipc/network software *)
        (* argument errors *)
  ENOTSOCK        = 38;   (* Socket operation on non-socket *)
  EDESTADDRREQ    = 39;   (* Destination address required *)
  EMSGSIZE        = 40;   (* Message too long *)
  EPROTOTYPE      = 41;   (* Protocol wrong type for socket *)
  ENOPROTOOPT     = 42;   (* Protocol not available *)
  EPROTONOSUPPORT = 43;   (* Protocol not supported *)
  ESOCKTNOSUPPORT = 44;   (* Socket type not supported *)
  EOPNOTSUPP      = 45;   (* Operation not supported on socket *)
  EPFNOSUPPORT    = 46;   (* Protocol family not supported *)
  EAFNOSUPPORT    = 47;   (* Address family not supported by protocol family *)
  EADDRINUSE      = 48;   (* Address already in use *)
  EADDRNOTAVAIL   = 49;   (* Can't assign requested address *)

        (* operational errors *)
  ENETDOWN        = 50;          (* Network is down *)
  ENETUNREACH     = 51;          (* Network is unreachable *)
  ENETRESET       = 52;          (* Network dropped connection on reset *)
  ECONNABORTED    = 53;          (* Software caused connection abort *)
  ECONNRESET      = 54;          (* Connection reset by peer *)
  ENOBUFS         = 55;          (* No buffer space available *)
  EISCONN         = 56;          (* Socket is already connected *)
  ENOTCONN        = 57;          (* Socket is not connected *)
  ESHUTDOWN       = 58;          (* Can't send after socket shutdown *)
  ETOOMANYREFS    = 59;          (* Too many references: can't splice *)
  ETIMEDOUT       = 60;          (* Connection timed out *)
  ECONNREFUSED    = 61;          (* Connection refused *)

        (* *)
  ELOOP           = 62;          (* Too many levels of symbolic links *)
  ENAMETOOLONG    = 63;          (* File name too long *)

  (* should be rearranged *)
  EHOSTDOWN       = 64;          (* Host is down *)
  EHOSTUNREACH    = 65;          (* No route to host *)
  ENOTEMPTY       = 66;          (* Directory not empty *)

  (* quotas & mush *)
  EPROCLIM        = 67;          (* Too many processes *)
  EUSERS          = 68;          (* Too many users *)
  EDQUOT          = 69;          (* Disc quota exceeded *)

  EVDBAD          = 70;
  ERFS		  = 70;		(* For VICE: Remote file system error -- 
				   could be anything; more precise details
				   should be reported elsewhere
				   by the system *)

  EDUMMY	  = 71;		(*  define it anyway so numbering is correct *)
  ENORMTWD	  = 71;		(* For VICE: Out of remote working
				   directory structures *)

  (* Network File System *)
  ESTALE	  = 72;		(* Stale NFS file handle *)
  EREMOTE	  = 73;		(* Too many levels of remote in path *)

  (* SystemV Record Locking *)
  EDEADLK	  = 80;		(* Deadlock condition. *)
  ENOLCK	  = 81;		(* No record locks available. *)


<*EXTERNAL*>
VAR
  errno: int;

(* Extention by mjordan *)
CONST
  Max = ENOLCK; (* should be exported from Uerror *)

<*EXTERNAL*> VAR
  sys_nerr: int;
  sys_errlist: ARRAY [0..Max] OF char_star;

PROCEDURE GetFrom_sys_errlist(n: INTEGER): char_star RAISES {};
(* returns entry 'n' of the 'sys_errlist' array; a checked runtime error
   unless 0 <= n <= sys_nerr. Its safer and more portable to use this
   procedure than to access the array directly.
*)

END Uerror.
