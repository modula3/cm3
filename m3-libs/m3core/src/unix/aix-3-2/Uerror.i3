(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Tue Mar 12 21:18:12 1991 by muller    *)

INTERFACE Uerror;

FROM Ctypes IMPORT char_star, int;

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

  ENOMSG    =  35;	(* No message of desired type *)
  EIDRM   = 36;	(* Identifier removed *)
  ECHRNG  = 37;	(* Channel number out of range *)
  EL2NSYNC   = 38;	(* Level 2 not synchronized *)
  EL3HLT   = 39;	(* Level 3 halted *)
  EL3RST   = 40;	(* Level 3 reset *)
  ELNRNG   = 41;	(* Link  number out of range *)
  EUNATCH   = 42;	(* Protocol driver not attached *)
  ENOCSI   = 43;	(* No CSI structure available *)
  EL2HLT   = 44;  	(* Level 2 halted *)
  EDEADLK   = 45;	(* Record locking deadlock *)

  ENOTREADY    = 46 ;	(* Device not ready *)
  EWRPROTECT   = 47;	(* Write-protected media *)
  EFORMAT   = 48;		(* Unformatted or incompatible media *)
  
  ENOLCK     = 49;		(* No locks available *)

  ENOCONNECT    = 50;	(* Cannot establish connection *)
  ESTALE   = 52;		(* Missing file or filestream *)
  EDIST   = 53; 		(* Requests blocked by Administrator *)

  (* non-blocking and intterupt i/o *)
  EWOULDBLOCK   = EAGAIN;	(* Operation would block *)
  EINPROGRESS   = 55;	(* Operation now in progress *)
  EALREADY   = 56;		(* Operation already in progress *)

(* ipc/network software *)
	(* argument errors *)
  ENOTSOCK   = 57;		(* Socket operation on non-socket *)
  EDESTADDRREQ   = 58;	(* Destination address required *)
  EMSGSIZE   = 59;		(* Message too long *)
  EPROTOTYPE   = 60;	(* Protocol wrong type for socket *)
  ENOPROTOOPT   = 61;	(* Protocol not available *)
  EPROTONOSUPPORT   = 62;	(* Protocol not supported *)
  ESOCKTNOSUPPORT   = 63;	(* Socket type not supported *)
  EOPNOTSUPP   = 64;	(* Operation not supported on socket *)
  EPFNOSUPPORT   = 65;	(* Protocol family not supported *)
  EAFNOSUPPORT   = 66;	(* Address family not supported by protocol *)
  EADDRINUSE   = 67;	(* Address already in use *)
  EADDRNOTAVAIL   = 68;	(* Cannot assign requested address *)

	(* operational error *)
  ENETDOWN   = 69;	(* Network is down *)
  ENETUNREACH   = 70;	(* Network is unreachable *)
  ENETRESET   = 71;		(* Network dropped connection on reset *)
  ECONNABORTED    = 72;        (* Software caused connection abort *)
  ECONNRESET      = 73;             	(* Connection reset by peer *)
  ENOBUFS         = 74;   	(* No buffer space available *)
  EISCONN         = 75;             	(* Socket is already connected *)
  ENOTCONN        = 76;             	(* Socket is not connected *)
  ESHUTDOWN       = 77;             	(* Can't send after socket shutdown *)
  ETIMEDOUT       = 78;             	(* Connection timed out *)
  ECONNREFUSED    = 79;         (* Connection refused *)

  EHOSTDOWN       = 80;         (* Host is down *)
  EHOSTUNREACH    = 81;         (* No route to host *)

  ERESTART	  = 82;		(* restart the system call *)
  EPROCLIM	  = 83;		(* too many processes *)
  EUSERS	  = 84;		(* too many users *)

  ELOOP           = 85;         (* Symbolic link loop *)
  ENAMETOOLONG    = 86;         (* File name too long *)
  ENOTEMPTY	  = EEXIST;	(* directory not empty *)
  EDQUOT          = 88;         (* Disc quota exceeded *)

  EREMOTE	= 93;		(* Item is not local to host *)
  ENOSYS	= 109;		(* function not implemented POSIX *)
  EMEDIA	= 110;		(* media surface error *)
  ESOFT		= 111;		(* I/O completed, but need relocation *)
  ENOATTR	= 112;		(* no attribute found *)
  ESAD		= 113;		(* security authentication denied *)
  ENOTRUST	= 114;		(* no a trusted program *)


<*EXTERNAL*>
VAR
  errno: int;

(* Extention by mjordan *)
CONST
  Max = ENOTRUST; (* should be exported from Uerror *)

<*EXTERNAL*> VAR
  sys_nerr: int;
  sys_errlist: ARRAY [0..Max] OF char_star;

PROCEDURE GetFrom_sys_errlist(n: INTEGER): char_star RAISES {};
(* returns entry 'n' of the 'sys_errlist' array; a checked runtime error
   unless 0 <= n <= sys_nerr. Its safer and more portable to use this
   procedure than to access the array directly.
*)

END Uerror.
