(* Copyright (C) 1990, Digital Equipment Corporation.          *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Feb  7 02:46:07 1991 by piet@cs.ruu.nl *)

INTERFACE Uerror;

FROM Ctypes IMPORT int, char_star;

(*** <errno.h> ***)

CONST

EPERM		= 1;	(* Not super-user *)
ENOENT		= 2;	(* No such file or directory *)
ESRCH		= 3;	(* No such process *)
EINTR		= 4;	(* interrupted system call *)
EIO		= 5;	(* I/O error *)
ENXIO		= 6;	(* No such device or address *)
E2BIG		= 7;    (* Arg list too long *)
ENOEXEC		= 8;	(* Exec format error *)
EBADF		= 9;	(* Bad file number *)
ECHILD		= 10;	(* No children *)
EAGAIN		= 11;	(* No more processes *)
ENOMEM		= 12;	(* Not enough core *)
EACCES		= 13;   (* Permission denied *)
EFAULT		= 14;	(* Bad address *)
EBUSY		= 16;	(* Mount device busy *)
EEXIST		= 17;	(* File exists *)
EXDEV		= 18;	(* Cross-device link *)
ENODEV		= 19;	(* No such device *)
ENOTDIR		= 20;	(* Not a directory *)
EISDIR		= 21;	(* Is a directory *)
EINVAL		= 22;	(* Invalid argument *)
ENFILE		= 23;	(* File table overflow *)
EMFILE		= 24;	(* Too many open files *)
ENOTTY		= 25;	(* Not a typewriter *)
EFBIG		= 27;	(* File too large *)
ENOSPC		= 28;	(* No space left on device *)
ESPIPE		= 29;	(* Illegal seek *)
EROFS		= 30;	(* Read only file system *)
EMLINK		= 31;	(* Too many links *)
EPIPE		= 32;	(* Broken pipe *)
EDEADLK 	= 45;	(* A deadlock would occur *)
ENOLCK  	= 46;	(* System record lock table was full  *)
ENOTEMPTY   	= 247;	(* Directory not empty           *)
ENAMETOOLONG 	= 248;	(* File name too long            *)
ENOSYS 	  	= 251;  (* Function not implemented      *)
ENOTBLK		= 15;	(* Block device required *)
ETXTBSY		= 26;	(* Text file busy *)
ENOMSG  	= 35;   (* No message of desired type    *)
EIDRM		= 36;	(* Identifier removed *)

(* math software *)

EDOM		= 33;	(* Math arg out of domain of func *)
ERANGE		= 34;	(* Math result not representable *)

(* Network File System  *)

ESTALE		= 70;	(* Stale NFS file handle  *)
EREMOTE		= 71;	(* Too many levels of remote in path  *)

(* ipc/network software  *)

(* argument errors  *)
ENOTSOCK		= 216;	(* Socket operation on non-socket  *)
EDESTADDRREQ		= 217;	(* Destination address required  *)
EMSGSIZE		= 218;	(* Message too long  *)
EPROTOTYPE		= 219;	(* Protocol wrong type for socket  *)
ENOPROTOOPT		= 220;	(* Protocol not available  *)
EPROTONOSUPPORT		= 221;	(* Protocol not supported  *)
ESOCKTNOSUPPORT		= 222;	(* Socket type not supported  *)
EOPNOTSUPP	 	= 223;	(* Operation not supported  *)
EPFNOSUPPORT 		= 224;	(* Protocol family not supported  *)
EAFNOSUPPORT 		= 225; 	(* Address family not supported by protocol family *)
EADDRINUSE		= 226;	(* Address already in use  *)
EADDRNOTAVAIL 		= 227;	(* Can't assign requested address  *)

(* operational errors  *)
ENETDOWN		= 228;	(* Network is down  *)
ENETUNREACH		= 229;	(* Network is unreachable  *)
ENETRESET		= 230;	(* Network dropped connection on reset  *)
ECONNABORTED		= 231;	(* Software caused connection abort  *)
ECONNRESET		= 232;	(* Connection reset by peer  *)
ENOBUFS			= 233;	(* No buffer space available  *)
EISCONN			= 234;	(* Socket is already connected  *)
ENOTCONN		= 235;	(* Socket is not connected  *)
ESHUTDOWN		= 236;	(* Can't send after socket shutdown  *)
ETOOMANYREFS		= 237;	(* Too many references: can't splice  *)
ETIMEDOUT		= 238;	(* Connection timed out  *)
ECONNREFUSED		= 239;	(* Connection refused  *)
EREMOTERELEASE		= 240;	(* Remote peer released connection  *)
EHOSTDOWN		= 241;	(* Host is down  *)
EHOSTUNREACH		= 242;	(* No route to host  *)

ENET  	    		= 243;	(* Network error  *)
EALREADY    		= 244;	(* Operation already in progress  *)
EINPROGRESS 		= 245;	(* Operation now in progress  *)
EWOULDBLOCK 		= 246;	(* Operation would block  *)
ELOOP	    		= 249;	(* Too many levels of symbolic links  *)

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
   procedure than to access the array directly. *)

END Uerror.
