(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Feb 24 14:53:26 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:02:55 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Thu Jul 21 00:00:00 1994 by sims@usa.acsys.com    *)
(*      modified on Thu Nov 22 05:20:45 1990 by muller                *)

(* constant values taken from /usr/include/linux/errno.h on
   Slackware 1.2 Linux box -- sims@usa.acsys.com *)
(* Altered for Linux 1.1.73 with libc 4.5.26 by rrw1000@cam.ac.uk *)

INTERFACE Uerror;

FROM Ctypes IMPORT int, char_star;

(*** <errno.h> ***)

CONST
  EPERM           = 1;    (* Not owner / operation not permitted  *)
  ENOENT          = 2;    (* No such file or directory *)
  ESRCH           = 3;    (* No such process *)
  EINTR           = 4;    (* Interrupted system call *)
  EIO             = 5;    (* I/O error *)
  ENXIO           = 6;    (* No such device or address *)
  E2BIG           = 7;    (* Arg list too long *)
  ENOEXEC         = 8;    (* Exec format error *)
  EBADF           = 9;    (* Bad file number *)
  ECHILD          = 10;   (* No children *)
  EAGAIN          = 11;   (* No more processes *)
  ENOMEM          = 12;   (* Not enough core *)
  EACCES          = 13;   (* Permission denied *)
  EFAULT          = 14;   (* Bad address *)
  ENOTBLK         = 15;   (* Block device required *)
  EBUSY           = 16;   (* Mount device busy *)
  EEXIST          = 17;   (* File exists *)
  EXDEV           = 18;   (* Cross-device link *)
  ENODEV          = 19;   (* No such device *)
  ENOTDIR         = 20;   (* Not a directory*)
  EISDIR          = 21;   (* Is a directory *)
  EINVAL          = 22;   (* Invalid argument *)
  ENFILE          = 23;   (* File table overflow *)
  EMFILE          = 24;   (* Too many open files *)
  ENOTTY          = 25;   (* Not a typewriter *)
  ETXTBSY         = 26;   (* Text file busy *)
  EFBIG           = 27;   (* File too large *)
  ENOSPC          = 28;   (* No space left on device *)
  ESPIPE          = 29;   (* Illegal seek *)
  EROFS           = 30;   (* Read-only file system *)
  EMLINK          = 31;   (* Too many links *)
  EPIPE           = 32;   (* Broken pipe *)

   (* math software *)
  EDOM            = 33;   (* Argument too large *)
  ERANGE          = 34;   (* Result too large *)

  EDEADLK         = 35;   (* resource deadlock would occur *)
  ENAMETOOLONG    = 36;   (* File name too long *)
  ENOLCK          = 37;   (* No record locks available *)
  ENOSYS          = 38;   (* Function not implemented *)
  ENOTEMPTY       = 39;   (* Directory not empty *)
  ELOOP           = 40;   (* Too many levels of symbolic links *)

   (* Non-blocking I/O  and IPC errors *)
  EWOULDBLOCK     = 11;   (* Operation would block *) (* Defined as EAGAIN, not
                             41 under 1.1.73 *)
  ENOMSG          = 42;   (* No message of desired type *)
  EIDRM           = 43;   (* Identifier removed *)
  ECHRNG          = 44;   (* Channel number out of range *)
  EL2NSYNC        = 45;   (* Level 2 not synchronized *)
  EL3HLT          = 46;   (* Level 3 halted *)
  EL3RST          = 47;   (* Level 3 reset *)
  ELNRNG          = 48;   (* Link number out of range *)
  EUNATCH         = 49;   (* Protocol driver not attached *)
  ENOCSI          = 50;   (* No CSI structure available *)
  EL2HLT          = 51;   (* Level 2 halted *)
  EBADE           = 52;   (* Invalid exchange *)
  EBADR           = 53;   (* Invalid request descriptor *)
  EXFULL          = 54;   (* Exchange full *)
  ENOANO          = 55;   (* No anode *)
  EBADRQC         = 56;   (* Invalid request code *)
  EBADSLT         = 57;   (* Invalid slot *)
  EDEADLOCK       = 58;   (* File locking deadlock error *)
  EBFONT          = 59;   (* Bad font file format *)
  ENOSTR          = 60;   (* Device not a stream *)
  ENODATA         = 61;   (* No data available *)
  ETIME           = 62;   (* Timer expired *)
  ENOSR           = 63;   (* Out of streams resources *)
  ENONET          = 64;   (* Machine is not on the network *)
  ENOPKG          = 65;   (* Package not installed *)
  EREMOTE         = 66;   (* Object is remote *)
  ENOLINK         = 67;   (* Link has been severed *)
  EADV            = 68;   (* Advertise error *)
  ESRMNT          = 69;   (* Srmount error *)
  ECOMM           = 70;   (* Communication error on send *)
  EPROTO          = 71;   (* Protocol error *)
  EMULTIHOP       = 72;   (* Multihop attempted *)
  EDOTDOT         = 73;   (* RFS specific error *)
  EBADMSG         = 74;   (* Not a data message *)
  EOVERFLOW       = 75;   (* Value too large for defined data type *)
  ENOTUNIQ        = 76;   (* Name not unique on network *)
  EBADFD          = 77;   (* File descriptor in bad state *)
  EREMCHG         = 78;   (* Remote address changed *)
  ELIBACC         = 79;   (* Can not access a needed shared library *)
  ELIBBAD         = 80;   (* Accessing a corrupted shared library *)
  ELIBSCN         = 81;   (* .lib section in a.out corrupted *)
  ELIBMAX         = 82;   (* Attempting to link in too many shared libraries *)
  ELIBEXEC        = 83;   (* Cannot exec a shared library directly *)
  EILSEQ          = 84;   (* Illegal byte sequence *)
  ERESTART        = 85;   (* Interrupted system call should be restarted *)

  (* ipc/network software *)
        (* argument errors *)
  ESTRPIPE        = 86;   (* Streams pipe error *)
  EUSERS          = 87;   (* Too many users *)
  ENOTSOCK        = 88;   (* Socket operation on non-socket *)
  EDESTADDRREQ    = 89;   (* Destination address required *)
  EMSGSIZE        = 90;   (* Message too long *)
  EPROTOTYPE      = 91;   (* Protocol wrong type for socket *)
  ENOPROTOOPT     = 92;   (* Protocol not available *)
  EPROTONOSUPPORT = 93;   (* Protocol not supported *)
  ESOCKTNOSUPPORT = 94;   (* Socket type not supported *)
  EOPNOTSUPP      = 95;   (* Operation not supported on socket *)
  EPFNOSUPPORT    = 96;   (* Protocol family not supported *)
  EAFNOSUPPORT    = 97;   (* Address family not supported by protocol family *)
  EADDRINUSE      = 98;   (* Address already in use *)
  EADDRNOTAVAIL   = 99;   (* Can't assign requested address *)

   (* operational errors *)
  ENETDOWN        = 100;   (* Network is down *)
  ENETUNREACH     = 101;   (* Network is unreachable *)
  ENETRESET       = 102;   (* Network dropped connection because of reset *)
  ECONNABORTED    = 103;   (* Software caused connection abort *)
  ECONNRESET      = 104;   (* Connection reset by peer *)
  ENOBUFS         = 105;   (* No buffer space available *)
  EISCONN         = 106;   (* Transport endpoint is already connected *)
  ENOTCONN        = 107;   (* Transport endpoint is not connected *)
  ESHUTDOWN       = 108;   (* Can't send after transport endpoint shutdown *)
  ETOOMANYREFS    = 109;   (* Too many references: can't splice *)
  ETIMEDOUT       = 110;   (* Connection timed out *)
  ECONNREFUSED    = 111;   (* Connection refused *)
  EHOSTDOWN       = 112;   (* Host is down *)
  EHOSTUNREACH    = 113;   (* No route to host *)
  EALREADY        = 114;   (* Operation already in progress *)
  EINPROGRESS     = 115;   (* Operation now in progress *)

  (* misc. FS errors *)
  ESTALE          = 116;   (* Stale NFS file handle *)
  EUCLEAN         = 117;   (* Structure needs cleaning *)
  ENOTNAM         = 118;   (* Not a XENIX named type file *)
  ENAVAIL         = 119;   (* No XENIX semaphores available *)
  EISNAM          = 120;   (* Is a named type file *)
  EREMOTEIO       = 121;   (* Remote I/O error *)
  EDQUOT          = 122;   (* Disc quota exceeded *)

  (* Should never be seen by user programs *)
  ERESTARTSYS     = 512;
  ERESTARTNOINTR  = 513;
  ERESTARTNOHAND  = 514;   (* restart if no handler.. *)
  ENOIOCTLCMD     = 515;   (* Present in Linux 1.1.73 *)

<*EXTERNAL*>
VAR
  errno: int;

(* Extension by mjordan *)
CONST
  Max = EDQUOT; (* should be exported from Uerror *)
  
<*EXTERNAL*>
VAR
  sys_nerr: int;
  sys_errlist: ARRAY [0..Max] OF char_star;

PROCEDURE GetFrom_sys_errlist(n: INTEGER): char_star RAISES {};
(* returns entry 'n' of the 'sys_errlist' array; a checked runtime error
   unless 0 <= n <= sys_nerr. Its safer and more portable to use this 
   procedure than to access the array directly.
*)

END Uerror.
