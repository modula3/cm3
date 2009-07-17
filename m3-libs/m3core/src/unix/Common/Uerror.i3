(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uerror;

FROM Ctypes IMPORT int;

(* CONST *)
<*EXTERNAL "Uerror__EACCES"*>        VAR EACCES: int;
<*EXTERNAL "Uerror__EADDRINUSE"*>    VAR EADDRINUSE: int;
<*EXTERNAL "Uerror__EADDRNOTAVAIL"*> VAR EADDRNOTAVAIL: int;
<*EXTERNAL "Uerror__EAGAIN"*>        VAR EAGAIN: int;
<*EXTERNAL "Uerror__EALREADY"*>      VAR EALREADY: int;
<*EXTERNAL "Uerror__EBADF"*>         VAR EBADF: int;
<*EXTERNAL "Uerror__ECHILD"*>        VAR ECHILD: int;
<*EXTERNAL "Uerror__ECONNABORTED"*>  VAR ECONNABORTED: int;
<*EXTERNAL "Uerror__ECONNREFUSED"*>  VAR ECONNREFUSED: int;
<*EXTERNAL "Uerror__ECONNRESET"*>    VAR ECONNRESET: int;
<*EXTERNAL "Uerror__EDOM"*>          VAR EDOM: int;
<*EXTERNAL "Uerror__EEXIST"*>        VAR EEXIST: int;
<*EXTERNAL "Uerror__EHOSTDOWN"*>     VAR EHOSTDOWN: int;
<*EXTERNAL "Uerror__EHOSTUNREACH"*>  VAR EHOSTUNREACH: int;
<*EXTERNAL "Uerror__EINPROGRESS"*>   VAR EINPROGRESS: int;
<*EXTERNAL "Uerror__EINTR"*>         VAR EINTR: int;
<*EXTERNAL "Uerror__EINVAL"*>        VAR EINVAL: int;
<*EXTERNAL "Uerror__EIO"*>           VAR EIO: int;
<*EXTERNAL "Uerror__EISCONN"*>       VAR EISCONN: int;
<*EXTERNAL "Uerror__EISDIR"*>        VAR EISDIR: int;
<*EXTERNAL "Uerror__EMFILE"*>        VAR EMFILE: int;
<*EXTERNAL "Uerror__ENAMETOOLONG"*>  VAR ENAMETOOLONG: int;
<*EXTERNAL "Uerror__ENETDOWN"*>      VAR ENETDOWN: int;
<*EXTERNAL "Uerror__ENETRESET"*>     VAR ENETRESET: int;
<*EXTERNAL "Uerror__ENETUNREACH"*>   VAR ENETUNREACH: int;
<*EXTERNAL "Uerror__ENFILE"*>        VAR ENFILE: int;
<*EXTERNAL "Uerror__ENOBUFS"*>       VAR ENOBUFS: int;
<*EXTERNAL "Uerror__ENOENT"*>        VAR ENOENT: int;
<*EXTERNAL "Uerror__ENOEXEC"*>       VAR ENOEXEC: int;
<*EXTERNAL "Uerror__ENOMEM"*>        VAR ENOMEM: int;
<*EXTERNAL "Uerror__ENOTDIR"*>       VAR ENOTDIR: int;
<*EXTERNAL "Uerror__ENOTEMPTY"*>     VAR ENOTEMPTY: int;
<*EXTERNAL "Uerror__ENOTSOCK"*>      VAR ENOTSOCK: int;
<*EXTERNAL "Uerror__EPERM"*>         VAR EPERM: int;
<*EXTERNAL "Uerror__EPIPE"*>         VAR EPIPE: int;
<*EXTERNAL "Uerror__ERANGE"*>        VAR ERANGE: int;
<*EXTERNAL "Uerror__ETIMEDOUT"*>     VAR ETIMEDOUT: int;
<*EXTERNAL "Uerror__EWOULDBLOCK"*>   VAR EWOULDBLOCK: int;

CONST
  Max = 248; (* approx, verified to be adequate in Uconstants.c *)

END Uerror.
