(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uerror;

FROM Ctypes IMPORT int;

(* CONST *)
<*EXTERNAL "Uerror__E2BIG"*>         VAR E2BIG: int;
<*EXTERNAL "Uerror__EACCES"*>        VAR EACCES: int;
<*EXTERNAL "Uerror__EADDRINUSE"*>    VAR EADDRINUSE: int;
<*EXTERNAL "Uerror__EADDRNOTAVAIL"*> VAR EADDRNOTAVAIL: int;
<*EXTERNAL "Uerror__EAFNOSUPPORT"*>  VAR EAFNOSUPPORT: int;
<*EXTERNAL "Uerror__EAGAIN"*>        VAR EAGAIN: int;
<*EXTERNAL "Uerror__EALREADY"*>      VAR EALREADY: int;
<*EXTERNAL "Uerror__EAUTH"*>         VAR EAUTH: int;
<*EXTERNAL "Uerror__EBADARCH"*>      VAR EBADARCH: int;
<*EXTERNAL "Uerror__EBADEEC"*>       VAR EBADEEC: int;
<*EXTERNAL "Uerror__EBADF"*>         VAR EBADF: int;
<*EXTERNAL "Uerror__EBADMACHO"*>     VAR EBADMACHO: int;
<*EXTERNAL "Uerror__EBADMSG"*>       VAR EBADMSG: int;
<*EXTERNAL "Uerror__EBADRPC"*>       VAR EBADRPC: int;
<*EXTERNAL "Uerror__EBUSY"*>         VAR EBUSY: int;
<*EXTERNAL "Uerror__ECANCELED"*>     VAR ECANCELED: int;
<*EXTERNAL "Uerror__ECHILD"*>        VAR ECHILD: int;
<*EXTERNAL "Uerror__ECONNABORTED"*>  VAR ECONNABORTED: int;
<*EXTERNAL "Uerror__ECONNREFUSED"*>  VAR ECONNREFUSED: int;
<*EXTERNAL "Uerror__ECONNRESET"*>    VAR ECONNRESET: int;
<*EXTERNAL "Uerror__EDEADLK"*>       VAR EDEADLK: int;
<*EXTERNAL "Uerror__EDESTADDRREQ"*>  VAR EDESTADDRREQ: int;
<*EXTERNAL "Uerror__EDEV"*>          VAR EDEV: int;
<*EXTERNAL "Uerror__EDEVERR"*>       VAR EDEVERR: int;
<*EXTERNAL "Uerror__EDOM"*>          VAR EDOM: int;
<*EXTERNAL "Uerror__EDQUOT"*>        VAR EDQUOT: int;
<*EXTERNAL "Uerror__EEXIST"*>        VAR EEXIST: int;
<*EXTERNAL "Uerror__EFAULT"*>        VAR EFAULT: int;
<*EXTERNAL "Uerror__EFBIG"*>         VAR EFBIG: int;
<*EXTERNAL "Uerror__EFTYPE"*>        VAR EFTYPE: int;
<*EXTERNAL "Uerror__EHOSTDOWN"*>     VAR EHOSTDOWN: int;
<*EXTERNAL "Uerror__EHOSTUNREACH"*>  VAR EHOSTUNREACH: int;
<*EXTERNAL "Uerror__EIDRM"*>         VAR EIDRM: int;
<*EXTERNAL "Uerror__EILSEQ"*>        VAR EILSEQ: int;
<*EXTERNAL "Uerror__EINPROGRESS"*>   VAR EINPROGRESS: int;
<*EXTERNAL "Uerror__EINTR"*>         VAR EINTR: int;
<*EXTERNAL "Uerror__EINVAL"*>        VAR EINVAL: int;
<*EXTERNAL "Uerror__EIO"*>           VAR EIO: int;
<*EXTERNAL "Uerror__EISCONN"*>       VAR EISCONN: int;
<*EXTERNAL "Uerror__EISDIR"*>        VAR EISDIR: int;
<*EXTERNAL "Uerror__ELOOP"*>         VAR ELOOP: int;
<*EXTERNAL "Uerror__EMFILE"*>        VAR EMFILE: int;
<*EXTERNAL "Uerror__EMLINK"*>        VAR EMLINK: int;
<*EXTERNAL "Uerror__EMSGSIZE"*>      VAR EMSGSIZE: int;
<*EXTERNAL "Uerror__EMULTIHOP"*>     VAR EMULTIHOP: int;
<*EXTERNAL "Uerror__ENAMETOOLONG"*>  VAR ENAMETOOLONG: int;
<*EXTERNAL "Uerror__ENEEDAUTH"*>     VAR ENEEDAUTH: int;
<*EXTERNAL "Uerror__ENETDOWN"*>      VAR ENETDOWN: int;
<*EXTERNAL "Uerror__ENETRESET"*>     VAR ENETRESET: int;
<*EXTERNAL "Uerror__ENETUNREACH"*>   VAR ENETUNREACH: int;
<*EXTERNAL "Uerror__ENFILE"*>        VAR ENFILE: int;
<*EXTERNAL "Uerror__ENIO"*>          VAR ENIO: int;
<*EXTERNAL "Uerror__ENOATTR"*>       VAR ENOATTR: int;
<*EXTERNAL "Uerror__ENOBUFS"*>       VAR ENOBUFS: int;
<*EXTERNAL "Uerror__ENODATA"*>       VAR ENODATA: int;
<*EXTERNAL "Uerror__ENODEV"*>        VAR ENODEV: int;
<*EXTERNAL "Uerror__ENOENT"*>        VAR ENOENT: int;
<*EXTERNAL "Uerror__ENOEXEC"*>       VAR ENOEXEC: int;
<*EXTERNAL "Uerror__ENOLCK"*>        VAR ENOLCK: int;
<*EXTERNAL "Uerror__ENOLINK"*>       VAR ENOLINK: int;
<*EXTERNAL "Uerror__ENOMEM"*>        VAR ENOMEM: int;
<*EXTERNAL "Uerror__ENOMSG"*>        VAR ENOMSG: int;
<*EXTERNAL "Uerror__ENOPOLICY"*>     VAR ENOPOLICY: int;
<*EXTERNAL "Uerror__ENOPROTOOPT"*>   VAR ENOPROTOOPT: int;
<*EXTERNAL "Uerror__ENOSPC"*>        VAR ENOSPC: int;
<*EXTERNAL "Uerror__ENOSR"*>         VAR ENOSR: int;
<*EXTERNAL "Uerror__ENOSTR"*>        VAR ENOSTR: int;
<*EXTERNAL "Uerror__ENOSYS"*>        VAR ENOSYS: int;
<*EXTERNAL "Uerror__ENOTBLK"*>       VAR ENOTBLK: int;
<*EXTERNAL "Uerror__ENOTCONN"*>      VAR ENOTCONN: int;
<*EXTERNAL "Uerror__ENOTDIR"*>       VAR ENOTDIR: int;
<*EXTERNAL "Uerror__ENOTEMPTY"*>     VAR ENOTEMPTY: int;
<*EXTERNAL "Uerror__ENOTSOCK"*>      VAR ENOTSOCK: int;
<*EXTERNAL "Uerror__ENOTSUP"*>       VAR ENOTSUP: int;
<*EXTERNAL "Uerror__ENOTTY"*>        VAR ENOTTY: int;
<*EXTERNAL "Uerror__EOPNOTSUPP"*>    VAR EOPNOTSUPP: int;
<*EXTERNAL "Uerror__EOVERFLOW"*>     VAR EOVERFLOW: int;
<*EXTERNAL "Uerror__EPERM"*>         VAR EPERM: int;
<*EXTERNAL "Uerror__EPFNOSUPPORT"*>  VAR EPFNOSUPPORT: int;
<*EXTERNAL "Uerror__EPIPE"*>         VAR EPIPE: int;
<*EXTERNAL "Uerror__EPROCLIM"*>      VAR EPROCLIM: int;
<*EXTERNAL "Uerror__EPROCUNAVAIL"*>  VAR EPROCUNAVAIL: int;
<*EXTERNAL "Uerror__EPROGMISMATCH"*> VAR EPROGMISMATCH: int;
<*EXTERNAL "Uerror__EPROGUNAVAIL"*>  VAR EPROGUNAVAIL: int;
<*EXTERNAL "Uerror__EPROTO"*>        VAR EPROTO: int;
<*EXTERNAL "Uerror__EPROTONOSUPPORT"*> VAR EPROTONOSUPPORT: int;
<*EXTERNAL "Uerror__EPROTOTYPE"*>    VAR EPROTOTYPE: int;
<*EXTERNAL "Uerror__EPWROFF"*>       VAR EPWROFF: int;
<*EXTERNAL "Uerror__ERANGE"*>        VAR ERANGE: int;
<*EXTERNAL "Uerror__EREMOTE"*>       VAR EREMOTE: int;
<*EXTERNAL "Uerror__EROFS"*>         VAR EROFS: int;
<*EXTERNAL "Uerror__ERPCMISMATCH"*>  VAR ERPCMISMATCH: int;
<*EXTERNAL "Uerror__ESHLIBVERS"*>    VAR ESHLIBVERS: int;
<*EXTERNAL "Uerror__ESHUTDOWN"*>     VAR ESHUTDOWN: int;
<*EXTERNAL "Uerror__ESOCKTNOSUPPORT"*> VAR ESOCKTNOSUPPORT: int;
<*EXTERNAL "Uerror__ESPIPE"*>        VAR ESPIPE: int;
<*EXTERNAL "Uerror__ESRCH"*>         VAR ESRCH: int;
<*EXTERNAL "Uerror__ESTALE"*>        VAR ESTALE: int;
<*EXTERNAL "Uerror__ETIME"*>         VAR ETIME: int;
<*EXTERNAL "Uerror__ETIMEDOUT"*>     VAR ETIMEDOUT: int;
<*EXTERNAL "Uerror__ETOOMANYREFS"*>  VAR ETOOMANYREFS: int;
<*EXTERNAL "Uerror__ETTBSY"*>        VAR ETTBSY: int;
<*EXTERNAL "Uerror__EUSERS"*>        VAR EUSERS: int;
<*EXTERNAL "Uerror__EWOULDBLOCK"*>   VAR EWOULDBLOCK: int;

CONST
  Max = 248; (* approx, verified to be adequate in Uconstants.c *)

END Uerror.
