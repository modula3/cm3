(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uerror;

FROM Ctypes IMPORT const_int;

<*EXTERNAL "Uerror__E2BIG"*>         VAR E2BIG: const_int;
<*EXTERNAL "Uerror__EACCES"*>        VAR EACCES: const_int;
<*EXTERNAL "Uerror__EADDRINUSE"*>    VAR EADDRINUSE: const_int;
<*EXTERNAL "Uerror__EADDRNOTAVAIL"*> VAR EADDRNOTAVAIL: const_int;
<*EXTERNAL "Uerror__EAFNOSUPPORT"*>  VAR EAFNOSUPPORT: const_int;
<*EXTERNAL "Uerror__EAGAIN"*>        VAR EAGAIN: const_int;
<*EXTERNAL "Uerror__EALREADY"*>      VAR EALREADY: const_int;
<*EXTERNAL "Uerror__EAUTH"*>         VAR EAUTH: const_int;
<*EXTERNAL "Uerror__EBADARCH"*>      VAR EBADARCH: const_int;
<*EXTERNAL "Uerror__EBADEEC"*>       VAR EBADEEC: const_int;
<*EXTERNAL "Uerror__EBADF"*>         VAR EBADF: const_int;
<*EXTERNAL "Uerror__EBADMACHO"*>     VAR EBADMACHO: const_int;
<*EXTERNAL "Uerror__EBADMSG"*>       VAR EBADMSG: const_int;
<*EXTERNAL "Uerror__EBADRPC"*>       VAR EBADRPC: const_int;
<*EXTERNAL "Uerror__EBUSY"*>         VAR EBUSY: const_int;
<*EXTERNAL "Uerror__ECANCELED"*>     VAR ECANCELED: const_int;
<*EXTERNAL "Uerror__ECHILD"*>        VAR ECHILD: const_int;
<*EXTERNAL "Uerror__ECONNABORTED"*>  VAR ECONNABORTED: const_int;
<*EXTERNAL "Uerror__ECONNREFUSED"*>  VAR ECONNREFUSED: const_int;
<*EXTERNAL "Uerror__ECONNRESET"*>    VAR ECONNRESET: const_int;
<*EXTERNAL "Uerror__EDEADLK"*>       VAR EDEADLK: const_int;
<*EXTERNAL "Uerror__EDESTADDRREQ"*>  VAR EDESTADDRREQ: const_int;
<*EXTERNAL "Uerror__EDEV"*>          VAR EDEV: const_int;
<*EXTERNAL "Uerror__EDEVERR"*>       VAR EDEVERR: const_int;
<*EXTERNAL "Uerror__EDOM"*>          VAR EDOM: const_int;
<*EXTERNAL "Uerror__EDQUOT"*>        VAR EDQUOT: const_int;
<*EXTERNAL "Uerror__EEXIST"*>        VAR EEXIST: const_int;
<*EXTERNAL "Uerror__EFAULT"*>        VAR EFAULT: const_int;
<*EXTERNAL "Uerror__EFBIG"*>         VAR EFBIG: const_int;
<*EXTERNAL "Uerror__EFTYPE"*>        VAR EFTYPE: const_int;
<*EXTERNAL "Uerror__EHOSTDOWN"*>     VAR EHOSTDOWN: const_int;
<*EXTERNAL "Uerror__EHOSTUNREACH"*>  VAR EHOSTUNREACH: const_int;
<*EXTERNAL "Uerror__EIDRM"*>         VAR EIDRM: const_int;
<*EXTERNAL "Uerror__EILSEQ"*>        VAR EILSEQ: const_int;
<*EXTERNAL "Uerror__EINPROGRESS"*>   VAR EINPROGRESS: const_int;
<*EXTERNAL "Uerror__EINTR"*>         VAR EINTR: const_int;
<*EXTERNAL "Uerror__EINVAL"*>        VAR EINVAL: const_int;
<*EXTERNAL "Uerror__EIO"*>           VAR EIO: const_int;
<*EXTERNAL "Uerror__EISCONN"*>       VAR EISCONN: const_int;
<*EXTERNAL "Uerror__EISDIR"*>        VAR EISDIR: const_int;
<*EXTERNAL "Uerror__ELOOP"*>         VAR ELOOP: const_int;
<*EXTERNAL "Uerror__EMFILE"*>        VAR EMFILE: const_int;
<*EXTERNAL "Uerror__EMLINK"*>        VAR EMLINK: const_int;
<*EXTERNAL "Uerror__EMSGSIZE"*>      VAR EMSGSIZE: const_int;
<*EXTERNAL "Uerror__EMULTIHOP"*>     VAR EMULTIHOP: const_int;
<*EXTERNAL "Uerror__ENAMETOOLONG"*>  VAR ENAMETOOLONG: const_int;
<*EXTERNAL "Uerror__ENEEDAUTH"*>     VAR ENEEDAUTH: const_int;
<*EXTERNAL "Uerror__ENETDOWN"*>      VAR ENETDOWN: const_int;
<*EXTERNAL "Uerror__ENETRESET"*>     VAR ENETRESET: const_int;
<*EXTERNAL "Uerror__ENETUNREACH"*>   VAR ENETUNREACH: const_int;
<*EXTERNAL "Uerror__ENFILE"*>        VAR ENFILE: const_int;
<*EXTERNAL "Uerror__ENIO"*>          VAR ENIO: const_int;
<*EXTERNAL "Uerror__ENOATTR"*>       VAR ENOATTR: const_int;
<*EXTERNAL "Uerror__ENOBUFS"*>       VAR ENOBUFS: const_int;
<*EXTERNAL "Uerror__ENODATA"*>       VAR ENODATA: const_int;
<*EXTERNAL "Uerror__ENODEV"*>        VAR ENODEV: const_int;
<*EXTERNAL "Uerror__ENOENT"*>        VAR ENOENT: const_int;
<*EXTERNAL "Uerror__ENOEXEC"*>       VAR ENOEXEC: const_int;
<*EXTERNAL "Uerror__ENOLCK"*>        VAR ENOLCK: const_int;
<*EXTERNAL "Uerror__ENOLINK"*>       VAR ENOLINK: const_int;
<*EXTERNAL "Uerror__ENOMEM"*>        VAR ENOMEM: const_int;
<*EXTERNAL "Uerror__ENOMSG"*>        VAR ENOMSG: const_int;
<*EXTERNAL "Uerror__ENOPOLICY"*>     VAR ENOPOLICY: const_int;
<*EXTERNAL "Uerror__ENOPROTOOPT"*>   VAR ENOPROTOOPT: const_int;
<*EXTERNAL "Uerror__ENOSPC"*>        VAR ENOSPC: const_int;
<*EXTERNAL "Uerror__ENOSR"*>         VAR ENOSR: const_int;
<*EXTERNAL "Uerror__ENOSTR"*>        VAR ENOSTR: const_int;
<*EXTERNAL "Uerror__ENOSYS"*>        VAR ENOSYS: const_int;
<*EXTERNAL "Uerror__ENOTBLK"*>       VAR ENOTBLK: const_int;
<*EXTERNAL "Uerror__ENOTCONN"*>      VAR ENOTCONN: const_int;
<*EXTERNAL "Uerror__ENOTDIR"*>       VAR ENOTDIR: const_int;
<*EXTERNAL "Uerror__ENOTEMPTY"*>     VAR ENOTEMPTY: const_int;
<*EXTERNAL "Uerror__ENOTSOCK"*>      VAR ENOTSOCK: const_int;
<*EXTERNAL "Uerror__ENOTSUP"*>       VAR ENOTSUP: const_int;
<*EXTERNAL "Uerror__ENOTTY"*>        VAR ENOTTY: const_int;
<*EXTERNAL "Uerror__EOPNOTSUPP"*>    VAR EOPNOTSUPP: const_int;
<*EXTERNAL "Uerror__EOVERFLOW"*>     VAR EOVERFLOW: const_int;
<*EXTERNAL "Uerror__EPERM"*>         VAR EPERM: const_int;
<*EXTERNAL "Uerror__EPFNOSUPPORT"*>  VAR EPFNOSUPPORT: const_int;
<*EXTERNAL "Uerror__EPIPE"*>         VAR EPIPE: const_int;
<*EXTERNAL "Uerror__EPROCLIM"*>      VAR EPROCLIM: const_int;
<*EXTERNAL "Uerror__EPROCUNAVAIL"*>  VAR EPROCUNAVAIL: const_int;
<*EXTERNAL "Uerror__EPROGMISMATCH"*> VAR EPROGMISMATCH: const_int;
<*EXTERNAL "Uerror__EPROGUNAVAIL"*>  VAR EPROGUNAVAIL: const_int;
<*EXTERNAL "Uerror__EPROTO"*>        VAR EPROTO: const_int;
<*EXTERNAL "Uerror__EPROTONOSUPPORT"*> VAR EPROTONOSUPPORT: const_int;
<*EXTERNAL "Uerror__EPROTOTYPE"*>    VAR EPROTOTYPE: const_int;
<*EXTERNAL "Uerror__EPWROFF"*>       VAR EPWROFF: const_int;
<*EXTERNAL "Uerror__ERANGE"*>        VAR ERANGE: const_int;
<*EXTERNAL "Uerror__EREMOTE"*>       VAR EREMOTE: const_int;
<*EXTERNAL "Uerror__EROFS"*>         VAR EROFS: const_int;
<*EXTERNAL "Uerror__ERPCMISMATCH"*>  VAR ERPCMISMATCH: const_int;
<*EXTERNAL "Uerror__ESHLIBVERS"*>    VAR ESHLIBVERS: const_int;
<*EXTERNAL "Uerror__ESHUTDOWN"*>     VAR ESHUTDOWN: const_int;
<*EXTERNAL "Uerror__ESOCKTNOSUPPORT"*> VAR ESOCKTNOSUPPORT: const_int;
<*EXTERNAL "Uerror__ESPIPE"*>        VAR ESPIPE: const_int;
<*EXTERNAL "Uerror__ESRCH"*>         VAR ESRCH: const_int;
<*EXTERNAL "Uerror__ESTALE"*>        VAR ESTALE: const_int;
<*EXTERNAL "Uerror__ETIME"*>         VAR ETIME: const_int;
<*EXTERNAL "Uerror__ETIMEDOUT"*>     VAR ETIMEDOUT: const_int;
<*EXTERNAL "Uerror__ETOOMANYREFS"*>  VAR ETOOMANYREFS: const_int;
<*EXTERNAL "Uerror__ETTBSY"*>        VAR ETTBSY: const_int;
<*EXTERNAL "Uerror__EUSERS"*>        VAR EUSERS: const_int;
<*EXTERNAL "Uerror__EWOULDBLOCK"*>   VAR EWOULDBLOCK: const_int;

CONST
  Max = 255; (* approx, verified to be adequate in Uconstants.c *)

END Uerror.
