/* $Id$ */

#include <stdio.h>
#define __BSD_VISIBLE 1
#include <errno.h>

int main()
{
    unsigned i;
    const static struct
    {
        const char* Format;
        unsigned Value;
    } Data[] =
{
"(* Copyright (C) 1990, Digital Equipment Corporation.         *)", 0,
"(* All rights reserved.                                       *)", 0,
"(* See the file COPYRIGHT for a full description.             *)", 0,
"", 0,
"(* This file was generated from " __FILE__ ". Do not edit it. *)", 0,
"", 0,
"INTERFACE Uerror;", 0,
"", 0,
"CONST", 0,
#define X(x) "  " #x " = %u;", x,
X(EPERM)
X(ENOENT)
X(ESRCH)
X(EINTR)
X(EIO)
X(ENXIO)
X(E2BIG)
X(ENOEXEC)
X(EBADF)
X(ECHILD)
X(EDEADLK)
X(ENOMEM)
X(EACCES)
X(EFAULT)
X(ENOTBLK)
X(EBUSY)
X(EEXIST)
X(EXDEV)
X(ENODEV)
X(ENOTDIR)
X(EISDIR)
X(EINVAL)
X(ENFILE)
X(EMFILE)
X(ENOTTY)
X(ETXTBSY)
X(EFBIG)
X(ENOSPC)
X(ESPIPE)
X(EROFS)
X(EMLINK)
X(EPIPE)
X(EDOM)
X(ERANGE)
X(EAGAIN)
"  EWOULDBLOCK = EAGAIN;", 0,
X(EINPROGRESS)
X(EALREADY)
X(ENOTSOCK)
X(EDESTADDRREQ)
X(EMSGSIZE)
X(EPROTOTYPE)
X(ENOPROTOOPT)
X(EPROTONOSUPPORT)
X(ESOCKTNOSUPPORT)
X(EOPNOTSUPP)
X(EPFNOSUPPORT)
X(EAFNOSUPPORT)
X(EADDRINUSE)
X(EADDRNOTAVAIL)
X(ENETDOWN)
X(ENETUNREACH)
X(ENETRESET)
X(ECONNABORTED)
X(ECONNRESET)
X(ENOBUFS)
X(EISCONN)
X(ENOTCONN)
X(ESHUTDOWN)
X(ETOOMANYREFS)
X(ETIMEDOUT)
X(ECONNREFUSED)
X(ELOOP)
X(ENAMETOOLONG)
X(EHOSTDOWN)
X(EHOSTUNREACH)
X(ENOTEMPTY)
X(EPROCLIM)
X(EUSERS)
X(EDQUOT)
X(ESTALE)
X(EREMOTE)
X(EBADRPC)
X(ERPCMISMATCH)
X(EPROGUNAVAIL)
X(EPROGMISMATCH)
X(EPROCUNAVAIL)
X(ENOLCK)
X(ENOSYS)
X(EFTYPE)
X(EAUTH)
X(ENEEDAUTH)
X(ELAST)
"", 0,
"(* Extension by mjordan *)", 0,
"  Max = ELAST;", 0,
"  ", 0,
"END Uerror.", 0
};
    for (i = 0 ; i != sizeof(Data)/sizeof(Data[0]) ; ++i)
    {
        printf(Data[i].Format, Data[i].Value);
        printf("\n");
    }
    return 0;
}
