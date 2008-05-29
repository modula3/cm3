/* $Id: Uerror.i3.cpp,v 1.4 2008-05-29 12:43:21 jkrell Exp $ */

#include <stdio.h>
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
"INTERFACE Uerror;", 0,
"", 0,
"CONST", 0,
"  EPERM           = %u;    (* Not owner / operation not permitted  *)", EPERM,
"  ENOENT          = %u;    (* No such file or directory *)", ENOENT,
"  EINTR           = %u;    (* Interrupted system call *)", EINTR,
"  EIO             = %u;    (* I/O error *)", EIO,
"  ENOEXEC         = %u;    (* Exec format error *)", ENOEXEC,
"  EBADF           = %u;    (* Bad file number *)", EBADF,
"  ECHILD          = %u;   (* No children *)", ECHILD,
"  EAGAIN          = %u;   (* No more processes *)", EAGAIN,
"  ENOMEM          = %u;   (* Not enough core *)", ENOMEM,
"  EACCES          = %u;   (* Permission denied *)", EACCES,
"  EEXIST          = %u;   (* File exists *)", EEXIST,
"  EISDIR          = %u;   (* Is a directory *)", EISDIR,
"  EINVAL          = %u;   (* Invalid argument *)", EINVAL,
"  ENFILE          = %u;   (* File table overflow *)", ENFILE,
"  EMFILE          = %u;   (* Too many open files *)", EMFILE,
"  EPIPE           = %u;   (* Broken pipe *)", EPIPE,
"", 0,
"   (* math software *)", 0,
"  EDOM            = %u;   (* Argument too large *)", EDOM,
"  ERANGE          = %u;   (* Result too large *)", ERANGE,
"", 0,
"  ENAMETOOLONG    = %u;   (* File name too long *)", ENAMETOOLONG,
"  ENOTEMPTY       = %u;   (* Directory not empty *)", ENOTEMPTY,
"", 0,
"   (* Non-blocking I/O  and IPC errors *)", 0,
"  EWOULDBLOCK     = EAGAIN;   (* Operation would block *)", EWOULDBLOCK,
"", 0,
"  (* ipc/network software *)", 0,
"        (* argument errors *)", 0,
"  ENOTSOCK        = %u;   (* Socket operation on non-socket *)", ENOTSOCK,
"  EADDRINUSE      = %u;   (* Address already in use *)", EADDRINUSE,
"  EADDRNOTAVAIL   = %u;   (* Can't assign requested address *)", EADDRNOTAVAIL,
"", 0,
"   (* operational errors *)", 0,
"  ENETDOWN        = %u;   (* Network is down *)", ENETDOWN,
"  ENETUNREACH     = %u;   (* Network is unreachable *)", ENETUNREACH,
"  ENETRESET       = %u;   (* Network dropped connection because of reset *)", ENETRESET,
"  ECONNABORTED    = %u;   (* Software caused connection abort *)", ECONNABORTED,
"  ECONNRESET      = %u;   (* Connection reset by peer *)", ECONNRESET,
"  EISCONN         = %u;   (* Transport endpoint is already connected *)", EISCONN,
"  ETIMEDOUT       = %u;   (* Connection timed out *)", ETIMEDOUT,
"  ECONNREFUSED    = %u;   (* Connection refused *)", ECONNREFUSED,
"  EHOSTDOWN       = %u;   (* Host is down *)", EHOSTDOWN,
"  EHOSTUNREACH    = %u;   (* No route to host *)", EHOSTUNREACH,
"  EINPROGRESS     = %u;   (* Operation now in progress *)", EINPROGRESS,
"  EALREADY        = %u;   (* Operation already in progress *)", EALREADY,
"", 0,
"(* Extension by mjordan *)", 0,
"CONST", 0,
"  Max = 140; (* should be exported from Uerror *)", 0,
"  ", 0,
"END Uerror.", 0,
"", 0,
};
    for (i = 0 ; i != sizeof(Data)/sizeof(Data[0]) ; ++i)
    {
        printf(Data[i].Format, Data[i].Value);
        printf("\n");
    }
    return 0;
}
