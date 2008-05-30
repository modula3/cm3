/* $Id: Uerror.i3.cpp,v 1.5 2008-05-30 07:48:28 jkrell Exp $ */

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
"  EPERM = %u;", EPERM,
"  ENOENT = %u;", ENOENT,
"  EINTR = %u;", EINTR,
"  EIO = %u;", EIO,
"  ENOEXEC = %u;", ENOEXEC,
"  EBADF = %u;", EBADF,
"  ECHILD = %u;", ECHILD,
"  EAGAIN = %u;", EAGAIN,
"  ENOMEM = %u;", ENOMEM,
"  EACCES = %u;", EACCES,
"  EEXIST = %u;", EEXIST,
"  EISDIR = %u;", EISDIR,
"  EINVAL = %u;", EINVAL,
"  ENFILE = %u;", ENFILE,
"  EMFILE = %u;", EMFILE,
"  EPIPE = %u;", EPIPE,
"  EDOM = %u;", EDOM,
"  ERANGE = %u;", ERANGE,
"  ENAMETOOLONG = %u;", ENAMETOOLONG,
"  ENOTEMPTY = %u;", ENOTEMPTY,
"  EWOULDBLOCK = EAGAIN;", EWOULDBLOCK,
"  ENOTSOCK = %u;", ENOTSOCK,
"  EADDRINUSE = %u;", EADDRINUSE,
"  EADDRNOTAVAIL = %u;", EADDRNOTAVAIL,
"  ENETDOWN = %u;", ENETDOWN,
"  ENETUNREACH = %u;", ENETUNREACH,
"  ENETRESET = %u;", ENETRESET,
"  ECONNABORTED = %u;", ECONNABORTED,
"  ECONNRESET = %u;", ECONNRESET,
"  EISCONN = %u;", EISCONN,
"  ETIMEDOUT = %u;", ETIMEDOUT,
"  ECONNREFUSED = %u;", ECONNREFUSED,
"  EHOSTDOWN = %u;", EHOSTDOWN,
"  EHOSTUNREACH = %u;", EHOSTUNREACH,
"  EINPROGRESS = %u;", EINPROGRESS,
"  EALREADY = %u;", EALREADY,
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
