/* $Id: Uerror.i3.cpp,v 1.6 2008-11-02 10:46:25 jkrell Exp $ */

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
"(* This file was generated from " __FILE__ ". Do not edit it. *)", 0,
"", 0,
"INTERFACE Uerror;", 0,
"", 0,
"CONST", 0,
#define X(x) "  " #x " = %u;", x,
X(EPERM)
X(ENOENT)
X(EINTR)
X(EIO)
X(ENOEXEC)
X(EBADF)
X(ECHILD)
X(EAGAIN)
X(ENOMEM)
X(EACCES)
X(EEXIST)
X(EISDIR)
X(EINVAL)
X(ENFILE)
X(EMFILE)
X(EPIPE)
X(EDOM)
X(ERANGE)
X(ENAMETOOLONG)
X(ENOTEMPTY)
"  EWOULDBLOCK = EAGAIN;", 0,
X(ENOTSOCK)
X(EADDRINUSE)
X(EADDRNOTAVAIL)
X(ENETDOWN)
X(ENETUNREACH)
X(ENETRESET)
X(ECONNABORTED)
X(ECONNRESET)
X(EISCONN)
X(ETIMEDOUT)
X(ECONNREFUSED)
X(EHOSTDOWN)
X(EHOSTUNREACH)
X(EINPROGRESS)
X(EALREADY)
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
