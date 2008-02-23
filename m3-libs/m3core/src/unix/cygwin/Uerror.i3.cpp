/* $Id: Uerror.i3.cpp,v 1.2 2008-02-23 08:32:27 jkrell Exp $ */

#include <signal.h>
#include <stdio.h>

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
"(* Last modified on Mon Jan  5 01:05:58 GMT 1998 by rrw               *)", 0,
"(*      modified on Fri Feb 24 14:53:26 PST 1995 by kalsow            *)", 0,
"(*      modified on Tue Feb 14 20:02:55 GMT 1995 by rrw1000@cam.ac.uk *)", 0,
"(*      modified on Thu Jul 21 00:00:00 1994 by sims@usa.acsys.com    *)", 0,
"(*      modified on Thu Nov 22 05:20:45 1990 by muller                *)", 0,
"", 0,
"INTERFACE Uerror;", 0,
"", 0,
"(*** <errno.h> ***)", 0,
"", 0,
"CONST", 0,
"  EPERM           = 1;    (* Not owner / operation not permitted  *)", 0,
"  ENOENT          = 2;    (* No such file or directory *)", 0,
"  EINTR           = 4;    (* Interrupted system call *)", 0,
"  EIO             = 5;    (* I/O error *)", 0,
"  ENOEXEC         = 8;    (* Exec format error *)", 0,
"  EBADF           = 9;    (* Bad file number *)", 0,
"  ECHILD          = 10;   (* No children *)", 0,
"  EAGAIN          = 11;   (* No more processes *)", 0,
"  ENOMEM          = 12;   (* Not enough core *)", 0,
"  EACCES          = 13;   (* Permission denied *)", 0,
"  EEXIST          = 17;   (* File exists *)", 0,
"  EISDIR          = 21;   (* Is a directory *)", 0,
"  EINVAL          = 22;   (* Invalid argument *)", 0,
"  ENFILE          = 23;   (* File table overflow *)", 0,
"  EMFILE          = 24;   (* Too many open files *)", 0,
"  EPIPE           = 32;   (* Broken pipe *)", 0,
"", 0,
"   (* math software *)", 0,
"  EDOM            = 33;   (* Argument too large *)", 0,
"  ERANGE          = 34;   (* Result too large *)", 0,
"", 0,
"  ENAMETOOLONG    = 91;   (* File name too long *)", 0,
"  ENOTEMPTY       = 90;   (* Directory not empty *)", 0,
"", 0,
"   (* Non-blocking I/O  and IPC errors *)", 0,
"  EWOULDBLOCK     = EAGAIN;   (* Operation would block *)", 0,
"", 0,
"  (* ipc/network software *)", 0,
"        (* argument errors *)", 0,
"  ENOTSOCK        = 108;   (* Socket operation on non-socket *)", 0,
"  EADDRINUSE      = 112;   (* Address already in use *)", 0,
"  EADDRNOTAVAIL   = 125;   (* Can't assign requested address *)", 0,
"", 0,
"   (* operational errors *)", 0,
"  ENETDOWN        = 115;   (* Network is down *)", 0,
"  ENETUNREACH     = 114;   (* Network is unreachable *)", 0,
"  ENETRESET       = 126;   (* Network dropped connection because of reset *)", 0,
"  ECONNABORTED    = 113;   (* Software caused connection abort *)", 0,
"  ECONNRESET      = 104;   (* Connection reset by peer *)", 0,
"  EISCONN         = 127;   (* Transport endpoint is already connected *)", 0,
"  ETIMEDOUT       = 116;   (* Connection timed out *)", 0,
"  ECONNREFUSED    = 111;   (* Connection refused *)", 0,
"  EHOSTDOWN       = 117;   (* Host is down *)", 0,
"  EHOSTUNREACH    = 118;   (* No route to host *)", 0,
"  EINPROGRESS     = 119;   (* Operation now in progress *)", 0,
"  EALREADY        = 120;   (* Operation already in progress *)", 0,
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
