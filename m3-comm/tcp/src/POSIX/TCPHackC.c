/* Copyright 1994 Digital Equipment Corporation.
/* Distributed only by permission. */
/* Created on Sat Jan 11 15:49:00 PST 1992 by wobber */

// TODO Just delete this old code? All the copies?
// git grep RefetchError

#include "m3core.h"
#ifndef _WIN32
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#else
#include <winsock2.h>
#include <stdlib.h>
#endif

#define TRUE 1
#define FALSE 0

/* https://github.com/natefoo/predef/wiki/OperatingSystems */
#if defined(ULTRIX)     || \
    defined(ultrix)     || \
    defined(__ultrix)   || \
    defined(__ultrix__) || \
    defined(__osf__)    || \
    defined(__osf)
static const char refetchError = TRUE;
#else
static const char refetchError;
#endif

#define RefetchError TCPHack__RefetchError

int
__cdecl
RefetchError(INTEGER fd);

int
__cdecl
RefetchError(INTEGER fd)
{
    if (refetchError)
    {
        int optbuf = 0;
        socklen_t optlen = sizeof(optbuf);

        return getsockopt((int)fd, IPPROTO_TCP, TCP_NODELAY, &optbuf, &optlen) >= 0;
    }
    else
    {
        return FALSE;
    }
}
