/* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */

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

#ifdef __cplusplus
extern "C" {
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
static const char refetchError = FALSE;
#endif

#define RefetchError SocketPosix__RefetchError
void RefetchError(INTEGER fd);

void
__cdecl
RefetchError(INTEGER fd)
{
  if (refetchError)
  {
    /* Awful hack to retrieve a meaningful error from a TCP accept
       socket.  Only works on Ultrix and OSF.  Leaves result
       in errno.  */
    int optbuf = 0;
    socklen_t optlen = sizeof(optbuf);
    getsockopt((int)fd, IPPROTO_TCP, TCP_NODELAY, &optbuf, &optlen);
  }
}

#ifdef __cplusplus
} /* extern C */
#endif
