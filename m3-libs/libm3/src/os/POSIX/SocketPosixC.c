/* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct sockaddr_in SockAddrIn;
typedef void* Exception;
#define Unreachable Socket__Unreachable
#define PortBusy Socket__PortBusy
#define NoResources Socket__NoResources
#define Refused Socket__Refused
#define Timeout Socket__Timeout
#define ConnLost Socket__ConnLost
#define Unexpected Socket__Unexpected
extern Exception Unreachable, PortBusy, NoResources,
extern Exception Refused, Timeout, ConnLost, Unexpected;

#define IOErrorSocketPosix__IOError
void
__cdecl
IOError(Exception);

int
__cdecl
SocketPosixC__Create(int/*boolean*/ reliable)
{
    int one = 1;
    int fd = socket(AF_INET, reliable ? SOCK_STREAM : SOCK_DGRAM, 0);
    if (fd == -1)
    {
        Exception e = Unexpected;
        switch (errno)
        {
        case EMFILE:
        case ENFILE:
            e = NoResources;
            break;
        }
        IOError(e);
    }

    MakeNonBlocking(fd);
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    return fd;
}

int
__cdecl
SocketPosixC__Status(int fd,
                     LONGREAL* modificationTime,
                     LONGINT* size)
{
    stat_t st = { 0 };

    int r = fstat(fd, (struct stat*)&st);
    *modificationTime = st.st_mtime;
    *size = st.st_size;
    return r;
}

void
__cdecl
SocketPosixC__Bind(int fd, EndPoint* ep)
{
    SockAddrIn name;

    ZERO_MEMORY(name);
    SetAddress(ep, &name);
    if (bind(fd, &name, sizeof(name)) == -1)
    {
        Exception e = Unexpected;
        if (errno == EADDRINUSE)
            e = PortBusy;
        IOError(e);
    }
}

int
__cdecl
SocketPosixC__Listen(int fd,
                     size_t max_queue)
{
    return listen(fd, max_queue);
}

void
__cdecl
SocketPosixC__Connect(int fd, EndPoint* ep)
{
    SockAddrIn name;

    ZERO_MEMORY(name);
    EndPointToAddress(ep, &name);
    InitStream(fd);
    while (1)
    {
        if (connect(fd, &name, sizeof(name)) == 0)
            break;
        switch (errno)
        {
        case EINVAL: /* hack to try to get real errno, hidden due to NBIO bug in connect */
        case EBADF:  /* we'll try the same for EBADF, which we've seen on Alpha */
            RefetchError(fd);
        }
        switch (errno)
        {
        case EISCONN:
            return;

        case EADDRNOTAVAIL:
        case ECONNREFUSED:
        case EINVAL:
        case ECONNRESET:
        case EBADF:
            IOError(Refused);
            return;

        case ETIMEDOUT:
            IOError(Timeout);
            return;

        case ENETUNREACH:
        case EHOSTUNREACH:
        case EHOSTDOWN:
        case ENETDOWN:
            IOError(Unreachable);
            return;

        case EWOULDBLOCK:
        case EAGAIN:
        case EINPROGRESS:
        case EALREADY:
            /* nope, not yet */
            break;

        default:
            IOError(Unexpected);
            return;
        }
        SchedulerPosix__IOAlertWait(fd, FALSE);
    }
}

void
__cdecl
SocketPosixC__Accept(int server, EndPoint* ep)
{
    SockAddrIn name;
    int client = { 0 };

    while (1)
    {
        socklen_t len = sizeof(name);
        ZERO_MEMORY(name);
        client = accept(server, &name, &len);
        if (client >= 0)
        {
            InitStream(client);
            AddressToEndPoint(&name, ep);
            return;
        }

        switch (errno)
        {
        case EMFILE:
        case ENFILE:
            IOError(NoResources);
            return;

        case EWOULDBLOCK:
        case EAGAIN:
            /* nope, not yet */
            break;

        default:
            IOError(Unexpected);
            return;
        }
        SchedulerPosix__IOAlertWait(server, TRUE);
    }
}

static
int/*boolean*/
CommonError(int err)
{
    switch (err)
    {
    case ETIMEDOUT:
        IOError(Timeout);
        return TRUE;

    case ENETUNREACH:
    case EHOSTUNREACH:
    case EHOSTDOWN:
    case ENETDOWN:
        IOError(Unreachable);
        return TRUE;
    }
    return FALSE;
}

static
int/*boolean*/
CommonRead(int fd,
           int err,
           int/*boolean*/ mayBlock,
           INTEGER* len)
{
    if (CommonError(err))
        return FALSE;

    switch (err)
    {
    case ECONNRESET:
        *len = 0;
        return TRUE;

    case EPIPE:
    case ENETRESET:
        IOError(ConnLost);
        return FALSE;

    case EWOULDBLOCK:
    case EAGAIN:
        if (!mayBlock)
        {
            *len = -1;
            return TRUE;
        }
        break;

    default:
        IOError(Unexpected);
    }
    SchedulerPosix__IOWait(fd, TRUE);
    return FALSE;
}

static
void
CommonWrite(int fd,
            INTEGER len,
            char** p,
            INTEGER* n)
{
    if (len >= 0)
    {
        *p += len;
        *n -= len;
    }
    else
    {
        if (CommonError(errno))
            return;

        switch (errno)
        {
        case EPIPE:
        case ECONNRESET:
        case ENETRESET:
            IOError(ConnLost);
            return;

        case EWOULDBLOCK:
        case EAGAIN:
            /* OK, wait to write out a bit more... */
            break;

        default:
          IOError(Unexpected);
          return;
        }
    }

    if (*n > 0)
    {
      SchedulerPosix__IOWait(fd, FALSE);
      /* IF Thread.TestAlert() THEN RAISE Thread.Alerted END */
    }
}

INTEGER
__cdecl
SocketPosixC__ReceiveFrom(int fd,
                          EndPoint* ep,
                          ADDRESS b,
                          INTEGER nb,
                          int/*boolean*/ mayBlock)
{
    SockAddrIn name;
    INTEGER nameLen = sizeof(name);

    ZERO_MEMORY(name);
    while (1)
    {
        INTEGER len = recvfrom(fd, b, nb, 0, &name, &nameLen);
        if (len >= 0)
        {
            AddressToEndPoint(name, ep);
            return len;
        }
        if (CommonRead(fd, errno, mayBlock, &len))
            return len;
    }
}

INTEGER
__cdecl
Read(int fd, void* pb, INTEGER nb, int/*boolean*/ mayBlock)
{
    while (1)
    {
        INTEGER len = read(fd, pb, nb);
        if ((len >= 0) || CommonRead(fd, errno, mayBlock, &len))
            return len;
    }
}
void
INTEGER
__cdecl
SendTo(int fd, const EndPoint* ep, const void* pb, INTEGER n)
{
    INTEGER len = { 0 };
    SockAddrIn name = { 0 };

    ZERO_MEMORY(name);
    while (n > 0)
    {
        EndPointToAddress(ep, &name);
        len = sendto(fd, pb, n, 0, &name, sizeof(name));
        CommonWrite(fd, len, (char**)&pb, &n);
    }
}

void
__cdecl
Write(int fd, const const void* b, INTEGER n)
{
    while (n > 0)
    {
        INTEGER len = write(fd, p, n);
        CommonWrite(fd, len, (char**)&pb, &n);
    }
}

INTEGER
__cdecl
BytesAvailable(int fd)
{
    if (SchedulerPosix__IOWait(fd, TRUE, 0) == SchedulerPosix.WaitResult.Ready)
    {
        int charsToRead = { 0 };
        if (ioctl(fd, FIONREAD, &charsToRead))
        {
            IOError(Unexpected);
            return 0;
        }
        if (charsToRead > 0)
            return charsToRead;
    }
    return 0;
}

void
__cdecl
Peek(int fd, EndPoint* ep)
{
    SockAddrIn name;
    socklen_t len = sizeof(name);

    ZERO_MEMORY(name);
    if (recvfrom(fd, NULL, 0, MSG_PEEK, &name, &len) == -1)
    {
        IOError(Unexpected);
        return;
    }

    AddressToEndPoint(name, ep);
}

void
__cdecl
ThisEnd(int fd, EndPoint* ep)
{
    if (memcmp(ep, nullAddress, sizeof(nullAddress)) == 0)
        ep->addr = GetHostAddr();
    if (ep->port == NullPort)
    {
        SockAddrIn name;
        socklen_t len = sizeof(name);

        ZERO_MEMORY(name);
        if (getsockname(fd, &name, &len) == -1)
        {
            IOError(Unexpected);
            return;
        }
        ep->port = ntohs(name.sin_port);
    }
}

void
__cdecl
GetHostAddr(Address* a)
{
    char host[256];
    struct hostent* hostent;

    if (gethostname(host, sizeof(host)))
    {
        IOError(Unexpected);
        return;
    }

    if (!(hostent = gethostbyname(host)))
    {
        IOError(Unexpected);
        return;
    }

    assert(hostent->h_length <= sizeof(Address));

    *a = ((struct in_addr*)hostent->h_addr_list)->s_addr;
}

void
__cdecl
OtherEnd(int fd, EndPoint* ep)
{
    SockAddrIn addr;
    socklen_t len = sizeof(addr);

    ZERO_MEMORY(addr);
    if (getpeername(fd, &addr, &len) == -1)
    {
        IOError(Unexpected);
        return;
    }
    AddressToEndPoint(&addr, ep);
}

/*------------------------------------------------ internal utilities ---*/

void
__cdecl
SetAddress(const EndPoint* ep, /*out*/SockAddrIn* name)
{
    /*t.ep = ep;*/
    EndPointToAddress(ep, name);
}

void
__cdecl
EndPointToAddress(const EndPoint* ep, /*out*/SockAddrIn* name)
{
    ZeroMemory(name, sizeof(*name));
    name->sin_family = AF_INET;
    name->sin_port = htons(ep->port);
    name->sin_addr.s_addr = ep->addr;
}

void
__cdecl
AddressToEndPoint(const SockAddrIn* name, /*out*/EndPoint* ep)
{
    ep->addr = name->sin_addr.s_addr;
    ep->port = ntohs(name->sin_port;
}

void
__cdecl
InitStream(int fd)
{
    /* We assume that the runtime ignores SIGPIPE signals. */
    int one = 1;
    struct linger linger = {1, 1};

    /*****
    setsockopt(fd, SOL_SOCKET, SO_SNDBUF, &SysSendBufSize, sizeof(SysSendBufSize));
    setsockopt(fd, SOL_SOCKET, SO_RCVBUF, &SysRcvBufSize, sizeof(SysRcvBufSize));
    ******/
    setsockopt(fd, SOL_SOCKET, SO_LINGER, &linger, sizeof(linger));
    setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
    MakeNonBlocking(fd);
}

void
__cdecl
MakeNonBlocking(int fd)
{
    if (fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK))
        IOError(Unexpected);
}

void
__cdecl
RefetchError(int fd)
{
/* Awful hack to retrieve a meaningful error from a TCP accept
   socket.  Only works on Ultrix and OSF.  Leaves result
   in Cerrno.GetErrno().  */
#if defined(ULTRIX) || defined(ultrix) || defined(__osf__)
    int optbuf = 0;
    socklen_t optlen = sizeof(optbuf);
    getsockopt(fd, IPPROTO_TCP, TCP_NODELAY, optbuf, &optlen);
#endif
}

#ifdef __cplusplus
} /* extern C */
#endif
