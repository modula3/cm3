/* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */

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
static char refetchError = TRUE;
#else
static char refetchError = FALSE;
#endif

#define RefetchError SocketPosix__RefetchError
void RefetchError(INTEGER fd);

#if 0

typedef double LONGREAL;

#ifdef __cplusplus
extern "C" {
#endif

enum {WaitResult_Ready, WaitResult_Error, WaitResult_FDError, WaitResult_Timeout};

int
__cdecl
SchedulerPosix__IOWait(int fd, int/*boolean*/read, LONGREAL timeout);

typedef struct sockaddr_in SockAddrIn;

typedef struct {
    INTEGER addr4;
    UINT8 addr6[16];
} Address;

typedef struct {
    INTEGER port;
    Address address;
} EndPoint;

#define NullPort 0
const EndPoint NullAddress = {0,{0,{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}};

static void InitStream(int fd);
static void GetHostAddr(Address* a);
static void MakeNonBlocking(int fd);
static void SetAddress(const EndPoint* ep, /*out*/SockAddrIn* name);
static void EndPointToAddress(const EndPoint* ep, /*out*/SockAddrIn* name);
static void AddressToEndPoint(const SockAddrIn* name, /*out*/EndPoint* ep);

typedef void* Exception;
#define Unreachable Socket__Unreachable
#define PortBusy Socket__PortBusy
#define NoResources Socket__NoResources
#define Refused Socket__Refused
#define Timeout Socket__Timeout
#define ConnLost Socket__ConnLost
#define Unexpected Socket__Unexpected
extern Exception Unreachable, PortBusy, NoResources;
extern Exception Refused, Timeout, ConnLost, Unexpected;

#define IOError SocketPosix__IOError
void
__cdecl
IOError(Exception);

#ifdef _WIN32
const SocketC__SockErr = SockErr;
#else
const SocketC__SockErr = -1;
#endif

#ifdef _WIN32
#define GetSocketError() WSAGetLastError()
#else
#define GetSocketError() errno
#endif

int
__cdecl
SocketPosixC__Create(int/*boolean*/ reliable)
{
    int one = 1;
    int fd = socket(AF_INET, reliable ? SOCK_STREAM : SOCK_DGRAM, 0);
    if (fd == -1)
    {
        Exception e = Unexpected;
        switch (GetSocketError())
        {
#ifdef _WIN32
        case WSAEMFILE:
#else
        case EMFILE:
        case ENFILE:
#endif
            e = NoResources;
            break;
        }
        IOError(e);
    }

#ifndef _WIN32
    MakeNonBlocking(fd);
#endif
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    return fd;
}

#ifndef _WIN32

int
__cdecl
SocketPosixC__Status(int fd,
                     LONGREAL* modificationTime,
                     LONGINT* size)
{
    struct stat st;
    int r;
    
    st.st_mtime = 0;
    st.st_size = 0;
    r = fstat(fd, &st);
    *modificationTime = st.st_mtime;
    *size = st.st_size;
    return r;
}

#endif

void
__cdecl
SocketPosixC__Bind(int fd, EndPoint* ep)
{
    SockAddrIn name;

    ZERO_MEMORY(name);
    SetAddress(ep, &name);
    if (bind(fd, (struct sockaddr *)&name, sizeof(name)) == -1)
    {
        Exception e = Unexpected;
        switch (GetSocketError())
        {
#ifdef _WIN32
        case WSAEADDRINUSE:
#else
        case EADDRINUSE:
#endif
            e = PortBusy;
            break;
        }
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

static
int/*boolean*/
CommonError(int err)
{
    switch (err)
    {
#ifdef _WIN32
    case WSAETIMEDOUT:
#else
    case ETIMEDOUT:
#endif
        IOError(Timeout);
        return TRUE;

#ifdef _WIN32
    case WSAENETUNREACH:
    case WSAEHOSTUNREACH:
    case WSAEHOSTDOWN:
    case WSAENETDOWN:
#else
    case ENETUNREACH:
    case EHOSTUNREACH:
    case EHOSTDOWN:
    case ENETDOWN:
#endif
        IOError(Unreachable);
        return TRUE;
    }
    return FALSE;
}

void
__cdecl
SocketPosixC__Connect(int fd, EndPoint* ep)
{
    SockAddrIn name;
    int error;

    ZERO_MEMORY(name);
    EndPointToAddress(ep, &name);
    InitStream(fd);
    while (1)
    {
        if (connect(fd, (struct sockaddr *)&name, sizeof(name)) == 0)
            break;
        error = GetSocketError();
        switch (error)
        {
        case EINVAL: /* hack to try to get real GetSocketError(), hidden due to NBIO bug in connect */
        case EBADF:  /* we'll try the same for EBADF, which we've seen on Alpha */
            RefetchError(fd, &error);
        }
        if (CommonError(error))
            return;
        switch (error)
        {
#ifdef _WIN32
        case WSAEISCONN:
#else
        case EISCONN:
#endif
            return;

#ifdef _WIN32
        case WSAEADDRNOTAVAIL:
        case WSAECONNREFUSED:
        case WSAEINVAL:
        case WSAECONNRESET:
        case WSAEBADF:
#else
        case EADDRNOTAVAIL:
        case ECONNREFUSED:
        case EINVAL:
        case ECONNRESET:
        case EBADF:
#endif
            IOError(Refused);
            return;

#ifdef _WIN32
        case WSAEWOULDBLOCK:
        case WSAEAGAIN:
        case WSAEINPROGRESS:
        case WSAEALREADY:
#else
        case EWOULDBLOCK:
#if EWOULDBLOCK != EAGAIN
        case EAGAIN:
#endif
        case EINPROGRESS:
        case EALREADY:
#endif
            /* nope, not yet */
            break;

        default:
            IOError(Unexpected);
            return;
        }
#ifndef _WIN32
        SchedulerPosix__IOAlertWait(fd, FALSE);
#endif
    }
}

int
__cdecl
SocketPosixC__Accept(int server, EndPoint* ep)
{
    SockAddrIn name;
    int client = { 0 };

    while (1)
    {
        socklen_t len = sizeof(name);
        ZERO_MEMORY(name);
        client = accept(server, (struct sockaddr *)&name, &len);
        if (client >= 0)
        {
            InitStream(client);
            AddressToEndPoint(&name, ep);
            return client;
        }

        switch (GetSocketError())
        {
        case EMFILE:
        case ENFILE:
            IOError(NoResources);
            return -1;

        case EWOULDBLOCK:
#if EAGAIN != EWOULDBLOCK
        case EAGAIN:
#endif
            /* nope, not yet */
            break;

        default:
            IOError(Unexpected);
            return -1;
        }
        SchedulerPosix__IOAlertWait(server, TRUE);
    }
}

static
int/*boolean*/
CommonRead(int fd, int err, int/*boolean*/ mayBlock, INTEGER* len)
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
#if EAGAIN != EWOULDBLOCK
    case EAGAIN:
#endif
        if (!mayBlock)
        {
            *len = -1;
            return TRUE;
        }
        break;

    default:
        IOError(Unexpected);
    }
    SchedulerPosix__IOWait(fd, TRUE, 0);
    return FALSE;
}

static
void
CommonWrite(int fd, INTEGER len, char** p, INTEGER* n)
{
    if (len >= 0)
    {
        *p += len;
        *n -= len;
    }
    else
    {
        if (CommonError(GetSocketError()))
            return;

        switch (GetSocketError())
        {
        case EPIPE:
        case ECONNRESET:
        case ENETRESET:
            IOError(ConnLost);
            return;

        case EWOULDBLOCK:
#if EWOULDBLOCK != EAGAIN
        case EAGAIN:
#endif
            /* OK, wait to write out a bit more... */
            break;

        default:
            IOError(Unexpected);
            return;
        }
    }

    if (*n > 0)
    {
      SchedulerPosix__IOWait(fd, FALSE, 0);
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

    ZERO_MEMORY(name);
    while (1)
    {
        socklen_t nameLen = sizeof(name);
        INTEGER len = recvfrom(fd, b, nb, 0, (struct sockaddr *)&name, &nameLen);
        if (len >= 0)
        {
            AddressToEndPoint(&name, ep);
            return len;
        }
        if (CommonRead(fd, GetSocketError(), mayBlock, &len))
            return len;
    }
}

INTEGER
__cdecl
SocketPosixC__Read(int fd, void* pb, INTEGER nb, int/*boolean*/ mayBlock)
{
    while (1)
    {
        INTEGER len = read(fd, pb, nb);
        if ((len >= 0) || CommonRead(fd, GetSocketError(), mayBlock, &len))
            return len;
    }
}

void
__cdecl
SocketPosixC__SendTo(int fd, const EndPoint* ep, const void* pb, INTEGER n)
{
    INTEGER len = { 0 };
    SockAddrIn name = { 0 };

    ZERO_MEMORY(name);
    while (n > 0)
    {
        EndPointToAddress(ep, &name);
        len = sendto(fd, pb, n, 0, (struct sockaddr *)&name, sizeof(name));
        CommonWrite(fd, len, (char**)&pb, &n);
    }
}

void
__cdecl
SocketPosixC__Write(int fd, const void* p, INTEGER n)
{
    while (n > 0)
    {
        INTEGER len = write(fd, p, n);
        CommonWrite(fd, len, (char**)&p, &n);
    }
}

INTEGER
__cdecl
SocketPosixC__BytesAvailable(int fd)
{
    if (SchedulerPosix__IOWait(fd, TRUE, 0) == WaitResult_Ready)
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
SocketPosixC__Peek(int fd, EndPoint* ep)
{
    SockAddrIn name;
    socklen_t len = sizeof(name);

    ZERO_MEMORY(name);
    if (recvfrom(fd, NULL, 0, MSG_PEEK, (struct sockaddr *)&name, &len) < 0)
    {
        IOError(Unexpected);
        return;
    }

    AddressToEndPoint(&name, ep);
}

void
__cdecl
SocketPosixC__ThisEnd(int fd, EndPoint* ep)
{
    if (memcmp(ep, &NullAddress, sizeof(NullAddress)) == 0)
    {
        GetHostAddr(&ep->address);
    }
    if (ep->port == NullPort)
    {
        SockAddrIn name;
        socklen_t len = sizeof(name);

        ZERO_MEMORY(name);
        if (getsockname(fd, (struct sockaddr *)&name, &len) != 0)
        {
            IOError(Unexpected);
            return;
        }
        ep->port = ntohs(name.sin_port);
    }
}

static void GetHostAddr(Address* a)
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

    a->addr4 = ((struct in_addr*)hostent->h_addr_list)->s_addr;
}

void
__cdecl
SocketPosixC__OtherEnd(int fd, EndPoint* ep)
{
    SockAddrIn addr;
    socklen_t len = sizeof(addr);

    ZERO_MEMORY(addr);
    if (getpeername(fd, (struct sockaddr *)&addr, &len) < 0)
    {
        IOError(Unexpected);
        return;
    }
    AddressToEndPoint(&addr, ep);
}

/*------------------------------------------------ internal utilities ---*/

static void SetAddress(const EndPoint* ep, /*out*/SockAddrIn* name)
{
    /*t.ep = ep;*/
    EndPointToAddress(ep, name);
}

static void EndPointToAddress(const EndPoint* ep, /*out*/SockAddrIn* name)
{
    ZeroMemory(name, sizeof(*name));
    name->sin_family = AF_INET;
    name->sin_port = htons(ep->port);
    name->sin_addr.s_addr = ep->address.addr4;
}

static void AddressToEndPoint(const SockAddrIn* name, /*out*/EndPoint* ep)
{
    ep->address.addr4 = name->sin_addr.s_addr;
    ep->port = ntohs(name->sin_port);
}

static void InitStream(int fd)
{
    /* We assume that the runtime ignores SIGPIPE signals. */
    int one = 1;
#ifdef _WIN32
    struct linger linger = {0, 0};
#else
    struct linger linger = {1, 1};
#endif

    /*****
    setsockopt(fd, SOL_SOCKET, SO_SNDBUF, &SysSendBufSize, sizeof(SysSendBufSize));
    setsockopt(fd, SOL_SOCKET, SO_RCVBUF, &SysRcvBufSize, sizeof(SysRcvBufSize));
    ******/
    setsockopt(fd, SOL_SOCKET, SO_LINGER, &linger, sizeof(linger));
    setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
    MakeNonBlocking(fd);
}

static void MakeNonBlocking(int fd)
{
    if (fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK) == -1)
        IOError(Unexpected);
}

#endif

void RefetchError(INTEGER fd)
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
