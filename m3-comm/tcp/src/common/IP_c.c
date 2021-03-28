// The interface to this C code has no traced references,
// in case they require special handling such as using barriers.

#define _WINSOCK_DEPRECATED_NO_WARNINGS 1
#ifdef _MSC_VER
#pragma warning(disable:4616) // x is not a valid warning with older compiler
#pragma warning(disable:4242) // integer conversion
#pragma warning(disable:4244) // integer conversion
#pragma warning(disable:4255) // () vs. (void)
#pragma warning(disable:4668) // padding
#pragma warning(disable:4710) // function not inlined
#pragma warning(disable:4820) // padding
#pragma warning(disable:5045) // Spectre
#endif
#ifdef _WIN32
#include "ws2tcpip.h"
#include "wspiapi.h"
//TODO #include <winsock2.h>
#endif
#include "m3core.h"
#ifndef _WIN32
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/un.h>
#endif
#include "IP_h.h"

M3_STATIC_ASSERT(sizeof(int) == 4);
M3_STATIC_ASSERT(M3_FIELD_SIZE(sockaddr_in, sin_addr) == 4);
M3_STATIC_ASSERT(AF_INET);
M3_STATIC_ASSERT(AF_INET6);
M3_STATIC_ASSERT(sizeof(in_addr) == 4);
M3_STATIC_ASSERT(sizeof(in6_addr) == 16);

#if __cplusplus
extern "C" {
#endif

#undef X
#define X(x) EXTERN_CONST int IPInternal__##x = x;
X(TRY_AGAIN)
X(NO_RECOVERY)
X(NO_ADDRESS)

union SockAddrUnionAll;
typedef union SockAddrUnionAll SockAddrUnionAll;

union SockAddrUnionAll
{
    sockaddr sa;
    sockaddr_storage sas;
#ifndef _WIN32
    sockaddr_un saun; // On AIX, larger than sas.
#endif
    sockaddr_in sa4;
    sockaddr_in6 sa6;
};

static
int
__cdecl
GetAddress(hostent* ent)
// The first address from a host entry, presumed to be IP4.
{
    return *(int*)&((in_addr*)*ent->h_addr_list)->s_addr;
}

#ifndef _WIN32

static
int
WSAGetLastError(void)
// Get the most recent networking error on not-Windows.
// h_errno is typically thread-local.
{
    return h_errno;
}

#endif

int
__cdecl
GetHostByName(const char* s, int* res, hostent** h)
// Get the IP4 address of a host, given its name.
{
#ifdef _WIN32
    int a = {0};
#endif
    *h = 0;
#ifdef _WIN32
    // Apparently WinSock "gethostbyname" does not resolve names
    // that happen to be dotted IP addresses (e.g. "123.33.44.44").
    // This function does.
    a = inet_addr(s);
    if (a != INADDR_NONE)
    {
        static hostent h_no_error;
        *res = a; // the name is already a dotted IP address
        *h = &h_no_error;
        return 0; // no error
    }
#endif
    *h = gethostbyname(s); // the name is not a dotted IP address
    if (*h)
    {
        *res = GetAddress(*h);
        return 0; // no error
    }

    return WSAGetLastError();
}

int
__cdecl
GetCanonicalByName(const char* s, TEXT* text, hostent** h)
{
#ifdef _WIN32
    int a = {0};
#endif
    *h = 0;
#ifdef _WIN32
    // Apparently WinSock "gethostbyname" does not resolve names
    // that happen to be dotted IP addresses (e.g. "123.33.44.44").
    // This function does.
    a = inet_addr(s);
    if (a != INADDR_NONE)
    {
        // the name is not a dotted IP address
        *h = gethostbyaddr((char*)&a, sizeof(a), PF_INET);
    }
    else
#endif
    {
        *h = gethostbyname(s);
    }

    if (*h)
    {
        IPInternal__CopyStoT((*h)->h_name, text); // under lock unfortunately
        return 0; // no error
    }

    return WSAGetLastError();
}

int
__cdecl
GetCanonicalByAddr(const int* addr, TEXT* result, hostent** h)
{
    sockaddr_in ua;
    M3_STATIC_ASSERT(sizeof(*addr) == 4);
    M3_STATIC_ASSERT(sizeof(ua.sin_addr) == 4);

    ZeroMemory(&ua, sizeof(ua));
    memcpy(&ua.sin_addr, addr, 4);

    *h = gethostbyaddr((char*)&ua, sizeof(ua), AF_INET);
    if (*h)
    {
        IPInternal__CopyStoT((*h)->h_name, result); // under lock unfortunately
        return 0; // no error
    }

    return WSAGetLastError();
}

int
__cdecl
GetHostAddr(int* address, hostent** h)
// Get the IP4 address of the current system.
// TODO Getting the name first seems silly.
{
    char hname[256];
    int err = {0};
    *h = 0;
    err = gethostname(hname, sizeof(hname));
    if (err == 0)
    {
        *h = gethostbyname(hname);

        if (*h == NULL)
            *h = gethostbyname("127.0.0.1");

        if (*h)
        {
            *address = GetAddress(*h);
        }
    }

    return err;
}

EXTERN_CONST int IPInternal__AF_INET6 = AF_INET6;

// Return the first usable address - there could be many - and more likely an IP4 address.
int
__cdecl
IPInternal__GetAddrInfo(IP__Endpoint** endpoint, const char* node, const char* port)
{
    M3_STATIC_ASSERT(AF_INET);
    M3_STATIC_ASSERT(AF_INET6);
    addrinfo* addressInfo = {0};
    int err = {0};
    addrinfo* free = {0};

    *endpoint = 0;
    err = getaddrinfo(node, port, NULL, &addressInfo);

    if (err)
        return err;

    free = addressInfo;
    while (addressInfo)
    {
        int family = addressInfo->ai_family;
        if (family == AF_INET)
        {
            sockaddr_in* sa = (sockaddr_in*)addressInfo->ai_addr;
            IPInternal__NewEndpoint4(endpoint, sa->sin_port, &sa->sin_addr);
            break;
        }
        else if (family == AF_INET6)
        {
            sockaddr_in6* sa = (sockaddr_in6*)addressInfo->ai_addr;
            IPInternal__NewEndpoint6(endpoint, sa->sin6_port, &sa->sin6_addr);
            break;
        }
        addressInfo = addressInfo->ai_next;
    }
    freeaddrinfo(free);
    return 0; // no error
}

int
__cdecl
IPInternal__GetNameInfo(int family, int port, const void* addr, TEXT* hostText, TEXT* serviceText)
{
    char host[NI_MAXHOST];
    char service[NI_MAXSERV];
    SockAddrUnionAll sa = {0};
    socklen_t size = sizeof(sa.sa4);
    int err = {0};

    M3_STATIC_ASSERT(sizeof(in_addr) == 4);
    M3_STATIC_ASSERT(sizeof(in6_addr) == 16);
    assert(family == AF_INET || family == AF_INET6);

    ZERO_MEMORY(sa);
    host[0] = 0;
    service[0] = 0;

    switch (family)
    {
    case AF_INET:
        sa.sa4.sin_family = family;
        sa.sa4.sin_port = port;
        sa.sa4.sin_addr = *(in_addr*)addr;
        size = sizeof(sa.sa4);
        break;
    case AF_INET6:
        sa.sa6.sin6_family = family;
        sa.sa6.sin6_port = port;
        sa.sa6.sin6_addr = *(in6_addr*)addr;
        size = sizeof(sa.sa6);
        break;
    }

    err = getnameinfo(&sa.sa,
                      size,
                      host,
                      sizeof(host),
                      port ? service : 0,
                      port ? sizeof(service) : 0,
                      0);
    if (err == 0)
    {
        IPInternal__CopyStoT(host, hostText);
        if (port)
            IPInternal__CopyStoT(service, serviceText);
    }

    return err;
}

#ifdef _WIN32

static
void
__cdecl
IPInternal__Exitor(void)
{
    WSACleanup();
}

#endif

void
__cdecl
IPInternal__Init(void)
{
#ifdef _WIN32

#define WinSockVersion 0x0101 // App version 1.1

    WSADATA data;
    ZeroMemory(&data, sizeof(data));
    if (WSAStartup(WinSockVersion, &data))
        IPError__Die();
    else
        Process__RegisterExitor(IPInternal__Exitor);
#endif
}

#ifndef _WIN32

INTEGER
__cdecl
IPInternal__getsockname_in(INTEGER fd, char* address, INTEGER* port)
{
    INTEGER err;
    struct sockaddr_in sa;
    socklen_t size = sizeof(sa);

    ZERO_MEMORY(sa);
    err = getsockname(fd, (struct sockaddr*)&sa, &size);
    if (err < 0)
        return err;

    if (address)
        memcpy(address, &sa.sin_addr, 4); // address might not be aligned

    *port = ntohs(sa.sin_port);

    return 0;
}

INTEGER
__cdecl
IPInternal__bind_in(INTEGER fd, const char* address, INTEGER port)
{
    struct sockaddr_in sa;

    ZERO_MEMORY(sa);
    sa.sin_family = AF_INET;
    sa.sin_port = htons(port);
    memcpy(&sa.sin_addr, address, 4); // address might not be aligned
    return bind(fd, (struct sockaddr*)&sa, sizeof(sa));
}

#endif

#if __cplusplus
} // extern "C"
#endif
