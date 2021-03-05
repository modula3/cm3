// The interface to this C code has no traced references,
// in case they require special handling such as using barriers.

#define _WINSOCK_DEPRECATED_NO_WARNINGS 1
#ifdef _MSC_VER
#pragma warning(disable:4242) // integer conversion
#pragma warning(disable:4244) // integer conversion
#pragma warning(disable:4255) // () vs. (void)
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

#if __cplusplus
extern "C" {
#endif

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
    *h = 0;
#ifdef _WIN32
    // Apparently WinSock "gethostbyname" does not resolve names
    // that happen to be dotted IP addresses (e.g. "123.33.44.44").
    // This function does.
    int a = inet_addr(s);
    if (a != INADDR_NONE)
    {
        *res = a; // the name is already a dotted IP address
        static hostent h_no_error;
        *h = &h_no_error;
        return 0; // no error
    }
#endif
    *h = gethostbyname(s);
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
    *h = 0;
#ifdef _WIN32
    // Apparently WinSock "gethostbyname" does not resolve names
    // that happen to be dotted IP addresses (e.g. "123.33.44.44").
    // This function does.
    int a = inet_addr(s);
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
GetCanonicalByAddr(const int* addr, TEXT* result)
{
    sockaddr_in ua;
    ZeroMemory(&ua, sizeof(ua));

    M3_STATIC_ASSERT(sizeof(*addr) == 4);
    M3_STATIC_ASSERT(sizeof(ua.sin_addr) == 4);

    memcpy(&ua.sin_addr, addr, 4);

    hostent* h = gethostbyaddr((char*)&ua, sizeof(ua), AF_INET);
    if (h)
    {
        IPInternal__CopyStoT(h->h_name, result); // under lock unfortunately
        return 0; // no error
    }

    return WSAGetLastError();
}

hostent*
__cdecl
GetHostAddr(int* address)
// Get the IP4 address of the current system.
// TODO Getting the name first seems silly.
{
    char hname[256];
    hostent* h = 0;
    int err = gethostname(hname, sizeof(hname));
    if (err == 0)
    {
        h = gethostbyname(hname);

        if (h == NULL)
            h = gethostbyname("127.0.0.1");

        if (h)
        {
            *address = GetAddress(h);
        }
    }

    return h;
}

EXTERN_CONST int IPInternal__AF_INET6 = AF_INET6;

// Return the first usable address - there could be many - and more likely an IP4 address.
int
__cdecl
IPInternal__GetAddrInfo(IP__Endpoint** endpoint, const char* node, const char* port)
{
    M3_STATIC_ASSERT(AF_INET);
    M3_STATIC_ASSERT(AF_INET6);

    *endpoint = 0;
    addrinfo* addressInfo = {0};
    int err = getaddrinfo(node, port, NULL, &addressInfo);

    if (err)
        return err;

    addrinfo* free = addressInfo;
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
    union {
        sockaddr sa;
        sockaddr_storage sas;
#ifndef _WIN32
        sockaddr_un sun; // On AIX, larger than sas.
#endif
        sockaddr_in sa4;
        sockaddr_in6 sa6;
    } sa;
    int size = sizeof(sa.sa4);

    M3_STATIC_ASSERT(sizeof(in_addr) == 4);
    M3_STATIC_ASSERT(sizeof(in6_addr) == 16);
    assert(family == AF_INET || family == AF_INET6);

    ZeroMemory(&sa, sizeof(sa));
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

    int err = getnameinfo(&sa.sa,
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

#if __cplusplus
} // extern "C"
#endif
