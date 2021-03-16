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

#if __cplusplus
extern "C" {
#endif

#define STRUCT_AND_TYPEDEF(x) struct x; typedef struct x x

STRUCT_AND_TYPEDEF(sockaddr_in);

void
__cdecl
UDPInternal__Assert(void)
{
    sockaddr_in sockaddr = {0};
    M3_STATIC_ASSERT(sizeof(sockaddr.sin_addr.s_addr) == 4);
}

#ifndef _WIN32

INTEGER
__cdecl
UDPInternal__Init(
    INTEGER* fd,
    char* addr,
    int port,
    int* err,
    int* status)
// addr is array of char so we cannot assume alignment.
{
    sockaddr_in sockaddr = {0};

    ZERO_MEMORY(sockaddr);

    Scheduler__DisableSwitching();

    // create socket via socket(2) system call
    *fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (*fd == -1)
    {
        *err = errno;
    }
    else
    {
        // bind socket via bind(2) system call
        sockaddr.sin_family = AF_INET;
        sockaddr.sin_port = htons(port);

        // Copy possibly unaligned 4 character array to 4 byte integer.
        memcpy(&sockaddr.sin_addr.s_addr, addr, 4);

        *status = bind(*fd, &sockaddr, sizeof(sockaddr));
        if (*status)
        {
            *err = errno;
        }
    }

    Scheduler__EnableSwitching();
}

INTEGER
__cdecl
UDPInternal__Send(
    INTEGER fd,
    void const* volatile* data,
    INTEGER len,
    const char* addr,
    int port)
// addr is array of char so we cannot assume alignment.
// data is volatile pointer to pointer on stack to try to cooperate with garbage collector
{
    sockaddr_in sockaddr = {0};

    Scheduler__DisableSwitching();

    ZERO_MEMORY(sockaddr);
    sockaddr.sin_family = AF_INET;
    sockaddr.sin_port = htons(port);

    // Copy possibly unaligned 4 character array to 4 byte integer.
    memcpy(&sockaddr.sin_addr.s_addr, addr, 4);

    len = sendto(fd, *data, len, 0 /* flags */, &sockaddr, sizeof(sockaddr));

    Scheduler__EnableSwitching();

    return len;
}

INTEGER
__cdecl
UDPInternal__Receive(
    INTEGER fd,
    void* volatile* data,
    INTEGER len,
    char* addr,
    int* port)
// data is volatile pointer to pointer on stack to try to cooperate with garbage collector
// addr is array of char so we cannot assume alignment.
{
    sockaddr_in sockaddr = {0};
    socklen_t addr_len = sizeof(sockaddr);

    ZERO_MEMORY(sockaddr);

    Scheduler__DisableSwitching();

    len = recvfrom(fd, *data, len, 0 /* flags */, &sockaddr, &addr_len);

    Scheduler__EnableSwitching();

    if (len >= 0)
    {
        assert(addr_len == sizeof(sockaddr_in));
        *port = ntohs(sockaddr.sin_port);

        // Copy 4 byte integer to possibly unaligned 4 character array.
        memcpy(addr, &sockaddr.sin_addr.s_addr, 4);
    }

    return len;
}

#endif

#if __cplusplus
} // extern "C"
#endif
