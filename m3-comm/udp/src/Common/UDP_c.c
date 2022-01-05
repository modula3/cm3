#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifndef _WIN32
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/un.h>
#endif

typedef struct sockaddr sockaddr;
typedef struct sockaddr_in sockaddr_in;

#if __cplusplus
extern "C" {
#endif

void
__cdecl
UDPInternal__Assert(void)
{
    sockaddr_in sa = {0};
    M3_STATIC_ASSERT(sizeof(sa.sin_addr.s_addr) == 4);
}

#ifndef _WIN32

INTEGER
__cdecl
UDPInternal__Send(
    INTEGER fd,
    void const* volatile* data,
    INTEGER len,
    const char* addr,
    INTEGER port)
// addr is array of char so we cannot assume alignment.
// data is volatile pointer to pointer on stack to try to cooperate with garbage collector
{
    sockaddr_in sa = {0};

    Scheduler__DisableSwitching();

    ZERO_MEMORY(sa);
    sa.sin_family = AF_INET;
    sa.sin_port = htons(port);

    // Copy possibly unaligned 4 character array to 4 byte integer.
    memcpy(&sa.sin_addr.s_addr, addr, 4);

    len = sendto(fd, *data, len, 0 /* flags */, (sockaddr*)&sa, sizeof(sa));

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
    INTEGER* port)
// data is volatile pointer to pointer on stack to try to cooperate with garbage collector
// addr is array of char so we cannot assume alignment.
{
    sockaddr_in sa = {0};
    m3c_socklen_t addr_len = sizeof(sa);

    ZERO_MEMORY(sa);

    Scheduler__DisableSwitching();

    len = recvfrom(fd, *data, len, 0 /* flags */, (sockaddr*)&sa, &addr_len);

    Scheduler__EnableSwitching();

    if (len >= 0)
    {
        assert(addr_len == sizeof(sockaddr_in));
        *port = ntohs(sa.sin_port);

        // Copy 4 byte integer to possibly unaligned 4 character array.
        memcpy(addr, &sa.sin_addr.s_addr, 4);
    }

    return len;
}

#endif

#if __cplusplus
} // extern "C"
#endif
