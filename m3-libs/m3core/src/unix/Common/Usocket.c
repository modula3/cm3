/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

// Most of the (static)asserts in this file are not likely needed.
// Strongly consider significantly trimming them.

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __DJGPP__

void __cdecl
Usocket__Assertions(void)
{
}

#else

M3_STATIC_ASSERT(AF_INET > 0);
M3_STATIC_ASSERT(AF_INET6 > 0);
#ifdef AF_UNIX
M3_STATIC_ASSERT(AF_UNIX > 0);
#endif

// Win32 does define AF_UNIX but historically not implement it.
#ifdef _WIN32
#undef AF_UNIX
#endif

// Idealized Modula3 IPv4 socket address.
// This does match many systems but not all.
// Sometimes family is 8 bits, adjacent to another 8 bits,
// and the ordering of those is the problem.
struct m3_sockaddr_in
{
    unsigned short family;
    unsigned short port;
    unsigned int   addr;
    unsigned char zero[8];
};
M3_STATIC_ASSERT(sizeof(sockaddr_in) >= 16); // if this fails, ok, remove it
M3_STATIC_ASSERT(sizeof(m3_sockaddr_in) == 16);

// Idealized Modula3 IPv6 socket address.
// Probably actually the same on all platforms.
//struct in6_addr
//{
//    unsigned char u6_addr8[16];
//};
M3_STATIC_ASSERT(sizeof(in6_addr) == 16);

// Idealized Modula3 IPv6 socket address.
// Sometimes family is 8 bits adjacent to another 8 bits.
struct m3_sockaddr_in6
{
    unsigned short family;
    unsigned short port;
    unsigned int   flowinfo;
    unsigned int   scope_id;
    // in6_addr is 16 bytes. On OSF/1 it is 8-aligned.
    // Ideally the largest elements are first, but keep family/port first to fit other protocols.
    unsigned int   padding_for_alignment; // 4 more bytes so addr is 8-aligned.
    in6_addr       addr;
};
M3_STATIC_ASSERT(sizeof(m3_sockaddr_in6) == 32);

// Idealized Modula3 Unix socket address.
// This is likely slightly larger than all native forms.
// if not make it larger, e.g. 256 - 2.
// Sometimes family is 8 bits adjacent to another 8 bits.
struct m3_sockaddr_un
{
    unsigned short family;
    char           path[128 - 2];
};
M3_STATIC_ASSERT(sizeof(m3_sockaddr_un) == 128);

// Union "all" native socket address types.
union NativeSockAddrUnionAll
{
    sockaddr sa;
    sockaddr_storage sas;
#ifdef AF_UNIX
    sockaddr_un un; // On AIX, larger than sas.
#endif
    sockaddr_in in;
    sockaddr_in6 in6;
};

// Assert that the idealized Modula3 forms suffice and that the
// native forms meet some assumptions.

// Family overlaps in all forms.
M3_STATIC_ASSERT(offsetof(sockaddr_in, sin_family) == offsetof(sockaddr_in6, sin6_family));
#ifdef AF_UNIX
M3_STATIC_ASSERT(offsetof(sockaddr_in, sin_family) == offsetof(sockaddr_un, sun_family));
#endif

// Modula-3 forms are usually same size as native or larger.
// This does not mean exactly the same, as Modula-3 never has "len"
// field and native sometimes does.
// Haiku sockaddr_in is larger.
M3_STATIC_ASSERT(sizeof(m3_sockaddr_in) <= sizeof(sockaddr_in));
#ifdef AF_UNIX
M3_STATIC_ASSERT(sizeof(m3_sockaddr_un) >= sizeof(sockaddr_un));
#endif
// Native family is 8 or 16 bits.
M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_in, family) >= M3_FIELD_SIZE(sockaddr_in, sin_family));
M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_in, port) == M3_FIELD_SIZE(sockaddr_in, sin_port));
M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_in, addr) == M3_FIELD_SIZE(sockaddr_in, sin_addr));

M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_in6, family) >= M3_FIELD_SIZE(sockaddr_in6, sin6_family));
M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_in6, port) == M3_FIELD_SIZE(sockaddr_in6, sin6_port));
M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_in6, flowinfo) == M3_FIELD_SIZE(sockaddr_in6, sin6_flowinfo));
M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_in6, addr) == M3_FIELD_SIZE(sockaddr_in6, sin6_addr));
M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_in6, scope_id) == M3_FIELD_SIZE(sockaddr_in6, sin6_scope_id));

#ifdef AF_UNIX
M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_un, family) >= M3_FIELD_SIZE(sockaddr_un, sun_family));
// m3 path must be larger than or the same size as native; we always copy native size
M3_STATIC_ASSERT(M3_FIELD_SIZE(m3_sockaddr_un, path) >= M3_FIELD_SIZE(sockaddr_un, sun_path));
// sun_path must be an array not a pointer because we copy its size
M3_STATIC_ASSERT(4 != M3_FIELD_SIZE(sockaddr_un, sun_path));
M3_STATIC_ASSERT(8 != M3_FIELD_SIZE(sockaddr_un, sun_path));
M3_STATIC_ASSERT(sizeof(void*) != M3_FIELD_SIZE(sockaddr_un, sun_path));
#endif

union M3SockAddrUnionAll
{
    unsigned short family;
    m3_sockaddr_in in;
    m3_sockaddr_in6 in6;
    m3_sockaddr_un un;
};

static
int
SockAddrM3ToNativeIn(const m3_sockaddr_in* m3, sockaddr_in* native)
{
    assert(m3 != (void*)native);
    native->sin_family = m3->family;
    native->sin_port = m3->port;
    native->sin_addr.s_addr = m3->addr;
    return sizeof(*native);
}

static
int
SockAddrNativeToM3In(const sockaddr_in* native, m3_sockaddr_in* m3)
{
    assert(m3 != (void*)native);
    m3->family = native->sin_family;
    m3->port = native->sin_port;
    m3->addr = native->sin_addr.s_addr;
    return sizeof(*m3);
}

static
int
SockAddrM3ToNativeIn6(const m3_sockaddr_in6* m3, sockaddr_in6* native)
{
    assert(m3 != (void*)native);
    native->sin6_family = m3->family;
    native->sin6_port = m3->port;
    native->sin6_addr = m3->addr;
    native->sin6_flowinfo = m3->flowinfo;
    native->sin6_scope_id = m3->scope_id;
    return sizeof(*native);
}

static
int
SockAddrNativeToM3In6(const sockaddr_in6* native, m3_sockaddr_in6* m3)
{
    assert(m3 != (void*)native);
    m3->family = native->sin6_family;
    m3->port = native->sin6_port;
    m3->addr = native->sin6_addr;
    m3->flowinfo = native->sin6_flowinfo;
    m3->scope_id = native->sin6_scope_id;
    return sizeof(*m3);
}

#ifdef AF_UNIX

static
int
SockAddrM3ToNativeUn(const m3_sockaddr_un* m3, sockaddr_un* native)
{
    M3_STATIC_ASSERT(sizeof(native->sun_path) != 4);
    M3_STATIC_ASSERT(sizeof(native->sun_path) != 8);
    M3_STATIC_ASSERT(sizeof(native->sun_path) <= sizeof(m3->path));
    assert(m3 != (void*)native);
    native->sun_family = m3->family;
    // TODO ensure it fits without truncation
    assert(strlen(m3->path) < sizeof(native->sun_path));
    memcpy(native->sun_path, m3->path, sizeof(native->sun_path));
    return sizeof(*native);
}

static
int
SockAddrNativeToM3Un(const sockaddr_un* native, m3_sockaddr_un* m3)
{
    M3_STATIC_ASSERT(sizeof(native->sun_path) != 4);
    M3_STATIC_ASSERT(sizeof(native->sun_path) != 8);
    M3_STATIC_ASSERT(sizeof(native->sun_path) <= sizeof(m3->path));
    assert(m3 != (void*)native);
    m3->family = native->sun_family;
    assert(strlen(native->sun_path) < sizeof(m3->path));
    memcpy(m3->path, native->sun_path, sizeof(native->sun_path));
    return sizeof(*m3);
}

#endif

static
int
SockAddrM3ToNative(const M3SockAddrUnionAll* m3, NativeSockAddrUnionAll* native)
{
    const unsigned family = m3->family;
#ifdef AF_UNIX
    assert(family == AF_INET || family == AF_INET6 || family == AF_UNIX);
#else
    assert(family == AF_INET || family == AF_INET6);
#endif
    switch (m3->family)
    {
    case AF_INET:
        return SockAddrM3ToNativeIn(&m3->in, &native->in);
    case AF_INET6:
        return SockAddrM3ToNativeIn6(&m3->in6, &native->in6);
#ifdef AF_UNIX
    case AF_UNIX:
        return SockAddrM3ToNativeUn(&m3->un, &native->un);
#endif
    }
    return -1;
}

static
int
SockAddrNativeToM3(const NativeSockAddrUnionAll* native, M3SockAddrUnionAll* m3)
{
    const unsigned family = native->in.sin_family;
#ifdef AF_UNIX
    assert(family == AF_INET || family == AF_INET6 || family == AF_UNIX);
#else
    assert(family == AF_INET || family == AF_INET6);
#endif
    switch (family)
    {
    case AF_INET:
        return SockAddrNativeToM3In(&native->in, &m3->in);
    case AF_INET6:
        return SockAddrNativeToM3In6(&native->in6, &m3->in6);
#ifndef _WIN32
    case AF_UNIX:
        return SockAddrNativeToM3Un(&native->un, &m3->un);
#endif
    }
}

#define TYPE_IS_SIGNED(t) (((t)~(t)0) < (t)0)
#define TYPES_MATCH(t, u) (TYPE_IS_SIGNED(t) == TYPE_IS_SIGNED(u) && sizeof(t) == sizeof(u))

/* m3_socklen_t is Word.T
 * socklen_t is any of uint32, int32, size_t.
 * Make sure we do not lose values in converting.
 * Typical values are quite small.
 */

#define LOSSLESS_SOCKLEN (TYPES_MATCH(m3c_socklen_t, m3_socklen_t))

static void __cdecl
Usocket__assert_plen_in(m3_socklen_t* plen)
{
    if (!(LOSSLESS_SOCKLEN || plen == NULL || (*plen <= (1UL << 30))))
    {
#ifndef _WIN32 /* Do not accidentally export printf. */
        printf("%u %u %x%08x\n",
               !!LOSSLESS_SOCKLEN,
               !!plen,
               (plen ? (unsigned)((*plen >> 31) >> 1) : 0),
               (plen ? (unsigned)*plen : 0));
#endif
        assert(LOSSLESS_SOCKLEN || plen == NULL || (*plen <= (1UL << 30)));
    }
}

static void __cdecl
Usocket__plen_out(m3_socklen_t* plen, m3c_socklen_t len)
{
    assert(LOSSLESS_SOCKLEN || plen == NULL || len <= (1UL << 30));
    if (plen)
        *plen = len;
}

#define ASSERT_PLEN_IN Usocket__assert_plen_in(plen);
#define PLEN_OUT Usocket__plen_out(plen, len);

#define ASSERT_LEN \
    assert(len <= (1UL << 30));

void __cdecl
Usocket__Assertions(void)
{
    /* assert that struct linger is literally { int l_onoff, l_linger },
     * those types (or at least sizes), that order, no padding, except on Cygwin and Win32
     */
    typedef struct linger T;
#if defined(__CYGWIN__) || defined(_WIN32)
    typedef short T2;
#else
    typedef int T2;
#endif
    typedef m3_linger_t M;
    
    /* Modula-3 type is two integers, oneoff followed by linger. */

    M3_STATIC_ASSERT(sizeof(M) == 8);
    M3_STATIC_ASSERT(offsetof(M, onoff) == 0);
    M3_STATIC_ASSERT(M3_FIELD_SIZE(M, onoff) == 4);
    M3_STATIC_ASSERT(offsetof(M, linger) == 4);
    M3_STATIC_ASSERT(M3_FIELD_SIZE(M, linger) == 4);

    /* C type is two integers or two shorts, oneoff followed by linger. */

    M3_STATIC_ASSERT(sizeof(T) == (M3_FIELD_SIZE(T, l_onoff) + M3_FIELD_SIZE(T, l_linger)));
    M3_STATIC_ASSERT(sizeof(T) == 2 * sizeof(T2));
    M3_STATIC_ASSERT(offsetof(T, l_onoff) == 0);
    M3_STATIC_ASSERT(M3_FIELD_SIZE(T, l_onoff) == sizeof(T2));
    M3_STATIC_ASSERT(offsetof(T, l_linger) == sizeof(T2));
    M3_STATIC_ASSERT(M3_FIELD_SIZE(T, l_linger) == sizeof(T2));
}

/* wrap everything */

#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Usocket
M3WRAP2(int, listen, int, int)
M3WRAP2(int, shutdown, int, int)
M3WRAP3(int, socket, int, int, int)

ssize_t __cdecl
Usocket__send (int a, const void* b, size_t c, int d)
{
    ssize_t r;
    Scheduler__DisableSwitching();

    r = send (a, (char*)b, c, d); // cast for Windows

    Scheduler__EnableSwitching();
    return r;
}

ssize_t __cdecl
Usocket__recv (int a, void* b, size_t c, int d)
{
    ssize_t r;
    Scheduler__DisableSwitching();

    r = recv (a, (char*)b, c, d); // cast for Windows

    Scheduler__EnableSwitching();
    return r;
}

// wrap everything taking input m3c_socklen_t or sockaddr
#define WRAP_SOCKADDR_INPUT1                    \
    ssize_t result = {0};                       \
    NativeSockAddrUnionAll native;              \
    M3SockAddrUnionAll m3;                      \
                                                \
    ZERO_MEMORY(native);                        \
    ZERO_MEMORY(m3);                            \
    assert(len > 0 && len <= sizeof(m3));       \
    memcpy(&m3, pm3_sockaddr, len);             \
    len = SockAddrM3ToNative(&m3, &native);     \
    assert(len > 0);                            \
    Scheduler__DisableSwitching();              \
    result =                                    \

#define WRAP_SOCKADDR_INPUT2 \
    Scheduler__EnableSwitching();               \
    return result;                              \

#define WRAP_SOCKADDR_OUTPUT1                   \
    ssize_t result = {0};                       \
    NativeSockAddrUnionAll native;              \
    M3SockAddrUnionAll m3;                      \
    m3c_socklen_t len = {0};                    \
    ZERO_MEMORY(native);                        \
    ZERO_MEMORY(m3);                            \
    ASSERT_PLEN_IN                              \
    Scheduler__DisableSwitching();              \
    result =                                    \

// Size checks could use tightening here, esp. for AF_UNIX.
#define WRAP_SOCKADDR_OUTPUT2                               \
    Scheduler__EnableSwitching();                           \
    if (!result)                                            \
    {                                                       \
        len = SockAddrNativeToM3(&native, pm3_sockaddr);    \
        assert(len <= sizeof(m3));                          \
        PLEN_OUT                                            \
    }                                                       \
    return result;

int __cdecl
Usocket__bind(int s, const M3SockAddrUnionAll* pm3_sockaddr, m3_socklen_t len)
{
    WRAP_SOCKADDR_INPUT1

    bind(s, &native.sa, len);

    WRAP_SOCKADDR_INPUT2
}

int __cdecl
Usocket__connect(int s, const M3SockAddrUnionAll* pm3_sockaddr, m3_socklen_t len)
{
    WRAP_SOCKADDR_INPUT1

    connect(s, &native.sa, len);

    WRAP_SOCKADDR_INPUT2
}

ssize_t __cdecl
Usocket__sendto(int s, const void* msg, size_t length, int flags, const M3SockAddrUnionAll* pm3_sockaddr, m3_socklen_t len)
{
    WRAP_SOCKADDR_INPUT1

    // cast for Windows
    sendto(s, (char*)msg, length, flags, &native.sa, len);

    WRAP_SOCKADDR_INPUT2
}

int __cdecl
Usocket__setsockopt(int s, int level, int optname, const void* optval, m3_socklen_t len)
{
    ASSERT_LEN
#if defined(__CYGWIN__) || defined(_WIN32)
    if (optname == SO_LINGER && optval != NULL)
    {
        struct linger b;
        m3_linger_t* a = (m3_linger_t*)optval;
        assert(len == sizeof(*a));
        ZeroMemory(&b, sizeof(b));
        b.l_onoff = a->onoff;
        b.l_linger = a->linger;
        return setsockopt(s, level, optname, (char*)&b, sizeof(b)); // cast for Windows
    }
#endif
    return setsockopt(s, level, optname, (char*)optval, len); // cast for Windows
}

/* wrap everything taking input/output m3c_socklen_t */

int __cdecl
Usocket__getpeername(int s, M3SockAddrUnionAll* pm3_sockaddr, m3_socklen_t* plen)
{
    WRAP_SOCKADDR_OUTPUT1

    getpeername(s, &native.sa, &len);

    WRAP_SOCKADDR_OUTPUT2
}

int __cdecl
Usocket__getsockname(int s, M3SockAddrUnionAll* pm3_sockaddr, m3_socklen_t* plen)
{
    WRAP_SOCKADDR_OUTPUT1

    getsockname(s, &native.sa, &len);

    WRAP_SOCKADDR_OUTPUT2
}

int __cdecl
Usocket__accept(int s, M3SockAddrUnionAll* pm3_sockaddr, m3_socklen_t* plen)
{
    WRAP_SOCKADDR_OUTPUT1

    accept(s, &native.sa, &len);

    WRAP_SOCKADDR_OUTPUT2
}

int __cdecl
Usocket__getsockopt(int s, int level, int optname, void* optval, m3_socklen_t* plen)
/*
Posix says l_onoff and l_linger are int, but they aren't on Cygwin.
As usual Posix does not mandate the order of the fields or that there aren't
all fields, but all known implementations have no additional fields and use
the same order. This is checked in Usocket__Assertions.
*/
{
    ASSERT_PLEN_IN
    {
        int r = { 0 };
        m3c_socklen_t len = plen ? *plen : 0;

#if defined(__CYGWIN__) || defined(_WIN32)
        m3_linger_t* a = { 0 };
        struct linger b;

        if (optname == SO_LINGER && optval != NULL)
        {
            assert(plen);
            assert(len == sizeof(*a));
            a = (m3_linger_t*)optval;
            optval = &b;
            ZeroMemory(&b, sizeof(b));
            ZeroMemory(a, sizeof(*a));
            len = sizeof(b);
        }
#endif
        r = getsockopt(s, level, optname, (char*)optval, plen ? &len : 0); // cast for Windows
        PLEN_OUT

#if defined(__CYGWIN__) || defined(_WIN32)
        if (a)
        {
            assert(len == sizeof(b));
            *plen = sizeof(*a);
            a->onoff = b.l_onoff;
            a->linger = b.l_linger;
        }
#endif
        return r;
    }
}

ssize_t __cdecl
Usocket__recvfrom(int s, void* buf, size_t length, int flags, M3SockAddrUnionAll* pm3_sockaddr, m3_socklen_t* plen)
{
    WRAP_SOCKADDR_OUTPUT1

    recvfrom(s, (char*)buf, length, flags, &native.sa, &len); // cast for Windows

    WRAP_SOCKADDR_OUTPUT2
}

#ifdef AF_UNIX

static
int
Usocket__un(
    sockaddr_un* addr,
    const char* path)
{
    int err = 0;
    addr->sun_family = AF_UNIX;
    addr->sun_path[0] = 0;
    if (path)
    {
        size_t length = strlen(path) + 1;
        if (length > sizeof(addr->sun_path))
        {
            errno = ENAMETOOLONG;
            err = -1;
        }
        else
        {
            memcpy(addr->sun_path, path, length);
        }
    }
    return err;
}

int
__cdecl
Usocket_accept_un(int fd)
{
    sockaddr_un addr;
    m3c_socklen_t socklen = sizeof(addr);
    addr.sun_family = AF_UNIX;
    addr.sun_path[0] = 0;
    return accept(fd, (sockaddr*)&addr, &socklen);
}

int
__cdecl
Usocket__bind_un(
    int fd,
    const char* path)
{
    sockaddr_un addr;
    return Usocket__un(&addr, path) ? -1 : bind(fd, (sockaddr*)&addr, sizeof(addr));
}

int
__cdecl
Usocket__connect_un(
    int fd,
    const char* path)
{
    sockaddr_un addr;
    return Usocket__un(&addr, path) ? -1 : connect(fd, (sockaddr*)&addr, sizeof(addr));
}

#endif

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
