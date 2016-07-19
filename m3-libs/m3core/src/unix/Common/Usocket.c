/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

#define TYPE_IS_SIGNED(t) (((t)~(t)0) < (t)0)
#define TYPES_MATCH(t, u) (TYPE_IS_SIGNED(t) == TYPE_IS_SIGNED(u) && sizeof(t) == sizeof(u))

/* m3_socklen_t is Word.T
 * socklen_t is any of uint32, int32, size_t.
 * Make sure we don't lose values in converting.
 */

#define LOSSLESS_SOCKLEN (TYPES_MATCH(socklen_t, m3_socklen_t))

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
Usocket__plen_out(m3_socklen_t* plen, socklen_t len)
{
    assert(LOSSLESS_SOCKLEN || plen == NULL || len <= (1UL << 30));
    if (plen)
        *plen = len;
}

#define ASSERT_PLEN_IN Usocket__assert_plen_in(plen);
#define PLEN_OUT Usocket__plen_out(plen, len);

#define ASSERT_LEN \
    assert(len <= (1UL << 30));

M3_DLL_LOCAL void __cdecl
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

#define M3MODULE Usocket
M3WRAP2(int, listen, int, int)
M3WRAP2(int, shutdown, int, int)
M3WRAP3(int, socket, int, int, int)
M3WRAP4(INTEGER, send, int, const void*, WORD_T, int)
M3WRAP4(INTEGER, recv, int, void*, WORD_T, int)

/* wrap everything taking input socklen_t */

M3_DLL_EXPORT int __cdecl
Usocket__bind(int s, struct sockaddr* name, m3_socklen_t len)
{
    ASSERT_LEN
    return bind(s, name, len);
}

M3_DLL_EXPORT int __cdecl
Usocket__connect(int s, struct sockaddr* name, m3_socklen_t len)
{
    ASSERT_LEN
    return connect(s, name, len);
}

M3_DLL_EXPORT INTEGER __cdecl
Usocket__sendto(int s, void* msg, WORD_T length, int flags, struct sockaddr* dest, m3_socklen_t len)
{
    ASSERT_LEN
    return sendto(s, msg, length, flags, dest, len);
}

M3_DLL_EXPORT int __cdecl
Usocket__setsockopt(int s, int level, int optname, void* optval, m3_socklen_t len)
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
        return setsockopt(s, level, optname, (void*)&b, sizeof(b));
    }
#endif
    return setsockopt(s, level, optname, optval, len);
}

/* wrap everything taking input/output socklen_t */

M3_DLL_EXPORT int __cdecl
Usocket__getpeername(int s, struct sockaddr* name, m3_socklen_t* plen)
{
    ASSERT_PLEN_IN
    {
        socklen_t len = plen ? *plen : 0;
        int r = getpeername(s, name, plen ? &len : 0);
        PLEN_OUT
        return r;
    }
}

M3_DLL_EXPORT int __cdecl
Usocket__getsockname(int s, struct sockaddr* name, m3_socklen_t* plen)
{
    ASSERT_PLEN_IN
    {
        socklen_t len = plen ? *plen : 0;
        int r = getsockname(s, name, plen ? &len : 0);
        PLEN_OUT
        return r;
    }
}

M3_DLL_EXPORT int __cdecl
Usocket__accept(int s, struct sockaddr* addr, m3_socklen_t* plen)
{
    ASSERT_PLEN_IN
    {
        socklen_t len = plen ? *plen : 0;
        int r = accept(s, addr, plen ? &len : 0);
        PLEN_OUT
        return r;
    }
}

M3_DLL_EXPORT int __cdecl
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
        socklen_t len = plen ? *plen : 0;

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
        r = getsockopt(s, level, optname, optval, plen ? &len : 0);
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

M3_DLL_EXPORT INTEGER __cdecl
Usocket__recvfrom(int s, void* buf, WORD_T length, int flags, struct sockaddr* address, m3_socklen_t* plen)
{
    ASSERT_PLEN_IN
    {
        socklen_t len = plen ? *plen : 0;
        INTEGER r = recvfrom(s, buf, length, flags, address, plen ? &len : 0);
        PLEN_OUT
        return r;
    }
}

#ifdef __cplusplus
} /* extern "C" */
#endif
