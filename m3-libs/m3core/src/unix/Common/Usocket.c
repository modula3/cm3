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

static void Usocket__assert_plen_in(m3_socklen_t* plen)
{
    assert(LOSSLESS_SOCKLEN || plen == NULL || (*plen <= (1UL << 30)));
}

static void Usocket__plen_out(m3_socklen_t* plen, socklen_t len)
{
    assert(LOSSLESS_SOCKLEN || plen == NULL || len <= (1UL << 30));
    if (plen)
        *plen = len;
}

#define ASSERT_PLEN_IN Usocket__assert_plen_in(plen);
#define PLEN_OUT Usocket__plen_out(plen, len);

#define ASSERT_LEN \
    assert(len <= (1UL << 30));

void Usocket__Assertions(void)
{
#ifndef _WIN32
    /* assert that struct linger is literally { int l_onoff, l_linger },
    those types (or at least sizes), that order, no padding, except on Cygwin */
    
    typedef struct linger T;
    typedef m3_linger_t M;
    
    M3_STATIC_ASSERT(sizeof(M) == 8);
    M3_STATIC_ASSERT(offsetof(M, onoff) == 0);
    M3_STATIC_ASSERT(M3_FIELD_SIZE(M, onoff) == 4);
    M3_STATIC_ASSERT(offsetof(M, linger) == 4);
    M3_STATIC_ASSERT(M3_FIELD_SIZE(M, linger) == 4);
    M3_STATIC_ASSERT(sizeof(T) == (M3_FIELD_SIZE(T, l_onoff) + M3_FIELD_SIZE(T, l_linger)));
#ifndef __CYGWIN__
    M3_STATIC_ASSERT(sizeof(T) == 8);
    M3_STATIC_ASSERT(offsetof(T, l_onoff) == 0);
    M3_STATIC_ASSERT(M3_FIELD_SIZE(T, l_onoff) == 4);
    M3_STATIC_ASSERT(offsetof(T, l_linger) == 4);
    M3_STATIC_ASSERT(M3_FIELD_SIZE(T, l_linger) == 4);
#endif
#endif
}

#ifndef _WIN32

/* wrap everything */

#define M3MODULE Usocket
M3WRAP2(int, listen, int, int)
M3WRAP2(int, shutdown, int, int)
M3WRAP3(int, socket, int, int, int)
M3WRAP4(INTEGER, send, int, const void*, size_t, int)
M3WRAP4(INTEGER, recv, int, void*, size_t, int)

/* wrap everything taking input socklen_t */

int Usocket__bind(int s, struct sockaddr* name, m3_socklen_t len)
{
    ASSERT_LEN
    return bind(s, name, len);
}

int Usocket__connect(int s, struct sockaddr* name, m3_socklen_t len)
{
    ASSERT_LEN
    return connect(s, name, len);
}

INTEGER Usocket__sendto(int s, void* msg, size_t length, int flags, struct sockaddr* dest, m3_socklen_t len)
{
    ASSERT_LEN
    return sendto(s, msg, length, flags, dest, len);
}

int Usocket__setsockopt(int s, int level, int optname, void* optval, m3_socklen_t len)
{
    ASSERT_LEN
#ifdef __CYGWIN__
    if (optname == SO_LINGER && optval != NULL)
    {
        struct linger b;
        m3_linger_t* a = (m3_linger_t*)optval;
        assert(len == sizeof(*a));
        b.l_onoff = a->onoff;
        b.l_linger = a->linger;
        return setsockopt(s, level, optname, &b, sizeof(b));
    }
#endif
    return setsockopt(s, level, optname, optval, len);
}

/* wrap everything taking input/output socklen_t */

int Usocket__getpeername(int s, struct sockaddr* name, m3_socklen_t* plen)
{
    ASSERT_PLEN_IN
    {
        socklen_t len = plen ? *plen : 0;
        int r = getpeername(s, name, plen ? &len : 0);
        PLEN_OUT
        return r;
    }
}

int Usocket__getsockname(int s, struct sockaddr* name, m3_socklen_t* plen)
{
    ASSERT_PLEN_IN
    {
        socklen_t len = plen ? *plen : 0;
        int r = getsockname(s, name, plen ? &len : 0);
        PLEN_OUT
        return r;
    }
}

int Usocket__accept(int s, struct sockaddr* addr, m3_socklen_t* plen)
{
    ASSERT_PLEN_IN
    {
        socklen_t len = plen ? *plen : 0;
        int r = accept(s, addr, plen ? &len : 0);
        PLEN_OUT
        return r;
    }
}

int Usocket__getsockopt(int s, int level, int optname, void* optval, m3_socklen_t* plen)
/*
Posix says l_onoff and l_linger are int, but they aren't on Cygwin.
As usual Posix does not mandate the order of the fields or that there aren't
all fields, but all known implementations have no additional fields and use
the same order. This is checked in Usocket__Assertions.
*/
{
    ASSERT_PLEN_IN
    {
        int r;
        socklen_t len = plen ? *plen : 0;

#ifdef __CYGWIN__
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

#ifdef __CYGWIN__
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

INTEGER Usocket__recvfrom(int s, void* buf, size_t length, int flags, struct sockaddr* address, m3_socklen_t* plen)
{
    ASSERT_PLEN_IN
    {
        socklen_t len = plen ? *plen : 0;
        INTEGER r = recvfrom(s, buf, length, flags, address, plen ? &len : 0);
        PLEN_OUT
        return r;
    }
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
