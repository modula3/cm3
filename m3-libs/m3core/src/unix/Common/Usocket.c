/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#define ASSERT_PLEN \
    assert((plen == NULL) || (((*plen) >= 0) && ((*plen) < (1 << 30))));

#define ASSERT_LEN \
    assert((len >= 0) && (len < (1 << 30)));

/* multi-part experiment */

/* 1: wrap everything */

int Usocket__listen(int s, int backlog)
{
    return listen(s, backlog);
}

int Usocket__shutdown(int s, int how)
{
    return shutdown(s, how);
}

int Usocket__socket(int af, int type, int protocol)
{
    return socket(af, type, protocol);
}

/* 2: wrap everything taking input socklen_t */

int Usocket__bind(int s, sockaddr_t* name, m3_socklen_t len)
{
    ASSERT_LEN
    return bind(s, name, len);
}

int Usocket__connect(int s, sockaddr_t* name, m3_socklen_t len)
{
    ASSERT_LEN
    return connect(s, name, len);
}

int Usocket__sendto(int s, void* msg, size_t length, int flags, sockaddr_t* dest, m3_socklen_t len)
{
    ASSERT_LEN
    return sendto(s, msg, length, flags, dest, len);
}

int Usocket__setsockopt(int s, int level, int optname, void* optval, m3_socklen_t len)
{
    ASSERT_LEN
    return setsockopt(s, level, optname, optval, len);
}

/* 3: wrap everything taking input/output socklen_t */

#if !defined(__hpux) || !defined(__LP64__)

int Usocket__getpeername(int s, sockaddr_t* name, m3_socklen_t* plen)
{
    ASSERT_PLEN
    return getpeername(s, name, plen);
}

int Usocket__getsockname(int s, sockaddr_t* name, m3_socklen_t* plen)
{
    ASSERT_PLEN
    return getsockname(s, name, plen);
}

int Usocket__accept(int s, sockaddr_t* addr, m3_socklen_t* plen)
{
    ASSERT_PLEN
    return accept(s, addr, plen);
}

int Usocket__getsockopt(int s, int level, int optname, void* optval, m3_socklen_t* plen)
{
    ASSERT_PLEN
    return getsockopt(s, level, optname, optval, plen);
}

int Usocket__recvfrom(int s, void* buf, size_t len, int flags, sockaddr_t* from, m3_socklen_t* plen)
{
    ASSERT_PLEN
    return recvfrom(s, buf, len, flags, from, plen);
}

#else

int Usocket__getpeername(int s, sockaddr_t* name, m3_socklen_t* plen)
{
    ASSERT_PLEN
    {
        socklen_t len = plen ? *plen : 0;
        int r = getpeername(s, name, plen ? &len : 0);
        if (plen) *plen = len;
        return r;
    }
}

int Usocket__getsockname(int s, sockaddr_t* name, m3_socklen_t* plen)
{
    ASSERT_PLEN
    {
        socklen_t len = plen ? *plen : 0;
        int r = getsockname(s, name, plen ? &len : 0);
        if (plen) *plen = len;
        return r;
    }
}

int Usocket__accept(int s, sockaddr_t* addr, m3_socklen_t* plen)
{
    ASSERT_PLEN
    {
        socklen_t len = plen ? *plen : 0;
        int r = accept(s, addr, plen ? &len : 0);
        if (plen) *plen = len;
        return r;
    }
}

int Usocket__getsockopt(int s, int level, int optname, void* optval, m3_socklen_t* plen)
{
    ASSERT_PLEN
    {
        socklen_t len = plen ? *plen : 0;
        int r = getsockopt(s, level, optname, optval, plen ? &len : 0);
        if (plen) *plen = len;
        return r;
    }
}

int Usocket__recvfrom(int s, void* buf, size_t length, int flags, sockaddr_t* address, m3_socklen_t* plen)
{
    ASSERT_PLEN
    {
        socklen_t len = plen ? *plen : 0;
        int r = recvfrom(s, buf, length, flags, address, plen ? &len : 0);
        if (plen) *plen = len;
        return r;
    }
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
