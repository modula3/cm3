/* Copyright (C) 1994, Digital Equipment Corporation. */
/* All rights reserved.                               */
/* See the file COPYRIGHT for a full description.     */

#include <stddef.h>
#include <time.h>
#include <sys/socket.h>
#include <netdb.h>
#include <assert.h>
#include <setjmp.h>
#include <stdio.h>

typedef struct linger linger_t;
typedef struct timeval timeval_t;
typedef struct tm tm_t;
typedef struct hostent hostent_t;

typedef struct T
{
    /* If these are both 11, then the sizes don't match between Modula-3 and C */
    float f[12];
    double d[12];
    struct {
        /* keep these sorted by name for easier human comprehension */
        size_t gid;
        size_t hostent_addrtype;
        size_t hostent_length;
        size_t linger;
        size_t mode;
        size_t pid;
        size_t socklen;
        size_t time;
        size_t timeval;
        size_t tm;
        size_t uid;
        /* pthreads omitted on purpose */
    } sizes;
    /* keep these sorted by name for easier human comprehension */
    /*size_t max_fdset;*/
    linger_t linger;
    size_t pad1;
    timeval_t timeval;
    size_t pad2;
} T;

#define SIZEOF_FIELD(t, f) (((t*)0)->f)

const T t1 =
{
    { 0.0, 1.0, 2.0, 3.0, -1.0, -2.0, -3.0, 12.34, -124.456, 1000.0, -10000.0, -123456.789e0 },
    { 0.0, 1.0, 2.0, 3.0, -1.0, -2.0, -3.0, 12.34, -124.456, 1000.0, -10000.0,  123456.789e0 },
    sizeof(gid_t),
    sizeof(SIZEOF_FIELD(hostent_t, h_addrtype)),
    sizeof(SIZEOF_FIELD(hostent_t, h_length)),
    sizeof(linger_t),
    sizeof(mode_t),
    sizeof(pid_t),
    sizeof(socklen_t),
    sizeof(time_t),
    sizeof(timeval_t),
    sizeof(tm_t),
    sizeof(uid_t),
    /*FD_SETSIZE,*/
    /* linger */ {1, 2},
    10,
    /* timeval */ {1, 2},
    20,
};

typedef unsigned U;
typedef unsigned char U8;

void Test__CheckFloatsAndTypes(const T* t2, size_t size, size_t jbsize)
{
    if (size != sizeof(t1))
    {
        printf("%x vs. %x\n", (U)size, (U)sizeof(t1));
        assert(size == sizeof(t1));
    }
    if (memcmp(&t1, t2, sizeof(t1)) != 0)
    {
        U i = 0;
        U8* a = (U8*)&t1;
        U8* b = (U8*)t2;
        printf("FD_SETSIZE 0x%x\n", (U)FD_SETSIZE);
        /*printf("offset of max_fdset 0x%x\n", (U)offsetof(T, max_fdset));*/
        for (i = 0; i != sizeof(t1); ++i)
        {
            if (a[i] != b[i])
            {
                printf("different at offset 0x%x\n", i);
            }
        }
        assert(memcmp(&t1, t2, sizeof(t1)) == 0);
    }
#if defined(__CYGWIN__)
    assert(jbsize >= (sizeof(jmp_buf) / 4));
#elif defined(__sun)
    assert((jbsize == sizeof(jmp_buf)) || (jbsize == sizeof(sigjmp_buf)));
#else
    if (jbsize != sizeof(jmp_buf))
    {
        fprintf(stderr, "%x vs. %x\n", (U)jbsize, (U)sizeof(jmp_buf));
        assert(jbsize == sizeof(jmp_buf));
    }
#endif
}
