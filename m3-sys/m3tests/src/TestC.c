/* Copyright (C) 1994, Digital Equipment Corporation. */
/* All rights reserved.                               */
/* See the file COPYRIGHT for a full description.     */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <stddef.h>
#include <time.h>
#include <netdb.h>
#include <assert.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>

typedef struct linger linger_t;
typedef struct timeval timeval_t;
typedef struct tm tm_t;
typedef struct hostent hostent_t;
typedef unsigned U;
typedef unsigned char U8;

#define SIZEOF_FIELD(t, f) (((t*)0)->f)
#define OFFSET(a, b) ((U)offsetof(a, b))
#define SIZE(a) ((U)sizeof(a))

typedef struct T
{
    double d[10];
    float f[10];
    struct {
        /* keep these sorted by name for easier human comprehension */
        size_t gid;
        size_t linger;
        size_t pid;
        size_t socklen;
        size_t time;
        size_t timeval;
        size_t tm;
        size_t uid;
        /* pthreads omitted on purpose */
    } sizes;
} T;

static const T t1 =
{
    { 0.0, 0.5, 1.0, 2.0, -1.0, -3.0, 12.34, -124.456, 1000.0, -10000.0 },
    { 0.0, 0.5, 1.0, 2.0, -1.0, -3.5, 12.34, -124.456, 1000.0, -10000.0 },
    {
    SIZE(gid_t),
    SIZE(linger_t),
    SIZE(pid_t),
    SIZE(socklen_t),
    SIZE(time_t),
    SIZE(timeval_t),
    SIZE(tm_t),
    SIZE(uid_t)
    }
};

void Test__CheckFloatsAndTypes(const T* t2, size_t size, size_t jbsize)
{
    if (size != SIZE(t1))
    {
        printf("%x vs. %x\n", (U)size, SIZE(t1));
        assert(size == SIZE(t1));
    }
    if (memcmp(&t1, t2, SIZE(t1)) != 0)
    {
        U i = 0;
        U8* a = (U8*)&t1;
        U8* b = (U8*)t2;
        printf("FD_SETSIZE 0x%x\n", (U)FD_SETSIZE);
        printf("d[0], d[1]: %x, %x\n", OFFSET(T, d[0]), OFFSET(T, d[1]));
        printf("f[0], f[1]: %x, %x\n", OFFSET(T, f[0]), OFFSET(T, f[1]));
        printf("sizes: %x\n", OFFSET(T, sizes));
        for (i = 0; i != SIZE(t1); ++i)
        {
            if (a[i] != b[i])
            {
                printf("different at offset 0x%x (%x %x)\n", i, a[i], b[i]);
            }
        }
        assert(memcmp(&t1, t2, SIZE(t1)) == 0);
    }
#if defined(__CYGWIN__)
    assert(jbsize >= (SIZE(jmp_buf) / 4));
#elif defined(__FreeBSD__) && defined(__i386__)
    assert(jbsize == SIZE(jmp_buf) || (jbsize + 4) == SIZE(jmp_buf));
#elif defined(__sun)
    assert(jbsize == SIZE(jmp_buf) || jbsize == SIZE(sigjmp_buf));
#else
    if (jbsize != SIZE(jmp_buf))
    {
        fprintf(stderr, "%x vs. %x\n", (U)jbsize, SIZE(jmp_buf));
        assert(jbsize == SIZE(jmp_buf));
    }
#endif
}
