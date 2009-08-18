/* Copyright (C) 1994, Digital Equipment Corporation. */
/* All rights reserved.                               */
/* See the file COPYRIGHT for a full description.     */

#include <stddef.h>
#include <setjmp.h>
#include <time.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

typedef unsigned char U8;
typedef unsigned U;
#define OFFSET(a, b) ((U)offsetof(a, b))
#define SIZE(a) ((U)sizeof(a))

typedef struct T T;

#ifndef _WIN32

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netdb.h>

typedef struct linger linger_t;
typedef struct timeval timeval_t;

#endif

typedef struct tm tm_t;

struct T
{
    double d[10];
    float f[10];
#ifndef _WIN32
    struct {
        /* keep these sorted by name for easier human comprehension */
        size_t linger;
        size_t time;
        size_t timeval;
        size_t tm;
    } sizes;
#endif
    double align;
};

static const T t1 =
{
    { 0.0, 0.5, 1.0, 2.0, -1.0, -3.0, 12.34, -124.456, 1000.0, -10000.0 },
    { 0.0, 0.5, 1.0, 2.0, -1.0, -3.5, 12.34, -124.456, 1000.0, -10000.0 },
#ifndef _WIN32
    {
    SIZE(linger_t),
    SIZE(time_t),
    SIZE(timeval_t),
    SIZE(tm_t),
    },
#endif
    0
};

void Test__CheckFloatsAndTypes(const T* t2, size_t size, size_t jbsize)
{
#ifndef _WIN32
    if (size != SIZE(t1))
    {
        printf("size: %x vs. %x\n", (U)size, SIZE(t1));
    }
    if (memcmp(&t1, t2, SIZE(t1)) != 0)
    {
        U i = 0;
        U8* c = (U8*)&t1;
        U8* m3 = (U8*)t2;
        printf("FD_SETSIZE 0x%x\n", (U)FD_SETSIZE);
        printf("d[0], d[1]: %x, %x\n", OFFSET(T, d[0]), OFFSET(T, d[1]));
        printf("f[0], f[1]: %x, %x\n", OFFSET(T, f[0]), OFFSET(T, f[1]));
        printf("d[8], d[9]: %x, %x\n", OFFSET(T, d[8]), OFFSET(T, d[9]));
        printf("linger: %x\n", OFFSET(T, sizes.linger));
        printf("time: %x\n", OFFSET(T, sizes.time));
        printf("timeval: %x\n", OFFSET(T, sizes.timeval));
        printf("tm: %x\n", OFFSET(T, sizes.tm));
        for (i = 0; i < SIZE(t1); ++i)
        {
            if (c[i] != m3[i])
            {
                printf("different at offset 0x%x (c:%x m3:%x)\n", i, c[i], m3[i]);
            }
        }
        assert(memcmp(&t1, t2, SIZE(t1)) == 0);
    }
    assert(size == SIZE(t1));
#endif /* Win32 */

    assert(memcmp(&t1.d, &t2->d, SIZE(t1.d)) == 0);
    assert(memcmp(&t1.f, &t2->f, SIZE(t1.f)) == 0);

#if defined(_WIN32)
    assert(jbsize >= SIZE(jmp_buf));
#elif defined(__CYGWIN__)
    assert(jbsize >= (SIZE(jmp_buf) / 4));
#elif defined(__FreeBSD__) && defined(__i386__)
    assert(jbsize == SIZE(jmp_buf) || (jbsize + 4) == SIZE(jmp_buf));
#elif defined(__sun) || defined(__APPLE__)
    assert(jbsize == SIZE(jmp_buf) || jbsize == SIZE(sigjmp_buf));
#else
    if (jbsize != SIZE(jmp_buf))
    {
        fprintf(stderr, "%x vs. %x\n", (U)jbsize, SIZE(jmp_buf));
        assert(jbsize == SIZE(jmp_buf));
    }
#endif
}
