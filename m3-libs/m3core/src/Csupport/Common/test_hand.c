/* Copyright (C) 1992, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Thu Feb  1 09:36:52 PST 1996 by heydon  */
/*      modified on Tue Jan 10 15:48:28 PST 1995 by kalsow  */
/*      modified on Tue Feb 11 15:18:40 PST 1992 by muller  */

#define M3_EXTRACT_INSERT_LINKAGE static /* for testing */

#include "hand.c"

#ifdef __cplusplus
extern "C"
{
#endif

M3_EXTRACT_INSERT(m3test_extract32, m3test_insert32, uint32)

#ifdef _MSC_VER
#if _MSC_VER < 1000
#pragma warning(disable:4702) /* unreachable code (due to assertion) */
#endif
#endif

#ifdef _MSC_VER
#define I64 "I64"
#else
#define I64 "ll"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <time.h>
#include <assert.h>

#if !defined(INT64_MAX)
#if defined(LLONG_MAX)
#define INT64_MAX LLONG_MAX
#elif defined(__LONG_LONG_MAX__)
#define INT64_MAX __LONG_LONG_MAX__
#elif defined(_I64_MAX)
#define INT64_MAX _I64_MAX
#endif
#endif

#if !defined(INT64_MIN)
#if defined(LLONG_MIN)
#define INT64_MIN LLONG_MIN
#elif defined(__LONG_LONG_MIN__)
#define INT64_MIN __LONG_LONG_MIN__
#elif defined(_I64_MIN)
#define INT64_MIN _I64_MIN
#elif defined(INT64_MAX)
#define INT64_MIN (-INT64_MAX-(int64)1)
#endif
#endif

static int64 values[] = {
    INT64_MIN,
    INT64_MIN + 1,
    INT64_MAX,
    INT64_MAX - 1,
    INT64_MAX / 2,
    INT64_MAX / 2 - 1,
    INT64_MAX / 2 + 1,
    INT64_MIN / 2,
    INT64_MIN / 2 + 1,
    INT64_MIN / 2 - 1,
    LONG_MIN,
    LONG_MIN + 1,
    LONG_MAX,
    LONG_MAX - 1,
    LONG_MAX / 2,
    LONG_MAX / 2 - 1,
    LONG_MAX / 2 + 1,
    LONG_MIN / 2,
    LONG_MIN / 2 + 1,
    LONG_MIN / 2 - 1,
    -10, -9, -8, -7, -6, -5, -4, -3, -2, -1,
    0,
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    99, 100, 101,
    -99, -100, -101,
    990, 1000, 1010,
    -990, -1000, -1010,
    99999,11111,22222,3333,444,55555,
    -99999,-11111,-22222,-3333,-444,-55555,
    0x99999,0x11111,0x22222,0x3333,0x444,0x55555,
    -0x99999,-0x11111,-0x22222,-0x3333,-0x444,-0x55555,
    0xFF, 0xFFFF, 0xFFFFFFFF, 0x8000,
};

static void TestHighLowBits(void)
{
#ifdef _WIN32
    unsigned i;

    if (sizeof(int) == sizeof(size_t))
    {
        for (i = 0; i <= 32; ++i)
        {
            assert(_LOWBITS(i) == _lowbits[i]);
            assert(_HIGHBITS(i) == _highbits[i]);
        }
    }
#endif
}

static unsigned char reverse(unsigned char a)
{
    return ((a & 0x80) ? 0x01 : 0)
         | ((a & 0x40) ? 0x02 : 0)
         | ((a & 0x20) ? 0x04 : 0)
         | ((a & 0x10) ? 0x08 : 0)
         | ((a & 0x08) ? 0x10 : 0)
         | ((a & 0x04) ? 0x20 : 0)
         | ((a & 0x02) ? 0x40 : 0)
         | ((a & 0x01) ? 0x80 : 0);
}

static void TestInsert()
{
    uint32 a32 = { 0 };
    uint32 b32 = { 0 };
    uint64 a64 = { 0 };
    uint64 b64 = { 0 };
    unsigned m = { 0 };
    unsigned n = { 0 };

    for (a32 = 0; a32 <= 33; ++a32)
    {
        for (b32 = 0; b32 <= 33; ++b32)
        {
            for (m = 0; m <= 10; ++m)
            {
                for (n = 0; n <= 10; ++n)
                {
                    uint32 result = m3test_insert32(a32, b32, m, n);
                    printf("insert32(a:0x%"I64"x, b:0x%"I64"x, m:0x%"I64"x, n:0x%"I64"x):0x%"I64"x\n",
                            (uint64)a32,
                            (uint64)b32,
                            (uint64)m,
                            (uint64)n,
                            (uint64)result);
                    if (n == 0)
                        assert(result == a32);
                }
            }
        }
    }

    for (a64 = 0; a64 <= 33; ++a64)
    {
        for (b64 = 0; b64 <= 33; ++b64)
        {
            for (m = 0; m <= 10; ++m)
            {
                for (n = 0; n <= 10; ++n)
                {
                    uint64 result = m3_insert64(a64, b64, m, n);
                    printf("insert64(a:0x%"I64"x, b:0x%"I64"x, m:0x%"I64"x, n:0x%"I64"x):0x%"I64"x\n",
                            (uint64)a64,
                            (uint64)b64,
                            (uint64)m,
                            (uint64)n,
                            (uint64)result);
                    if (n == 0)
                        assert(result == a64);
                }
            }
        }
    }
}

static void TestExtract()
{
    uint32 a32 = { 0 };
    uint64 a64 = { 0 };
    unsigned m = { 0 };
    unsigned n = { 0 };

    for (a32 = 0; a32 <= 33; ++a32)
    {
        for (m = 0; m <= 10; ++m)
        {
            for (n = 0; n <= 10; ++n)
            {
                uint32 result = m3test_extract32(a32, m, n);
                printf("extract32(value:0x%"I64"x, m:0x%"I64"x, n:0x%"I64"x):0x%"I64"x\n",
                        (uint64)a32,
                        (uint64)m,
                        (uint64)n,
                        (uint64)result);
                if (n == 0)
                    assert(result == 0);
            }
        }
    }

    for (a64 = 0; a64 <= 33; ++a64)
    {
        for (m = 0; m <= 10; ++m)
        {
            for (n = 0; n <= 10; ++n)
            {
                uint64 result = m3_extract64(a64, m, n);
                printf("extract64(value:0x%"I64"x, m:0x%"I64"x, n:0x%"I64"x):0x%"I64"x\n",
                        (uint64)a64,
                        (uint64)m,
                        (uint64)n,
                        (uint64)result);
                if (n == 0)
                    assert(result == 0);
            }
        }
    }
}

int main()
{
    /*TestInsert();*/
    TestExtract();

    return 0;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
