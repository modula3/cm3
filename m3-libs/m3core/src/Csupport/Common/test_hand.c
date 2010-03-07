/* Copyright (C) 1992, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Thu Feb  1 09:36:52 PST 1996 by heydon  */
/*      modified on Tue Jan 10 15:48:28 PST 1995 by kalsow  */
/*      modified on Tue Feb 11 15:18:40 PST 1992 by muller  */

#include "hand.c"

#ifdef __cplusplus
extern "C"
{           
#endif

M3_EXTRACT_INSERT(m3test_extract32, m3test_extract_and_sign_extend32, m3test_insert32, uint32)

#ifdef _MSC_VER
#if _MSC_VER < 1000
#pragma warning(disable:4702) /* unreachable code (due to assertion) */
#endif
#endif

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <time.h>

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

static int errors_div;
static int errors_mod;

static void TestDiv64(int64 a, int64 b)
{
    if (a == INT64_MIN && b == -1) /* avoid overflow */
        return;
    if (b)
    {
        int64 old = m3_divL_old(b, a);
        int64 current = m3_div64(b, a);
        errors_div += (current != old);
        if ((b < 0) == (a < 0))
        {
            assert(old >= 0 || (old == -current && a == INT64_MIN && b < 0)); /* bug in old version */
            if (current < 0)
            {
                printf("%"I64"d / %"I64"d = current:%"I64"d old:%"I64"d\n", a, b, current, old);
            }
            assert(current >= 0);
        }
        else
        {
            assert(old <= 0);
            assert(current <= 0);
        }
    }
}

static void TestDivx(int64 a, int64 b)
{
    if (b)
    {
        TestDiv64(a, b);
    }
    if (a)
    {
        TestDiv64(b, a);
    }
}

static void TestDiv(void)
{
    long a, b;
    long n = sizeof(values) / sizeof(values[0]);
    
#if 1
    for (a = -1000; a < 1000; ++a)
    {
        for (b = -1000; b < 1000; ++b)
        {
            TestDivx(a, b);
            TestDivx((a > 0) ? (LONG_MAX - a) : (LONG_MIN + a), (b > 0) ? (LONG_MIN + b) : (LONG_MIN - b));
        }
    }
#endif

    for (a = 0; a < n; ++a)
        for (b = 0; b < n; ++b)
            TestDivx(values[a], values[b]);
}

static void TestMod64(int64 a, int64 b)
{
    int64 old, current;
    if ((a == INT64_MIN && b == -1) || b == 0) /* avoid overflow */
        return;
    old = m3_modL_old(b, a);
    current = m3_mod64(b, a);
    errors_mod += (old != current);
    /* old version is wrong for INT64_MIN mod negative */
    if (a != INT64_MIN || b >= 0 || old == current)
    {
        assert(old == current);
        assert((b < 0) ? (old > b && old <= 0) : (old < b && old >= 0));
        assert(old == a - b * m3_div64(b, a));
    }
    assert(current == a - b * m3_div64(b, a));
    assert((b < 0) ? (current > b && current <= 0) : (current < b && current >= 0));
}

static void TestModx(int64 a, int64 b)
{
    if (b)
    {
        TestMod64(a, b);
    }
    if (a)
    {
        TestMod64(b, a);
    }
}

static void TestMod(void)
{
    long a, b;
    long n = sizeof(values) / sizeof(values[0]);
#if 1
    for (a = -1000; a < 1000; ++a)
        for (b = -1000; b < 1000; ++b)
        {
            TestModx(a, b);
            TestModx((a > 0) ? (LONG_MAX - a) : (LONG_MIN + a), (b > 0) ? (LONG_MIN + b) : (LONG_MIN - b));
        }
#endif

    for (a = 0; a < n; ++a)
        for (b = 0; b < n; ++b)
            TestModx(values[a], values[b]);
    
    srand((unsigned)time(0));
    for (a = 0; a < 10000000; ++a)
        TestModx(rand(), rand());
}

static void TestHighLowBits(void)
{
    unsigned i;

    if (sizeof(int) == sizeof(size_t))
    {
        for (i = 0; i <= 32; ++i)
        {
            assert(_LOWBITS(i) == _lowbits[i]);
            assert(_HIGHBITS(i) == _highbits[i]);
        }
    }
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
    uint32 sign_extend = { 0 };
    unsigned m = { 0 };
    unsigned n = { 0 };

    for (a32 = 0; a32 <= 33; ++a32)
    {
        for (m = 0; m <= 10; ++m)
        {
            for (n = 0; n <= 10; ++n)
            {
                for (sign_extend = 0; sign_extend < 2; ++sign_extend)
                {
                    uint32 result;
                    if (sign_extend)
                        result = m3test_extract_and_sign_extend32(a32, m, n);
                    else
                        result = m3test_extract32(a32, m, n);
                    printf("extract32(value:0x%"I64"x, m:0x%"I64"x, n:0x%"I64"x, sign_extend:0x%"I64"x):0x%"I64"x\n", 
                            (uint64)a32,
                            (uint64)m,
                            (uint64)n,
                            (uint64)sign_extend,
                            (uint64)result);
                    if (n == 0)
                        assert(result == 0);
                }
            }
        }
    }

    for (a64 = 0; a64 <= 33; ++a64)
    {
        for (m = 0; m <= 10; ++m)
        {
            for (n = 0; n <= 10; ++n)
            {
                for (sign_extend = 0; sign_extend < 2; ++sign_extend)
                {
                    uint64 result;
                    if (sign_extend)
                        result = m3_extract_and_sign_extend64(a64, m, n);
                    else
                        result = m3_extract64(a64, m, n);
                    printf("extract64(value:0x%"I64"x, m:0x%"I64"x, n:0x%"I64"x, sign_extend:0x%"I64"x):0x%"I64"x\n", 
                            (uint64)a64,
                            (uint64)m,
                            (uint64)n,
                            (uint64)sign_extend,
                            (uint64)result);
                    if (n == 0)
                        assert(result == 0);
                }
            }
        }
    }
}

int main()
{
    /*TestDiv();*/
    /*TestMod();*/
    /*printf("errors_div:%d errors_mod:%d\n", errors_div, errors_mod);*/

    TestHighLowBits();

    /*TestInsert();*/
    /*TestExtract();*/

    return 0;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
