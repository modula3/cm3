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

static void BuildTables ()
{
	unsigned i;
	ulong LoBits[SET_GRAIN] = { 0 };  /* LoBits32[i] = SET { 0..i } */
	ulong HiBits[SET_GRAIN] = { 0 };  /* HiBits32[i] = SET { i..31 } */
	uint32 LoBits32[32] = { 0 };  /* LoBits32[i] = SET { 0..i } */
	uint32 HiBits32[32] = { 0 };  /* HiBits32[i] = SET { i..31 } */
	uint64 LoBits64[64] = { 0 };  /* LoBits64[i] = SET { 0..i } */
	uint64 HiBits64[64] = { 0 };  /* HiBits64[i] = SET { i..63 } */

	{
		uint32 j;

		/* LoBits [i] = SET { 0..i } */
		j = 0;  /* == SET { } */
		for (i = 0; i != 32; i++) {
			j = (j << 1) + 1;
			LoBits32[i] = j;
		}

		/* HiBits [i] = SET { i..GRAIN-1 } */
		j = ~ (uint32) 0; /* == SET { 0..GRAIN-1 } */
		for (i = 0; i != 32; i++) {
			HiBits32[i] = j;
			j = (j << 1);
		}
	}

	{
		uint64 j;

		/* LoBits [i] = SET { 0..i } */
		j = 0;  /* == SET { } */
		for (i = 0; i != 64; i++) {
			j = (j << 1) + 1;
			LoBits64[i] = j;
		}

		/* HiBits [i] = SET { i..GRAIN-1 } */
		j = ~ (uint64) 0; /* == SET { 0..GRAIN-1 } */
		for (i = 0; i != 64; i++) {
			HiBits64[i] = j;
			j = (j << 1);
		}
	}

	{
		ulong j;

		/* LoBits [i] = SET { 0..i } */
		j = 0;  /* == SET { } */
		for (i = 0; i != SET_GRAIN; i++) {
			j = (j << 1) + 1;
			LoBits[i] = j;
		}

		/* HiBits [i] = SET { i..GRAIN-1 } */
		j = ~ (ulong) 0; /* == SET { 0..GRAIN-1 } */
		for (i = 0; i != SET_GRAIN; i++) {
			HiBits[i] = j;
			j = (j << 1);
		}

		for (i = 0; i != SET_GRAIN; i++) {
#ifdef _WIN32
			assert(LoBits[i] == LoBits32[i]);
			assert(HiBits[i] == HiBits32[i]);
			assert(LoBits[i] == _lowbits[i + 1]);
			assert(HiBits[i] == _highbits[i]);
#else
			assert((LoBits[i] == LoBits32[i]) || (LoBits[i] == LoBits64[i]));
			assert((HiBits[i] == HiBits32[i]) || (HiBits[i] == HiBits64[i]));
#endif
		}
	}

	printf("#include <limits.h>\n\n");

	printf("typedef unsigned long ulong;\n\n");

	printf("#if ULONG_MAX == 0xffffffff\n\n");

	printf("static const ulong LoBits[] = {\n");

	for (i = 0; i != 32; i++)
		printf("0x%08lx%s%s", (ulong) LoBits32[i], &","[i == 31], &"\n"[!!((i + 1) % 4)]);

	printf("};\n\nstatic const ulong HiBits[] = {\n");

	for (i = 0; i != 32; i++)
		printf("0x%08lx%s%s", (ulong) HiBits32[i], &","[i == 31], &"\n"[!!((i + 1) % 4)]);

	printf("};\n\n");
	printf("#elif ULONG_MAX == 0xffffffffffffffff\n\n");

	printf("static const ulong LoBits[] = {\n");

	for (i = 0; i != 64; i++)
		printf("0x%08lx%08lx%s%s", (ulong) (LoBits64[i] >> 32), (ulong) LoBits64[i], &","[i == 63], &"\n"[!!((i + 1) % 4)]);

	printf("};\n\nstatic const ulong HiBits[] = {\n");

	for (i = 0; i != 64; i++)
		printf("0x%08lx%08lx%s%s", (ulong) (HiBits64[i] >> 32), (ulong) HiBits64[i], &","[i == 63], &"\n"[!!((i + 1) % 4)]);

	printf("\n#else\n#error unknown size of ulong\n#endif\n\n");
}

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

    for (i = 0; i < SET_GRAIN; ++i)
    {
        assert(HIGH_BITS(i) == HiBits[i]);
        assert(LOW_BITS(i) == LoBits[i + LOW_BITS_ADJUST]);
    }
	
    for (i = 0; i <= 32; ++i)
    {
        assert(_LOWBITS(i) == _lowbits[i]);
        assert(_HIGHBITS(i) == _highbits[i]);
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

static void PrintSet(void* a, ulong b)
{
    ulong c;
    for (c = 0; c < b; ++c)
        printf("%02x ", reverse(((unsigned char*)a)[c]));
}

static void TestSetRangex(unsigned a, unsigned b)
{
    ulong bits[4];
    ulong bits_new[4];

    memset(bits, 0, sizeof(bits));
    memset(bits_new, 0, sizeof(bits_new));
    set_range(a, b, bits);
    set_range_new(a, b, bits_new);
    assert(memcmp(bits, bits_new, sizeof(bits)) == 0);
    /*
    printf("set_range(%u, %u):", a, b);
    PrintSet(bits, sizeof(bits));
    printf("\n");
    */
}

static void TestSetRange(void)
{
    ulong bits[100];
    unsigned a, b;
    double t1 = 0, t2 = 0, t3 = 0, t4 = 0, t5 = 0;

    for (a = 0; a < 4 * SET_GRAIN; ++a)
        for (b = 0; b < 4 * SET_GRAIN; ++b)
            TestSetRangex(a, b);

#ifdef _MSC_VER
    t1 = (double)__rdtsc(); /* read time stamp counter */
#endif

    for (a = 0; a < 100 * SET_GRAIN; ++a)
        for (b = 0; b < 100 * SET_GRAIN; ++b)
            set_range(a, b, bits);

#ifdef _MSC_VER
    t2 = (double)__rdtsc();
#endif

    for (a = 0; a < 100 * SET_GRAIN; ++a)
        for (b = 0; b < 100 * SET_GRAIN; ++b)
            set_range_new(a, b, bits);

#ifdef _MSC_VER
    t3 = (double)__rdtsc();
#endif

    t4 = (t2 - t1);
    t5 = (t3 - t2);

    printf("old:%f\n", t4);
    printf("new:%f\n", t5);
    printf("diff:%f\n", (t5 - t4) / t4);
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
    /*BuildTables();*/
    /*TestDiv();*/
    /*TestMod();*/
    /*printf("errors_div:%d errors_mod:%d\n", errors_div, errors_mod);*/

    TestHighLowBits();

    /*TestSetRange();*/

    /*TestInsert();*/
    /*TestExtract();*/

    return 0;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
