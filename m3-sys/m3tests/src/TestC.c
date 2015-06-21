/* Copyright (C) 1994, Digital Equipment Corporation. */
/* All rights reserved.                               */
/* See the file COPYRIGHT for a full description.     */


typedef   signed char       INT8;
typedef unsigned char      UINT8;
typedef   signed short      INT16;
typedef unsigned short     UINT16;
typedef   signed int        INT32;
typedef unsigned int       UINT32;
#if defined(_MSC_VER) || defined(__DECC) || defined(__DECCXX) || defined(__int64)
typedef   signed __int64  INT64;
typedef unsigned __int64 UINT64;
#else
typedef   signed long long  INT64;
typedef unsigned long long UINT64;
#endif

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

#endif

#ifdef _MSC_VER
#pragma warning(disable:4035) /* no return value */
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef __cplusplus
extern "C" {
#endif

struct T
{
    double d[10];
    float f[10];
    double align;
};

static const T t1 =
{
    { 0.0, 0.5, 1.0, 2.0, -1.0, -3.0, 12.34, -124.456, 1000.0, -10000.0 },
    { 0.0, 0.5, 1.0, 2.0, -1.0, -3.5, 12.34, -124.456, 1000.0, -10000.0 },
    0
};

void __cdecl Test__CheckFloatsAndTypes(const T* t2, size_t size)
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
}

/* Test sign/zero extension of
 * the return value of functions returning types
 * smaller than 32bits. It is most effective
 * to test it on NT386, esp. with older versions of
 * Visual C++, or hand written assembly.
 */

INT8 __cdecl NegativeInt8(void)
{
#ifdef _M_IX86
    __asm mov al, -1
#else
    return -1;
#endif
}

UINT8 __cdecl NegativeUInt8(void)
{
#ifdef _M_IX86
    __asm mov al, -2
#else
    return -2;
#endif
}

INT16 __cdecl NegativeInt16(void)
{
#ifdef _M_IX86
    __asm mov ax, -3
#else
    return -3;
#endif
}

UINT16 __cdecl NegativeUInt16(void)
{
#ifdef _M_IX86
    __asm mov ax, -4
#else
    return -4;
#endif
}

INT32 __cdecl NegativeInt32(void)
{
    return -5;
}

UINT32 __cdecl NegativeUInt32(void)
{
    return (UINT32)-6;
}

INT64  __cdecl NegativeInt64(void)
{
    return -7;
}

UINT64 __cdecl NegativeUInt64(void)
{
    return (UINT64)-8;
}

INT8 __cdecl PositiveInt8(void)
{
#ifdef _M_IX86
    __asm mov al, 1
#else
    return 1;
#endif
}

UINT8 __cdecl PositiveUInt8(void)
{
#ifdef _M_IX86
    __asm mov al, 2
#else
    return 2;
#endif
}

INT16 __cdecl PositiveInt16(void)
{
#ifdef _M_IX86
    __asm mov ax, 3
#else
    return 3;
#endif
}

UINT16 __cdecl PositiveUInt16(void)
{
#ifdef _M_IX86
    __asm mov ax, 4
#else
    return 4;
#endif
}

INT32 __cdecl PositiveInt32(void)
{
    return 5;
}

UINT32 __cdecl PositiveUInt32(void)
{
    return 6;
}

INT64  __cdecl PositiveInt64(void)
{
    return 7;
}

UINT64 __cdecl PositiveUInt64(void)
{
    return 8;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
