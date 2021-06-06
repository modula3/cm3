/* Do not accidentally export printf.
   This will break p227/p228. */
#ifndef _WIN32

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef _MSC_VER
typedef __int64 int64;
typedef unsigned __int64 uint64;
#else
typedef long long int64;
typedef unsigned long long uint64;
#endif

#ifdef _WIN32
#define I64 "I64"
#else
#define I64 "ll"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define Flush       RTIO__Flush
#define PutE        RTIO__PutE
#define PutF        RTIO__PutF
#define PutG        RTIO__PutG
#define PutBytes    RTIO__PutBytes
#define PutLong     RTIO__PutLong
#define PutLongHex  RTIO__PutLongHex

void __cdecl Flush(void);

void __cdecl PutE(double a)
{
    Flush();
    printf("%e", a);
    fflush(NULL);
}

void __cdecl PutF(double a)
{
    Flush();
    printf("%f", a);
    fflush(NULL);
}

void __cdecl PutG(double a)
{
    Flush();
    printf("%g", a);
    fflush(NULL);
}

void __cdecl PutBytes(ADDRESS addr, INTEGER icount)
{
    WORD_T const count = (WORD_T)icount; // Modula-3 lacks unsigned types, pass as signed and cast.
    unsigned char const * const p = (const unsigned char*)addr;
    char buffer[33]; /* size must be odd */
    const static char hex[] = "0123456789ABCDEF";
    WORD_T i = { 0 };
    WORD_T j = { 0 };
    
    Flush();
    for (i = 0; i < count; ++i)
    {
        unsigned char c = p[i];
        buffer[j++] = hex[(c >> 4) & 0xF];
        buffer[j++] = hex[c & 0xF];
        if (j == (sizeof(buffer) - 1))
        {
            buffer[j++] = 0;
            printf("%s", buffer);
            j = 0;
        }
    }
    buffer[j++] = 0;
    printf("%s", buffer);
    fflush(NULL);
}

void __cdecl RTIO__PutLong(int64 i)
{
    Flush();
    printf("%" I64 "d", i);
    fflush(NULL);
}

void __cdecl RTIO__PutLongHex(uint64 i)
{
    Flush();
    printf("0x%" I64 "x", i);
    fflush(NULL);
}

#ifdef __cplusplus
} /* extern C */
#endif

#endif
