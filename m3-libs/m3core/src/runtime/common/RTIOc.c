#ifdef _WIN32 /* Do not accidentally export printf. This will break p227/p228. */

char RTIOc_avoid_empty_file_warning_unique_0671f719f3a74ea2b7ba81b160caa215;

#else

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef _WIN32
#define I64 "I64"
#else
#define I64 "ll"
#endif

#ifdef __cplusplus
extern "C" {
#endif

void __cdecl RTIO__Flush(void);

void __cdecl RTIO__PutE(double a)
{
    RTIO__Flush();
    printf("%e", a);
    fflush(NULL);
}

void __cdecl RTIO__PutF(double a)
{
    RTIO__Flush();
    printf("%f", a);
    fflush(NULL);
}

void __cdecl RTIO__PutG(double a)
{
    RTIO__Flush();
    printf("%g", a);
    fflush(NULL);
}

void __cdecl RTIO__PutBytes(ADDRESS addr, INTEGER icount)
{
    size_t const count = (size_t)icount; // Modula-3 lacks unsigned types, pass as signed and cast.
    unsigned char const * const p = (const unsigned char*)addr;
    char buffer[33]; /* size must be odd */
    const static char hex[] = "0123456789ABCDEF";
    size_t i = { 0 };
    size_t j = { 0 };
    
    RTIO__Flush();
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

void __cdecl RTIO__PutLong(LONGINT i)
{
    RTIO__Flush();
    printf("%" I64 "d", i);
    fflush(NULL);
}

void __cdecl RTIO__PutLongHex(LONGINT i)
{
    RTIO__Flush();
    printf("0x%" I64 "x", (UINT64)i);
    fflush(NULL);
}

#ifdef __cplusplus
} /* extern C */
#endif

#endif
