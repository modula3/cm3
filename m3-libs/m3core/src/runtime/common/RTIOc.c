#include <stdio.h>
#include <string.h>

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define Flush   RTIO__Flush
#define PutE    RTIO__PutE
#define PutF    RTIO__PutF
#define PutG    RTIO__PutG
#define PutBytes RTIO__PutBytes

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

void __cdecl RTIO__PutBytes(const unsigned char* p, size_t count)
{
    char buffer[33];
    const static char hex[] = "0123456789ABCDEF";
    size_t i = { 0 };
    size_t j = { 0 };
    
    RTIO__Flush();
    for (i = 0; i < count; ++i)
    {
        unsigned char c = p[i];
        buffer[j++] = hex[(c >> 4) & 0xF];
        buffer[j++] = hex[c & 0xF];
        if (j >= (sizeof(buffer) - 1))
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

#ifdef __cplusplus
} /* extern C */
#endif
