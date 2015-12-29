#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef int BOOL;
#define FALSE 0
#define TRUE 1

TEXT
__cdecl
M3CC__IntToText(UINT64 a, WORD_T base, BOOL neg);

#if 0
#include <stdio.h>
#include <string.h>
#define M3toC__StoT(a) (a)
#define M3toC__CopyStoT(a) (strdup(a))
int main()
{
    int i = { 0 };
    printf("%s\n", M3CC__UInt64ToText(0, 2, FALSE));
    printf("%s\n", M3CC__UInt64ToText(255, 16, FALSE));
    printf("%s\n", M3CC__UInt64ToText(9, 8, FALSE));
    for (i = 0; i < 37; ++i)
        printf("%s\n", M3CC__UInt64ToText(i, 36, FALSE));
}
#endif

TEXT
__cdecl
M3CC__IntToText(UINT64 a, WORD_T base, BOOL neg)
{
    base &= 0xFF;
    assert(base > 1 && base <= 36);
    if (a == 0)
        return M3toC__StoT("0");
    else
    {
        char buf[66];
        unsigned i = 66;
        base &= 0xFF;
        buf[--i] = 0;
        do
        {
            unsigned c = (unsigned)(a % (unsigned)base);
            a /= base;
#if 0 /* EBCDIC: 'A' - 'Z' are not adjacent */
            buf[--i] = ((c <= 9) ? (c + '0') : (c - 10 + 'A'));
#else
            buf[--i] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[c];
#endif
        } while(a);
        if (neg)
          buf[--i] = '-';
        return M3toC__CopyStoT(&buf[i]);
    }
}

UINT64 __cdecl M3CC__Abs(INTEGER a)
{
    if (a < 0)
      return ((UINT64)(-(a + 1))) + 1;
    return (UINT64)a;
}
TEXT __cdecl M3CC__IntToDec(INTEGER a)
{
    return M3CC__IntToText(M3CC__Abs(a), 10, a < 0);
}

TEXT __cdecl M3CC__IntToHex(INTEGER a)
{
    return M3CC__IntToText(M3CC__Abs(a), 16, a < 0);
}

TEXT __cdecl M3CC__UIntToHex(INTEGER a)
{
    return M3CC__IntToText((UINT64)(size_t)a, 16, FALSE);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
