#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

TEXT
__cdecl
M3CC__UInt64ToText(UINT64 a, WORD_T base);

#if 0
#include <stdio.h>
#include <string.h>
#define M3toC__StoT(a) (a)
#define M3toC__CopyStoT(a) (strdup(a))
int main()
{
    int i = { 0 };
    printf("%s\n", M3CC__UInt64ToText(0, 2));
    printf("%s\n", M3CC__UInt64ToText(255, 16));
    printf("%s\n", M3CC__UInt64ToText(9, 8));
    for (i = 0; i < 37; ++i)
        printf("%s\n", M3CC__UInt64ToText(i, 36));
}
#endif

TEXT
__cdecl
M3CC__UInt64ToText(UINT64 a, WORD_T base)
{
    assert(base > 1 && base <= 36);
    if (a == 0)
        return M3toC__StoT("0");
    else
    {
        char buf[65];
        unsigned i = 65;
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
        return M3toC__CopyStoT(&buf[i]);
    }
}

#ifdef __cplusplus
} /* extern "C" */
#endif
