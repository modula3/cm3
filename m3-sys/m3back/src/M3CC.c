#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

TEXT
__cdecl
M3CC__UInt64ToText(UINT64 a, WORD_T base)
{
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
            buf[--i] = ((c <= 9) ? (c + '0') : (c - 10 + 'A'));
        } while(a);
        return M3toC__CopyStoT(&buf[i]);
    }
}

#ifdef __cplusplus
} /* extern "C" */
#endif
