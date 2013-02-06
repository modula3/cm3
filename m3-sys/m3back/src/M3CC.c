#include "m3core.h"

M3_EXTERN_C_BEGIN

TEXT
__cdecl
M3toC__StoT(const char*);

TEXT
__cdecl
M3CC__UInt64ToText(UINT64 a, WORD_T base)
{
    if (a == 0)
        return M3toC__StoT("0");
    else
    {
        char buf[65] = { 0 };
        unsigned i = 64;
        base &= 0xFF;
        do
        {
            unsigned c = (unsigned)(a % (unsigned)base);
            a /= base;
            buf[--i] = ((c <= 9) ? (c + '0') : (c - 10 + 'A'));
        } while(a);
        return M3toC__CopyStoT(buf);
    }
}

M3_EXTERN_C_END
