#include <stddef.h>

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef __cplusplus
extern "C" {
#endif

size_t
__cdecl
MxConfigC__ifdef_win32(void)
{
#ifdef _WIN32
    return 1;
#else
    return 0;
#endif
}

#ifdef __cplusplus
} // extern "C"
#endif
