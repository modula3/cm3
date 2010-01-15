#include <stddef.h>
#include <limits.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/stat.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32

ptrdiff_t Utils__GetFileSize(const char* a)
{
    WIN32_FIND_DATAA data;
    HANDLE h = FindFirstFileA(a, &data);
    if (h == INVALID_HANDLE_VALUE)
        return 0;
    FindClose(h);
    if (data.nFileSizeHigh || data.nFileSizeLow > INT_MAX)
        return 0;
    return (ptrdiff_t)data.nFileSizeLow;
}

#else

ptrdiff_t Utils__GetFileSize(const char* a)
{
    struct stat st;
    if (stat(a, &st) || st.st_size > INT_MAX)
        return 0;
    return (ptrdiff_t)st.st_size;
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
