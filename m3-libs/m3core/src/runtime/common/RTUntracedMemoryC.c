/*
This is a thin layer over malloc/calloc/free.
It exists strictly so that other allocators might be used instead,
such as on Windows going directly to HeapAlloc(GetProcessHeap()) and
reducing C runtime dependency.
*/

#include <stddef.h>

#ifdef _WIN32
#include <windows.h>
#define WIN(x) x
#define POSIX(x) /* nothing */
#else
#include <stdlib.h>
#define WIN(x) /* nothing */
#define POSIX(x) x
#endif

#if defined(__STDC__) || defined(__cplusplus) || defined(_MSC_VER)
#define ANSI(x) x
#define KR(x)
#else
#define ANSI(x)
#define KR(x) x
#define void char
#endif

#define COMMA ,

#ifdef __cplusplus
extern "C" {
#endif


void* RTUntracedMemory__AllocZ ANSI((size_t count)) KR((count) size_t count;)
/* Z = zeroed = calloc */
{
    return WIN(HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, count))
           POSIX(calloc(count, 1));
}

void* RTUntracedMemory__AllocZV ANSI((size_t count, size_t size)) KR((count, size) size_t count COMMA size;)
/* ZV = zeroed vector = calloc */
{
    size_t max = (~(size_t)0);
    if ((count > 1) && (size > 1) && count > max / size) /* implies count * size > max */
        return 0;
#ifdef _WIN32
    return WIN(HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, count));
#else
    return calloc(count, size);
#endif
}

void RTUntracedMemory__Free ANSI((void** p)) KR((p) void** p;)
{
    void* q = *p;
    *p = 0;
    if (q)
        WIN(HeapFree(GetProcessHeap(), 0, q))
        POSIX(free(q));
    KR(return 0;)
}

#ifdef __cplusplus
} /* extern C */
#endif
