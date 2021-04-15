/*
This is a thin layer over malloc/calloc/free.
It exists strictly so that other allocators might be used instead,
such as on Windows going directly to HeapAlloc(GetProcessHeap()) and
reducing C runtime dependency.
*/

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef _MSC_VER
#pragma warning(disable:4115) /* named type definition in paren (windows.h) */
#pragma warning(disable:4131) /* old style */
#pragma warning(disable:4201) /* nonstandard extension (windows.h) */
#pragma warning(disable:4214) /* nonstandard extension (windows.h) */
#pragma warning(disable:4514) /* unreferenced inline function */
#endif

#ifdef _WIN32
#define WIN(x) x
#define POSIX(x) /* nothing */
#else
#define WIN(x) /* nothing */
#define POSIX(x) x
#endif

#ifdef __cplusplus
extern "C" {
#endif

void* __cdecl RTUntracedMemory__AllocZ(WORD_T count)
/* Z = zeroed = calloc */
{
    return WIN(HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, count))
           POSIX(calloc(count, 1));
}

void* __cdecl RTUntracedMemory__AllocZV(WORD_T count, WORD_T size)
/* ZV = zeroed vector = calloc */
{
    WORD_T max = ~(WORD_T)0;
    if (count > 1 && size > 1 && count > (max / size)) /* implies count * size > max */
        return 0;
    return WIN(HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, count * size))
           POSIX(calloc(count, size));
}

void __cdecl RTUntracedMemory__Free(void** p)
{
    void* q = *p;
    *p = 0;
    if (q)
        WIN(HeapFree(GetProcessHeap(), 0, q))
        POSIX(free(q));
}

#ifdef __cplusplus
} /* extern C */
#endif
