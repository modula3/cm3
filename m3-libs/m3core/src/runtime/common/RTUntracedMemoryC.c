/*
This is a thin layer over malloc/calloc/free.
It exists strictly so that other allocators might be used instead,
such as on Windows going directly to HeapAlloc(GetProcessHeap()) and
reducing C runtime dependency.
*/

#include <stddef.h>

#ifdef _MSC_VER
#pragma warning(disable:4115) /* named type definition in paren (windows.h) */
#pragma warning(disable:4131) /* old style */
#pragma warning(disable:4201) /* nonstandard extension (windows.h) */
#pragma warning(disable:4214) /* nonstandard extension (windows.h) */
#pragma warning(disable:4514) /* unreferenced inline function */
#else
#ifndef __cdecl
#define __cdecl /* nothing */
#endif
#endif

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
#define PROTO1(t1, a1) (t1 a1)
#define PROTO2(t1, a1, t2, a2) (t1 a1, t2 a2)
#define VOID_VALUE /* nothing */
#else
#define void char
#define PROTO1(t1, a1) (a1) t1 a1;
#define PROTO2(t1, a1, t2, a2) (a1, a2) t1 a1; t2 a2;
#define VOID_VALUE 0
#endif

#ifdef __cplusplus
extern "C" {
#endif

void* __cdecl RTUntracedMemory__AllocZ PROTO1(size_t, count)
/* Z = zeroed = calloc */
{
    return WIN(HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, count))
           POSIX(calloc(count, 1));
}

void* __cdecl RTUntracedMemory__AllocZV PROTO2(size_t, count, size_t, size)
/* ZV = zeroed vector = calloc */
{
    size_t max = ~(size_t)0;
    if (count > 1 && size > 1 && count > (max / size)) /* implies count * size > max */
        return 0;
    return WIN(HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, count))
           POSIX(calloc(count, size));
}

void __cdecl RTUntracedMemory__Free PROTO1(void**, p)
{
    void* q = *p;
    *p = 0;
    if (q)
        WIN(HeapFree(GetProcessHeap(), 0, q))
        POSIX(free(q));
    return VOID_VALUE;
}

#ifdef __cplusplus
} /* extern C */
#endif
