#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

// TODO: Consolidate RTOSs.c
BOOLEAN __cdecl RTOS__Cygwin(void)
{
#ifdef __CYGWIN__
    return TRUE;
#else
    return FALSE;
#endif
}

#ifndef _WIN32

#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
#define MAP_ANON MAP_ANONYMOUS
#endif

ADDRESS
__cdecl
RTOS__GetMemory(INTEGER isize)
{
    WORD_T const size = (WORD_T)isize; // Modula-3 lacks unsigned types, pass as signed and cast.
    void* p;
    Scheduler__DisableSwitching ();
    // TODO autoconf/make HAVE_MMAP
#if defined(ULTRIX)                           || \
    defined(ultrix)                           || \
    defined(__ultrix)                         || \
    defined(__ultrix__)                       || \
    defined(__DJGPP__)                           \
    /* AIX386    */                              \
    /* IBMR2     */                              \
    /* IBMRT     */                              \
    /* IRIX5     */                              \
    /* SPARC SunOS 4.x */
    // This is the historic implementation, kept only for systems without
    // mmap(MAP_ANON).  Nowhere else: sbrk moves the same program break as
    // malloc, which serializes its own calls under its arena lock but not
    // ours.  With threads, an sbrk here can land between malloc's check of
    // the break and its trim of it (glibc systrim), and one heap overwrites
    // or unmaps the other.
    //
    // AMD64_LINUX used sbrk from 2008 (384b3cc05f) because mmap'd chunks
    // were then too spread out for the collector's per-page side table.
    // Linux now places successive mmaps next to each other, so the heap
    // stays dense.
    //
    // History:
    // https://github.com/modula3/cm3/commit/518f93e67ed8f3291a5cd5cf0a39d1fefbc79969
    // https://github.com/modula3/cm3/commit/384b3cc05fcedf4307f077f49cde3d6675b039c6
    // http://modula3.elegosoft.com/cgi-bin/cvsweb.cgi/cm3/m3-libs/m3core/src/runtime/POSIX/RTOS.m3?rev=1.1;content-type=text%2Fplain
    p = sbrk(size);

#else

    p = mmap(0, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);

#endif
    Scheduler__EnableSwitching ();
    return (ADDRESS)p;
}

#endif

#ifdef __cplusplus
} // extern "C"
#endif
