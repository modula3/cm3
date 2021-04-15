#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
#define MAP_ANON MAP_ANONYMOUS
#endif

void*
__cdecl
RTOS__GetMemory(WORD_T size)
{
    // TODO autoconf/make HAVE_MMAP
#if defined(ULTRIX)                           || \
    defined(ultrix)                           || \
    defined(__ultrix)                         || \
    defined(__ultrix__)                       || \
    /* AIX386    */                              \
    /* IBMR2     */                              \
    /* IBMRT     */                              \
    /* IRIX5     */                              \
    /* SPARC SunOS 4.x */                        \
    (defined(__x86_64__) && defined(__linux))
    // This is the historic and more portable implementation.
    // http://modula3.elegosoft.com/cgi-bin/cvsweb.cgi/cm3/m3-libs/m3core/src/runtime/POSIX/RTOS.m3?rev=1.1;content-type=text%2Fplain
    //
    // TODO change Linux/amd64 and all systems with mmap to mmap, once
    // the garbage collector is changed to a multi-level array.
    //
    // sbrk is considered out of date but has the advantage of providing denser results.
    //
    // Anecdotal evidence:
    // https://github.com/modula3/cm3/commit/518f93e67ed8f3291a5cd5cf0a39d1fefbc79969
    // https://github.com/modula3/cm3/commit/384b3cc05fcedf4307f077f49cde3d6675b039c6
    return sbrk(size);

#else

    return mmap(0, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);

#endif
}

#ifdef __cplusplus
} // extern "C"
#endif
