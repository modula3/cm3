typedef void (*ForkHandler)(void);
 
#include <stddef.h>
#ifndef _WIN32
#include <pthread.h>
#include <errno.h>
#include <unistd.h>
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* NOTE: Even userthreads now depends
 * on availability of pthreads.
 * This can be fixed if need be.
 */

ptrdiff_t
__cdecl
RTProcess__RegisterForkHandlers(
    ForkHandler prepare,
    ForkHandler parent,
    ForkHandler child)
{
#ifdef _WIN32
    return 0;
#else
    while (1)
    {
      int i = pthread_atfork(prepare, parent, child);
      if (i != EAGAIN)
        return i;
      sleep(0);
    }
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
