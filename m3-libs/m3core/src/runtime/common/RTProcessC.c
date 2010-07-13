typedef void (*ForkHandler)(void);
 
#include "m3core.h"

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
#if defined(_WIN32) \
        || defined(__vms) \
        || (defined(__FreeBSD__) && (__FreeBSD__ < 6))
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
