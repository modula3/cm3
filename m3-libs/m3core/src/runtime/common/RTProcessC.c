typedef void (*ForkHandler)(void);
 
#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

/* NOTE: Even userthreads now depends
 * on availability of pthreads.
 * This can be fixed if need be.
 */

INTEGER
__cdecl
RTProcess__RegisterForkHandlers(
    ForkHandler prepare,
    ForkHandler parent,
    ForkHandler child)
{
/* FreeBSD < 6 lacks pthread_atfork. Would be good to use autoconf.
 * VMS lacks pthread_atfork? Would be good to use autoconf.
 * Win32 lacks pthread_atfork and fork. OK.
 * OpenBSD pthread_atfork causes us to need libpthread, and then
 * sigsuspend on 4.6/x86 hangs in the userthread code.
 * 
 * I expect therefore cvsup is broken with user threads on these systems.
 *
 * OpenBSD we could fix by going back to jmpbuf hacking (see 5.8.6 release),
 * but it is nice to have portable code, and cvsup maybe is expendable (again,
 * only on OpenBSD).
 *
 * As well, for all Posix systems, we could implement
 * atfork ourselves, as long as we provide a fork()
 * wrapper that code uses.
 */
#if defined(_WIN32) \
        || defined(__vms) \
        || defined(__OpenBSD__) \
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
