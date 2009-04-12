#ifdef __FreeBSD__
#include <sys/types.h>
#include <libutil.h>
#endif

void m3_setproctitle(const char *title)
{
#ifdef __FreeBSD__
    setproctitle("%s", title);
#endif
}
