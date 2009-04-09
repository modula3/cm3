#include <sys/types.h>
#include <libutil.h>

void
m3_setproctitle(const char *title)
{
    setproctitle("%s", title);
}
