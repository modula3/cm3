#include "wrap.h"
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>

int
m3_select(int nfds, fd_set *readfds, fd_set *writefds,
  fd_set *exceptfds, struct timeval *timeout)
{
  return select(nfds, readfds, writefds, exceptfds, timeout);
}
