#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

int
m3_getpeername(int s, struct sockaddr *name, socklen_t *namelen)
{
  return getpeername(s, name, namelen);
}
