#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

int
m3_connect(int s, const struct sockaddr *name, int namelen)
{
  return connect(s, name, namelen);
}
