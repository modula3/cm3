#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

int
m3_bind(int s, const struct sockaddr *name, int namelen)
{
  return bind(s, name, namelen);
}
