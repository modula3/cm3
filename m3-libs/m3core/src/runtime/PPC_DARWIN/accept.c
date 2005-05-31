#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

int
m3_accept(int s, struct sockaddr *addr, socklen_t *addrlen)
{
  return accept(s, addr, addrlen);
}
