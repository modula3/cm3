#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

int
m3_accept(int s, struct sockaddr *addr, int *addrlen)
{
  return accept(s, addr, addrlen);
}
