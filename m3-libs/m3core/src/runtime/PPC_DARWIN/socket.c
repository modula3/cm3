#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

int
m3_socket(int domain, int type, int protocol)
{
  return socket(domain, type, protocol);
}
