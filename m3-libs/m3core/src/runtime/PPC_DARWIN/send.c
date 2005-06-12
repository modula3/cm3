#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

ssize_t
m3_send(int s, const void *msg, size_t len, int flags)
{
  return send(s, msg, len, flags);
}
