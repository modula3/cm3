#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

ssize_t
m3_sendto(int s, const void *msg, size_t len, int flags,
  const struct sockaddr *to, int tolen)
{
  return sendto(s, msg, len, flags, to, tolen);
}
