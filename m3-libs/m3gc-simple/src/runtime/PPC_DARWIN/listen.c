#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

int
m3_listen(int s, int backlog)
{
  return listen(s, backlog);
}
