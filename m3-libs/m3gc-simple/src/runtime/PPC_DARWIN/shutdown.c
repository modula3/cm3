#include "wrap.h"
#include <sys/types.h>
#include <sys/socket.h>

int
m3_shutdown(int s, int how)
{
  return shutdown(s, how);
}
