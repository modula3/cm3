#include "wrap.h"
#include <sys/types.h>
#include <unistd.h>

ssize_t
m3_read(int d, void *buf, size_t nbytes)
{
  return read(d, buf, nbytes);
}
