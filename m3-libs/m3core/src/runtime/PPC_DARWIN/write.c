#include "wrap.h"
#include <sys/types.h>
#include <unistd.h>

size_t
m3_write(int fd, const void *buf, int nbytes)
{
  return write(fd, buf, nbytes);
}
