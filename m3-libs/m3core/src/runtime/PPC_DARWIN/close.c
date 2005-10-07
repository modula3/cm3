#include "wrap.h"
#include <unistd.h>

int
m3_close(int d)
{
  return close(d);
}
