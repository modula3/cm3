#include "wrap.h"
#include <unistd.h>

int
m3_dup(int oldd)
{
  return dup(oldd);
}
