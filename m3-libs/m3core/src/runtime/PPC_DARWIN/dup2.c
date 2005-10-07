#include "wrap.h"
#include <unistd.h>

int
m3_dup2(int oldd, int newd)
{
  return dup2(oldd, newd);
}
