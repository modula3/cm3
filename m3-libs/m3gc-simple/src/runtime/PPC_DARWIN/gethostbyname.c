#include "wrap.h"
#include <netdb.h>

struct hostent *
m3_gethostbyname(const char *name)
{
  return gethostbyname(name);
}
